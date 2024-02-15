// Copyright (c) 2024, Peter Ohler, All rights reserved.

package main_test

import (
	"fmt"
	"net"
	"os"
	"os/exec"
	"strconv"
	"syscall"
	"testing"
	"time"
)

var (
	mongoURL string
	mongoPID int
)

func TestMain(m *testing.M) {
	status := wrapRun(m)

	os.Exit(status)
}

func wrapRun(m *testing.M) (status int) {
	proc := startMongoServer()
	defer func() {
		if rec := recover(); rec != nil {
			fmt.Printf("*-*-* panic: %s\n", rec)
			status = 1
		}
		if proc != nil {
			done := make(chan int)
			go func() {
				ps, _ := proc.Wait()
				done <- ps.ExitCode()
			}()
			if err := proc.Signal(syscall.SIGTERM); err != nil {
				fmt.Println("*-*-* failed to shutdown mongodb with a SIGTERM")
				return
			}
			select {
			case <-done:
			case <-time.After(time.Second * 5):
				_ = proc.Kill()
				fmt.Println("mongodb killed")
			}
		}
	}()
	status = m.Run()

	return
}

func startMongoServer() (proc *os.Process) {
	mongoURL, _ = os.LookupEnv("TEST_MONGO_URL")
	if len(mongoURL) == 0 {
		dbpath := "mongo-data"
		_ = os.RemoveAll(dbpath)
		if err := os.Mkdir(dbpath, 0666); err != nil {
			panic(err)
		}
		port := availablePort()
		mongoURL = fmt.Sprintf("mongodb://localhost:%d", port)
		cmd := exec.Command("mongod", "-dbpath", dbpath, "-port", strconv.Itoa(port))
		if err := cmd.Start(); err != nil {
			panic(err)
		}
		proc = cmd.Process
	}
	return
}

func availablePort() int {
	addr, err := net.ResolveTCPAddr("tcp", "localhost:0")
	if err != nil {
		panic(err)
	}
	var listener *net.TCPListener
	if listener, err = net.ListenTCP("tcp", addr); err != nil {
		panic(err)
	}
	defer listener.Close()

	return listener.Addr().(*net.TCPAddr).Port
}
