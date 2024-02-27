// Copyright (c) 2024, Peter Ohler, All rights reserved.

package test_test

import (
	"bytes"
	"context"
	"fmt"
	"net"
	"os"
	"os/exec"
	"strconv"
	"syscall"
	"testing"
	"time"

	"github.com/ohler55/ojg/tt"
	"go.mongodb.org/mongo-driver/mongo"
	"go.mongodb.org/mongo-driver/mongo/options"
)

var (
	mongoURL string
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
	// export TEST_MONGO_URL=mongodb://localhost:27017 to use a local mongod
	mongoURL, _ = os.LookupEnv("TEST_MONGO_URL")
	if len(mongoURL) == 0 {
		dbpath := "mongo-data"
		_ = os.RemoveAll(dbpath)
		if err := os.Mkdir(dbpath, 0755); err != nil {
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
	ctx, cf := context.WithTimeout(context.Background(), time.Second*10)
	defer cf()

	opts := options.Client()
	opts = opts.ApplyURI(string(mongoURL))
	mc, err := mongo.Connect(ctx, opts)
	if err != nil {
		panic(err)
	}
	start := time.Now()
	for time.Since(start) < time.Second*5 {
		if mc.Ping(ctx, nil) == nil {
			break
		}
		time.Sleep(time.Millisecond * 100)
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

func TestWithSlip(t *testing.T) {
	cmd := exec.Command("go", "run", "cmd/slip/main.go",
		// "-t", // uncomment to trace
		"-e", fmt.Sprintf(`(progn (setq *murl* %q) (run-tests))`, mongoURL),
		"../slip-mongo/test/tests.lisp")

	cmd.Dir = "../../slip"

	out, err := cmd.CombinedOutput()
	if err != nil {
		panic(err)
	}
	if testing.Verbose() || bytes.Contains(out, []byte("FAIL")) {
		fmt.Println(string(out))
	}
	tt.Equal(t, false, bytes.Contains(out, []byte("FAIL")))
}
