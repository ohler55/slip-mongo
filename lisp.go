// Copyright (c) 2024, Peter Ohler, All rights reserved.

package main

import (
	"embed"
	"io"
	"io/fs"
	"strings"

	"github.com/ohler55/slip"
)

//go:embed lisp
var lispFS embed.FS

func loadLisp() {
	cp := slip.CurrentPackage
	defer func() {
		slip.CurrentPackage = cp
	}()
	slip.CurrentPackage = &Pkg

	entries, err := lispFS.ReadDir("lisp")
	if err != nil {
		panic(err)
	}
	for _, e := range entries {
		if !e.IsDir() && strings.HasSuffix(e.Name(), ".lisp") {
			var f fs.File
			if f, err = lispFS.Open("lisp/" + e.Name()); err != nil {
				panic(err)
			}
			var code []byte
			code, err = io.ReadAll(f)
			_ = f.Close()
			if err != nil {
				panic(err)
			}
			_ = slip.Compile(code)
		}
	}
}
