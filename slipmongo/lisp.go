// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slipmongo

import (
	"embed"
	"io"

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

	scope := slip.NewScope()
	// Use explicit path names to assure the proper order.
	for _, path := range []string{
		"lisp/actor.lisp",
		"lisp/find-actor.lisp",
		"lisp/insert-actor.lisp",
		"lisp/update-actor.lisp",
		"lisp/replace-actor.lisp",
	} {
		f, err := lispFS.Open(path)
		if err != nil {
			panic(err)
		}
		var code []byte
		code, err = io.ReadAll(f)
		_ = f.Close()
		if err != nil {
			panic(err)
		}
		_ = slip.Read(code, scope).Eval(scope, nil)
	}
}
