// Copyright (c) 2024, Peter Ohler, All rights reserved.

package main

import (
	"github.com/ohler55/slip"
)

var (
	// Pkg is the message package.
	Pkg = slip.Package{
		Name:      "mongo",
		Nicknames: []string{"mongo"},
		Doc:       "Home of symbols defined for the mongodb functions, variables, and constants.",
		PreSet:    slip.DefaultPreSet,
	}
)

func init() {
	slip.AddPackage(&Pkg)
	slip.UserPkg.Use(&Pkg)
}
