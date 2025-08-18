// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slipmongo

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/bag"
	"github.com/ohler55/slip/pkg/clos"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/pkg/generic"
	"github.com/ohler55/slip/pkg/gi"
)

// Pkg is the message package.
var Pkg = slip.Package{
	Name:      "mongo",
	Nicknames: []string{"mongo"},
	Doc:       "Home of symbols defined for the mongodb functions, variables, and constants.",
	PreSet:    slip.DefaultPreSet,
}

func init() {
	Pkg.Initialize(nil)
	slip.AddPackage(&Pkg)
	initIndexes()
	initCollection()
	initDatabase()
	initClient()
	initConnect()
	// Loading the LISP file requires functions in several packages so have
	// this package use the necessary ones.
	Pkg.Use(&slip.CLPkg)
	Pkg.Use(&gi.Pkg)
	Pkg.Use(&bag.Pkg)
	Pkg.Use(&flavors.Pkg)
	Pkg.Use(&clos.Pkg)
	Pkg.Use(&generic.Pkg)
	loadLisp()
	// Add to user package after finishing loading the LISP files.
	slip.UserPkg.Use(&Pkg)
}
