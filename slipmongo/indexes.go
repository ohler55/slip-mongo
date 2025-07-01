// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slipmongo

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

var (
	indexesFlavor *flavors.Flavor
)

func initIndexes() {
	indexesFlavor = flavors.DefFlavor("mongo-indexes",
		map[string]slip.Object{}, // instance variables
		[]string{},               // inherited flavors
		slip.List{
			slip.List{
				slip.Symbol(":documentation"),
				slip.String(`A view onto indexes in a collection that can be used to list, create,
and remove indexes on a collection.
`),
			},
		},
		&Pkg,
	)
	indexesFlavor.GoMakeOnly = true

	indexesFlavor.DefMethod(":create", "", indexesCreateCaller{})
	flavors.FlosFun("mongo-indexes-create", ":create", indexesCreateCaller{}.FuncDocs(), &Pkg)

	indexesFlavor.DefMethod(":drop", "", indexesDropCaller{})
	flavors.FlosFun("mongo-indexes-drop", ":drop", indexesDropCaller{}.FuncDocs(), &Pkg)

	indexesFlavor.DefMethod(":list", "", indexesListCaller{})
	flavors.FlosFun("mongo-indexes-list", ":list", indexesListCaller{}.FuncDocs(), &Pkg)
}
