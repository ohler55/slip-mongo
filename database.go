// Copyright (c) 2024, Peter Ohler, All rights reserved.

package main

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

var (
	databaseFlavor *flavors.Flavor
)

func init() {
	databaseFlavor = flavors.DefFlavor("mongo-database",
		map[string]slip.Object{
			"client": nil,
		}, // instance variables
		[]string{}, // inherited flavors
		slip.List{
			slip.List{
				slip.Symbol(":documentation"),
				slip.String(`A connection to a mongo database connection.
`),
			},
			slip.Symbol(":gettable-instance-variables"),
		},
	)
	databaseFlavor.GoMakeOnly = true

	databaseFlavor.DefMethod(":name", "", databaseNameCaller{})
	flavors.FlosFun("mongo-database-name", ":name", databaseNameCaller{}.Docs())

	// - :aggregate
	// - :client
	// - :collection (name)
	// - :create-view
	// - :drop
	// - :collections (filter) ;; collection specifications
	// - :watch
}
