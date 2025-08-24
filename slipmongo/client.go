// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slipmongo

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

var (
	clientFlavor *flavors.Flavor
)

func initClient() {
	clientFlavor = flavors.DefFlavor("mongo-client",
		map[string]slip.Object{}, // instance variables
		[]string{},               // inherited flavors
		slip.List{
			slip.List{
				slip.Symbol(":documentation"),
				slip.String(`A connection to a mongo database connection.
`),
			},
		},
		&Pkg,
	)
	// clientFlavor.GoMakeOnly = true

	clientFlavor.DefMethod(":ping", "", clientPingCaller{})
	flavors.FlosFun("mongo-ping", ":ping", clientPingCaller{}.FuncDocs(), &Pkg)

	clientFlavor.DefMethod(":disconnect", "", clientDisconnectCaller{})
	flavors.FlosFun("mongo-disconnect", ":disconnect", clientDisconnectCaller{}.FuncDocs(), &Pkg)

	clientFlavor.DefMethod(":database", "", clientDatabaseCaller{})
	flavors.FlosFun("mongo-database", ":database", clientDatabaseCaller{}.FuncDocs(), &Pkg)

	clientFlavor.DefMethod(":databases", "", clientDatabasesCaller{})
	flavors.FlosFun("mongo-databases", ":databases", clientDatabasesCaller{}.FuncDocs(), &Pkg)

	// clientFlavor.DefMethod(":watch", "", clientWatchCaller{})
	// flavors.FlosFun("mongo-watch", ":watch", clientWatchCaller{}.Docs(), &Pkg)
}
