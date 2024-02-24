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
	Pkg.Initialize(nil)
	databaseFlavor = flavors.DefFlavor("mongo-database",
		map[string]slip.Object{
			"client": nil,
		}, // instance variables
		[]string{}, // inherited flavors
		slip.List{
			slip.List{
				slip.Symbol(":documentation"),
				slip.String(`A representative for a mongodb database.`),
			},
			slip.Symbol(":gettable-instance-variables"),
		},
		&Pkg,
	)
	databaseFlavor.GoMakeOnly = true

	flavors.FlosFun("mongo-database-client", ":client", "returns the client used to create the database instance", &Pkg)

	databaseFlavor.DefMethod(":name", "", databaseNameCaller{})
	flavors.FlosFun("mongo-database-name", ":name", databaseNameCaller{}.Docs(), &Pkg)

	databaseFlavor.DefMethod(":drop", "", databaseDropCaller{})
	flavors.FlosFun("mongo-database-drop", ":drop", databaseDropCaller{}.Docs(), &Pkg)

	databaseFlavor.DefMethod(":collection", "", databaseCollectionCaller{})
	flavors.FlosFun("mongo-database-collection", ":collection", databaseCollectionCaller{}.Docs(), &Pkg)

	databaseFlavor.DefMethod(":collections", "", databaseCollectionsCaller{})
	flavors.FlosFun("mongo-database-collections", ":collections", databaseCollectionsCaller{}.Docs(), &Pkg)

	// databaseFlavor.DefMethod(":create-view", "", databaseCreateViewCaller{})
	// flavors.FlosFun("mongo-database-create-view", ":create-view", databaseCreateViewCaller{}.Docs(), &Pkg)

	// databaseFlavor.DefMethod(":aggregate", "", databaseAggregateCaller{})
	// flavors.FlosFun("mongo-database-aggregate", ":aggregate", databaseAggregateCaller{}.Docs(), &Pkg)

	// databaseFlavor.DefMethod(":watch", "", databaseWatchCaller{})
	// flavors.FlosFun("mongo-database-watch", ":watch", databaseWatchCaller{}.Docs(), &Pkg)
}
