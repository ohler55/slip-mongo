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
				slip.String(`A representative for a mongodb database.`),
			},
			slip.Symbol(":gettable-instance-variables"),
		},
	)
	databaseFlavor.GoMakeOnly = true

	flavors.FlosFun("mongo-database-client", ":client", "returns the client used to create the database instance")

	databaseFlavor.DefMethod(":name", "", databaseNameCaller{})
	flavors.FlosFun("mongo-database-name", ":name", databaseNameCaller{}.Docs())

	databaseFlavor.DefMethod(":drop", "", databaseDropCaller{})
	flavors.FlosFun("mongo-database-drop", ":drop", databaseDropCaller{}.Docs())

	databaseFlavor.DefMethod(":collection", "", databaseCollectionCaller{})
	flavors.FlosFun("mongo-database-collection", ":collection", databaseCollectionCaller{}.Docs())

	databaseFlavor.DefMethod(":collections", "", databaseCollectionsCaller{})
	flavors.FlosFun("mongo-database-collections", ":collections", databaseCollectionsCaller{}.Docs())

	// databaseFlavor.DefMethod(":create-view", "", databaseCreateViewCaller{})
	// flavors.FlosFun("mongo-database-create-view", ":create-view", databaseCreateViewCaller{}.Docs())

	// databaseFlavor.DefMethod(":aggregate", "", databaseAggregateCaller{})
	// flavors.FlosFun("mongo-database-aggregate", ":aggregate", databaseAggregateCaller{}.Docs())

	// databaseFlavor.DefMethod(":watch", "", databaseWatchCaller{})
	// flavors.FlosFun("mongo-database-watch", ":watch", databaseWatchCaller{}.Docs())
}
