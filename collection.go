// Copyright (c) 2024, Peter Ohler, All rights reserved.

package main

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

var (
	collectionFlavor *flavors.Flavor
)

func init() {
	collectionFlavor = flavors.DefFlavor("mongo-collection",
		map[string]slip.Object{
			"client":   nil,
			"database": nil,
		}, // instance variables
		[]string{}, // inherited flavors
		slip.List{
			slip.List{
				slip.Symbol(":documentation"),
				slip.String(`A representative for a mongodb collection.`),
			},
			slip.Symbol(":gettable-instance-variables"),
		},
	)
	collectionFlavor.GoMakeOnly = true

	flavors.FlosFun("mongo-collection-client", ":client",
		"returns the client used to create the database of the collection instance")
	flavors.FlosFun("mongo-collection-database", ":database",
		"returns the database used to create the collection instance")

	collectionFlavor.DefMethod(":name", "", collectionNameCaller{})
	flavors.FlosFun("mongo-collection-name", ":name", collectionNameCaller{}.Docs())

	collectionFlavor.DefMethod(":drop", "", collectionDropCaller{})
	flavors.FlosFun("mongo-collection-drop", ":drop", collectionDropCaller{}.Docs())

	// collectionFlavor.DefMethod(":collection", "", collectionCollectionCaller{})
	// flavors.FlosFun("mongo-collection-collection", ":collection", collectionCollectionCaller{}.Docs())

	// collectionFlavor.DefMethod(":aggregate", "", collectionAggregateCaller{})
	// flavors.FlosFun("mongo-collection-aggregate", ":aggregate", collectionAggregateCaller{}.Docs())

	// collectionFlavor.DefMethod(":create-view", "", collectionCreateViewCaller{})
	// flavors.FlosFun("mongo-collection-create-view", ":create-view", collectionCreateViewCaller{}.Docs())

	// collectionFlavor.DefMethod(":collections", "", collectionCollectionsCaller{})
	// flavors.FlosFun("mongo-collection-collections", ":collections", collectionCollectionsCaller{}.Docs())

	// collectionFlavor.DefMethod(":watch", "", collectionWatchCaller{})
	// flavors.FlosFun("mongo-collection-watch", ":watch", collectionWatchCaller{}.Docs())
}
