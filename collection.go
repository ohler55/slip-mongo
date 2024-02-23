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

	// collectionFlavor.DefMethod(":aggregate", "", collectionAggregateCaller{})
	// flavors.FlosFun("mongo-collection-aggregate", ":aggregate", collectionAggregateCaller{}.Docs())

	// collectionFlavor.DefMethod(":bulk-write", "", collectionBulkWriteCaller{})
	// flavors.FlosFun("mongo-collection-bulk-write", ":bulk-write", collectionBulkWriteCaller{}.Docs())

	// collectionFlavor.DefMethod(":clone", "", collectionCloneCaller{})
	// flavors.FlosFun("mongo-collection-clone", ":clone", collectionCloneCaller{}.Docs())

	collectionFlavor.DefMethod(":count-documents", "", collectionCountDocumentsCaller{})
	flavors.FlosFun("mongo-collection-count-documents", ":count-documents", collectionCountDocumentsCaller{}.Docs())

	collectionFlavor.DefMethod(":delete-many", "", collectionDeleteManyCaller{})
	flavors.FlosFun("mongo-collection-delete-many", ":delete-many", collectionDeleteManyCaller{}.Docs())

	collectionFlavor.DefMethod(":delete-one", "", collectionDeleteOneCaller{})
	flavors.FlosFun("mongo-collection-delete-one", ":delete-one", collectionDeleteOneCaller{}.Docs())

	collectionFlavor.DefMethod(":distinct", "", collectionDistinctCaller{})
	flavors.FlosFun("mongo-collection-distinct", ":distinct", collectionDistinctCaller{}.Docs())

	collectionFlavor.DefMethod(":estimated-document-count", "", collectionEstimatedDocumentCountCaller{})
	flavors.FlosFun(
		"mongo-collection-estimated-document-count",
		":estimated-document-count",
		collectionEstimatedDocumentCountCaller{}.Docs())

	collectionFlavor.DefMethod(":find", "", collectionFindCaller{})
	flavors.FlosFun("mongo-collection-find", ":find", collectionFindCaller{}.Docs())

	collectionFlavor.DefMethod(":find-one", "", collectionFindOneCaller{})
	flavors.FlosFun("mongo-collection-find-one", ":find-one", collectionFindOneCaller{}.Docs())

	// collectionFlavor.DefMethod(":find-one-and-delete", "", collectionFindOneAndDeleteCaller{})
	// flavors.FlosFun(
	// 	"mongo-collection-find-one-and-delete",
	// 	":find-one-and-delete",
	// 	collectionFindOneAndDeleteCaller{}.Docs())

	// collectionFlavor.DefMethod(":find-one-and-replace", "", collectionFindOneAndReplaceCaller{})
	// flavors.FlosFun(
	// 	"mongo-collection-find-one-and-replace",
	// 	":find-one-and-replace",
	// 	collectionFindOneAndReplaceCaller{}.Docs())

	// collectionFlavor.DefMethod(":find-one-and-update", "", collectionFindOneAndUpdateCaller{})
	// flavors.FlosFun(
	// 	"mongo-collection-find-one-and-update",
	// 	":find-one-and-update",
	// 	collectionFindOneAndUpdateCaller{}.Docs())

	// collectionFlavor.DefMethod(":indexes", "", collectionIndexesCaller{})
	// flavors.FlosFun("mongo-collection-indexes", ":indexes", collectionIndexesCaller{}.Docs())

	collectionFlavor.DefMethod(":insert-many", "", collectionInsertManyCaller{})
	flavors.FlosFun("mongo-collection-insert-many", ":insert-many", collectionInsertManyCaller{}.Docs())

	collectionFlavor.DefMethod(":insert-one", "", collectionInsertOneCaller{})
	flavors.FlosFun("mongo-collection-insert-one", ":insert-one", collectionInsertOneCaller{}.Docs())

	collectionFlavor.DefMethod(":replace-one", "", collectionReplaceOneCaller{})
	flavors.FlosFun("mongo-collection-replace-one", ":replace-one", collectionReplaceOneCaller{}.Docs())

	// collectionFlavor.DefMethod(":search-indexes", "", collectionSearchIndexesCaller{})
	// flavors.FlosFun("mongo-collection-search-indexes", ":search-indexes", collectionSearchIndexesCaller{}.Docs())

	collectionFlavor.DefMethod(":update-by-id", "", collectionUpdateByIDCaller{})
	flavors.FlosFun("mongo-collection-update-by-id", ":update-by-id", collectionUpdateByIDCaller{}.Docs())

	collectionFlavor.DefMethod(":update-many", "", collectionUpdateManyCaller{})
	flavors.FlosFun("mongo-collection-update-many", ":update-many", collectionUpdateManyCaller{}.Docs())

	collectionFlavor.DefMethod(":update-one", "", collectionUpdateOneCaller{})
	flavors.FlosFun("mongo-collection-update-one", ":update-one", collectionUpdateOneCaller{}.Docs())

	// collectionFlavor.DefMethod(":watch", "", collectionWatchCaller{})
	// flavors.FlosFun("mongo-collection-watch", ":watch", collectionWatchCaller{}.Docs())
}
