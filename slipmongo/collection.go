// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slipmongo

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

var (
	collectionFlavor *flavors.Flavor
)

func initCollection() {
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
		&Pkg,
	)
	collectionFlavor.GoMakeOnly = true

	collectionFlavor.DefMethod(":aggregate", "", collectionAggregateCaller{})
	flavors.FlosFun("mongo-collection-aggregate", ":aggregate", collectionAggregateCaller{}.Docs(), &Pkg)

	flavors.FlosFun("mongo-collection-client", ":client",
		"returns the client used to create the database of the collection instance", &Pkg)

	collectionFlavor.DefMethod(":count-documents", "", collectionCountDocumentsCaller{})
	flavors.FlosFun("mongo-collection-count-documents", ":count-documents",
		collectionCountDocumentsCaller{}.Docs(), &Pkg)

	flavors.FlosFun("mongo-collection-database", ":database",
		"returns the database used to create the collection instance", &Pkg)

	collectionFlavor.DefMethod(":delete-many", "", collectionDeleteManyCaller{})
	flavors.FlosFun("mongo-collection-delete-many", ":delete-many", collectionDeleteManyCaller{}.Docs(), &Pkg)

	collectionFlavor.DefMethod(":delete-one", "", collectionDeleteOneCaller{})
	flavors.FlosFun("mongo-collection-delete-one", ":delete-one", collectionDeleteOneCaller{}.Docs(), &Pkg)

	collectionFlavor.DefMethod(":distinct", "", collectionDistinctCaller{})
	flavors.FlosFun("mongo-collection-distinct", ":distinct", collectionDistinctCaller{}.Docs(), &Pkg)

	collectionFlavor.DefMethod(":drop", "", collectionDropCaller{})
	flavors.FlosFun("mongo-collection-drop", ":drop", collectionDropCaller{}.Docs(), &Pkg)

	collectionFlavor.DefMethod(":estimated-document-count", "", collectionEstimatedDocumentCountCaller{})
	flavors.FlosFun(
		"mongo-collection-estimated-document-count",
		":estimated-document-count",
		collectionEstimatedDocumentCountCaller{}.Docs(), &Pkg)

	collectionFlavor.DefMethod(":find", "", collectionFindCaller{})
	flavors.FlosFun("mongo-collection-find", ":find", collectionFindCaller{}.Docs(), &Pkg)

	collectionFlavor.DefMethod(":find-one", "", collectionFindOneCaller{})
	flavors.FlosFun("mongo-collection-find-one", ":find-one", collectionFindOneCaller{}.Docs(), &Pkg)

	collectionFlavor.DefMethod(":find-one-and-delete", "", collectionFindOneAndDeleteCaller{})
	flavors.FlosFun(
		"mongo-collection-find-one-and-delete",
		":find-one-and-delete",
		collectionFindOneAndDeleteCaller{}.Docs(),
		&Pkg)

	collectionFlavor.DefMethod(":find-one-and-replace", "", collectionFindOneAndReplaceCaller{})
	flavors.FlosFun(
		"mongo-collection-find-one-and-replace",
		":find-one-and-replace",
		collectionFindOneAndReplaceCaller{}.Docs(),
		&Pkg)

	collectionFlavor.DefMethod(":find-one-and-update", "", collectionFindOneAndUpdateCaller{})
	flavors.FlosFun(
		"mongo-collection-find-one-and-update",
		":find-one-and-update",
		collectionFindOneAndUpdateCaller{}.Docs(),
		&Pkg)

	collectionFlavor.DefMethod(":indexes", "", collectionIndexesCaller{})
	flavors.FlosFun("mongo-collection-indexes", ":indexes", collectionIndexesCaller{}.Docs(), &Pkg)

	collectionFlavor.DefMethod(":insert-many", "", collectionInsertManyCaller{})
	flavors.FlosFun("mongo-collection-insert-many", ":insert-many", collectionInsertManyCaller{}.Docs(), &Pkg)

	collectionFlavor.DefMethod(":insert-one", "", collectionInsertOneCaller{})
	flavors.FlosFun("mongo-collection-insert-one", ":insert-one", collectionInsertOneCaller{}.Docs(), &Pkg)

	collectionFlavor.DefMethod(":name", "", collectionNameCaller{})
	flavors.FlosFun("mongo-collection-name", ":name", collectionNameCaller{}.Docs(), &Pkg)

	collectionFlavor.DefMethod(":replace-one", "", collectionReplaceOneCaller{})
	flavors.FlosFun("mongo-collection-replace-one", ":replace-one", collectionReplaceOneCaller{}.Docs(), &Pkg)

	// collectionFlavor.DefMethod(":search-indexes", "", collectionSearchIndexesCaller{})
	// flavors.FlosFun("mongo-collection-search-indexes", ":search-indexes", collectionSearchIndexesCaller{}.Docs(), &Pkg)

	collectionFlavor.DefMethod(":update-by-id", "", collectionUpdateByIDCaller{})
	flavors.FlosFun("mongo-collection-update-by-id", ":update-by-id", collectionUpdateByIDCaller{}.Docs(), &Pkg)

	collectionFlavor.DefMethod(":update-many", "", collectionUpdateManyCaller{})
	flavors.FlosFun("mongo-collection-update-many", ":update-many", collectionUpdateManyCaller{}.Docs(), &Pkg)

	collectionFlavor.DefMethod(":update-one", "", collectionUpdateOneCaller{})
	flavors.FlosFun("mongo-collection-update-one", ":update-one", collectionUpdateOneCaller{}.Docs(), &Pkg)

	// collectionFlavor.DefMethod(":watch", "", collectionWatchCaller{})
	// flavors.FlosFun("mongo-collection-watch", ":watch", collectionWatchCaller{}.Docs(), &Pkg)
}
