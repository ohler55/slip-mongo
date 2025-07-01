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
	flavors.FlosFun("mongo-collection-aggregate", ":aggregate", collectionAggregateCaller{}.FuncDocs(), &Pkg)

	flavors.FlosFun("mongo-collection-client", ":client",
		"returns the client used to create the database of the collection instance", &Pkg)

	collectionFlavor.DefMethod(":count-documents", "", collectionCountDocumentsCaller{})
	flavors.FlosFun("mongo-collection-count-documents", ":count-documents",
		collectionCountDocumentsCaller{}.FuncDocs(), &Pkg)

	flavors.FlosFun("mongo-collection-database", ":database",
		"returns the database used to create the collection instance", &Pkg)

	collectionFlavor.DefMethod(":delete-many", "", collectionDeleteManyCaller{})
	flavors.FlosFun("mongo-collection-delete-many", ":delete-many", collectionDeleteManyCaller{}.FuncDocs(), &Pkg)

	collectionFlavor.DefMethod(":delete-one", "", collectionDeleteOneCaller{})
	flavors.FlosFun("mongo-collection-delete-one", ":delete-one", collectionDeleteOneCaller{}.FuncDocs(), &Pkg)

	collectionFlavor.DefMethod(":distinct", "", collectionDistinctCaller{})
	flavors.FlosFun("mongo-collection-distinct", ":distinct", collectionDistinctCaller{}.FuncDocs(), &Pkg)

	collectionFlavor.DefMethod(":drop", "", collectionDropCaller{})
	flavors.FlosFun("mongo-collection-drop", ":drop", collectionDropCaller{}.FuncDocs(), &Pkg)

	collectionFlavor.DefMethod(":estimated-document-count", "", collectionEstimatedDocumentCountCaller{})
	flavors.FlosFun(
		"mongo-collection-estimated-document-count",
		":estimated-document-count",
		collectionEstimatedDocumentCountCaller{}.FuncDocs(), &Pkg)

	collectionFlavor.DefMethod(":find", "", collectionFindCaller{})
	flavors.FlosFun("mongo-collection-find", ":find", collectionFindCaller{}.FuncDocs(), &Pkg)

	collectionFlavor.DefMethod(":find-one", "", collectionFindOneCaller{})
	flavors.FlosFun("mongo-collection-find-one", ":find-one", collectionFindOneCaller{}.FuncDocs(), &Pkg)

	collectionFlavor.DefMethod(":find-one-and-delete", "", collectionFindOneAndDeleteCaller{})
	flavors.FlosFun(
		"mongo-collection-find-one-and-delete",
		":find-one-and-delete",
		collectionFindOneAndDeleteCaller{}.FuncDocs(),
		&Pkg)

	collectionFlavor.DefMethod(":find-one-and-replace", "", collectionFindOneAndReplaceCaller{})
	flavors.FlosFun(
		"mongo-collection-find-one-and-replace",
		":find-one-and-replace",
		collectionFindOneAndReplaceCaller{}.FuncDocs(),
		&Pkg)

	collectionFlavor.DefMethod(":find-one-and-update", "", collectionFindOneAndUpdateCaller{})
	flavors.FlosFun(
		"mongo-collection-find-one-and-update",
		":find-one-and-update",
		collectionFindOneAndUpdateCaller{}.FuncDocs(),
		&Pkg)

	collectionFlavor.DefMethod(":indexes", "", collectionIndexesCaller{})
	flavors.FlosFun("mongo-collection-indexes", ":indexes", collectionIndexesCaller{}.FuncDocs(), &Pkg)

	collectionFlavor.DefMethod(":insert-many", "", collectionInsertManyCaller{})
	flavors.FlosFun("mongo-collection-insert-many", ":insert-many", collectionInsertManyCaller{}.FuncDocs(), &Pkg)

	collectionFlavor.DefMethod(":insert-one", "", collectionInsertOneCaller{})
	flavors.FlosFun("mongo-collection-insert-one", ":insert-one", collectionInsertOneCaller{}.FuncDocs(), &Pkg)

	collectionFlavor.DefMethod(":name", "", collectionNameCaller{})
	flavors.FlosFun("mongo-collection-name", ":name", collectionNameCaller{}.FuncDocs(), &Pkg)

	collectionFlavor.DefMethod(":replace-one", "", collectionReplaceOneCaller{})
	flavors.FlosFun("mongo-collection-replace-one", ":replace-one", collectionReplaceOneCaller{}.FuncDocs(), &Pkg)

	// collectionFlavor.DefMethod(":search-indexes", "", collectionSearchIndexesCaller{})
	// flavors.FlosFun("mongo-collection-search-indexes", ":search-indexes", collectionSearchIndexesCaller{}.Docs(), &Pkg)

	collectionFlavor.DefMethod(":update-by-id", "", collectionUpdateByIDCaller{})
	flavors.FlosFun("mongo-collection-update-by-id", ":update-by-id", collectionUpdateByIDCaller{}.FuncDocs(), &Pkg)

	collectionFlavor.DefMethod(":update-many", "", collectionUpdateManyCaller{})
	flavors.FlosFun("mongo-collection-update-many", ":update-many", collectionUpdateManyCaller{}.FuncDocs(), &Pkg)

	collectionFlavor.DefMethod(":update-one", "", collectionUpdateOneCaller{})
	flavors.FlosFun("mongo-collection-update-one", ":update-one", collectionUpdateOneCaller{}.FuncDocs(), &Pkg)

	// collectionFlavor.DefMethod(":watch", "", collectionWatchCaller{})
	// flavors.FlosFun("mongo-collection-watch", ":watch", collectionWatchCaller{}.FuncDocs(), &Pkg)
}
