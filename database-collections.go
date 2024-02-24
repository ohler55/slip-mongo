// Copyright (c) 2024, Peter Ohler, All rights reserved.

package main

import (
	"context"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/mongo"
)

type databaseCollectionsCaller struct{}

func (caller databaseCollectionsCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	db := self.Any.(*mongo.Database)

	ctx, cf := context.WithTimeout(context.Background(), instTimeout(self))
	defer cf()

	var filter any
	if 0 < len(args) {
		filter = filterFromArg(args[0])
	}
	if filter == nil {
		filter = bson.D{}
	}
	result, err := db.ListCollectionSpecifications(ctx, filter)
	if err != nil {
		panic(err)
	}
	assoc := make(slip.List, 0, len(result))
	for _, spec := range result {
		var readOnly slip.Object
		if spec.ReadOnly {
			readOnly = slip.True
		}
		typ := slip.Symbol(":collection")
		if spec.Type == "view" {
			typ = slip.Symbol(":view")
		}
		assoc = append(assoc, slip.List{slip.String(spec.Name), typ, readOnly})
	}
	return assoc
}

func (caller databaseCollectionsCaller) Docs() string {
	return `__:collections__ &optional _filter_ => _list_
   _filter_ [bag|assoc|string] to be applied to the collections list. A _string_ value is taken as a regexp.


Returns an associate list of all collections that match the _filter_. Each
element of the returned list has a _car_ of the collection name followed by
the type which can be either :collection or :view and then a read-only flag,
`
}
