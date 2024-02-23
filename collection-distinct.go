// Copyright (c) 2024, Peter Ohler, All rights reserved.

package main

import (
	"context"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/bson/primitive"
	"go.mongodb.org/mongo-driver/mongo"
)

type collectionDistinctCaller struct{}

func (caller collectionDistinctCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)

	field := slip.MustBeString(args[0], "field")
	filter := ToBson(args[1])
	if _, ok := filter.(primitive.Null); ok || filter == nil {
		filter = bson.D{}
	}
	ctx, cf := context.WithTimeout(context.Background(), instTimeout(self))
	defer cf()
	values, err := self.Any.(*mongo.Collection).Distinct(ctx, field, filter)
	if err != nil {
		panic(err)
	}
	list := make(slip.List, len(values))
	for i, v := range values {
		list[i] = BsonToObject(v, false)
	}
	return list
}

func (caller collectionDistinctCaller) Docs() string {
	return `__:distinct__ _field_ _filter_ => _list_
   _field_ [string|symbol] name to find distinct values of.
   _filter_ [bag|list] for the search.


Executes a distinct on the collection for the _field_ and _filter_ specified and returns a list
of all the distinct values of the _field_ in the matching records.
`
}
