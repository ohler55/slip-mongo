// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slipmongo

import (
	"context"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"go.mongodb.org/mongo-driver/v2/bson"
	"go.mongodb.org/mongo-driver/v2/mongo"
)

type collectionDistinctCaller struct{}

func (caller collectionDistinctCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)

	field := slip.MustBeString(args[0], "field")
	filter := ToBson(args[1])
	if _, ok := filter.(bson.Null); ok || filter == nil {
		filter = bson.D{}
	}
	ctx, cf := context.WithTimeout(context.Background(), timeoutFromArgs(s, args[2:], depth))
	defer cf()
	dr := self.Any.(*mongo.Collection).Distinct(ctx, field, filter)
	if dr.Err() != nil {
		panic(dr.Err())
	}
	var values []any
	_ = dr.Decode(&values)
	list := make(slip.List, len(values))
	for i, v := range values {
		list[i] = BsonToObject(v, false)
	}
	return list
}

func (caller collectionDistinctCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":distinct",
		Text: `Executes a distinct on the collection for the _field_ and _filter_ specified
and returns a list of all the distinct values of the _field_ in the matching records.`,
		Args: []*slip.DocArg{
			{
				Name: "field",
				Type: "string|symbol",
				Text: "Field name to find distinct values of.",
			},
			{
				Name: "filter",
				Type: "bag|list",
				Text: "Filter for the search.",
			},
			{Name: "&key"},
			{
				Name: "timeout",
				Type: "fixnum",
				Text: "is the number of seconds to wait before giving up",
			},
		},
		Return: "list",
	}
}
