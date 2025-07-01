// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slipmongo

import (
	"context"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/bson/primitive"
	"go.mongodb.org/mongo-driver/mongo"
	"go.mongodb.org/mongo-driver/mongo/options"
)

type collectionCountDocumentsCaller struct{}

func (caller collectionCountDocumentsCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)

	opts := options.Count()
	if value, has := slip.GetArgsKeyValue(args[1:], slip.Symbol(":skip")); has {
		if num, ok := value.(slip.Fixnum); ok {
			opts = opts.SetSkip(int64(num))
		} else {
			slip.PanicType(":skip", value, "fixnum")
		}
	}
	if value, has := slip.GetArgsKeyValue(args[1:], slip.Symbol(":limit")); has {
		if num, ok := value.(slip.Fixnum); ok {
			opts = opts.SetLimit(int64(num))
		} else {
			slip.PanicType(":skip", value, "fixnum")
		}
	}
	filter := ToBson(args[0])
	if _, ok := filter.(primitive.Null); ok || filter == nil {
		filter = bson.D{}
	}
	ctx, cf := context.WithTimeout(context.Background(), instTimeout(self))
	defer cf()

	cnt, err := self.Any.(*mongo.Collection).CountDocuments(ctx, filter, opts)
	if err != nil {
		panic(err)
	}
	return slip.Fixnum(cnt)
}

func (caller collectionCountDocumentsCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":count-documents",
		Text: `Returns the number of documents that match the _filter_ limited or adjusted
by _skip_ and _limit_ values.`,
		Args: []*slip.DocArg{
			{
				Name: "filter",
				Type: "bag|list",
				Text: "Filter for the search.",
			},
			{Name: "&key"},
			{
				Name: ":skip",
				Type: "fixnum",
				Text: "Matches to skip before returning a matching record.",
			},
			{
				Name: ":limit",
				Type: "fixnum",
				Text: "Maximum number of records to count.",
			},
		},
		Return: "fixnum",
	}
}
