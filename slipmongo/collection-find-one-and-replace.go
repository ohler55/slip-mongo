// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slipmongo

import (
	"context"
	"errors"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/bag"
	"github.com/ohler55/slip/pkg/flavors"
	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/bson/primitive"
	"go.mongodb.org/mongo-driver/mongo"
	"go.mongodb.org/mongo-driver/mongo/options"
)

type collectionFindOneAndReplaceCaller struct{}

func (caller collectionFindOneAndReplaceCaller) Call(s *slip.Scope, args slip.List, depth int) (record slip.Object) {
	self := s.Get("self").(*flavors.Instance)

	var (
		native bool
		wrap   bool
	)
	kargs := args[2:]
	opts := options.FindOneAndReplace()
	if value, has := slip.GetArgsKeyValue(kargs, slip.Symbol(":projection")); has {
		opts = opts.SetProjection(ToBson(value))
	}
	if value, has := slip.GetArgsKeyValue(kargs, slip.Symbol(":sort")); has {
		opts = opts.SetSort(ToBson(value))
	}
	if value, has := slip.GetArgsKeyValue(kargs, slip.Symbol(":after")); has && value != nil {
		opts = opts.SetReturnDocument(options.After)
	}
	if value, has := slip.GetArgsKeyValue(kargs, slip.Symbol(":native")); has && value != nil {
		native = true
	}
	if value, has := slip.GetArgsKeyValue(kargs, slip.Symbol(":wrap")); has && value != nil {
		wrap = true
	}
	ctx, cf := context.WithTimeout(context.Background(), instTimeout(self))
	defer cf()

	filter := ToBson(args[0])
	if _, ok := filter.(primitive.Null); ok || filter == nil {
		filter = bson.D{}
	}
	replacement := ToBson(args[1])
	sr := self.Any.(*mongo.Collection).FindOneAndReplace(ctx, filter, replacement, opts)
	err := sr.Err()
	switch {
	case err == nil:
		var brec any
		if err = sr.Decode(&brec); err != nil {
			panic(err)
		}
		if native {
			record = BsonToObject(brec, wrap)
		} else {
			bg := bag.Flavor().MakeInstance().(*flavors.Instance)
			bg.Init(s, slip.List{}, depth)
			bg.Any = SimplifyBson(brec, wrap)
			record = bg
		}
	case errors.Is(err, mongo.ErrNoDocuments):
		// record remains nil
	default:
		panic(err)
	}
	return
}

func (caller collectionFindOneAndReplaceCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":find-one-and-replace",
		Text: `Executes a find on the collection and and replaces one matching
document. Either the original (before) or the replacement document is returned
depending on the value of the _:after_ keywork argument.`,
		Args: []*slip.DocArg{
			{
				Name: "filter",
				Type: "bag|list",
				Text: "Filter for the search.",
			},
			{
				Name: "replacement",
				Type: "bag|list",
				Text: "The replacement document.",
			},
			{Name: "&key"},
			{
				Name: ":projection",
				Type: "bag|list",
				Text: "Projection of the results to return.",
			},
			{
				Name: ":sort",
				Type: "bag|list",
				Text: "Sort according to the provided map.",
			},
			{
				Name: ":after",
				Type: "boolean",
				Text: "If true the replacement document is returned otherwise the original is returned.",
			},
			{
				Name: ":native",
				Type: "boolean",
				Text: "If true return records will be a list and not a _bag-flavor_ instance.",
			},
			{
				Name: ":wrap",
				Type: "boolean",
				Text: "If true wrap non-native such as ObjectId with an indicator of the type.",
			},
		},
		Return: "object",
	}
}
