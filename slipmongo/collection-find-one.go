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

type collectionFindOneCaller struct{}

func (caller collectionFindOneCaller) Call(s *slip.Scope, args slip.List, depth int) (record slip.Object) {
	self := s.Get("self").(*flavors.Instance)

	var (
		native bool
		wrap   bool
	)
	opts := options.FindOne()
	if value, has := slip.GetArgsKeyValue(args[1:], slip.Symbol(":projection")); has && value != nil {
		opts = opts.SetProjection(ToBson(value))
	}
	if value, has := slip.GetArgsKeyValue(args[1:], slip.Symbol(":sort")); has && value != nil {
		opts = opts.SetSort(ToBson(value))
	}
	if value, has := slip.GetArgsKeyValue(args[1:], slip.Symbol(":skip")); has && value != nil {
		if num, ok := value.(slip.Fixnum); ok {
			opts = opts.SetSkip(int64(num))
		} else {
			slip.TypePanic(s, depth, ":skip", value, "fixnum")
		}
	}
	if value, has := slip.GetArgsKeyValue(args[1:], slip.Symbol(":native")); has && value != nil {
		native = true
	}
	if value, has := slip.GetArgsKeyValue(args[1:], slip.Symbol(":wrap")); has && value != nil {
		wrap = true
	}
	ctx, cf := context.WithTimeout(context.Background(), instTimeout(self))
	defer cf()

	filter := ToBson(args[0])
	if _, ok := filter.(primitive.Null); ok || filter == nil {
		filter = bson.D{}
	}
	sr := self.Any.(*mongo.Collection).FindOne(ctx, filter, opts)
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

func (caller collectionFindOneCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":find-one",
		Text: `Executes a find on the collection and returns a matching record or nil is no
document is found matching the provided criteria.`,
		Args: []*slip.DocArg{
			{
				Name: "filter",
				Type: "bag|list",
				Text: "Filter for the search.",
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
				Name: ":skip",
				Type: "fixnum",
				Text: "Matches to skip before returning a matching record.",
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
