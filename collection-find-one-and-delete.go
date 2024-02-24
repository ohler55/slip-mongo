// Copyright (c) 2024, Peter Ohler, All rights reserved.

package main

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

type collectionFindOneAndDeleteCaller struct{}

func (caller collectionFindOneAndDeleteCaller) Call(s *slip.Scope, args slip.List, depth int) (record slip.Object) {
	self := s.Get("self").(*flavors.Instance)

	var (
		native bool
		wrap   bool
	)
	opts := options.FindOneAndDelete()
	if value, has := slip.GetArgsKeyValue(args[1:], slip.Symbol(":projection")); has {
		opts = opts.SetProjection(ToBson(value))
	}
	if value, has := slip.GetArgsKeyValue(args[1:], slip.Symbol(":sort")); has {
		opts = opts.SetSort(ToBson(value))
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
	sr := self.Any.(*mongo.Collection).FindOneAndDelete(ctx, filter, opts)
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

func (caller collectionFindOneAndDeleteCaller) Docs() string {
	return `__:find-one-and-delete__ _filter_ &key _projection_ _sort_ _native_ _wrap_ => _object_
   _filter_ [bag|list] for the search.
   _:projection_ [bag|list] of the results to return.
   _:sort_ [bag|list] according to the provided map.
   _:native_ [boolean] if true return records will be a list and not a _bag-flavor_ instance.
   _:wrap_ [boolean] if true wrap non-native such as ObjectId with an indicator of the type.


Executes a find on the collection and and deletes one matching document. That document is returned.
`
}
