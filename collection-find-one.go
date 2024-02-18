// Copyright (c) 2024, Peter Ohler, All rights reserved.

package main

import (
	"context"
	"errors"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/bag"
	"github.com/ohler55/slip/pkg/flavors"
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
	if value, has := slip.GetArgsKeyValue(args[1:], slip.Symbol(":projection")); has {
		opts = opts.SetProjection(ToBson(value))
	}
	if value, has := slip.GetArgsKeyValue(args[1:], slip.Symbol(":sort")); has {
		opts = opts.SetSort(ToBson(value))
	}
	if value, has := slip.GetArgsKeyValue(args[1:], slip.Symbol(":skip")); has {
		if num, ok := value.(slip.Fixnum); ok {
			opts = opts.SetSkip(int64(num))
		} else {
			slip.PanicType(":skip", value, "fixnum")
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

	sr := self.Any.(*mongo.Collection).FindOne(ctx, ToBson(args[0]), opts)
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

func (caller collectionFindOneCaller) Docs() string {
	return `__:find-one__ _filter_ &key _projection_ _sort_ _skip_ _native_ _wrap_ => _object_
   _filter_ [bag|list] for the search.
   _:projection_ [bag|list] of the results to return.
   _:sort_ [bag|list] according to the provided map.
   _:skip_ [fixnum] matches to skip before returning a matching record.
   _:native_ [boolean] if true return records will be a list and not a _bag-flavor_ instance.
   _:wrap_ [boolean] if true wrap non-native such as ObjectId with an indicator of the type.


Executes a find on the collection and returns a matching record or nil is no
document is found matching the provided criteria.
`
}
