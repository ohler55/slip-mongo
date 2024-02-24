// Copyright (c) 2024, Peter Ohler, All rights reserved.

package main

import (
	"context"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/bag"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/pkg/flavors"
	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/bson/primitive"
	"go.mongodb.org/mongo-driver/mongo"
	"go.mongodb.org/mongo-driver/mongo/options"
)

type collectionFindCaller struct{}

func (caller collectionFindCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)

	var (
		native bool
		wrap   bool
	)
	kargs := args[2:]
	opts := options.Find()
	if value, has := slip.GetArgsKeyValue(kargs, slip.Symbol(":projection")); has {
		opts = opts.SetProjection(ToBson(value))
	}
	if value, has := slip.GetArgsKeyValue(kargs, slip.Symbol(":sort")); has {
		opts = opts.SetSort(ToBson(value))
	}
	if value, has := slip.GetArgsKeyValue(kargs, slip.Symbol(":skip")); has {
		if num, ok := value.(slip.Fixnum); ok {
			opts = opts.SetSkip(int64(num))
		} else {
			slip.PanicType(":skip", value, "fixnum")
		}
	}
	if value, has := slip.GetArgsKeyValue(kargs, slip.Symbol(":limit")); has {
		if num, ok := value.(slip.Fixnum); ok {
			opts = opts.SetLimit(int64(num))
		} else {
			slip.PanicType(":limit", value, "fixnum")
		}
	}
	if value, has := slip.GetArgsKeyValue(kargs, slip.Symbol(":batch")); has {
		if num, ok := value.(slip.Fixnum); ok {
			opts = opts.SetBatchSize(int32(num))
		} else {
			slip.PanicType(":batch", value, "fixnum")
		}
	}
	if value, has := slip.GetArgsKeyValue(kargs, slip.Symbol(":allow-disk-use")); has && value != nil {
		opts = opts.SetAllowDiskUse(true)
	}
	if value, has := slip.GetArgsKeyValue(kargs, slip.Symbol(":native")); has && value != nil {
		native = true
	}
	if value, has := slip.GetArgsKeyValue(kargs, slip.Symbol(":wrap")); has && value != nil {
		wrap = true
	}
	ctx, cf := context.WithTimeout(context.Background(), instTimeout(self))
	defer cf()

	fun := cl.ResolveToCaller(s, args[0], depth)
	filter := ToBson(args[1])
	if _, ok := filter.(primitive.Null); ok || filter == nil {
		filter = bson.D{}
	}
	cursor, err := self.Any.(*mongo.Collection).Find(ctx, filter, opts)
	if err != nil {
		panic(err)
	}
	defer func() { _ = cursor.Close(ctx) }()
	d2 := depth + 1
	for cursor.Next(ctx) {
		var brec any
		if err = cursor.Decode(&brec); err != nil {
			panic(err)
		}
		var record slip.Object
		if native {
			record = BsonToObject(brec, wrap)
		} else {
			bg := bag.Flavor().MakeInstance().(*flavors.Instance)
			bg.Init(s, slip.List{}, depth)
			bg.Any = SimplifyBson(brec, wrap)
			record = bg
		}
		_ = fun.Call(s, slip.List{record}, d2)
	}
	return nil
}

func (caller collectionFindCaller) Docs() string {
	return `__:find__ _function_ _filter_
&key _projection_ _sort_ _skip_ _native_ _wrap_ _limit_ _allow-disk-use_ _batch_
   _function_ [function] to call with each record found.
   _filter_ [bag|list] for the search.
   _:projection_ [bag|list] of the results to return.
   _:sort_ [bag|list] according to the provided map.
   _:skip_ [fixnum] matches to skip before returning a matching record.
   _:native_ [boolean] if true return records will be a list and not a _bag-flavor_ instance.
   _:wrap_ [boolean] if true wrap non-native such as ObjectId with an indicator of the type.
   _:limit_ [fixnum] of the number of records to find.
   _:allow-disk-use_ [boolean] if true allows the use of the disk during the query.
   _:batch_ [fixnum] size of each fetch from the server.


Executes a find on the collection and calls _function_ on each matching record.
`
}
