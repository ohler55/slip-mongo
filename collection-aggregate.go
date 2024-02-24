// Copyright (c) 2024, Peter Ohler, All rights reserved.

package main

import (
	"context"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/bag"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/pkg/flavors"
	"go.mongodb.org/mongo-driver/mongo"
	"go.mongodb.org/mongo-driver/mongo/options"
)

type collectionAggregateCaller struct{}

func (caller collectionAggregateCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)

	var (
		native bool
		wrap   bool
	)
	kargs := args[2:]
	opts := options.Aggregate()
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
	cursor, err := self.Any.(*mongo.Collection).Aggregate(ctx, ToBson(args[1]), opts)
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

func (caller collectionAggregateCaller) Docs() string {
	return `__:aggregate__ _function_ _pipeline_ &key _native_ _wrap_ _allow-disk-use_ _batch_
   _function_ [function] to call with each record found.
   _pipeline_ [bag|list] to execute.
   _:native_ [boolean] if true return records will be a list and not a _bag-flavor_ instance.
   _:wrap_ [boolean] if true wrap non-native such as ObjectId with an indicator of the type.
   _:allow-disk-use_ [boolean] if true allows the use of the disk during the query.
   _:batch_ [fixnum] size of each fetch from the server.


Executes a aggregate pipeline on the collection and calls _function_ on each document.
`
}
