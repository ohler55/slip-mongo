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

type collectionUpdateOneCaller struct{}

func (caller collectionUpdateOneCaller) Call(s *slip.Scope, args slip.List, _ int) (count slip.Object) {
	self := s.Get("self").(*flavors.Instance)

	filter := ToBson(args[0])
	if _, ok := filter.(primitive.Null); ok || filter == nil {
		filter = bson.D{}
	}
	update := ToBson(args[1])
	ctx, cf := context.WithTimeout(context.Background(), instTimeout(self))
	defer cf()

	if ur, err := self.Any.(*mongo.Collection).UpdateOne(ctx, filter, update); err == nil {
		count = slip.Fixnum(ur.ModifiedCount)
	} else {
		panic(err)
	}
	return
}

func (caller collectionUpdateOneCaller) Docs() string {
	return `__:update-one__ _filter_ _update_ => _fixnum_
   _filter_ [bag|list] that selects the records to be updateed.
   _update_ [bag|list] the modification directives to apply a record.


If _filter_ is an instance of the _bag-flavor_ then the contents of the
instance is the filter. If a _list_ then the list is converted to mongodb bson
and used as the filter. The number of records modified is returned.
`
}
