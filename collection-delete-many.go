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

type collectionDeleteManyCaller struct{}

func (caller collectionDeleteManyCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)

	filter := ToBson(args[0])
	if _, ok := filter.(primitive.Null); ok || filter == nil {
		filter = bson.D{}
	}
	ctx, cf := context.WithTimeout(context.Background(), instTimeout(self))
	defer cf()
	dr, err := self.Any.(*mongo.Collection).DeleteMany(ctx, filter)
	if err != nil {
		panic(err)
	}
	return slip.Fixnum(dr.DeletedCount)
}

func (caller collectionDeleteManyCaller) Docs() string {
	return `__:delete-many__ _filter_ => _fixnum_
   _filter_ [bag|list] to find the document to delete.


Deletes the all matches in the collection for the _filter_ specified and
returns the number of documents deleted.
`
}
