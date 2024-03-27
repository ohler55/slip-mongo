// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slipmongo

import (
	"context"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/bson/primitive"
	"go.mongodb.org/mongo-driver/mongo"
)

type collectionReplaceOneCaller struct{}

func (caller collectionReplaceOneCaller) Call(s *slip.Scope, args slip.List, _ int) (count slip.Object) {
	self := s.Get("self").(*flavors.Instance)

	filter := ToBson(args[0])
	if _, ok := filter.(primitive.Null); ok || filter == nil {
		filter = bson.D{}
	}
	replace := ToBson(args[1])
	ctx, cf := context.WithTimeout(context.Background(), instTimeout(self))
	defer cf()

	if ur, err := self.Any.(*mongo.Collection).ReplaceOne(ctx, filter, replace); err == nil {
		count = slip.Fixnum(ur.ModifiedCount)
	} else {
		panic(err)
	}
	return
}

func (caller collectionReplaceOneCaller) Docs() string {
	return `__:replace-one__ _filter_ _replacement_ => _fixnum_
   _filter_ [bag|list] that selects the records to be replaced.
   _replacement_ [bag|list] the new document to replace the first existing match.


If _filter_ is an instance of the _bag-flavor_ then the contents of the
instance is the filter. If a _list_ then the list is converted to mongodb bson
and used as the filter. The number of records replaced is returned.
`
}
