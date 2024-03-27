// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slipmongo

import (
	"context"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"go.mongodb.org/mongo-driver/mongo"
)

type collectionUpdateByIDCaller struct{}

func (caller collectionUpdateByIDCaller) Call(s *slip.Scope, args slip.List, _ int) (count slip.Object) {
	self := s.Get("self").(*flavors.Instance)

	id := ToBson(args[0])
	update := ToBson(args[1])
	ctx, cf := context.WithTimeout(context.Background(), instTimeout(self))
	defer cf()

	if ur, err := self.Any.(*mongo.Collection).UpdateByID(ctx, id, update); err == nil {
		count = slip.Fixnum(ur.ModifiedCount)
	} else {
		panic(err)
	}
	return
}

func (caller collectionUpdateByIDCaller) Docs() string {
	return `__:update-by-id__ _id_ _update_ => _fixnum_
   _id_ [object] the id of the document. (hint: if an ObjectId wrap with {$toObjectId: <id>}.
   _update_ [bag|list] the modification directives to apply a record.


Update the document with a match _id_. The number of records modified is returned.
`
}
