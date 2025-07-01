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

func (caller collectionUpdateByIDCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":update-by-id",
		Text: `Update the document with a matching _id_. The number of records modified is returned.`,
		Args: []*slip.DocArg{
			{
				Name: "id",
				Type: "object",
				Text: "The id of the document. (hint: if an ObjectId wrap with {$toObjectId: <id>}.",
			},
			{
				Name: "update",
				Type: "bag|list",
				Text: "The modification directives to apply to a record.",
			},
		},
		Return: "fixnum",
	}
}
