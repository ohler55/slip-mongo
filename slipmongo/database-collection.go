// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slipmongo

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"go.mongodb.org/mongo-driver/mongo"
)

type databaseCollectionCaller struct{}

func (caller databaseCollectionCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	db := self.Any.(*mongo.Database)
	table := db.Collection(slip.MustBeString(args[0], "name"))

	inst := collectionFlavor.MakeInstance().(*flavors.Instance)
	inst.Init(s, slip.List{}, depth)
	inst.Any = table
	inst.Let("database", self)
	inst.Let("client", self.Get("client"))

	return inst
}

func (caller databaseCollectionCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":collection",
		Text: `Returns a collection in the database with the specified _name_.`,
		Args: []*slip.DocArg{
			{
				Name: "name",
				Type: "string",
				Text: "The name of a collection.",
			},
		},
		Return: "mongo-collection",
	}
}
