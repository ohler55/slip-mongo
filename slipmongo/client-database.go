// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slipmongo

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"go.mongodb.org/mongo-driver/mongo"
)

type clientDatabaseCaller struct{}

func (caller clientDatabaseCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	mc := self.Any.(*mongo.Client)
	db := mc.Database(slip.MustBeString(args[0], "name"))

	inst := databaseFlavor.MakeInstance().(*flavors.Instance)
	inst.Init(s, slip.List{}, depth)
	inst.Any = db
	inst.Let("client", self)

	return inst
}

func (caller clientDatabaseCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":database",
		Text: "Database returns the named database on the server.",
		Args: []*slip.DocArg{
			{
				Name: "name",
				Type: "string",
				Text: "Name of the database.",
			},
		},
		Return: "mongo-database",
	}
}
