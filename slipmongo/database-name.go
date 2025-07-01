// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slipmongo

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"go.mongodb.org/mongo-driver/mongo"
)

type databaseNameCaller struct{}

func (caller databaseNameCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	db := self.Any.(*mongo.Database)

	return slip.String(db.Name())
}

func (caller databaseNameCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":name",
		Text:   `Returns the name of the database.`,
		Return: "string",
	}
}
