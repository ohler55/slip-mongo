// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slipmongo

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"go.mongodb.org/mongo-driver/mongo"
)

type collectionIndexesCaller struct{}

func (caller collectionIndexesCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)

	inst := indexesFlavor.MakeInstance().(*flavors.Instance)
	inst.Init(s, slip.List{}, depth)
	inst.Any = self.Any.(*mongo.Collection).Indexes()

	return inst
}

func (caller collectionIndexesCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":indexes",
		Text:   `Returns an instance of the mongo-indexes flavor for the collection.`,
		Return: "mongo-indexes",
	}
}
