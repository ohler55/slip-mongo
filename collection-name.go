// Copyright (c) 2024, Peter Ohler, All rights reserved.

package main

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"go.mongodb.org/mongo-driver/mongo"
)

type collectionNameCaller struct{}

func (caller collectionNameCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	table := self.Any.(*mongo.Collection)

	return slip.String(table.Name())
}

func (caller collectionNameCaller) Docs() string {
	return `__:name__ => _string_


Returns the name of the collection.
`
}
