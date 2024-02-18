// Copyright (c) 2024, Peter Ohler, All rights reserved.

package main

import (
	"context"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"go.mongodb.org/mongo-driver/mongo"
)

type databaseDropCaller struct{}

func (caller databaseDropCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*flavors.Instance)

	ctx, cf := context.WithTimeout(context.Background(), instTimeout(self))
	defer cf()

	if err := self.Any.(*mongo.Database).Drop(ctx); err != nil {
		panic(err)
	}
	return nil
}

func (caller databaseDropCaller) Docs() string {
	return `__:drop__


Drops the database.
`
}
