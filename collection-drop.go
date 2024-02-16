// Copyright (c) 2024, Peter Ohler, All rights reserved.

package main

import (
	"context"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"go.mongodb.org/mongo-driver/mongo"
)

type collectionDropCaller struct{}

func (caller collectionDropCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*flavors.Instance)

	timeout := defaultTimeout
	client := self.Get("client").(*flavors.Instance)
	if tp := client.Any.(*mongo.Client).Timeout(); tp != nil {
		timeout = *tp
	}
	ctx, cf := context.WithTimeout(context.Background(), timeout)
	defer cf()

	if err := self.Any.(*mongo.Collection).Drop(ctx); err != nil {
		panic(err)
	}
	return nil
}

func (caller collectionDropCaller) Docs() string {
	return `__:drop__


Drops the collection.
`
}
