// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slipmongo

import (
	"context"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"go.mongodb.org/mongo-driver/mongo"
)

type indexesDropCaller struct{}

func (caller indexesDropCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*flavors.Instance)

	var name string
	if 0 < len(args) {
		name = slip.MustBeString(args[0], "name")
	}
	ctx, cf := context.WithTimeout(context.Background(), defaultTimeout)
	defer cf()

	var err error
	if 0 < len(name) {
		_, err = self.Any.(mongo.IndexView).DropOne(ctx, name)
	} else {
		_, err = self.Any.(mongo.IndexView).DropAll(ctx)
	}
	if err != nil {
		panic(err)
	}
	return nil
}

func (caller indexesDropCaller) Docs() string {
	return `__:drop__ &optional _name_
   _name_ [string] name of the index to drop.


Drops the index with the provided _name_ if one is provided. If no _name_ is
provided then all indexes are dropped.
`
}
