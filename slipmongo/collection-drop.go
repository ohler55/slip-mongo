// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slipmongo

import (
	"context"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"go.mongodb.org/mongo-driver/v2/mongo"
)

type collectionDropCaller struct{}

func (caller collectionDropCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)

	ctx, cf := context.WithTimeout(context.Background(), timeoutFromArgs(s, args, depth))
	defer cf()

	if err := self.Any.(*mongo.Collection).Drop(ctx); err != nil {
		panic(err)
	}
	return nil
}

func (caller collectionDropCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":drop",
		Text: `Drops the collection.`,
		Args: []*slip.DocArg{
			{Name: "&key"},
			{
				Name: "timeout",
				Type: "fixnum",
				Text: "is the number of seconds to wait before giving up",
			},
		},
	}
}
