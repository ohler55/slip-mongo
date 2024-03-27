// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slipmongo

import (
	"context"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"go.mongodb.org/mongo-driver/mongo"
)

type collectionEstimatedDocumentCountCaller struct{}

func (caller collectionEstimatedDocumentCountCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)

	ctx, cf := context.WithTimeout(context.Background(), instTimeout(self))
	defer cf()

	cnt, err := self.Any.(*mongo.Collection).EstimatedDocumentCount(ctx)
	if err != nil {
		panic(err)
	}
	return slip.Fixnum(cnt)
}

func (caller collectionEstimatedDocumentCountCaller) Docs() string {
	return `__:estimated-document-count__ => _fixnum_


Returns the estimated number of documents in the collection.
`
}
