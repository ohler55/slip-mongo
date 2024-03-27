// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slipmongo

import (
	"context"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/mongo"
)

type clientDatabasesCaller struct{}

func (caller clientDatabasesCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	mc := self.Any.(*mongo.Client)

	timeout := defaultTimeout
	if to := mc.Timeout(); to != nil {
		timeout = *to
	}
	ctx, cf := context.WithTimeout(context.Background(), timeout)
	defer cf()

	var filter any
	if 0 < len(args) {
		filter = filterFromArg(args[0])
	}
	if filter == nil {
		filter = bson.D{}
	}
	result, err := mc.ListDatabases(ctx, filter)
	if err != nil {
		panic(err)
	}
	assoc := make(slip.List, 0, len(result.Databases))
	for _, spec := range result.Databases {
		assoc = append(assoc, slip.List{slip.String(spec.Name), slip.Tail{Value: slip.Fixnum(spec.SizeOnDisk)}})
	}
	return assoc
}

func (caller clientDatabasesCaller) Docs() string {
	return `__:databases__ &optional _filter_ => _list_
   _filter_ [bag|assoc|string] to be applied to the databases list. A _string_ value is taken as a regexp.


Returns an associate list of all databases that match the _filter_. Each
element of the returned list has a _car_ of the database name followed by size
of the database on disk.
`
}
