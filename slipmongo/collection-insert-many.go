// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slipmongo

import (
	"context"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"go.mongodb.org/mongo-driver/mongo"
)

type collectionInsertManyCaller struct{}

func (caller collectionInsertManyCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*flavors.Instance)

	var wrap bool
	if value, has := slip.GetArgsKeyValue(args[1:], slip.Symbol(":wrap")); has && value != nil {
		wrap = true
	}
	list, ok := args[0].(slip.List)
	if !ok {
		slip.PanicType("docs", args[0], "list")
	}
	docs := make([]any, len(list))
	for i, val := range list {
		docs[i] = ToBson(val)
	}
	ctx, cf := context.WithTimeout(context.Background(), instTimeout(self))
	defer cf()

	if imr, err := self.Any.(*mongo.Collection).InsertMany(ctx, docs); err == nil {
		ids := make(slip.List, len(imr.InsertedIDs))
		for i, id := range imr.InsertedIDs {
			ids[i] = BsonToObject(id, wrap)
		}
		return ids
	} else {
		panic(err)
	}
}

func (caller collectionInsertManyCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":insert-many",
		Text: `If the elements in _docs_ are instances of the _bag-flavor_ then the
contents of the instance is inserted. If a _list_ then the list is converted
to mongodb bson and inserted. The ids of the inserted record are
returned. Usually these are object id but can be what ever the caller
specifies in the record.`,
		Args: []*slip.DocArg{
			{
				Name: "docs",
				Type: "list",
				Text: "List of the records to be inserted. Records can be _bags_ or _lists_.",
			},
			{Name: "&key"},
			{
				Name: ":wrap",
				Type: "boolean",
				Text: "If true wrap non-native such as ObjectId with an indicator of the type.",
			},
		},
		Return: "list",
	}
}
