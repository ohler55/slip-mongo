// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slipmongo

import (
	"context"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"go.mongodb.org/mongo-driver/mongo"
)

type collectionInsertOneCaller struct{}

func (caller collectionInsertOneCaller) Call(s *slip.Scope, args slip.List, _ int) (id slip.Object) {
	self := s.Get("self").(*flavors.Instance)

	var wrap bool
	if value, has := slip.GetArgsKeyValue(args[1:], slip.Symbol(":wrap")); has && value != nil {
		wrap = true
	}
	doc := ToBson(args[0])
	ctx, cf := context.WithTimeout(context.Background(), instTimeout(self))
	defer cf()

	if ior, err := self.Any.(*mongo.Collection).InsertOne(ctx, doc); err == nil {
		id = BsonToObject(ior.InsertedID, wrap)
	} else {
		panic(err)
	}
	return
}

func (caller collectionInsertOneCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":insert-one",
		Text: `If _doc_ is an instance of the _bag-flavor_ then the contents of the instance
is inserted. If a _list_ then the list is converted to mongodb bson and
inserted. The id of the inserted record is returned. Usually this is an object
id but can be what ever the caller specifies in the record.`,
		Args: []*slip.DocArg{
			{
				Name: "doc",
				Type: "bag|list",
				Text: "The data to be inserted.",
			},
			{Name: "&key"},
			{
				Name: ":wrap",
				Type: "boolean",
				Text: "If true wrap non-native such as ObjectId with an indicator of the type.",
			},
		},
		Return: "object",
	}
}
