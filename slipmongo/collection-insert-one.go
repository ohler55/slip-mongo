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

func (caller collectionInsertOneCaller) Docs() string {
	return `__:insert-one__ _doc_ &key _wrap_ => _object_
   _doc_ [bag|list] the data to be inserted.
   _:wrap_ [boolean] if true wraps non-native record IDs such as ObjectId with an indicator of the type.


If _doc_ is an instance of the _bag-flavor_ then the contents of the instance
is inserted. If a _list_ then the list is converted to mongodb bson and
inserted. The id of the inserted record is returned. Usually this is an object
id but can be what ever the caller specifies in the record.
`
}
