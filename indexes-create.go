// Copyright (c) 2024, Peter Ohler, All rights reserved.

package main

import (
	"context"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"go.mongodb.org/mongo-driver/mongo"
	"go.mongodb.org/mongo-driver/mongo/options"
)

type indexesCreateCaller struct{}

func (caller indexesCreateCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*flavors.Instance)

	kargs := args[1:]
	opts := options.Index()
	if value, has := slip.GetArgsKeyValue(kargs, slip.Symbol(":expire-after")); has {
		if num, ok := value.(slip.Fixnum); ok {
			opts = opts.SetExpireAfterSeconds(int32(num))
		} else {
			slip.PanicType(":expire-after", value, "fixnum")
		}
	}
	if value, has := slip.GetArgsKeyValue(kargs, slip.Symbol(":name")); has {
		opts = opts.SetName(slip.MustBeString(value, ":name"))
	}
	if value, has := slip.GetArgsKeyValue(kargs, slip.Symbol(":sparse")); has && value != nil {
		opts = opts.SetSparse(true)
	}
	if value, has := slip.GetArgsKeyValue(kargs, slip.Symbol(":unique")); has && value != nil {
		opts = opts.SetUnique(true)
	}
	if value, has := slip.GetArgsKeyValue(kargs, slip.Symbol(":language")); has {
		opts = opts.SetDefaultLanguage(slip.MustBeString(value, ":language"))
	}

	ctx, cf := context.WithTimeout(context.Background(), defaultTimeout)
	defer cf()

	name, err := self.Any.(mongo.IndexView).CreateOne(ctx, mongo.IndexModel{Keys: ToBson(args[0]), Options: opts})
	if err != nil {
		panic(err)
	}
	return slip.String(name)
}

func (caller indexesCreateCaller) Docs() string {
	return `__:create__ _keys_ &key _background_ _expire-after_ _name_ _sparse_ _unique_ _language_ => _string_
   _keys_ [bag|list] that defines the index.
   _:expire-after_ [fixnum] number of seconds to retain documents in the index.
   _:name_ [string] of the index overrides the generated name.
   _:sparse_ [boolean] if true only documents with the key fields are excluded.
   _:unique_ [boolean] if true inserts attempted with duplicate keys are rejected.
   _:language_ [string] specifies the default language if a text index.


Creates an index with the provided _keys_ and various keywords. The name of the index is returned.
`
}
