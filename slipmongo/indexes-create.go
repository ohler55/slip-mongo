// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slipmongo

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

func (caller indexesCreateCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":create",
		Text: `Creates an index with the provided _keys_ and various keywords. The name of the index is returned.`,
		Args: []*slip.DocArg{
			{
				Name: "keys",
				Type: "bag|list",
				Text: "The definition of the index.",
			},
			{Name: "&key"},
			{
				Name: ":expire-after",
				Type: "fixnum",
				Text: "The number of seconds to retain documents in the index.",
			},
			{
				Name: ":name",
				Type: "string",
				Text: "Name of the index overrides the generated name.",
			},
			{
				Name: ":sparse",
				Type: "boolean",
				Text: "If true only documents with the key fields are excluded.",
			},
			{
				Name: ":unique",
				Type: "boolean",
				Text: "If true inserts attempted with duplicate keys are rejected.",
			},
			{
				Name: ":language",
				Type: "string",
				Text: "Specifies the default language if a text index.",
			},
		},
		Return: "string",
	}
}
