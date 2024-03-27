// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slipmongo

import (
	"context"
	"time"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"go.mongodb.org/mongo-driver/mongo"
	"go.mongodb.org/mongo-driver/mongo/options"
)

const defaultTimeout = time.Second * 20

func initConnect() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Connect{Function: slip.Function{Name: "mongo-connect", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "mongo-connect",
			Args: []*slip.DocArg{
				{
					Name: "url",
					Type: "string",
					Text: `URL of the mongo database server to connect to.`,
				},
				{Name: "&key"},
				{
					Name: "timeout",
					Type: "fixnum",
					Text: "is the number of seconds to wait before giving up",
				},
			},
			Return: "mongo-client",
			Text:   `__mongo-connect__ attempts to connect to a mongo database server with the _url_.`,
			Examples: []string{
				`(mongo-connect "mongodb://localhost:27017/test") => #<mongo-client 12345>`,
			},
		}, &Pkg)
}

// Connect represents the make-flow function.
type Connect struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Connect) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 3)
	timeout := defaultTimeout
	opts := options.Client()
	opts = opts.ApplyURI(string(slip.MustBeString(args[0], "url")))
	if v, has := slip.GetArgsKeyValue(args[1:], slip.Symbol(":timeout")); has {
		if num, ok := v.(slip.Fixnum); ok {
			timeout = time.Second * time.Duration(num)
		} else {
			slip.PanicType(":timeout", v, "fixnum")
		}
	}
	opts = opts.SetTimeout(timeout)
	ctx, cf := context.WithTimeout(context.Background(), timeout)
	defer cf()
	mc, err := mongo.Connect(ctx, opts)
	if err != nil {
		panic(err)
	}
	if err = mc.Ping(ctx, nil); err != nil {
		panic(err)
	}
	self := clientFlavor.MakeInstance().(*flavors.Instance)
	self.Init(s, slip.List{}, depth)
	self.Any = mc

	return self
}
