// Copyright (c) 2024, Peter Ohler, All rights reserved.

package main

import (
	"context"
	"time"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"go.mongodb.org/mongo-driver/mongo"
	"go.mongodb.org/mongo-driver/mongo/options"
)

const defaultTimeout = time.Second * 20

func init() {
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
					Name: "user",
					Type: "string",
					Text: `for the connection authentication`,
				},
				{
					Name: "password",
					Type: "string",
					Text: `for the connection authentication`,
				},
				{
					Name: "timeout",
					Type: "fixnum",
					Text: "is the number of seconds to wait before giving up",
				},
			},
			Return: "mongo-client",
			Text: `__mongo-connect__ attempts to connect to a mongo database server using
the _user_ and _password_ along with the _url_.`,
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
	slip.ArgCountCheck(f, args, 1, 7)
	timeout := defaultTimeout
	opts := options.Client()
	opts = opts.ApplyURI(string(slip.MustBeString(args[0], "url")))
	var auth options.Credential
	if ss, has := slip.GetArgsKeyValue(args[1:], slip.Symbol(":user")); has {
		auth.Username = slip.MustBeString(ss, ":user")
	}
	if ss, has := slip.GetArgsKeyValue(args[1:], slip.Symbol(":password")); has {
		auth.Password = slip.MustBeString(ss, ":password")
	}
	if 0 < len(auth.Username) || 0 < len(auth.Password) {
		opts.SetAuth(auth)
	}
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
