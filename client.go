// Copyright (c) 2024, Peter Ohler, All rights reserved.

package main

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"go.mongodb.org/mongo-driver/mongo"
)

var (
	clientFlavor *flavors.Flavor
)

func init() {
	clientFlavor = flavors.DefFlavor("mongo-client",
		map[string]slip.Object{},
		[]string{}, // inherited flavors
		slip.List{
			slip.List{
				slip.Symbol(":documentation"),
				slip.String(`A connection to a mongo database connection.
`),
			},
		},
	)
	clientFlavor.DefMethod(":init", "", clientInitCaller{})
	// clientFlavor.DefMethod(":database", "", clientDatabaseCaller{})
	// clientFlavor.DefMethod(":disconnect", "", clientDisconnectCaller{})
	//   call ListDatabases and make assoc with key as the name
	// clientFlavor.DefMethod(":databases", "", clientDatabasesCaller{})
	// clientFlavor.DefMethod(":ping", "", clientPingCaller{})
	// clientFlavor.DefMethod(":watch", "", clientWatchCaller{})
}

type client struct {
	self *flavors.Instance
	mc   *mongo.Client
}

type clientInitCaller struct{}

func (caller clientInitCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)

	// TBD connect also called by mongo-connect

	obj.Any = &client{self: obj}

	return nil
}

func (caller clientInitCaller) Docs() string {
	return `__:init__ &key _url_ _user_ _password_ _timeout_
   _:url_ [string] for connecting to the mongodb server.
   _:user_ [string] for the connection authentication.
   _:password_ [string] for the connection authentication
   _:timeout_ [fixnum] in seconds.


Sets the initial value when _make-instance_ is called.
`
}
