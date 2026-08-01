// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slipmongo

import (
	"context"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"go.mongodb.org/mongo-driver/v2/mongo"
)

type clientPingCaller struct{}

func (caller clientPingCaller) Call(s *slip.Scope, args slip.List, _ int) (ok slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	mc := self.Any.(*mongo.Client)

	ctx, cf := context.WithTimeout(context.Background(), defaultTimeout)
	defer cf()

	if mc.Ping(ctx, nil) == nil {
		ok = slip.True
	}
	return
}

func (caller clientPingCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":ping",
		Text:   `Pings the server returning _t_ on success or nil on timeout.`,
		Return: "boolean",
	}
}
