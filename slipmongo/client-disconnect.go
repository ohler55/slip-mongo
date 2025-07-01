// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slipmongo

import (
	"context"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"go.mongodb.org/mongo-driver/mongo"
)

type clientDisconnectCaller struct{}

func (caller clientDisconnectCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	mc := self.Any.(*mongo.Client)

	timeout := defaultTimeout
	if to := mc.Timeout(); to != nil {
		timeout = *to
	}
	ctx, cf := context.WithTimeout(context.Background(), timeout)
	defer cf()

	if err := mc.Disconnect(ctx); err != nil {
		panic(err)
	}
	return nil
}

func (caller clientDisconnectCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":disconnect",
		Text: `Disconnects the client from the server.`,
	}
}
