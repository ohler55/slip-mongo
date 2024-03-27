// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slipmongo_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCollectionDrop(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let ((db (send mc :database "quux")))
                      (send (send db :collection "cool") :drop))`,
			Expect: "nil",
		}).Test(t)
	})
}

func TestCollectionDropError(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let ((db (send mc :database "admin")))
                      (send (send db :collection "system.version") :drop))`,
			PanicType: slip.Symbol("error"),
		}).Test(t)
	})
}
