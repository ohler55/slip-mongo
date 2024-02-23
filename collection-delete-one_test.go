// Copyright (c) 2024, Peter Ohler, All rights reserved.

package main_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCollectionDeleteOneFilter(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-delete-one")))
                      (send col :insert-one '((a . 1) (b . 2)))
                      (send col :insert-one '((a . 1) (b . 4)))
                      (send col :insert-one '((a . 2) (b . 8)))
                      (list
                       (send col :delete-one '((a . 1)))
                       (send col :count-documents nil)))`,
			Expect: `(1 2)`,
		}).Test(t)
	})
}

func TestCollectionDeleteOneAny(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-delete-one-any")))
                      (send col :insert-one '((a . 1) (b . 2)))
                      (send col :insert-one '((a . 1) (b . 4)))
                      (send col :insert-one '((a . 2) (b . 8)))
                      (list
                       (send col :delete-one nil)
                       (send col :count-documents nil)))`,
			Expect: `(1 2)`,
		}).Test(t)
	})
}

func TestCollectionDeleteOneBadFilter(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-delete-many-any")))
                       (send col :delete-one '(1)))`,
			PanicType: slip.Symbol("error"),
		}).Test(t)
	})
}
