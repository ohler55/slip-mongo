// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slipmongo_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCollectionDistinct(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-distinct")))
                      (send col :insert-one '((a . 1) (b . 2)))
                      (send col :insert-one '((a . 1) (b . 4)))
                      (send col :insert-one '((a . 2) (b . 8)))
                      (send col :distinct "b" '((a . 1))))`,
			Expect: `(2 4)`,
		}).Test(t)
	})
}

func TestCollectionDistinctNoFilter(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-distinct")))
                      (send col :insert-one '((a . 1) (b . 2)))
                      (send col :insert-one '((a . 1) (b . 4)))
                      (send col :insert-one '((a . 2) (b . 8)))
                      (send col :distinct "b" nil))`,
			Expect: `(2 4 8)`,
		}).Test(t)
	})
}

func TestCollectionDistinctBadFilter(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-distinct")))
                       (send col :distinct 'b '(1)))`,
			PanicType: slip.Symbol("error"),
		}).Test(t)
	})
}
