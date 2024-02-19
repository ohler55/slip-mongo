// Copyright (c) 2024, Peter Ohler, All rights reserved.

package main_test

import (
	"testing"

	"github.com/ohler55/ojg/jp"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/sliptest"
)

// Some tests for :find-one are covered in the :insert-one tests.

func TestCollectionFindOneProjectSort(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-sort")))
                      (send col :insert-one '((a . 1) (b . 2)))
                      (send col :insert-one '((a . 1) (b . 4)))
                      (send col :insert-one '((a . 2) (b . 8)))
                      (send col :find-one '((a . 1)) :native t :projection '((b . t)) :sort '((b . -1))))`,
			Expect: `/\("b" . 4\)/`,
		}).Test(t)
	})
}

func TestCollectionFindOneProjectSkip(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-skip")))
                      (send col :insert-one '((a . 1) (b . 2)))
                      (send col :insert-one '((a . 1) (b . 4)))
                      (send col :insert-one '((a . 2) (b . 8)))
                      (send col :find-one nil :projection '((b . t)) :skip 1))`,
			Validate: func(t *testing.T, v slip.Object) {
				bg := v.(*flavors.Instance)
				tt.Equal(t, 4, jp.C("b").First(bg.Any))
			},
		}).Test(t)
	})
}

func TestCollectionFindOneNotFound(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-not-found")))
                      (send col :find-one '((a . 1))))`,
			Expect: "nil",
		}).Test(t)
	})
}

func TestCollectionFindOneBadSkip(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-skip")))
                      (send col :find-one '((a . 1)) :native t :skip t))`,
			PanicType: slip.Symbol("type-error"),
		}).Test(t)
	})
}
