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

func TestCollectionFindOneAndDeleteProjectSort(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `
(let* ((db (send mc :database "quux"))
       (col (send db :collection "cool-find-delete")))
 (send col :insert-one '((a . 1) (b . 2)))
 (send col :insert-one '((a . 1) (b . 4)))
 (send col :insert-one '((a . 2) (b . 8)))
 (list
  (cadr (send col :find-one-and-delete '((a . 2)) :native t :projection '((b . t)) :sort '((b . -1))))
  (send col :find-one '((a . 2)) :native t)))`,
			Expect: `(("b" . 8) nil)`,
		}).Test(t)
	})
}

func TestCollectionFindOneAndDeleteWrap(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `
(let* ((db (send mc :database "quux"))
       (col (send db :collection "cool-find-delete-wrap")))
 (send col :insert-one '((a . 1) (b . 2)))
 (send col :insert-one '((a . 1) (b . 4)))
 (send col :insert-one '((a . 2) (b . 8)))
 (send col :find-one-and-delete nil :projection '((b . t)) :wrap t))`,
			Validate: func(t *testing.T, v slip.Object) {
				bg := v.(*flavors.Instance)
				tt.Equal(t, true, jp.C("_id").C("$toObjectId").Has(bg.Any))
				tt.Equal(t, 2, jp.C("b").First(bg.Any))
			},
		}).Test(t)
	})
}

func TestCollectionFindOneAndDeleteNotFound(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-not-found")))
                      (send col :find-one-and-delete '((a . 1))))`,
			Expect: "nil",
		}).Test(t)
	})
}

func TestCollectionFindOneAndDeleteBadFilter(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-find-one-and-delete")))
                       (send col :find-one-and-delete '(1)))`,
			PanicType: slip.Symbol("error"),
		}).Test(t)
	})
}
