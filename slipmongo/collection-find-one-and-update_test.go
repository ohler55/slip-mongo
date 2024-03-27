// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slipmongo_test

import (
	"testing"

	"github.com/ohler55/ojg/jp"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/sliptest"
)

func TestCollectionFindOneAndUpdateProjectSort(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `
(let* ((db (send mc :database "quux"))
       (col (send db :collection "cool-find-update")))
 (send col :insert-one '((a . 1) (b . 2)))
 (send col :insert-one '((a . 1) (b . 4)))
 (send col :insert-one '((a . 2) (b . 8)))
 (list
  (cadr
   (send col :find-one-and-update '((a . 2)) '(("$set" (b . 7)))
                                   :native t
                                   :projection '((b . t))
                                   :sort '((b . 1))))
  (cadr (send col :find-one '((a . 2)) :native t :projection '((b . t))))))`,
			Expect: `(("b" . 8) ("b" . 7))`,
		}).Test(t)
	})
}

func TestCollectionFindOneAndUpdateWrap(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `
(let* ((db (send mc :database "quux"))
       (col (send db :collection "cool-find-update-wrap")))
 (send col :insert-one '((a . 1) (b . 2)))
 (send col :insert-one '((a . 1) (b . 4)))
 (send col :insert-one '((a . 2) (b . 8)))
 (send col :find-one-and-update nil '(("$set" (b . 7))) :projection '((b . t)) :after t :wrap t))`,
			Validate: func(t *testing.T, v slip.Object) {
				bg := v.(*flavors.Instance)
				tt.Equal(t, true, jp.C("_id").C("$toObjectId").Has(bg.Any))
				tt.Equal(t, 7, jp.C("b").First(bg.Any))
			},
		}).Test(t)
	})
}

func TestCollectionFindOneAndUpdateNotFound(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-not-found")))
                      (send col :find-one-and-update '((a . 1)) '(("$set" (b . 7)))))`,
			Expect: "nil",
		}).Test(t)
	})
}

func TestCollectionFindOneAndUpdateBadFilter(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-find-one-and-update")))
                       (send col :find-one-and-update '(1) '(("$set" (b . 7)))))`,
			PanicType: slip.Symbol("error"),
		}).Test(t)
	})
}
