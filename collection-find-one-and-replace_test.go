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

func TestCollectionFindOneAndReplaceProjectSort(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `
(let* ((db (send mc :database "quux"))
       (col (send db :collection "cool-find-replace")))
 (send col :insert-one '((a . 1) (b . 2)))
 (send col :insert-one '((a . 1) (b . 4)))
 (send col :insert-one '((a . 2) (b . 8)))
 (list
  (cadr
   (send col :find-one-and-replace '((a . 2)) '((a . 2) (b . 7))
                                   :native t
                                   :projection '((b . t))
                                   :sort '((b . -1))))
  (cadr (send col :find-one '((a . 2)) :native t :projection '((b . t))))))`,
			Expect: `(("b" . 8) ("b" . 7))`,
		}).Test(t)
	})
}

func TestCollectionFindOneAndReplaceWrap(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `
(let* ((db (send mc :database "quux"))
       (col (send db :collection "cool-find-replace-wrap")))
 (send col :insert-one '((a . 1) (b . 2)))
 (send col :insert-one '((a . 1) (b . 4)))
 (send col :insert-one '((a . 2) (b . 8)))
 (send col :find-one-and-replace nil '((a . 1) (b . 3)) :projection '((b . t)) :after t :wrap t))`,
			Validate: func(t *testing.T, v slip.Object) {
				bg := v.(*flavors.Instance)
				tt.Equal(t, true, jp.C("_id").C("$toObjectId").Has(bg.Any))
				tt.Equal(t, 3, jp.C("b").First(bg.Any))
			},
		}).Test(t)
	})
}

func TestCollectionFindOneAndReplaceNotFound(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-not-found")))
                      (send col :find-one-and-replace '((a . 1)) '((a . 1) (b . 3))))`,
			Expect: "nil",
		}).Test(t)
	})
}

func TestCollectionFindOneAndReplaceBadFilter(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-find-one-and-replace")))
                       (send col :find-one-and-replace '(1) '((a . 1))))`,
			PanicType: slip.Symbol("error"),
		}).Test(t)
	})
}
