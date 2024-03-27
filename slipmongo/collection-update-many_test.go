// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slipmongo_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCollectionUpdateManyBasic(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-update-many")))
                      (send col :insert-one '((a . 1) (b . 2)))
                      (list
                       (send col :update-many '((a . 1)) '(("$set" (b . 3))))
                       (assoc "b" (send col :find-one '((a . 1)) :native t :projection '((b . t))))))`,
			Expect: `(1 ("b" . 3))`,
		}).Test(t)
	})
}

func TestCollectionUpdateManyNilFilter(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-update-many2")))
                      (send col :insert-one '((a . 1) (b . 2)))
                      (list
                       (send col :update-many nil '(("$set" (b . 3))))
                       (assoc "b" (send col :find-one '((a . 1)) :native t :projection '((b . t))))))`,
			Expect: `(1 ("b" . 3))`,
		}).Test(t)
	})
}

func TestCollectionUpdateManyNilUpdate(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-update")))
                      (send col :insert-one '((a . 1) (b . 2)))
                      (send col :update-many '((a . 1)) nil))`,
			PanicType: slip.Symbol("error"),
		}).Test(t)
	})
}
