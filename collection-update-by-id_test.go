// Copyright (c) 2024, Peter Ohler, All rights reserved.

package main_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCollectionUpdateByIDBasic(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-update-by-id")))
                      (send col :insert-one '((_id . one) (b . 2)))
                      (list
                       (send col :update-by-id 'one '(("$set" (b . 3))))
                       (assoc "b" (send col :find-one '((_id . one)) :native t :projection '((b . t))))))`,
			Expect: `(1 ("b" . 3))`,
		}).Test(t)
	})
}

func TestCollectionUpdateByIDNone(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-update-by-id-none")))
                      (send col :insert-one '((_id . one) (b . 2)))
                      (list
                       (send col :update-by-id 'two '(("$set" (b . 3))))
                       (assoc "b" (send col :find-one '((_id . one)) :native t :projection '((b . t))))))`,
			Expect: `(0 ("b" . 2))`,
		}).Test(t)
	})
}

func TestCollectionUpdateByIDBadID(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-update-bad")))
                      (send col :insert-one '((_id . one) (b . 2)))
                      (send col :update-by-id 'one '(3)))`,
			PanicType: slip.Symbol("error"),
		}).Test(t)
	})
}
