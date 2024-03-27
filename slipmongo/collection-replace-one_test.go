// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slipmongo_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCollectionReplaceOneBasic(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-replace")))
                      (send col :insert-one '((a . 1) (b . 2)))
                      (list
                       (send col :replace-one '((a . 1)) '((a . 1) (b . 3)))
                       (assoc "b" (send col :find-one '((a . 1)) :native t :projection '((b . t))))))`,
			Expect: `(1 ("b" . 3))`,
		}).Test(t)
	})
}

func TestCollectionReplaceOneNilFilter(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-replace-any")))
                      (send col :insert-one '((a . 1) (b . 2)))
                      (list
                       (send col :replace-one nil '((a . 1) (b . 3)))
                       (assoc "b" (send col :find-one '((a . 1)) :native t :projection '((b . t))))))`,
			Expect: `(1 ("b" . 3))`,
		}).Test(t)
	})
}

func TestCollectionReplaceOneNilReplace(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-replace")))
                      (send col :insert-one '((a . 1) (b . 2)))
                      (send col :replace-one '((a . 1)) nil))`,
			PanicType: slip.Symbol("error"),
		}).Test(t)
	})
}
