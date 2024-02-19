// Copyright (c) 2024, Peter Ohler, All rights reserved.

package main_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCollectionFindBasic(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-find"))
                            (results nil))
                      (send col :insert-one '((a . 1) (b . 2)))
                      (send col :insert-one '((a . 1) (b . 4)))
                      (send col :insert-one '((a . 2) (b . 8)))
                      (send col :find (lambda (rec) (setq results (add results (cadr rec)))) nil
                                     :native t :wrap t :projection '((b . t)) :sort '((b . -1)))
                      results)`,
			Expect: `(("b" . 8) ("b" . 4) ("b" . 2))`,
		}).Test(t)
	})
}

func TestCollectionFindKeys(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-find-keys"))
                            (results nil))
                      (send col :insert-one '((a . 1) (b . 2)))
                      (send col :insert-one '((a . 1) (b . 4)))
                      (send col :insert-one '((a . 2) (b . 8)))
                      (send col :find (lambda (rec) (setq results (add results (bag-get rec "b")))) nil
                                     :projection '((b . t))
                                     :sort '((b . 1))
                                     :skip 1
                                     :limit 1
                                     :allow-disk-use t
                                     :batch 4)
                      results)`,
			Expect: `(4)`,
		}).Test(t)
	})
}

func TestCollectionFindBadSkip(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-find")))
                      (send col :find (lambda (rec) nil) nil :skip t))`,
			PanicType: slip.Symbol("type-error"),
		}).Test(t)
	})
}

func TestCollectionFindBadLimit(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-find")))
                      (send col :find (lambda (rec) nil) nil :limit t))`,
			PanicType: slip.Symbol("type-error"),
		}).Test(t)
	})
}

func TestCollectionFindBadBatch(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-find")))
                      (send col :find (lambda (rec) nil) nil :batch t))`,
			PanicType: slip.Symbol("type-error"),
		}).Test(t)
	})
}
