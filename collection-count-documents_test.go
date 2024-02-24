// Copyright (c) 2024, Peter Ohler, All rights reserved.

package main_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCollectionCountDocumentsBasic(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-count")))
                      (send col :insert-one '((a . 1) (b . 2)))
                      (send col :insert-one '((a . 1) (b . 4)))
                      (send col :insert-one '((a . 2) (b . 8)))
                      (send col :count-documents '((a . 1))))`,
			Expect: `2`,
		}).Test(t)
	})
}

func TestCollectionCountDocumentsSkipLimit(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-count-skip-limit")))
                      (send col :insert-one '((a . 1) (b . 2)))
                      (send col :insert-one '((a . 1) (b . 4)))
                      (send col :insert-one '((a . 2) (b . 8)))
                      (send col :count-documents nil :skip 1 :limit 2))`,
			Expect: `2`,
		}).Test(t)
	})
}

func TestCollectionCountDocumentsBadSkip(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-count-skip-limit")))
                      (send col :count-documents nil :skip t :limit 2))`,
			PanicType: slip.Symbol("type-error"),
		}).Test(t)
	})
}

func TestCollectionCountDocumentsBadLimit(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-count-skip-limit")))
                      (send col :count-documents nil :skip 1 :limit t))`,
			PanicType: slip.Symbol("type-error"),
		}).Test(t)
	})
}

func TestCollectionCountDocumentsBadFilter(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-count")))
                       (send col :count-documents '(1)))`,
			PanicType: slip.Symbol("error"),
		}).Test(t)
	})
}
