// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slipmongo_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCollectionestimatedDocumentCountsBasic(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-estimated-count")))
                      (send col :insert-one '((a . 1) (b . 2)))
                      (send col :insert-one '((a . 1) (b . 4)))
                      (send col :insert-one '((a . 2) (b . 8)))
                      (send col :estimated-document-count))`,
			Expect: `3`,
		}).Test(t)
	})
}
