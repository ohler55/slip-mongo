// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slipmongo_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestIndexesDropNamed(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `
(let* ((db (send mc :database "quux"))
       (col (send db :collection "cool-indexes-drop"))
       (indexes (send col :indexes)))
 (send col :insert-one '((a . 1) (b . 2))) ;; _id index is created on insert
 (send indexes :create '((a . 1)) :name "aaa")
 (send indexes :create '((b . 1)) :name "bbb")
 (send indexes :drop "aaa")
 (mapcar (lambda (x) (car x)) (send indexes :list)))
`,
			Expect: `("_id_" "bbb")`,
		}).Test(t)
	})
}

func TestIndexesDropAll(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `
(let* ((db (send mc :database "quux"))
       (col (send db :collection "cool-indexes-drop"))
       (indexes (send col :indexes)))
 (send col :insert-one '((a . 1) (b . 2))) ;; _id index is created on insert
 (send indexes :create '((a . 1)) :name "aaa")
 (send indexes :create '((b . 1)) :name "bbb")
 (send indexes :drop)
 (mapcar (lambda (x) (car x)) (send indexes :list)))
`,
			Expect: `("_id_")`,
		}).Test(t)
	})
}

func TestIndexesDropError(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `
(let* ((db (send mc :database "quux"))
       (col (send db :collection "cool-indexes-drop"))
       (indexes (send col :indexes)))
 (send col :insert-one '((a . 1) (b . 2))) ;; _id index is created on insert
 (send indexes :drop "xxx"))
`,
			PanicType: slip.Symbol("error"),
		}).Test(t)
	})
}
