// Copyright (c) 2024, Peter Ohler, All rights reserved.

package main_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestIndexesCreateSparseUnique(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `
(let* ((db (send mc :database "quux"))
       (col (send db :collection "cool-indexes-create"))
       (indexes (send col :indexes)))
 (send col :insert-one '((a . 1) (b . 2))) ;; _id index is created on insert
 (send indexes :create '((a . -1)) :sparse t :unique t)
 (assoc "a_-1" (send indexes :list)))
`,
			Validate: func(t *testing.T, v slip.Object) {
				for _, x := range v.(slip.List) {
					cons, ok := x.(slip.List)
					if !ok {
						continue
					}
					switch cons.Car() {
					case slip.String("keys"):
						tt.Equal(t, `(("a" . -1))`, slip.ObjectString(cons.Cdr()))
					case slip.String("unique"), slip.String("sparse"):
						tt.Equal(t, slip.True, cons.Cdr())
					}
				}
			},
		}).Test(t)
	})
}

func TestIndexesCreateName(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `
(let* ((db (send mc :database "quux"))
       (col (send db :collection "cool-indexes-named"))
       (indexes (send col :indexes)))
 (send col :insert-one '((a . 1) (b . 2))) ;; _id index is created on insert
 (send indexes :create '((a . 1)) :name "aaa" :language "english" :expire-after 100)
 (assoc "aaa" (send indexes :list)))
`,
			Validate: func(t *testing.T, v slip.Object) {
				for _, x := range v.(slip.List) {
					cons, ok := x.(slip.List)
					if !ok {
						continue
					}
					switch cons.Car() {
					case slip.String("keys"):
						tt.Equal(t, `(("a" . 1))`, slip.ObjectString(cons.Cdr()))
					case slip.String("expire-after"):
						tt.Equal(t, slip.Fixnum(100), cons.Cdr())
					}
				}
			},
		}).Test(t)
	})
}

func TestIndexesCreateBadExpire(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `
(let* ((db (send mc :database "quux"))
       (col (send db :collection "cool-indexes-bad"))
       (indexes (send col :indexes)))
 (send indexes :create '((a . 1)) :expire-after t))
`,
			PanicType: slip.Symbol("type-error"),
		}).Test(t)
	})
}

func TestIndexesCreateBadKeys(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `
(let* ((db (send mc :database "quux"))
       (col (send db :collection "cool-indexes-bad"))
       (indexes (send col :indexes)))
 (send indexes :create '(1)))
`,
			PanicType: slip.Symbol("error"),
		}).Test(t)
	})
}
