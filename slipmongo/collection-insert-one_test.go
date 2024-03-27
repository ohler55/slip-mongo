// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slipmongo_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCollectionInsertOneBasic(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-one")))
                      (list
                       (send col :insert-one '((a . 1)))
                       (send col :find-one '((a . 1)) :native t)))`,
			Expect: `/\("[0-9a-f]+\" \(\("_id" . "[0-9a-f]+"\) \("a" . 1\)\)\)/`,
		}).Test(t)
	})
}

func TestCollectionInsertOneWrap(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-too")))
                      (list
                       (send col :insert-one '((a . 1)) :wrap t)
                       (send col :find-one '((a . 1)) :native t :wrap t)))`,
			Validate: func(t *testing.T, v slip.Object) {
				list := v.(slip.List)
				tt.Equal(t, 2, len(list))
				cons := list[0].(slip.List)[0].(slip.List)
				id := cons.Cdr()
				tt.Equal(t, slip.String("$toObjectId"), cons.Car())

				list = list[1].(slip.List)
				tt.Equal(t, `("a" . 1)`, slip.ObjectString(list[1]))
				cons = list[0].(slip.List)
				tt.Equal(t, slip.String("_id"), cons.Car())
				cons = cons[1].(slip.List)
				tt.Equal(t, slip.String("$toObjectId"), cons.Car())
				tt.Equal(t, id, cons.Cdr())
			},
		}).Test(t)
	})
}

func TestCollectionInsertOneError(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-bad")))
                      (send col :insert-one 'bad))`,
			PanicType: slip.Symbol("error"),
		}).Test(t)
	})
}
