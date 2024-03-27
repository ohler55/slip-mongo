// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slipmongo_test

import (
	"fmt"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCollectionInsertManyBasic(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-many")))
                      (list
                       (send col :insert-many '(((a . 1)) ((a . 2))))
                       (send col :find-one '((a . 1)) :native t)
                       (send col :find-one '((a . 2)) :native t)))`,
			Validate: func(t *testing.T, v slip.Object) {
				list := v.(slip.List)
				tt.Equal(t, 3, len(list))
				tt.Equal(t,
					fmt.Sprintf(`(("_id" . %s) ("a" . 1))`, list[0].(slip.List)[0]),
					slip.ObjectString(list[1]))
				tt.Equal(t,
					fmt.Sprintf(`(("_id" . %s) ("a" . 2))`, list[0].(slip.List)[1]),
					slip.ObjectString(list[2]))
			},
		}).Test(t)
	})
}

func TestCollectionInsertManyWrap(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-many-wrap")))
                      (list
                       (send col :insert-many '(((a . 1)) ((a . 2))) :wrap t)
                       (send col :find-one '((a . 1)) :native t)
                       (send col :find-one '((a . 2)) :native t)))`,
			Validate: func(t *testing.T, v slip.Object) {
				list := v.(slip.List)
				tt.Equal(t, 3, len(list))
				tt.Equal(t,
					fmt.Sprintf(`(("_id" . %s) ("a" . 1))`, list[0].(slip.List)[0].(slip.List)[0].(slip.List).Cdr()),
					slip.ObjectString(list[1]))
				tt.Equal(t,
					fmt.Sprintf(`(("_id" . %s) ("a" . 2))`, list[0].(slip.List)[1].(slip.List)[0].(slip.List).Cdr()),
					slip.ObjectString(list[2]))
			},
		}).Test(t)
	})
}

func TestCollectionInsertManyBadDocs(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-bad")))
                      (send col :insert-many 'bad))`,
			PanicType: slip.Symbol("type-error"),
		}).Test(t)
	})
}

func TestCollectionInsertManyError(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-bad")))
                      (send col :insert-many '(bad)))`,
			PanicType: slip.Symbol("error"),
		}).Test(t)
	})
}
