// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slipmongo_test

import (
	"testing"

	"github.com/ohler55/ojg/jp"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/sliptest"
)

func TestCollectionAggregateNative(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `
(let* ((db (send mc :database "quux"))
       (col (send db :collection "cool-aggregate"))
       (results nil))
 (send col :insert-one '((a . 1) (b . 2)))
 (send col :insert-one '((a . 1) (b . 4)))
 (send col :insert-one '((a . 2) (b . 8)))
 (send col :aggregate
           (lambda (rec) (setq results (add results (cadr rec))))
           (list (make-bag "{$group: {_id: $a total: {$sum: $b}}}"))
           :native t
           :batch 5
           :allow-disk-use t
           :wrap t)
 results)`,
			Validate: func(t *testing.T, v slip.Object) {
				list := v.(slip.List)
				tt.Equal(t, 2, len(list))
				var (
					six   bool
					eight bool
				)
				for _, v := range list {
					cons, _ := v.(slip.List)
					switch cons.Cdr() {
					case slip.Fixnum(6):
						six = true
					case slip.Fixnum(8):
						eight = true
					}
				}
				tt.Equal(t, true, six)
				tt.Equal(t, true, eight)
			},
			Expect: `(("total" . 6) ("total" . 8))`,
		}).Test(t)
	})
}

func TestCollectionAggregateBag(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `
(let* ((db (send mc :database "quux"))
       (col (send db :collection "cool-aggregate-bag"))
       (results nil))
 (send col :insert-one '((a . 1) (b . 2)))
 (send col :insert-one '((a . 1) (b . 4)))
 (send col :insert-one '((a . 2) (b . 8)))
 (send col :aggregate
           (lambda (rec) (setq results (add results rec)))
           (list (make-bag "{$group: {_id: $a total: {$sum: $b}}}")))
 results)`,
			Validate: func(t *testing.T, v slip.Object) {
				list := v.(slip.List)
				tt.Equal(t, 2, len(list))
				for _, v := range list {
					bg := v.(*flavors.Instance)
					tt.Equal(t, true, jp.MustParseString("[?@ == 6 || @ == 8]").Has(bg.Any))
				}
			},
		}).Test(t)
	})
}

func TestCollectionAggregateBadBatch(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-find-one-and-delete")))
                       (send col :aggregate (lambda (rec) nil) '(1)))`,
			PanicType: slip.Symbol("error"),
		}).Test(t)
	})
}

func TestCollectionAggregateBadPipeline(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let* ((db (send mc :database "quux"))
                            (col (send db :collection "cool-find-one-and-delete")))
                       (send col :aggregate (lambda (rec) nil)
                                            (list (make-bag "{$group: {_id: $a total: {$sum: $b}}}")) :batch t))`,
			PanicType: slip.Symbol("type-error"),
		}).Test(t)
	})
}
