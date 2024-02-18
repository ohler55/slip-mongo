// Copyright (c) 2024, Peter Ohler, All rights reserved.

package main_test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCollectionDocs(t *testing.T) {
	scope := slip.NewScope()
	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	for _, method := range []string{
		":name",
		":drop",
	} {
		_ = slip.ReadString(fmt.Sprintf(`(describe-method mongo-collection %s out)`, method)).Eval(scope, nil)
		tt.Equal(t, true, strings.Contains(out.String(), method))
		out.Reset()
	}
}

func TestCollectionName(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("murl"), slip.String(mongoURL))
	scope.Let(slip.Symbol("mc"), nil)
	defer func() {
		_ = slip.ReadString(`(send mc :disconnect)`).Eval(scope, nil)
	}()

	(&sliptest.Function{
		Scope: scope,
		Source: `(progn
                  (setq mc (mongo-connect murl :timeout 3))
                  (let ((db (send mc :database "quux")))
                   (send (send db :collection "cool") :name)))`,
		Expect: `"cool"`,
	}).Test(t)
}

func TestCollectionDrop(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("murl"), slip.String(mongoURL))
	scope.Let(slip.Symbol("mc"), nil)
	defer func() {
		_ = slip.ReadString(`(send mc :disconnect)`).Eval(scope, nil)
	}()

	(&sliptest.Function{
		Scope: scope,
		Source: `(progn
                  (setq mc (mongo-connect murl :timeout 3))
                  (let ((db (send mc :database "quux")))
                   (send (send db :collection "cool") :drop)))`,
		Expect: "nil",
	}).Test(t)
}

func TestCollectionDropError(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("murl"), slip.String(mongoURL))
	scope.Let(slip.Symbol("mc"), nil)
	defer func() {
		_ = slip.ReadString(`(send mc :disconnect)`).Eval(scope, nil)
	}()

	(&sliptest.Function{
		Scope: scope,
		Source: `(progn
                  (setq mc (mongo-connect murl :timeout 3))
                  (let ((db (send mc :database "admin")))
                   (send (send db :collection "system.version") :drop)))`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
}

func TestCollectionInsertFindOne(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("murl"), slip.String(mongoURL))
	scope.Let(slip.Symbol("mc"), nil)
	defer func() {
		_ = slip.ReadString(`(send mc :disconnect)`).Eval(scope, nil)
	}()

	(&sliptest.Function{
		Scope: scope,
		Source: `(progn
                  (setq mc (mongo-connect murl :timeout 3))
                  (let* ((db (send mc :database "quux"))
                         (col (send db :collection "cool")))
                   (send col :insert-one '((a . 1)))
                   (send col :find-one '((a . 1)) :native t)))`,
		Expect: `/\(\("_id" . "[0-9a-f]+"\) \("a" . 1\)\)/`,
	}).Test(t)
}
