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

func TestDatabaseDocs(t *testing.T) {
	scope := slip.NewScope()
	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	for _, method := range []string{
		":name",
		":drop",
		":collection",
		":collections",
	} {
		_ = slip.ReadString(fmt.Sprintf(`(describe-method mongo-database %s out)`, method)).Eval(scope, nil)
		tt.Equal(t, true, strings.Contains(out.String(), method))
		out.Reset()
	}
}

func TestDatabaseName(t *testing.T) {
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
                  (send (send mc :database 'quux) :name))`,
		Expect: `"quux"`,
	}).Test(t)
}

func TestDatabaseCollection(t *testing.T) {
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

func TestDatabaseCollections(t *testing.T) {
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
                  (send (send mc :database "admin") :collections))`,
		Expect: "/\"system.version\" :collection nil/",
	}).Test(t)
	(&sliptest.Function{
		Scope: scope,
		Source: `(progn
	              (setq mc (mongo-connect murl :timeout 3))
                  (send (send mc :database "admin") :collections "system"))`,
		Expect: "/\"system.version\" :collection nil/",
	}).Test(t)

	(&sliptest.Function{
		Scope: scope,
		Source: `(progn
	              (setq mc (mongo-connect murl :timeout 3))
                  (send (send mc :database "admin") :collections (make-bag "[1]")))`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
}

func TestDatabaseDrop(t *testing.T) {
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
                  (send (send mc :database "quux") :drop))`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Scope: scope,
		Source: `(progn
                  (setq mc (mongo-connect murl :timeout 3))
                  (send (send mc :database "admin") :drop))`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
}
