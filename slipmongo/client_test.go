// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slipmongo_test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestClientDocs(t *testing.T) {
	scope := slip.NewScope()
	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	for _, method := range []string{
		":ping",
		":disconnect",
		":database",
		":databases",
	} {
		_ = slip.ReadString(fmt.Sprintf(`(describe-method mongo-client %s out)`, method), scope).Eval(scope, nil)
		tt.Equal(t, true, strings.Contains(out.String(), method))
		out.Reset()
	}
}

func TestClientPing(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("murl"), slip.String(mongoURL))
	scope.Let(slip.Symbol("mc"), nil)
	defer func() {
		_ = slip.ReadString(`(send mc :disconnect)`, scope).Eval(scope, nil)
	}()

	(&sliptest.Function{
		Scope:  scope,
		Source: `(progn (setq mc (mongo-connect murl :timeout 3)) (list (send mc :ping) (mongo-ping mc)))`,
		Expect: "(t t)",
	}).Test(t)
}

func TestClientDatabase(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("murl"), slip.String(mongoURL))
	scope.Let(slip.Symbol("mc"), nil)
	defer func() {
		_ = slip.ReadString(`(send mc :disconnect)`, scope).Eval(scope, nil)
	}()

	(&sliptest.Function{
		Scope:  scope,
		Source: `(progn (setq mc (mongo-connect murl :timeout 3)) (send (send mc :database "test") :name))`,
		Expect: `"test"`,
	}).Test(t)
}

func TestClientDatabases(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("murl"), slip.String(mongoURL))
	scope.Let(slip.Symbol("mc"), nil)
	defer func() {
		_ = slip.ReadString(`(send mc :disconnect)`, scope).Eval(scope, nil)
	}()

	(&sliptest.Function{
		Scope: scope,
		Source: `(progn
                  (setq mc (mongo-connect murl :timeout 3))
                  (send mc :databases))`,
		Expect: "/\"admin\" . [0-9]+/",
	}).Test(t)
	(&sliptest.Function{
		Scope: scope,
		Source: `(progn
                  (setq mc (mongo-connect murl :timeout 3))
                  (send mc :databases "adm"))`,
		Expect: "/\"admin\" . [0-9]+/",
	}).Test(t)
	(&sliptest.Function{
		Scope: scope,
		Source: `(progn
                  (setq mc (mongo-connect murl :timeout 3))
                  (send mc :databases (make-bag "{name:admin}")))`,
		Expect: "/\"admin\" . [0-9]+/",
	}).Test(t)
	(&sliptest.Function{
		Scope: scope,
		Source: `(progn
                  (setq mc (mongo-connect murl :timeout 3))
                  (send mc :databases '((name . admin))))`,
		Expect: "/\"admin\" . [0-9]+/",
	}).Test(t)

	(&sliptest.Function{
		Scope: scope,
		Source: `(progn
                  (setq mc (mongo-connect murl :timeout 3))
                  (send mc :databases (make-instance 'vanilla-flavor)))`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Scope: scope,
		Source: `(progn
                  (setq mc (mongo-connect murl :timeout 3))
                  (send mc :databases '(t)))`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Scope: scope,
		Source: `(progn
                  (setq mc (mongo-connect murl :timeout 3))
                  (send mc :databases '((t t))))`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Scope: scope,
		Source: `(progn
                  (setq mc (mongo-connect murl :timeout 3))
                  (send mc :databases 7))`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Scope: scope,
		Source: `(progn
                  (setq mc (mongo-connect murl :timeout 3))
                  (send mc :databases (make-bag "[1]")))`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
}

func TestClientDoubleDisconnect(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("murl"), slip.String(mongoURL))

	(&sliptest.Function{
		Scope:     scope,
		Source:    `(let ((mc (mongo-connect murl))) (send mc :disconnect) (send mc :disconnect))`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
}

func TestConnectBadURL(t *testing.T) {
	(&sliptest.Function{
		Source:    `(mongo-connect t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    fmt.Sprintf(`(mongo-connect "mongodb://localhost:%d" :timeout 1)`, availablePort()),
		PanicType: slip.Symbol("error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(mongo-connect "http://localhost:27017" :timeout 1)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
}

func TestConnectBadTimeout(t *testing.T) {
	(&sliptest.Function{
		Source:    `(mongo-connect "mongodb://localhost:27017" :timeout t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestClientGoMakeOnly(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-instance 'mongo-client)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
