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
		_ = slip.ReadString(fmt.Sprintf(`(describe-method mongo-client %s out)`, method)).Eval(scope, nil)
		tt.Equal(t, true, strings.Contains(out.String(), method))
		out.Reset()
	}
}

func TestClientPing(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("murl"), slip.String(mongoURL))
	scope.Let(slip.Symbol("mc"), nil)
	defer func() {
		_ = slip.ReadString(`(send mc :disconnect)`).Eval(scope, nil)
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
		_ = slip.ReadString(`(send mc :disconnect)`).Eval(scope, nil)
	}()

	(&sliptest.Function{
		Scope:  scope,
		Source: `(progn (setq mc (mongo-connect murl :timeout 3)) (send mc :database "test"))`,
		Expect: "/#<mongo-database [0-9a-f]+>/",
	}).Test(t)
}
