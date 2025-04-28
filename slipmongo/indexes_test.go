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

func TestIndexesDocs(t *testing.T) {
	scope := slip.NewScope()
	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	for _, method := range []string{
		":create",
		":drop",
		":list",
	} {
		_ = slip.ReadString(fmt.Sprintf(`(describe-method mongo-indexes %s out)`, method), scope).Eval(scope, nil)
		tt.Equal(t, true, strings.Contains(out.String(), method))
		out.Reset()
	}
}

func TestIndexesList(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `
(let* ((db (send mc :database "quux"))
       (col (send db :collection "cool-indexes")))
 (send col :insert-one '((a . 1))) ;; _id index is created on insert
 (send (send col :indexes) :list))
`,
			Expect: `(("_id_" ("keys" ("_id" . 1)) ("namespace" . "quux.cool-indexes") ("version" . 2)))`,
		}).Test(t)
	})
}
