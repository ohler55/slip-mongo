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

func TestCollectionDocs(t *testing.T) {
	scope := slip.NewScope()
	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	for _, method := range []string{
		":aggregate",
		":count-documents",
		":delete-many",
		":delete-one",
		":distinct",
		":drop",
		":estimated-document-count",
		":find",
		":find-one",
		":find-one-and-delete",
		":find-one-and-replace",
		":find-one-and-update",
		":indexes",
		":insert-many",
		":insert-one",
		":name",
		":replace-one",
		":update-by-id",
		":update-many",
		":update-one",
	} {
		_ = slip.ReadString(fmt.Sprintf(`(describe-method mongo-collection %s out)`, method), scope).Eval(scope, nil)
		tt.Equal(t, true, strings.Contains(out.String(), method))
		out.Reset()
	}
}

func TestCollectionName(t *testing.T) {
	testWithConnect(t, func(t *testing.T, scope *slip.Scope) {
		(&sliptest.Function{
			Scope: scope,
			Source: `(let ((db (send mc :database "quux")))
                      (send (send db :collection "cool") :name))`,
			Expect: `"cool"`,
		}).Test(t)
	})
}
