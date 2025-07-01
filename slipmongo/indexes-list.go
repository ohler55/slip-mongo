// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slipmongo

import (
	"context"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/mongo"
)

type indexesListCaller struct{}

func (caller indexesListCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*flavors.Instance)

	ctx, cf := context.WithTimeout(context.Background(), defaultTimeout)
	defer cf()

	specs, err := self.Any.(mongo.IndexView).ListSpecifications(ctx)
	if err != nil {
		panic(err)
	}
	list := make(slip.List, len(specs))
	for i, spec := range specs {
		a := slip.List{slip.String(spec.Name)}
		var kd any
		_ = bson.Unmarshal(spec.KeysDocument, &kd)
		if klist, ok := BsonToObject(kd, false).(slip.List); ok {
			a = append(a, append(slip.List{slip.String("keys")}, klist...))
		}
		a = append(a, slip.List{slip.String("namespace"), slip.Tail{Value: slip.String(spec.Namespace)}})
		a = append(a, slip.List{slip.String("version"), slip.Tail{Value: slip.Fixnum(spec.Version)}})
		if spec.Unique != nil && *spec.Unique {
			a = append(a, slip.List{slip.String("unique"), slip.Tail{Value: slip.True}})
		}
		if spec.Sparse != nil && *spec.Sparse {
			a = append(a, slip.List{slip.String("sparse"), slip.Tail{Value: slip.True}})
		}
		if spec.ExpireAfterSeconds != nil {
			a = append(a,
				slip.List{slip.String("expire-after"), slip.Tail{Value: slip.Fixnum(*spec.ExpireAfterSeconds)}})
		}
		list[i] = a
	}
	return list
}

func (caller indexesListCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":list",
		Text: `Returns a list of the indexes. Each element of the list is an association list
of the index name followed by a _cons_ for all other non-nil attributes of
keys, version, expiration time, a flag indicating sparseness, and a flag
indicating uniqueness.`,
		Return: "list",
	}
}
