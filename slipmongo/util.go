// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slipmongo

import (
	"time"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/bag"
	"github.com/ohler55/slip/pkg/flavors"
	"go.mongodb.org/mongo-driver/v2/bson"
)

func filterFromArg(s *slip.Scope, arg slip.Object, depth int) (filter any) {
	switch ta := arg.(type) {
	case nil:
		// leave filter as nil
	case slip.String:
		filter = map[string]any{
			"name": map[string]any{
				"$regex": string(ta),
			},
		}
	case *flavors.Instance:
		if ta.Type != bag.Flavor() {
			slip.TypePanic(s, depth, "filter", ta, "gi:bag", "list", "nil")
		}
		filter = ta.Any
	case slip.List:
		d := bson.D{}
		for _, v := range ta {
			if cons, ok := v.(slip.List); ok && 0 < len(cons) {
				cdr := cons.Cdr()
				if list, _ := cdr.(slip.List); 0 < len(list) {
					cdr = list[0]
				}
				d = append(d, bson.E{
					Key:   slip.MustBeString(cons.Car(), "filter element key"),
					Value: slip.Simplify(cdr),
				})
			} else {
				slip.TypePanic(s, depth, "filter list element", v, "cons", "list")
			}
		}
		filter = d
	default:
		slip.TypePanic(s, depth, "filter", ta, "gi:bag", "list", "nil")
	}
	return
}

func timeoutFromArgs(s *slip.Scope, args slip.List, depth int) time.Duration {
	timeout := defaultTimeout
	if v, has := slip.GetArgsKeyValue(args, slip.Symbol(":timeout")); has {
		if num, ok := v.(slip.Fixnum); ok {
			timeout = time.Second * time.Duration(num)
		} else {
			slip.TypePanic(s, depth, ":timeout", v, "fixnum")
		}
	}
	return timeout
}
