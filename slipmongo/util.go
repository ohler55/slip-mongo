// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slipmongo

import (
	"time"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/bag"
	"github.com/ohler55/slip/pkg/flavors"
	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/mongo"
)

func filterFromArg(arg slip.Object) (filter any) {
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
			slip.PanicType("filter", ta, "gi:bag", "list", "nil")
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
				slip.PanicType("filter list element", v, "cons", "list")
			}
		}
		filter = d
	default:
		slip.PanicType("filter", ta, "gi:bag", "list", "nil")
	}
	return
}

func instTimeout(inst *flavors.Instance) time.Duration {
	timeout := defaultTimeout
	client := inst.Get("client").(*flavors.Instance)
	if tp := client.Any.(*mongo.Client).Timeout(); tp != nil {
		timeout = *tp
	}
	return timeout
}
