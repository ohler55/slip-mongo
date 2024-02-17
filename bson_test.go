// Copyright (c) 2024, Peter Ohler, All rights reserved.

package main_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	sm "github.com/ohler55/slip-mongo"
	"go.mongodb.org/mongo-driver/bson"
)

func TestToBsonNonSlip(t *testing.T) {
	tt.Equal(t, "a string", sm.ToBson("a string"))
	tt.Equal(t, int32(71), sm.ToBson(int64(71)))
	tt.Equal(t, int64(2147483648), sm.ToBson(int64(2147483648)))
	tt.Equal(t, int64(2147483648), sm.ToBson(2147483648))
	tt.Equal(t, bson.A{int32(1), int32(2), int32(3)}, sm.ToBson([]any{1, 2, 3}))
}

func TestToBsonSlip(t *testing.T) {
	tt.Equal(t, "a string", sm.ToBson(slip.String("a string")))
	tt.Equal(t, int32(71), sm.ToBson(slip.Fixnum(71)))
	tt.Equal(t, int64(2147483648), sm.ToBson(slip.Fixnum(2147483648)))
}
