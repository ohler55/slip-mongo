// Copyright (c) 2024, Peter Ohler, All rights reserved.

package main_test

import (
	"math/big"
	"testing"
	"time"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	sm "github.com/ohler55/slip-mongo"
	"github.com/ohler55/slip/pkg/gi"
	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/bson/primitive"
)

func TestToBsonNonSlip(t *testing.T) {
	tt.Equal(t, "a string", sm.ToBson("a string"))
	tt.Equal(t, int32(71), sm.ToBson(int64(71)))
	tt.Equal(t, int64(2147483648), sm.ToBson(int64(2147483648)))
	tt.Equal(t, int64(2147483648), sm.ToBson(2147483648))
	tt.Equal(t, bson.A{int32(1), int32(2), int32(3)}, sm.ToBson([]any{1, 2, 3}))
	tt.Equal(t, bson.M{"a": int32(1)}, sm.ToBson(map[string]any{"a": 1}))
	tt.Equal(t,
		primitive.Timestamp{T: 397716434, I: 2011714325},
		sm.ToBson(map[string]any{"$timestamp": int64(1708179079_123456789)}))
	tt.Equal(t,
		primitive.Binary{
			Subtype: bson.TypeBinaryUUID,
			Data: []byte{
				0x6e, 0xf1, 0x69, 0x94, 0x70, 0x1d, 0x44, 0xd5, 0x87, 0xec, 0x7e, 0xf3, 0xe2, 0xe5, 0x70, 0x9b,
			},
		},
		sm.ToBson(map[string]any{"$uuid": "6ef16994-701d-44d5-87ec-7ef3e2e5709b"}))
	tt.Equal(t,
		primitive.Binary{
			Subtype: bson.TypeBinaryMD5,
			Data: []byte{
				0x6e, 0xf1, 0x69, 0x94, 0x70, 0x1d, 0x44, 0xd5, 0x87, 0xec, 0x7e, 0xf3, 0xe2, 0xe5, 0x70, 0x9b,
			},
		},
		sm.ToBson(map[string]any{"$md5": "6ef16994701d44d587ec7ef3e2e5709b"}))
	dstr := "123456789012345678901234567890"
	d128, _ := primitive.ParseDecimal128(dstr)
	tt.Equal(t, d128, sm.ToBson(map[string]any{"$decimal128": dstr}))
	tt.Equal(t, primitive.Null{}, sm.ToBson(nil))
	// primitive.DateTime rounds to milliseconds
	tm := time.Unix(0, 1708179079_123456789).UTC()
	tt.Equal(t, primitive.NewDateTimeFromTime(tm), sm.ToBson(tm))
	tt.Equal(t, int32(71), sm.ToBson(int8(71)))
	tt.Equal(t, int32(71), sm.ToBson(int16(71)))
	tt.Equal(t, int32(71), sm.ToBson(uint8(71)))
	tt.Equal(t, int32(71), sm.ToBson(uint16(71)))
	tt.Equal(t, int32(71), sm.ToBson(uint32(71)))
	tt.Equal(t, int32(71), sm.ToBson(uint64(71)))
	tt.Equal(t, int64(2147483648), sm.ToBson(uint32(2147483648)))
	tt.Equal(t, int64(2147483648), sm.ToBson(uint64(2147483648)))
	tt.Equal(t, int64(2147483648), sm.ToBson(uint(2147483648)))
	tt.Equal(t, int32(71), sm.ToBson(uint(71)))
	tt.Equal(t, 2.5, sm.ToBson(float32(2.5)))
	d128, _ = primitive.ParseDecimal128("1234567890123456789")
	tt.Equal(t, d128, sm.ToBson(big.NewInt(1234567890123456789)))
	bbn := big.NewInt(int64(1234567890123456789))
	bbn = bbn.Mul(bbn, bbn)
	tt.Equal(t, "1524157875323883675019051998750190521", sm.ToBson(bbn))
	tt.Equal(t, primitive.Null{}, sm.ToBson(primitive.Null{}))
	tt.Panic(t, func() { _ = sm.ToBson(struct{}{}) })
}

func TestToBsonSlip(t *testing.T) {
	tt.Equal(t, "a string", sm.ToBson(slip.String("a string")))
	tt.Equal(t, int32(71), sm.ToBson(slip.Fixnum(71)))
	tt.Equal(t, int64(2147483648), sm.ToBson(slip.Fixnum(2147483648)))
	tt.Equal(t, "quux", sm.ToBson(slip.Symbol("quux")))
	tt.Equal(t, false, sm.ToBson(slip.Symbol(":false")))
	tt.Equal(t, bson.A{int32(1), int32(2)}, sm.ToBson(slip.List{slip.Fixnum(1), slip.Fixnum(2)}))
	tt.Equal(t,
		bson.D{{Key: "a", Value: int32(1)}, {Key: "b", Value: int32(2)}},
		sm.ToBson(slip.List{
			slip.List{slip.Symbol("a"), slip.Tail{Value: slip.Fixnum(1)}},
			slip.List{slip.String("b"), slip.Tail{Value: slip.Fixnum(2)}},
		}))
	tt.Equal(t,
		bson.A{
			bson.A{"a", int32(1)},
			bson.A{int32(3), int32(2)},
		},
		sm.ToBson(slip.List{
			slip.List{slip.Symbol("a"), slip.Tail{Value: slip.Fixnum(1)}},
			slip.List{slip.Fixnum(3), slip.Tail{Value: slip.Fixnum(2)}},
		}))
	tt.Equal(t, 2.5, sm.ToBson(slip.DoubleFloat(2.5)))
	tt.Equal(t, 2.5, sm.ToBson(slip.SingleFloat(2.5)))
	tm := time.Unix(0, 1708179079_123456789).UTC()
	tt.Equal(t, primitive.NewDateTimeFromTime(tm), sm.ToBson(slip.Time(tm)))
	tt.Equal(t,
		primitive.Binary{
			Subtype: bson.TypeBinaryUUID,
			Data: []byte{
				0x6e, 0xf1, 0x69, 0x94, 0x70, 0x1d, 0x44, 0xd5, 0x87, 0xec, 0x7e, 0xf3, 0xe2, 0xe5, 0x70, 0x9b,
			},
		},
		sm.ToBson(gi.UUIDParse("6ef16994-701d-44d5-87ec-7ef3e2e5709b")))
	d128, _ := primitive.ParseDecimal128("1234567890123456789")
	tt.Equal(t, d128, sm.ToBson((*slip.Bignum)(big.NewInt(1234567890123456789))))
	bbn := big.NewInt(int64(1234567890123456789))
	bbn = bbn.Mul(bbn, bbn)
	tt.Equal(t, "1524157875323883675019051998750190521", sm.ToBson((*slip.Bignum)(bbn)))
	tt.Equal(t, "2.5", sm.ToBson((*slip.LongFloat)(big.NewFloat(2.5))))

	bg := slip.ReadString(`(make-bag "[1]")`).Eval(slip.NewScope(), nil)
	tt.Equal(t, bson.A{int32(1)}, sm.ToBson(bg))
	bg = slip.ReadString(`(make-instance 'vanilla-flavor)`).Eval(slip.NewScope(), nil)
	tt.Panic(t, func() { _ = sm.ToBson(bg) })
	tt.Equal(t, true, sm.ToBson(slip.True))
}

func TestSimplifyBson(t *testing.T) {
	tt.Equal(t, nil, sm.SimplifyBson(nil, true))
	tt.Equal(t, 2.5, sm.SimplifyBson(2.5, true))
	oid := "0123456789abcdef01234567"
	poid, _ := primitive.ObjectIDFromHex(oid)
	tt.Equal(t, map[string]any{"$toObjectId": oid}, sm.SimplifyBson(poid, true))
	tt.Equal(t, "quux", sm.SimplifyBson([]byte("quux"), true))
	tt.Equal(t, int64(71), sm.SimplifyBson(int32(71), true))
	tt.Equal(t, int64(71), sm.SimplifyBson(int(71), true))
	tt.Equal(t, map[string]any{"a": int64(1)}, sm.SimplifyBson(bson.M{"a": int32(1)}, true))
	tt.Equal(t, map[string]any{"a": int64(1)}, sm.SimplifyBson(bson.D{{Key: "a", Value: int32(1)}}, true))
	tt.Equal(t, []any{int64(1), int64(2)}, sm.SimplifyBson(bson.A{int32(1), int64(2)}, true))
	tt.Equal(t, map[string]any{"a": int64(1)}, sm.SimplifyBson(map[string]any{"a": int64(1)}, true))
	tt.Equal(t, []any{int64(1), int64(2)}, sm.SimplifyBson([]any{int32(1), int64(2)}, true))
	tt.Equal(t, nil, sm.SimplifyBson(primitive.Null{}, true))
	tt.Equal(t, "quux", sm.SimplifyBson(primitive.Symbol("quux"), true))
	tm := time.Unix(0, 1708179079_123000000).UTC()
	tt.Equal(t, tm, sm.SimplifyBson(primitive.NewDateTimeFromTime(tm), true))
	dstr := "123456789012345678901234567890"
	d128, _ := primitive.ParseDecimal128(dstr)
	tt.Equal(t, map[string]any{"$decimal128": dstr}, sm.SimplifyBson(d128, true))
	tt.Equal(t,
		map[string]any{"$timestamp": int64(1708179079_123456789)},
		sm.SimplifyBson(primitive.Timestamp{T: 397716434, I: 2011714325}, true))
	tt.Equal(t, "abc", sm.SimplifyBson(primitive.Regex{Pattern: "abc"}, true))
	tt.Equal(t, "abc/i", sm.SimplifyBson(primitive.Regex{Pattern: "abc", Options: "i"}, true))
	tt.Equal(t, "a = 1;", sm.SimplifyBson(primitive.JavaScript("a = 1;"), true))
	tt.Equal(t,
		map[string]any{"$uuid": "6ef16994-701d-44d5-87ec-7ef3e2e5709b"},
		sm.SimplifyBson(
			primitive.Binary{
				Subtype: bson.TypeBinaryUUID,
				Data: []byte{
					0x6e, 0xf1, 0x69, 0x94, 0x70, 0x1d, 0x44, 0xd5, 0x87, 0xec, 0x7e, 0xf3, 0xe2, 0xe5, 0x70, 0x9b,
				},
			}, true))

	tt.Equal(t, "{}", sm.SimplifyBson(struct{}{}, true))
	tt.Equal(t,
		map[string]any{"$md5": "6ef16994701d44d587ec7ef3e2e5709b"},
		sm.SimplifyBson(
			primitive.Binary{
				Subtype: bson.TypeBinaryMD5,
				Data: []byte{
					0x6e, 0xf1, 0x69, 0x94, 0x70, 0x1d, 0x44, 0xd5, 0x87, 0xec, 0x7e, 0xf3, 0xe2, 0xe5, 0x70, 0x9b,
				},
			}, true))
	tt.Equal(t,
		"quux",
		sm.SimplifyBson(
			primitive.Binary{
				Subtype: bson.TypeBinaryGeneric,
				Data:    []byte("quux"),
			}, true))

	tt.Equal(t, "{}", sm.SimplifyBson(struct{}{}, true))
}

func TestBsonToObject(t *testing.T) {
	tt.Equal(t, nil, sm.BsonToObject(nil, true))
	tt.Equal(t, slip.True, sm.BsonToObject(true, true))
	tt.Equal(t, slip.Symbol(":false"), sm.BsonToObject(false, true))
	tt.Equal(t, slip.String("quux"), sm.BsonToObject("quux", true))
	tt.Equal(t, slip.Fixnum(71), sm.BsonToObject(int32(71), true))
	tt.Equal(t, slip.Fixnum(71), sm.BsonToObject(int64(71), true))
	tt.Equal(t, slip.Fixnum(71), sm.BsonToObject(int(71), true))
	tt.Equal(t, slip.DoubleFloat(2.5), sm.BsonToObject(float64(2.5), true))
	tm := time.Unix(0, 1708179079_123000000).UTC()
	tt.Equal(t, slip.Time(tm), sm.BsonToObject(primitive.NewDateTimeFromTime(tm), true))
	tt.Equal(t, slip.Time(tm), sm.BsonToObject(tm, true))
	oid := "0123456789abcdef01234567"
	poid, _ := primitive.ObjectIDFromHex(oid)
	tt.Equal(t,
		slip.List{slip.List{slip.String("$toObjectId"), slip.Tail{Value: slip.String(oid)}}},
		sm.BsonToObject(poid, true))
	tt.Equal(t, slip.String("quux"), sm.BsonToObject([]byte("quux"), true))
	tt.Equal(t,
		slip.List{slip.List{slip.String("a"), slip.Tail{Value: slip.Fixnum(1)}}},
		sm.BsonToObject(bson.M{"a": int32(1)}, true))
	tt.Equal(t,
		slip.List{slip.List{slip.String("a"), slip.Fixnum(1), slip.Fixnum(2)}},
		sm.BsonToObject(bson.M{"a": bson.A{int32(1), int32(2)}}, true))

	tt.Equal(t,
		slip.List{slip.List{slip.String("a"), slip.Tail{Value: slip.Fixnum(1)}}},
		sm.BsonToObject(bson.D{{Key: "a", Value: int32(1)}}, true))
	tt.Equal(t,
		slip.List{slip.List{slip.String("a"), slip.Fixnum(1), slip.Fixnum(2)}},
		sm.BsonToObject(bson.D{{Key: "a", Value: bson.A{int32(1), int32(2)}}}, true))

	tt.Equal(t,
		slip.List{slip.List{slip.String("a"), slip.Tail{Value: slip.Fixnum(1)}}},
		sm.BsonToObject(map[string]any{"a": int32(1)}, true))
	tt.Equal(t,
		slip.List{slip.List{slip.String("a"), slip.Fixnum(1), slip.Fixnum(2)}},
		sm.BsonToObject(map[string]any{"a": []any{int32(1), int32(2)}}, true))

	tt.Equal(t, slip.Symbol("quux"), sm.BsonToObject(primitive.Symbol("quux"), true))
	tt.Equal(t,
		slip.List{slip.Fixnum(1), slip.Fixnum(2)},
		sm.BsonToObject([]any{int32(1), int32(2)}, true))
	dstr := "1234567890123456789"
	d128, _ := primitive.ParseDecimal128(dstr)
	tt.Equal(t, (*slip.Bignum)(big.NewInt(1234567890123456789)), sm.BsonToObject(d128, true))
	dstr = "12345678901234567890.123"
	d128, _ = primitive.ParseDecimal128(dstr)
	tt.Equal(t,
		slip.List{slip.List{slip.String("$decimal128"), slip.Tail{Value: slip.String("12345678901234567890.123")}}},
		sm.BsonToObject(d128, true))
	tt.Equal(t,
		slip.List{slip.List{slip.String("$timestamp"), slip.Tail{Value: slip.Fixnum(1708179079_123456789)}}},
		sm.BsonToObject(primitive.Timestamp{T: 397716434, I: 2011714325}, true))
	tt.Equal(t, slip.String("abc"), sm.BsonToObject(primitive.Regex{Pattern: "abc"}, true))
	tt.Equal(t, slip.String("abc/i"), sm.BsonToObject(primitive.Regex{Pattern: "abc", Options: "i"}, true))
	tt.Equal(t, slip.String("a = 1;"), sm.BsonToObject(primitive.JavaScript("a = 1;"), true))

	tt.Equal(t,
		gi.UUIDParse("6ef16994-701d-44d5-87ec-7ef3e2e5709b"),
		sm.BsonToObject(
			primitive.Binary{
				Subtype: bson.TypeBinaryUUID,
				Data: []byte{
					0x6e, 0xf1, 0x69, 0x94, 0x70, 0x1d, 0x44, 0xd5, 0x87, 0xec, 0x7e, 0xf3, 0xe2, 0xe5, 0x70, 0x9b,
				},
			}, true))

	tt.Equal(t,
		slip.List{
			slip.List{slip.String("$md5"), slip.Tail{Value: slip.String("6ef16994701d44d587ec7ef3e2e5709b")}},
		},
		sm.BsonToObject(
			primitive.Binary{
				Subtype: bson.TypeBinaryMD5,
				Data: []byte{
					0x6e, 0xf1, 0x69, 0x94, 0x70, 0x1d, 0x44, 0xd5, 0x87, 0xec, 0x7e, 0xf3, 0xe2, 0xe5, 0x70, 0x9b,
				},
			}, true))
	tt.Equal(t,
		slip.String("quux"),
		sm.BsonToObject(
			primitive.Binary{
				Subtype: bson.TypeBinaryGeneric,
				Data:    []byte("quux"),
			}, true))

	tt.Equal(t, slip.String("{}"), sm.BsonToObject(struct{}{}, true))
}
