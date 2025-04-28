// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slipmongo_test

import (
	"math/big"
	"testing"
	"time"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip-mongo/slipmongo"
	"github.com/ohler55/slip/pkg/gi"
	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/bson/primitive"
)

func TestToBsonNonSlip(t *testing.T) {
	tt.Equal(t, "a string", slipmongo.ToBson("a string"))
	tt.Equal(t, int32(71), slipmongo.ToBson(int64(71)))
	tt.Equal(t, int64(2147483648), slipmongo.ToBson(int64(2147483648)))
	tt.Equal(t, int64(2147483648), slipmongo.ToBson(2147483648))
	tt.Equal(t, bson.A{int32(1), int32(2), int32(3)}, slipmongo.ToBson([]any{1, 2, 3}))
	tt.Equal(t, bson.M{"a": int32(1)}, slipmongo.ToBson(map[string]any{"a": 1}))
	tt.Equal(t,
		primitive.Timestamp{T: 397716434, I: 2011714325},
		slipmongo.ToBson(map[string]any{"$timestamp": int64(1708179079_123456789)}))
	tt.Equal(t,
		primitive.Binary{
			Subtype: bson.TypeBinaryUUID,
			Data: []byte{
				0x6e, 0xf1, 0x69, 0x94, 0x70, 0x1d, 0x44, 0xd5, 0x87, 0xec, 0x7e, 0xf3, 0xe2, 0xe5, 0x70, 0x9b,
			},
		},
		slipmongo.ToBson(map[string]any{"$uuid": "6ef16994-701d-44d5-87ec-7ef3e2e5709b"}))
	tt.Equal(t,
		primitive.Binary{
			Subtype: bson.TypeBinaryMD5,
			Data: []byte{
				0x6e, 0xf1, 0x69, 0x94, 0x70, 0x1d, 0x44, 0xd5, 0x87, 0xec, 0x7e, 0xf3, 0xe2, 0xe5, 0x70, 0x9b,
			},
		},
		slipmongo.ToBson(map[string]any{"$md5": "6ef16994701d44d587ec7ef3e2e5709b"}))
	dstr := "123456789012345678901234567890"
	d128, _ := primitive.ParseDecimal128(dstr)
	tt.Equal(t, d128, slipmongo.ToBson(map[string]any{"$decimal128": dstr}))
	tt.Equal(t, primitive.Null{}, slipmongo.ToBson(nil))
	// primitive.DateTime rounds to milliseconds
	tm := time.Unix(0, 1708179079_123456789).UTC()
	tt.Equal(t, primitive.NewDateTimeFromTime(tm), slipmongo.ToBson(tm))
	tt.Equal(t, int32(71), slipmongo.ToBson(int8(71)))
	tt.Equal(t, int32(71), slipmongo.ToBson(int16(71)))
	tt.Equal(t, int32(71), slipmongo.ToBson(uint8(71)))
	tt.Equal(t, int32(71), slipmongo.ToBson(uint16(71)))
	tt.Equal(t, int32(71), slipmongo.ToBson(uint32(71)))
	tt.Equal(t, int32(71), slipmongo.ToBson(uint64(71)))
	tt.Equal(t, int64(2147483648), slipmongo.ToBson(uint32(2147483648)))
	tt.Equal(t, int64(2147483648), slipmongo.ToBson(uint64(2147483648)))
	tt.Equal(t, int64(2147483648), slipmongo.ToBson(uint(2147483648)))
	tt.Equal(t, int32(71), slipmongo.ToBson(uint(71)))
	tt.Equal(t, 2.5, slipmongo.ToBson(float32(2.5)))
	d128, _ = primitive.ParseDecimal128("1234567890123456789")
	tt.Equal(t, d128, slipmongo.ToBson(big.NewInt(1234567890123456789)))
	bbn := big.NewInt(int64(1234567890123456789))
	bbn = bbn.Mul(bbn, bbn)
	tt.Equal(t, "1524157875323883675019051998750190521", slipmongo.ToBson(bbn))
	tt.Equal(t, primitive.Null{}, slipmongo.ToBson(primitive.Null{}))
	tt.Panic(t, func() { _ = slipmongo.ToBson(struct{}{}) })
}

func TestToBsonSlip(t *testing.T) {
	tt.Equal(t, "a string", slipmongo.ToBson(slip.String("a string")))
	tt.Equal(t, int32(71), slipmongo.ToBson(slip.Fixnum(71)))
	tt.Equal(t, int64(2147483648), slipmongo.ToBson(slip.Fixnum(2147483648)))
	tt.Equal(t, "quux", slipmongo.ToBson(slip.Symbol("quux")))
	tt.Equal(t, false, slipmongo.ToBson(slip.Symbol(":false")))
	tt.Equal(t, bson.A{int32(1), int32(2)}, slipmongo.ToBson(slip.List{slip.Fixnum(1), slip.Fixnum(2)}))
	tt.Equal(t,
		bson.D{{Key: "a", Value: int32(1)}, {Key: "b", Value: int32(2)}},
		slipmongo.ToBson(slip.List{
			slip.List{slip.Symbol("a"), slip.Tail{Value: slip.Fixnum(1)}},
			slip.List{slip.String("b"), slip.Tail{Value: slip.Fixnum(2)}},
		}))
	tt.Equal(t,
		bson.A{
			bson.A{"a", int32(1)},
			bson.A{int32(3), int32(2)},
		},
		slipmongo.ToBson(slip.List{
			slip.List{slip.Symbol("a"), slip.Tail{Value: slip.Fixnum(1)}},
			slip.List{slip.Fixnum(3), slip.Tail{Value: slip.Fixnum(2)}},
		}))
	tt.Equal(t, 2.5, slipmongo.ToBson(slip.DoubleFloat(2.5)))
	tt.Equal(t, 2.5, slipmongo.ToBson(slip.SingleFloat(2.5)))
	tm := time.Unix(0, 1708179079_123456789).UTC()
	tt.Equal(t, primitive.NewDateTimeFromTime(tm), slipmongo.ToBson(slip.Time(tm)))
	tt.Equal(t,
		primitive.Binary{
			Subtype: bson.TypeBinaryUUID,
			Data: []byte{
				0x6e, 0xf1, 0x69, 0x94, 0x70, 0x1d, 0x44, 0xd5, 0x87, 0xec, 0x7e, 0xf3, 0xe2, 0xe5, 0x70, 0x9b,
			},
		},
		slipmongo.ToBson(gi.UUIDParse("6ef16994-701d-44d5-87ec-7ef3e2e5709b")))
	d128, _ := primitive.ParseDecimal128("1234567890123456789")
	tt.Equal(t, d128, slipmongo.ToBson((*slip.Bignum)(big.NewInt(1234567890123456789))))
	bbn := big.NewInt(int64(1234567890123456789))
	bbn = bbn.Mul(bbn, bbn)
	tt.Equal(t, "1524157875323883675019051998750190521", slipmongo.ToBson((*slip.Bignum)(bbn)))
	tt.Equal(t, "2.5", slipmongo.ToBson((*slip.LongFloat)(big.NewFloat(2.5))))

	scope := slip.NewScope()
	bg := slip.ReadString(`(make-bag "[1]")`, scope).Eval(slip.NewScope(), nil)
	tt.Equal(t, bson.A{int32(1)}, slipmongo.ToBson(bg))
	bg = slip.ReadString(`(make-instance 'vanilla-flavor)`, scope).Eval(scope, nil)
	tt.Panic(t, func() { _ = slipmongo.ToBson(bg) })
	tt.Equal(t, true, slipmongo.ToBson(slip.True))
}

func TestSimplifyBson(t *testing.T) {
	tt.Equal(t, nil, slipmongo.SimplifyBson(nil, true))
	tt.Equal(t, 2.5, slipmongo.SimplifyBson(2.5, true))
	oid := "0123456789abcdef01234567"
	poid, _ := primitive.ObjectIDFromHex(oid)
	tt.Equal(t, map[string]any{"$toObjectId": oid}, slipmongo.SimplifyBson(poid, true))
	tt.Equal(t, "quux", slipmongo.SimplifyBson([]byte("quux"), true))
	tt.Equal(t, int64(71), slipmongo.SimplifyBson(int32(71), true))
	tt.Equal(t, int64(71), slipmongo.SimplifyBson(int(71), true))
	tt.Equal(t, map[string]any{"a": int64(1)}, slipmongo.SimplifyBson(bson.M{"a": int32(1)}, true))
	tt.Equal(t, map[string]any{"a": int64(1)}, slipmongo.SimplifyBson(bson.D{{Key: "a", Value: int32(1)}}, true))
	tt.Equal(t, []any{int64(1), int64(2)}, slipmongo.SimplifyBson(bson.A{int32(1), int64(2)}, true))
	tt.Equal(t, map[string]any{"a": int64(1)}, slipmongo.SimplifyBson(map[string]any{"a": int64(1)}, true))
	tt.Equal(t, []any{int64(1), int64(2)}, slipmongo.SimplifyBson([]any{int32(1), int64(2)}, true))
	tt.Equal(t, nil, slipmongo.SimplifyBson(primitive.Null{}, true))
	tt.Equal(t, "quux", slipmongo.SimplifyBson(primitive.Symbol("quux"), true))
	tm := time.Unix(0, 1708179079_123000000).UTC()
	tt.Equal(t, tm, slipmongo.SimplifyBson(primitive.NewDateTimeFromTime(tm), true))
	dstr := "123456789012345678901234567890"
	d128, _ := primitive.ParseDecimal128(dstr)
	tt.Equal(t, map[string]any{"$decimal128": dstr}, slipmongo.SimplifyBson(d128, true))
	tt.Equal(t,
		map[string]any{"$timestamp": int64(1708179079_123456789)},
		slipmongo.SimplifyBson(primitive.Timestamp{T: 397716434, I: 2011714325}, true))
	tt.Equal(t, "abc", slipmongo.SimplifyBson(primitive.Regex{Pattern: "abc"}, true))
	tt.Equal(t, "abc/i", slipmongo.SimplifyBson(primitive.Regex{Pattern: "abc", Options: "i"}, true))
	tt.Equal(t, "a = 1;", slipmongo.SimplifyBson(primitive.JavaScript("a = 1;"), true))
	tt.Equal(t,
		map[string]any{"$uuid": "6ef16994-701d-44d5-87ec-7ef3e2e5709b"},
		slipmongo.SimplifyBson(
			primitive.Binary{
				Subtype: bson.TypeBinaryUUID,
				Data: []byte{
					0x6e, 0xf1, 0x69, 0x94, 0x70, 0x1d, 0x44, 0xd5, 0x87, 0xec, 0x7e, 0xf3, 0xe2, 0xe5, 0x70, 0x9b,
				},
			}, true))

	tt.Equal(t, "{}", slipmongo.SimplifyBson(struct{}{}, true))
	tt.Equal(t,
		map[string]any{"$md5": "6ef16994701d44d587ec7ef3e2e5709b"},
		slipmongo.SimplifyBson(
			primitive.Binary{
				Subtype: bson.TypeBinaryMD5,
				Data: []byte{
					0x6e, 0xf1, 0x69, 0x94, 0x70, 0x1d, 0x44, 0xd5, 0x87, 0xec, 0x7e, 0xf3, 0xe2, 0xe5, 0x70, 0x9b,
				},
			}, true))
	tt.Equal(t,
		"quux",
		slipmongo.SimplifyBson(
			primitive.Binary{
				Subtype: bson.TypeBinaryGeneric,
				Data:    []byte("quux"),
			}, true))

	tt.Equal(t, "{}", slipmongo.SimplifyBson(struct{}{}, true))
}

func TestBsonToObject(t *testing.T) {
	tt.Equal(t, nil, slipmongo.BsonToObject(nil, true))
	tt.Equal(t, slip.True, slipmongo.BsonToObject(true, true))
	tt.Equal(t, slip.Symbol(":false"), slipmongo.BsonToObject(false, true))
	tt.Equal(t, slip.String("quux"), slipmongo.BsonToObject("quux", true))
	tt.Equal(t, slip.Fixnum(71), slipmongo.BsonToObject(int32(71), true))
	tt.Equal(t, slip.Fixnum(71), slipmongo.BsonToObject(int64(71), true))
	tt.Equal(t, slip.Fixnum(71), slipmongo.BsonToObject(int(71), true))
	tt.Equal(t, slip.DoubleFloat(2.5), slipmongo.BsonToObject(float64(2.5), true))
	tm := time.Unix(0, 1708179079_123000000).UTC()
	tt.Equal(t, slip.Time(tm), slipmongo.BsonToObject(primitive.NewDateTimeFromTime(tm), true))
	tt.Equal(t, slip.Time(tm), slipmongo.BsonToObject(tm, true))
	oid := "0123456789abcdef01234567"
	poid, _ := primitive.ObjectIDFromHex(oid)
	tt.Equal(t,
		slip.List{slip.List{slip.String("$toObjectId"), slip.Tail{Value: slip.String(oid)}}},
		slipmongo.BsonToObject(poid, true))
	tt.Equal(t, slip.String("quux"), slipmongo.BsonToObject([]byte("quux"), true))
	tt.Equal(t,
		slip.List{slip.List{slip.String("a"), slip.Tail{Value: slip.Fixnum(1)}}},
		slipmongo.BsonToObject(bson.M{"a": int32(1)}, true))
	tt.Equal(t,
		slip.List{slip.List{slip.String("a"), slip.Fixnum(1), slip.Fixnum(2)}},
		slipmongo.BsonToObject(bson.M{"a": bson.A{int32(1), int32(2)}}, true))

	tt.Equal(t,
		slip.List{slip.List{slip.String("a"), slip.Tail{Value: slip.Fixnum(1)}}},
		slipmongo.BsonToObject(bson.D{{Key: "a", Value: int32(1)}}, true))
	tt.Equal(t,
		slip.List{slip.List{slip.String("a"), slip.Fixnum(1), slip.Fixnum(2)}},
		slipmongo.BsonToObject(bson.D{{Key: "a", Value: bson.A{int32(1), int32(2)}}}, true))

	tt.Equal(t,
		slip.List{slip.List{slip.String("a"), slip.Tail{Value: slip.Fixnum(1)}}},
		slipmongo.BsonToObject(map[string]any{"a": int32(1)}, true))
	tt.Equal(t,
		slip.List{slip.List{slip.String("a"), slip.Fixnum(1), slip.Fixnum(2)}},
		slipmongo.BsonToObject(map[string]any{"a": []any{int32(1), int32(2)}}, true))

	tt.Equal(t, slip.Symbol("quux"), slipmongo.BsonToObject(primitive.Symbol("quux"), true))
	tt.Equal(t,
		slip.List{slip.Fixnum(1), slip.Fixnum(2)},
		slipmongo.BsonToObject([]any{int32(1), int32(2)}, true))
	dstr := "1234567890123456789"
	d128, _ := primitive.ParseDecimal128(dstr)
	tt.Equal(t, (*slip.Bignum)(big.NewInt(1234567890123456789)), slipmongo.BsonToObject(d128, true))
	dstr = "12345678901234567890.123"
	d128, _ = primitive.ParseDecimal128(dstr)
	tt.Equal(t,
		slip.List{slip.List{slip.String("$decimal128"), slip.Tail{Value: slip.String("12345678901234567890.123")}}},
		slipmongo.BsonToObject(d128, true))
	tt.Equal(t,
		slip.List{slip.List{slip.String("$timestamp"), slip.Tail{Value: slip.Fixnum(1708179079_123456789)}}},
		slipmongo.BsonToObject(primitive.Timestamp{T: 397716434, I: 2011714325}, true))
	tt.Equal(t, slip.String("abc"), slipmongo.BsonToObject(primitive.Regex{Pattern: "abc"}, true))
	tt.Equal(t, slip.String("abc/i"), slipmongo.BsonToObject(primitive.Regex{Pattern: "abc", Options: "i"}, true))
	tt.Equal(t, slip.String("a = 1;"), slipmongo.BsonToObject(primitive.JavaScript("a = 1;"), true))

	tt.Equal(t,
		gi.UUIDParse("6ef16994-701d-44d5-87ec-7ef3e2e5709b"),
		slipmongo.BsonToObject(
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
		slipmongo.BsonToObject(
			primitive.Binary{
				Subtype: bson.TypeBinaryMD5,
				Data: []byte{
					0x6e, 0xf1, 0x69, 0x94, 0x70, 0x1d, 0x44, 0xd5, 0x87, 0xec, 0x7e, 0xf3, 0xe2, 0xe5, 0x70, 0x9b,
				},
			}, true))
	tt.Equal(t,
		slip.String("quux"),
		slipmongo.BsonToObject(
			primitive.Binary{
				Subtype: bson.TypeBinaryGeneric,
				Data:    []byte("quux"),
			}, true))

	tt.Equal(t, slip.String("{}"), slipmongo.BsonToObject(struct{}{}, true))
}
