// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slipmongo

import (
	"encoding/hex"
	"fmt"
	"math"
	"math/big"
	"strings"
	"time"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/bag"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/pkg/gi"
	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/bson/primitive"
)

const hexChars = "0123456789abcdef"

func ToBson(value any) (bs any) {
	// Address the most likely cases first assuming the contents of a bag
	// (simplified) and the types expected in a simplified value. After that
	// the common lisp types. The remainder follow the more common.
retry:
	switch tv := value.(type) {
	case string, bool, float64, int32:
		bs = tv
	case int64:
		if math.MinInt32 <= tv && tv <= math.MaxInt32 {
			bs = int32(tv)
		} else {
			bs = int64(tv)
		}
	case []any:
		a := make(bson.A, len(tv))
		for i, e := range tv {
			a[i] = ToBson(e)
		}
		bs = a
	case map[string]any:
		if len(tv) == 1 {
			var (
				key string
				v   any
			)
			for key, v = range tv {
			}
			switch strings.ToLower(key) {
			case "$timestamp":
				if nsec, ok := v.(int64); ok {
					bs = primitive.Timestamp{
						T: uint32(uint64(nsec) >> 32),
						I: uint32(uint64(nsec) & 0x00000000FFFFFFFF),
					}
				}
			case "$decimal128":
				if str, ok := v.(string); ok {
					if dec, err := primitive.ParseDecimal128(str); err == nil {
						bs = dec
					}
				}
			case "$uuid":
				if str, ok := v.(string); ok {
					if u := gi.UUIDParse(str); u[0] != 0 || u[1] != 0 {
						bs = primitive.Binary{Subtype: bson.TypeBinaryUUID, Data: u.Bytes()}
					}
				}
			case "$md5":
				if str, ok := v.(string); ok {
					pb := primitive.Binary{Subtype: bson.TypeBinaryMD5}
					var err error
					if pb.Data, err = hex.DecodeString(str); err == nil {
						bs = pb
					}
				}
			}
			if bs != nil {
				break
			}
		}
		m := bson.M{}
		for k, e := range tv {
			m[k] = ToBson(e)
		}
		bs = m
	case nil:
		bs = primitive.Null{}
	case time.Time:
		bs = primitive.NewDateTimeFromTime(tv.UTC())

	case slip.Symbol:
		if strings.EqualFold(":false", string(tv)) {
			bs = false
		} else {
			bs = string(tv)
		}
	case slip.String:
		bs = string(tv)
	case slip.List:
		if isAssoc(tv) {
			d := make(bson.D, len(tv))
			for i, e := range tv {
				cons := e.(slip.List)
				var key string
				switch tc := cons.Car().(type) {
				case slip.Symbol:
					key = string(tc)
				case slip.String:
					key = string(tc)
				}
				d[i] = bson.E{Key: key, Value: ToBson(cons.Cdr())}
			}
			bs = d
		} else {
			a := make(bson.A, len(tv))
			for i, e := range tv {
				a[i] = ToBson(e)
			}
			bs = a
		}
	case slip.Fixnum:
		if math.MinInt32 <= tv && tv <= math.MaxInt32 {
			bs = int32(tv)
		} else {
			bs = int64(tv)
		}
	case slip.DoubleFloat:
		bs = float64(tv)
	case slip.Time:
		bs = primitive.NewDateTimeFromTime(time.Time(tv).UTC())
	case gi.UUID:
		bs = primitive.Binary{Subtype: bson.TypeBinaryUUID, Data: tv.Bytes()}
	case slip.Tail:
		value = tv.Value
		goto retry

	case int:
		if math.MinInt32 <= tv && tv <= math.MaxInt32 {
			bs = int32(tv)
		} else {
			bs = int64(tv)
		}
	case int8:
		bs = int32(tv)
	case int16:
		bs = int32(tv)
	case uint:
		if tv <= math.MaxInt32 {
			bs = int32(tv)
		} else {
			bs = int64(tv)
		}
	case uint8:
		bs = int32(tv)
	case uint16:
		bs = int32(tv)
	case uint32:
		if tv <= math.MaxInt32 {
			bs = int32(tv)
		} else {
			bs = int64(tv)
		}
	case uint64:
		if tv <= math.MaxInt32 {
			bs = int32(tv)
		} else {
			bs = int64(tv)
		}
	case slip.SingleFloat:
		bs = float64(tv)
	case float32:
		bs = float64(tv)
	case *slip.LongFloat:
		bs = tv.String()

	case *flavors.Instance:
		if tv.Flavor != bag.Flavor() {
			panic(fmt.Sprintf("an instance of %s is not convertible to a BSON type", tv.Flavor.Name()))
		}
		bs = ToBson(tv.Any)

	case *slip.Bignum:
		var ok bool
		if bs, ok = primitive.ParseDecimal128FromBigInt((*big.Int)(tv), 0); !ok {
			bs = tv.String()
		}
	case *big.Int:
		var ok bool
		if bs, ok = primitive.ParseDecimal128FromBigInt(tv, 0); !ok {
			bs = tv.String()
		}
	case primitive.Null, bson.A, bson.D, bson.E, bson.M:
		bs = tv
	default:
		if slip.True == tv {
			bs = true
			break
		}
		panic(fmt.Sprintf("a %T is not convertible to a BSON type", tv))
	}
	return
}

// SimplifyBson simplifies BSON into values that can be used in a bag-flavor
// instance. If wrap is true then values that can not be represented with a
// loss of type are wrapped in a map with one entry. An example is an ObjectId
// wrapped would be {$toObjectId:"0123456789abcdef012345678"}
func SimplifyBson(bs any, wrap bool) (sv any) {
	switch tb := bs.(type) {
	case nil:
		// leave sv as nil
	case string, bool, int64, float64, time.Time:
		sv = bs
	case primitive.ObjectID:
		sv = tb.Hex()
		if wrap {
			sv = map[string]any{"$toObjectId": sv}
		}
	case []byte:
		sv = string(tb)
	case int32:
		sv = int64(tb)
	case bson.M:
		m := map[string]any{}
		for k, v := range tb {
			m[k] = SimplifyBson(v, wrap)
		}
		sv = m
	case bson.D:
		m := map[string]any{}
		for _, e := range tb {
			m[e.Key] = SimplifyBson(e.Value, wrap)
		}
		sv = m
	case bson.A:
		a := make([]any, len(tb))
		for i, v := range tb {
			a[i] = SimplifyBson(v, wrap)
		}
		sv = a

	case int:
		sv = int64(tb)
	case []any:
		a := make([]any, len(tb))
		for i, v := range tb {
			a[i] = SimplifyBson(v, wrap)
		}
		sv = a
	case map[string]any:
		// deep copy
		m := map[string]any{}
		for k, v := range tb {
			m[k] = SimplifyBson(v, wrap)
		}
		sv = m
	case primitive.Null:
		// leave sv as nil
	case primitive.Symbol:
		sv = string(tb)
	case primitive.DateTime:
		sv = tb.Time().UTC()
	case primitive.Decimal128:
		sv = tb.String()
		if wrap {
			sv = map[string]any{"$decimal128": sv}
		}
	case primitive.Timestamp:
		sv = int64(uint64(tb.T)<<32 | uint64(tb.I))
		if wrap {
			sv = map[string]any{"$timestamp": sv}
		}
	case primitive.Regex:
		if 0 < len(tb.Options) {
			rx := make([]byte, 0, len(tb.Pattern)+1+len(tb.Options))
			rx = append(rx, tb.Pattern...)
			rx = append(rx, '/')
			rx = append(rx, tb.Options...)
			sv = string(rx)
		} else {
			sv = tb.Pattern
		}
	case primitive.JavaScript:
		sv = string(tb)
	case primitive.Binary:
		switch tb.Subtype {
		case bson.TypeBinaryUUID, bson.TypeBinaryUUIDOld:
			var b []byte
			for i, v := range tb.Data {
				b = append(b, hexChars[v>>4], hexChars[v&0x0f])
				if i == 3 || i == 5 || i == 7 || i == 9 {
					b = append(b, '-')
				}
			}
			sv = string(b)
			if wrap {
				sv = map[string]any{"$uuid": sv}
			}
		case bson.TypeBinaryMD5:
			sv = hex.EncodeToString(tb.Data)
			if wrap {
				sv = map[string]any{"$md5": sv}
			}
		default:
			sv = string(tb.Data)
		}
	default:
		sv = fmt.Sprintf("%v", tb)
	}
	return
}

func BsonToObject(value any, wrap bool) (obj slip.Object) {
	switch tv := value.(type) {
	case nil:
		// leave sv as nil
	case bool:
		if tv {
			obj = slip.True
		} else {
			obj = slip.Symbol(":false")
		}
	case string:
		obj = slip.String(tv)
	case int32:
		obj = slip.Fixnum(tv)
	case int64:
		obj = slip.Fixnum(tv)
	case float64:
		obj = slip.DoubleFloat(tv)
	case time.Time:
		obj = slip.Time(tv.UTC())
	case primitive.ObjectID:
		obj = slip.String(tv.Hex())
		if wrap {
			obj = slip.List{slip.List{slip.String("$toObjectId"), slip.Tail{Value: obj}}}
		}
	case []byte:
		obj = slip.String(string(tv))
	case bson.M:
		a := make(slip.List, 0, len(tv))
		for k, v := range tv {
			vo := BsonToObject(v, wrap)
			if vl, ok := vo.(slip.List); ok {
				a = append(a, append(slip.List{slip.String(k)}, vl...))
			} else {
				a = append(a, slip.List{slip.String(k), slip.Tail{Value: vo}})
			}
		}
		obj = a
	case bson.D:
		a := make(slip.List, len(tv))
		for i, e := range tv {
			vo := BsonToObject(e.Value, wrap)
			if vl, ok := vo.(slip.List); ok {
				a[i] = append(slip.List{slip.String(e.Key)}, vl...)
			} else {
				a[i] = slip.List{slip.String(e.Key), slip.Tail{Value: vo}}
			}
		}
		obj = a
	case bson.A:
		list := make(slip.List, len(tv))
		for i, v := range tv {
			list[i] = BsonToObject(v, wrap)
		}
		obj = list

	case int:
		obj = slip.Fixnum(tv)
	case []any:
		list := make(slip.List, len(tv))
		for i, v := range tv {
			list[i] = BsonToObject(v, wrap)
		}
		obj = list
	case map[string]any:
		a := make(slip.List, 0, len(tv))
		for k, v := range tv {
			vo := BsonToObject(v, wrap)
			if vl, ok := vo.(slip.List); ok {
				a = append(a, append(slip.List{slip.String(k)}, vl...))
			} else {
				a = append(a, slip.List{slip.String(k), slip.Tail{Value: vo}})
			}
		}
		obj = a
	case primitive.Null:
		// leave sv as nil
	case primitive.Symbol:
		obj = slip.Symbol(tv)
	case primitive.DateTime:
		obj = slip.Time(tv.Time().UTC())
	case primitive.Decimal128:
		if bi, exp, err := tv.BigInt(); err == nil && exp == 0 {
			obj = (*slip.Bignum)(bi)
		} else {
			obj = slip.String(tv.String())
			if wrap {
				obj = slip.List{slip.List{slip.String("$decimal128"), slip.Tail{Value: obj}}}
			}
		}
	case primitive.Timestamp:
		obj = slip.Fixnum(uint64(tv.T)<<32 | uint64(tv.I))
		if wrap {
			obj = slip.List{slip.List{slip.String("$timestamp"), slip.Tail{Value: obj}}}
		}
	case primitive.Regex:
		if 0 < len(tv.Options) {
			rx := make([]byte, 0, len(tv.Pattern)+1+len(tv.Options))
			rx = append(rx, tv.Pattern...)
			rx = append(rx, '/')
			rx = append(rx, tv.Options...)
			obj = slip.String(rx)
		} else {
			obj = slip.String(tv.Pattern)
		}
	case primitive.JavaScript:
		obj = slip.String(tv)
	case primitive.Binary:
		switch tv.Subtype {
		case bson.TypeBinaryUUID:
			var u gi.UUID
			for i, v := range tv.Data {
				if i < 8 {
					u[0] = u[0]<<8 | uint64(v)
				} else {
					u[1] = u[1]<<8 | uint64(v)
				}
			}
			obj = u
		case bson.TypeBinaryMD5:
			obj = slip.String(hex.EncodeToString(tv.Data))
			if wrap {
				obj = slip.List{slip.List{slip.String("$md5"), slip.Tail{Value: obj}}}
			}
		default:
			obj = slip.String(tv.Data)
		}
	default:
		obj = slip.String(fmt.Sprintf("%v", tv))
	}
	return
}

// Returns true if all element are list or cons with at least one element.
func isAssoc(list slip.List) bool {
	for _, v := range list {
		if lv, ok := v.(slip.List); ok && 0 < len(lv) {
			switch lv.Car().(type) {
			case slip.String, slip.Symbol:
				// ok
			default:
				return false
			}
			continue
		}
		return false
	}
	return true
}
