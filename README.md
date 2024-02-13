# slip-mongo

SLIP-Mongo is a set of MongoDB APIs for SLIP.

The concepts follow those found in the driver package at
[go.mongodb.org/mongo-driver/mongo](go.mongodb.org/mongo-driver/mongo). To
start a connection is made to the database. Collections are then used
to access or query the records in the database.

## Notes

### Type Conversions

MongoDB is based on bson data. In this package the bson data is mapped
to SLIP bag instances which for most purposes can be thought of as
JSON data which does map to LISP with some minor exceptions. JSON
objects which are represented as golang maps are converted to LISP
association lists but the reverse can lose data if there are more than
once association element with the same key. While an association list
does map to a bson D type that is not supported by this package. A
bson M type is used for bad maps and for association lists.

### Controlling Data Types

Basic type such as fixnums or strings map to go types of int64 and
string and will be inserted into mongo as bson long and bson
strings. To convert to different values the bson types the mongo
converters such as
[`$toDate`](https://www.mongodb.com/docs/manual/reference/operator/aggregation/toDate)
can be used.
