package lang

import (
	"fmt"
	"math/big"
)

type TypeName string

const (
	TpUint8   TypeName = "uint8"   // Unsigned 8-bit integer
	TpUint16  TypeName = "uint16"  // Unsigned 16-bit integer
	TpUint24  TypeName = "uint24"  // Unsigned 24-bit integer
	TpUint32  TypeName = "uint32"  // Unsigned 32-bit integer
	TpUint64  TypeName = "uint64"  // Unsigned 64-bit integer
	TpInt8    TypeName = "int8"    // Signed 8-bit integer
	TpInt16   TypeName = "int16"   // Signed 16-bit integer
	TpInt24   TypeName = "int24"   // Signed 24-bit integer
	TpInt32   TypeName = "int32"   // Signed 32-bit integer
	TpInt64   TypeName = "int64"   // Signed 64-bit integer
	TpFloat32 TypeName = "float32" // "Single-precision" 32-bit floating point number
	TpFloat64 TypeName = "float64" // "Double-precision" 64-bit floating point number
	TpByte    TypeName = "byte"    // A single byte. Distinct from uint8 since it is intended for character sequences.

	// The following are special types that are invoked by other means than their type name.

	TpArray  TypeName = "array"  // Sequence of N items of type X - type[n].
	TpMatch  TypeName = "match"  // Exact byte string match - "string".
	TpStruct TypeName = "struct" // Struct referenced by identifier.
)

var AvailableTypeNames = []TypeName{
	TpUint8,
	TpUint16,
	TpUint24,
	TpUint32,
	TpUint64,
	TpInt8,
	TpInt16,
	TpInt24,
	TpInt32,
	TpInt64,
	TpFloat32,
	TpFloat64,
	TpByte,
}

type ResultKind string

const (
	ResBoolean ResultKind = "Boolean"
	ResInteger ResultKind = "Integer"
	ResFloat   ResultKind = "Float"
	ResString  ResultKind = "String"
	ResMap     ResultKind = "Map"
	ResList    ResultKind = "List"
	ResType    ResultKind = "Type"
	ResMeta    ResultKind = "Meta"
	ResStruct  ResultKind = "Struct"
)

type Result interface {
	Kind() ResultKind
}

type BooleanResult bool
type IntResult struct{ *big.Int }
type FloatResult struct{ *big.Float }
type StringResult string
type MapResult map[Result]Result
type ListResult []Result
type TypeResult struct {
	Name   TypeName
	Params []Result
}

type MetaResult Metadata

type StructResult struct {
	Name      string
	Fields    []StructField
	Modifiers map[string]Result
}

type StructField struct {
	Name      string
	Expr      Node
	Modifiers map[string]Result
}

func (br BooleanResult) Kind() ResultKind { return ResBoolean }
func (ir IntResult) Kind() ResultKind     { return ResInteger }
func (fr FloatResult) Kind() ResultKind   { return ResFloat }
func (sr StringResult) Kind() ResultKind  { return ResString }
func (mr MapResult) Kind() ResultKind     { return ResMap }
func (lr ListResult) Kind() ResultKind    { return ResList }
func (tr TypeResult) Kind() ResultKind    { return ResType }

func (mr MetaResult) Kind() ResultKind   { return ResMeta }
func (sr StructResult) Kind() ResultKind { return ResStruct }

func ResultMustBe[T Result](result Result) (T, error) {
	var empty T

	value, ok := result.(T)
	if !ok {
		return empty, fmt.Errorf("expected type of value to be %s, not %s", empty.Kind(), result.Kind())
	}

	return value, nil
}

func ToStringList(list ListResult) (items []string, err error, errIndex int) {
	for idx, item := range list {
		item, err := ResultMustBe[StringResult](item)
		if err != nil {
			return nil, err, idx
		}

		items = append(items, string(item))
	}

	return items, nil, -1
}

func GetMapKey[T Result](mapping map[string]Result, name string, required bool) (T, error) {
	var empty T

	valueRes, exists := mapping[name]
	if !exists && required {
		return empty, fmt.Errorf("required key %q does not exist", name)
	} else if !exists {
		return empty, nil
	}

	value, exists := valueRes.(T)
	if !exists {
		return empty, fmt.Errorf("value of key %q must be of type %s, not %s", name, empty.Kind(), valueRes.Kind())
	}

	return value, nil
}
