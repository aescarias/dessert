package lang

import (
	"encoding/binary"
	"fmt"
	"io"
	"math/big"
)

func nativeToResult(native any) Result {
	switch nt := native.(type) {
	case uint8:
		return IntResult{new(big.Int).SetUint64(uint64(nt))}
	case uint16:
		return IntResult{new(big.Int).SetUint64(uint64(nt))}
	case uint32:
		return IntResult{new(big.Int).SetUint64(uint64(nt))}
	case uint64:
		return IntResult{new(big.Int).SetUint64(nt)}
	case int8:
		return IntResult{new(big.Int).SetInt64(int64(nt))}
	case int16:
		return IntResult{new(big.Int).SetInt64(int64(nt))}
	case int32:
		return IntResult{new(big.Int).SetInt64(int64(nt))}
	case int64:
		return IntResult{new(big.Int).SetInt64(nt)}
	case float32:
		return FloatResult{new(big.Float).SetFloat64(float64(nt))}
	case float64:
		return FloatResult{new(big.Float).SetFloat64(nt)}
	case []byte:
		return StringResult(nt)
	case []any:
		items := ListResult{}
		for _, item := range nt {
			items = append(items, nativeToResult(item))
		}
		return items
	default:
		panic(fmt.Sprintf("could not convert native type %v to internal representation", native))
	}
}

func readNumeric(handle io.ReadSeeker, kind TypeResult, endian binary.ByteOrder) (any, error) {
	switch kind.Name {
	case TpUint8:
		var num uint8
		if err := binary.Read(handle, endian, &num); err != nil {
			return nil, err
		}
		return num, nil
	case TpInt8:
		var num int8
		if err := binary.Read(handle, endian, &num); err != nil {
			return nil, err
		}
		return num, nil
	case TpUint16:
		var num uint16
		if err := binary.Read(handle, endian, &num); err != nil {
			return nil, err
		}
		return num, nil
	case TpInt16:
		var num int16
		if err := binary.Read(handle, endian, &num); err != nil {
			return nil, err
		}
		return num, nil
	case TpUint24:
		intBytes := make([]byte, 3)
		if _, err := handle.Read(intBytes); err != nil {
			return nil, err
		}

		var num uint32
		switch endian {
		case binary.BigEndian:
			num = uint32(intBytes[2]) | uint32(intBytes[1])<<8 | uint32(intBytes[0])<<16
		case binary.LittleEndian:
			num = uint32(intBytes[0]) | uint32(intBytes[1])<<8 | uint32(intBytes[2])<<16
		default:
			return nil, fmt.Errorf("unexpected internal endian value")
		}

		return num, nil
	case TpInt24:
		intBytes := make([]byte, 3)
		if _, err := handle.Read(intBytes); err != nil {
			return nil, err
		}

		var num uint32
		switch endian {
		case binary.BigEndian:
			num = uint32(intBytes[2]) | uint32(intBytes[1])<<8 | uint32(intBytes[0])<<16
		case binary.LittleEndian:
			num = uint32(intBytes[0]) | uint32(intBytes[1])<<8 | uint32(intBytes[2])<<16
		default:
			return nil, fmt.Errorf("unexpected internal endian value")
		}

		if num&0x800000 != 0 {
			// sign bit set, flip the bits
			return int32(num | 0xFF000000), nil
		}

		return int32(num), nil
	case TpUint32:
		var num uint32
		if err := binary.Read(handle, endian, &num); err != nil {
			return nil, err
		}
		return num, nil
	case TpInt32:
		var num int32
		if err := binary.Read(handle, endian, &num); err != nil {
			return nil, err
		}
		return num, nil
	case TpUint64:
		var num uint64
		if err := binary.Read(handle, endian, &num); err != nil {
			return nil, err
		}
		return num, nil
	case TpInt64:
		var num int64
		if err := binary.Read(handle, endian, &num); err != nil {
			return nil, err
		}
		return num, nil
	case TpFloat32:
		var num float32
		if err := binary.Read(handle, endian, &num); err != nil {
			return nil, err
		}
		return num, nil
	case TpFloat64:
		var num float64
		if err := binary.Read(handle, endian, &num); err != nil {
			return nil, err
		}
		return num, nil
	default:
		return nil, fmt.Errorf("%s is not a supported numeric type", kind.Name)
	}
}

func readType(handle io.ReadSeeker, kind TypeResult, endian binary.ByteOrder) (any, error) {
	switch kind.Name {
	case TpUint8, TpInt8, TpUint16, TpInt16, TpUint24, TpInt24, TpUint32, TpInt32, TpUint64, TpInt64, TpFloat32, TpFloat64:
		return readNumeric(handle, kind, endian)
	case TpMatch:
		matchStr, err := ResultMustBe[StringResult](kind.Params[0])
		if err != nil {
			return nil, err
		}

		matched := make([]byte, len(matchStr))
		if _, err := handle.Read(matched); err != nil {
			return nil, err
		}

		if string(matched) != string(matchStr) {
			return nil, fmt.Errorf("%q is not %q", matched, matchStr)
		}

		return matched, nil
	case TpByte:
		byteLen, err := ResultMustBe[IntResult](kind.Params[0])
		if err != nil {
			return nil, err
		}

		data := make([]byte, byteLen.Int64())
		if _, err := handle.Read(data); err != nil {
			return nil, err
		}

		return data, nil
	case TpArray:
		itemType, err := ResultMustBe[TypeResult](kind.Params[0])
		if err != nil {
			return nil, err
		}

		itemCount, err := ResultMustBe[IntResult](kind.Params[1])
		if err != nil {
			return nil, err
		}

		var items []any
		for range itemCount.Int64() {
			it, err := readType(handle, itemType, endian)
			if err != nil {
				return nil, err
			}

			items = append(items, it)
		}

		return items, nil
	case TpStruct:
		structRef, err := ResultMustBe[StructResult](kind.Params[0])
		if err != nil {
			return nil, err
		}

		endian, err := GetEndian(structRef.Modifiers, false)
		if err != nil {
			return nil, fmt.Errorf("struct %s modifier: %w", structRef.Name, err)
		}

		items := []DefinitionItem{}
		runtime := NewRuntime()

		for _, field := range structRef.Fields {
			expr, err := runtime.EvaluateExpr(field.Expr)
			if err != nil {
				return nil, fmt.Errorf("%s > %s: %w", structRef.Name, field.Name, err)
			}

			var tp TypeResult
			switch expr.Kind() {
			case ResString:
				tp = TypeResult{Name: TpMatch, Params: []Result{expr}}
			case ResStruct:
				tp = TypeResult{Name: TpStruct, Params: []Result{expr}}
			case ResType:
				tp = expr.(TypeResult)
			default:
				return TypeResult{}, fmt.Errorf("%s > %s: expected type for field value, not %s", structRef.Name, field.Name, tp.Kind())
			}

			fieldEndian, err := GetEndian(field.Modifiers, false)
			if err != nil {
				return nil, fmt.Errorf("%s > %s modifier: %w", structRef.Name, field.Name, err)
			}

			if fieldEndian == nil {
				fieldEndian = endian
			}

			name, err := GetMapKey[StringResult](field.Modifiers, "name", false)
			if err != nil {
				return nil, fmt.Errorf("%s > %s modifier: %w", structRef.Name, field.Name, err)
			}

			data, err := readType(handle, tp, fieldEndian)
			if err != nil {
				return nil, fmt.Errorf("%s > %s: %w", structRef.Name, field.Name, err)
			}

			items = append(items, DefinitionItem{Id: field.Name, Name: string(name), Value: data})

			runtime.Globals.Set(field.Name, nativeToResult(data))
		}

		return items, nil
	default:
		return nil, fmt.Errorf("unsupported type: %s", kind.Name)
	}
}

func GetEndian(mapping map[string]Result, required bool) (binary.ByteOrder, error) {
	endianStr, err := GetMapKey[StringResult](mapping, "endian", required)
	if err != nil {
		return nil, err
	}

	if endianStr == "" {
		return nil, nil
	}

	var endian binary.ByteOrder
	switch string(endianStr) {
	case "little":
		endian = binary.LittleEndian
	case "big":
		endian = binary.BigEndian
	default:
		return nil, fmt.Errorf("endian must be 'little' or 'big', not %q", endianStr)
	}

	return endian, nil
}

type DefinitionItem struct {
	Id    string
	Name  string
	Value any
}

type ParsedDefinition struct {
	Metadata
	Items []DefinitionItem
}

func ParseFromReader(handle io.ReadSeeker, definition []Result) (*ParsedDefinition, error) {
	if _, err := handle.Seek(0, 0); err != nil {
		return nil, err
	}

	var root *StructResult
	var meta *MetaResult

	for _, stmt := range definition {
		if structure, ok := stmt.(StructResult); ok {
			isRootValue, exists := structure.Modifiers["root"]
			if !exists {
				continue
			}

			if isRoot, ok := isRootValue.(BooleanResult); !ok {
				return nil, fmt.Errorf("in struct %s: root modifier must be boolean", structure.Name)
			} else if isRoot {
				root = &structure
				break
			}
		}

		if metadata, ok := stmt.(MetaResult); ok && meta == nil {
			meta = &metadata
		}
	}

	if root == nil {
		return nil, fmt.Errorf("no root structure was declared")
	}

	if meta == nil {
		return nil, fmt.Errorf("no metadata block was declared")
	}

	rootEndian, err := GetEndian(root.Modifiers, true)
	if err != nil {
		return nil, fmt.Errorf("%s modifier: %w", root.Name, err)
	}

	items, err := readType(handle, TypeResult{Name: TpStruct, Params: []Result{*root}}, rootEndian)
	if err != nil {
		return nil, err
	}

	return &ParsedDefinition{Metadata: Metadata(*meta), Items: items.([]DefinitionItem)}, nil
}
