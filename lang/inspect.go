package lang

import (
	"encoding/binary"
	"fmt"
	"io"
)

func readType(handle io.ReadSeeker, kind TypeResult, endian binary.ByteOrder) (any, error) {
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

func ParseFromReader(handle io.ReadSeeker, definition []Result) error {
	if _, err := handle.Seek(0, 0); err != nil {
		return err
	}

	var root *StructResult

	for _, stmt := range definition {
		if structure, ok := stmt.(StructResult); ok {
			isRootValue, exists := structure.Modifiers["root"]
			if !exists {
				continue
			}

			if isRoot, ok := isRootValue.(BooleanResult); !ok {
				return fmt.Errorf("in struct %s: root modifier must be boolean", structure.Name)
			} else if isRoot {
				root = &structure
				break
			}
		}
	}

	if root == nil {
		return fmt.Errorf("no root structure was declared")
	}

	endian, err := GetEndian(root.Modifiers, true)
	if err != nil {
		return fmt.Errorf("struct %s modifier: %w", root.Name, err)
	}

	for _, field := range root.Fields {
		kind, err := ResultMustBe[TypeResult](field.Value)
		if err != nil {
			return fmt.Errorf("%s > %s: %s\n", root.Name, field.Name, err)
		}

		fieldEndian, err := GetEndian(field.Modifiers, false)
		if err != nil {
			return fmt.Errorf("%s > %s modifier: %w\n", root.Name, field.Name, err)
		}

		if fieldEndian != nil {
			fieldEndian = endian
		}

		data, err := readType(handle, kind, fieldEndian)
		if err != nil {
			return fmt.Errorf("%s > %s: %w", root.Name, field.Name, err)
		}

		fmt.Printf("%s: %v\n", field.Name, data)
	}

	return nil
}
