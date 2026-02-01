package lang

import (
	"testing"
)

func TestEqualsOperator(t *testing.T) {
	type input struct {
		left, right Result
		expected    bool
	}

	inputs := []input{
		{left: BooleanResult(true), right: BooleanResult(true), expected: true},
		{left: BooleanResult(true), right: BooleanResult(false), expected: false},

		{left: NewIntResult(0), right: NewIntResult(0), expected: true},
		{left: NewFloatResult(0), right: NewFloatResult(0), expected: true},

		{left: NewIntResult(1), right: NewIntResult(2), expected: false},
		{left: NewFloatResult(1), right: NewFloatResult(2), expected: false},

		{left: NewIntResult(10), right: NewFloatResult(10), expected: true},
		{left: NewFloatResult(20.5), right: NewIntResult(20), expected: false},

		{left: ListResult{}, right: ListResult{}, expected: true},
		{left: MapResult{}, right: MapResult{}, expected: true},
		{left: StringResult(""), right: StringResult(""), expected: true},

		{left: ListResult{NewIntResult(10)}, right: ListResult{NewIntResult(10)}, expected: true},
		{
			left:     MapResult{StringResult("foo"): StringResult("bar")},
			right:    MapResult{StringResult("foo"): StringResult("bar")},
			expected: true,
		},
		{left: ListResult{}, right: ListResult{NewFloatResult(10)}, expected: false},
		{left: MapResult{}, right: ListResult{}, expected: false},
	}

	runtime := NewRuntime()

	for _, inp := range inputs {
		actual := bool(runtime.doBinOpEquals(inp.left, inp.right))
		if actual != inp.expected {
			t.Fatalf("expected %v == %v to be %v, got %v", inp.left, inp.right, inp.expected, actual)
		}
	}
}

func TestLtOperator(t *testing.T) {
	type input struct {
		left, right  Result
		expected, ok bool
	}

	inputs := []input{
		{left: NewIntResult(10), right: NewIntResult(20), expected: true, ok: true},
		{left: NewIntResult(20), right: NewIntResult(10), expected: false, ok: true},
		{left: NewIntResult(20), right: NewIntResult(20), expected: false, ok: true},

		{left: NewFloatResult(10.5), right: NewFloatResult(10.6), expected: true, ok: true},
		{left: NewFloatResult(10.6), right: NewFloatResult(10.5), expected: false, ok: true},
		{left: NewFloatResult(20), right: NewFloatResult(20), expected: false, ok: true},

		{left: NewIntResult(10), right: NewFloatResult(10.1), expected: true, ok: true},
		{left: NewFloatResult(20.5), right: NewIntResult(21), expected: true, ok: true},
		{left: NewIntResult(10), right: NewFloatResult(10), expected: false, ok: true},

		{left: ListResult{}, right: ListResult{}, expected: false, ok: false},
		{left: ListResult{}, right: NewIntResult(20), expected: false, ok: false},
	}

	runtime := NewRuntime()

	for _, inp := range inputs {
		actualRes, actualOk := runtime.doBinOpLt(inp.left, inp.right)

		if !inp.ok && bool(actualOk) {
			t.Fatalf("expected %v < %v to fail, got %v", inp.left, inp.right, actualRes)
		}

		if bool(actualRes) != inp.expected {
			t.Fatalf("expected %v < %v to be %v, got %v", inp.left, inp.right, inp.expected, actualRes)
		}
	}
}

func TestGtOperator(t *testing.T) {
	type input struct {
		left, right  Result
		expected, ok bool
	}

	inputs := []input{
		{left: NewIntResult(20), right: NewIntResult(10), expected: true, ok: true},
		{left: NewIntResult(10), right: NewIntResult(20), expected: false, ok: true},
		{left: NewIntResult(10), right: NewIntResult(10), expected: false, ok: true},

		{left: NewFloatResult(10.6), right: NewFloatResult(10.5), expected: true, ok: true},
		{left: NewFloatResult(10.5), right: NewFloatResult(10.6), expected: false, ok: true},
		{left: NewFloatResult(10), right: NewFloatResult(10), expected: false, ok: true},

		{left: NewIntResult(21), right: NewFloatResult(20.5), expected: true, ok: true},
		{left: NewIntResult(10), right: NewFloatResult(10.1), expected: false, ok: true},
		{left: NewIntResult(10), right: NewFloatResult(10), expected: false, ok: true},

		{left: ListResult{}, right: ListResult{}, expected: false, ok: false},
		{left: ListResult{}, right: NewIntResult(20), expected: false, ok: false},
	}

	runtime := NewRuntime()

	for _, inp := range inputs {
		actualRes, actualOk := runtime.doBinOpGt(inp.left, inp.right)

		if !inp.ok && bool(actualOk) {
			t.Fatalf("expected %v > %v to fail, got %v", inp.left, inp.right, actualRes)
		}

		if bool(actualRes) != inp.expected {
			t.Fatalf("expected %v > %v to be %v, got %v", inp.left, inp.right, inp.expected, actualRes)
		}
	}
}

func TestTruthiness(t *testing.T) {
	type input struct {
		value    Result
		expected bool
	}

	inputs := []input{
		{value: BooleanResult(true), expected: true},
		{value: BooleanResult(false), expected: false},

		{value: NewIntResult(10), expected: true},
		{value: NewIntResult(-10), expected: true},
		{value: NewIntResult(0), expected: false},

		{value: NewFloatResult(10.0), expected: true},
		{value: NewFloatResult(-10.0), expected: true},
		{value: NewFloatResult(0.0), expected: false},

		{value: ListResult{}, expected: false},
		{value: ListResult{NewIntResult(10), NewIntResult(20)}, expected: true},

		{value: MapResult{}, expected: false},
		{value: MapResult{StringResult("foo"): StringResult("bar")}, expected: true},
	}

	for _, inp := range inputs {
		if actual := ResultIsTruthy(inp.value); actual != inp.expected {
			t.Fatalf("expected value %v to be %v, got %v", inp.value, inp.expected, actual)
		}
	}
}
