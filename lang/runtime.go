package lang

import (
	"fmt"
	"math/big"
)

type Runtime struct {
	Environment map[Node]Node
	Metadata    Meta
}

type Meta struct {
	Dessert    string
	Name       string
	Extensions []string
	Mime       []string
	Docs       string
}

func ParseMetadata(mapping MapResult) (*Meta, error) {
	dessert, err := GetKeyByIdent[StringResult](mapping, "dessert", true)
	if err != nil {
		return nil, fmt.Errorf("meta: %s", err)
	}

	name, err := GetKeyByIdent[StringResult](mapping, "name", true)
	if err != nil {
		return nil, fmt.Errorf("meta: %s", err)
	}

	exts, err := GetKeyByIdent[ListResult](mapping, "exts", false)
	if err != nil {
		return nil, fmt.Errorf("meta: %s", err)
	}

	extensions, err, errIdx := ToStringList(exts)
	if err != nil {
		return nil, fmt.Errorf("meta > exts[%d]: %w", errIdx, err)
	}

	mimes, err := GetKeyByIdent[ListResult](mapping, "mime", false)
	if err != nil {
		return nil, fmt.Errorf("meta: %s", err)
	}

	mimeTypes, err, errIdx := ToStringList(mimes)
	if err != nil {
		return nil, fmt.Errorf("meta > exts[%d]: %w", errIdx, err)
	}

	docs, err := GetKeyByIdent[StringResult](mapping, "docs", false)
	if err != nil {
		return nil, fmt.Errorf("meta: %w", err)
	}

	return &Meta{
		Dessert:    string(dessert),
		Name:       string(name),
		Extensions: extensions,
		Mime:       mimeTypes,
		Docs:       string(docs),
	}, nil
}

type ResultKind string

const (
	ResInteger    ResultKind = "Integer"
	ResFloat      ResultKind = "Float"
	ResString     ResultKind = "String"
	ResMap        ResultKind = "Map"
	ResIdentifier ResultKind = "Identifier"
	ResList       ResultKind = "List"
	ResMeta       ResultKind = "Meta"
)

type Result interface {
	Kind() ResultKind
}

type IntResult struct{ *big.Int }
type FloatResult struct{ *big.Float }
type StringResult string
type MapResult map[Result]Result
type ListResult []Result
type IdentResult string

type MetaResult Meta

func (ir IntResult) Kind() ResultKind    { return ResInteger }
func (fr FloatResult) Kind() ResultKind  { return ResFloat }
func (sr StringResult) Kind() ResultKind { return ResString }
func (mr MapResult) Kind() ResultKind    { return ResMap }
func (ir IdentResult) Kind() ResultKind  { return ResIdentifier }
func (lr ListResult) Kind() ResultKind   { return ResList }

func (mr MetaResult) Kind() ResultKind { return ResMeta }

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

func GetKeyByIdent[T Result](mapping MapResult, name string, required bool) (T, error) {
	var empty T

	valueRes, exists := mapping[IdentResult(name)]
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

func (r *Runtime) EvaluateBinOp(node BinOpNode) (Result, error) {
	left, err := r.EvaluateExpr(node.Left)
	if err != nil {
		return nil, err
	}

	right, err := r.EvaluateExpr(node.Right)
	if err != nil {
		return nil, err
	}

	switch node.Op.Kind {
	case TokenPlus:
		switch {
		case left.Kind() == ResInteger && right.Kind() == ResInteger:
			return IntResult{new(big.Int).Add(left.(IntResult).Int, right.(IntResult).Int)}, nil
		case left.Kind() == ResFloat && right.Kind() == ResFloat:
			return FloatResult{new(big.Float).Add(left.(FloatResult).Float, right.(FloatResult).Float)}, nil
		case left.Kind() == ResInteger && right.Kind() == ResFloat:
			leftFloat := new(big.Float).SetInt(left.(IntResult).Int)
			return FloatResult{new(big.Float).Add(leftFloat, right.(FloatResult).Float)}, nil
		case left.Kind() == ResFloat && right.Kind() == ResInteger:
			rightFloat := new(big.Float).SetInt(right.(IntResult).Int)
			return FloatResult{new(big.Float).Add(left.(FloatResult).Float, rightFloat)}, nil
		case left.Kind() == ResString && right.Kind() == ResString:
			return left.(StringResult) + right.(StringResult), nil
		}
	case TokenMinus:
		switch {
		case left.Kind() == ResInteger && right.Kind() == ResInteger:
			return IntResult{new(big.Int).Sub(left.(IntResult).Int, right.(IntResult).Int)}, nil
		case left.Kind() == ResFloat && right.Kind() == ResFloat:
			return FloatResult{new(big.Float).Sub(left.(FloatResult).Float, right.(FloatResult).Float)}, nil
		case left.Kind() == ResInteger && right.Kind() == ResFloat:
			leftFloat := new(big.Float).SetInt(left.(IntResult).Int)
			return FloatResult{new(big.Float).Sub(leftFloat, right.(FloatResult).Float)}, nil
		case left.Kind() == ResFloat && right.Kind() == ResInteger:
			rightFloat := new(big.Float).SetInt(right.(IntResult).Int)
			return FloatResult{new(big.Float).Sub(left.(FloatResult).Float, rightFloat)}, nil
		}
	case TokenMul:
		switch {
		case left.Kind() == ResInteger && right.Kind() == ResInteger:
			return IntResult{new(big.Int).Mul(left.(IntResult).Int, right.(IntResult).Int)}, nil
		case left.Kind() == ResFloat && right.Kind() == ResFloat:
			return FloatResult{new(big.Float).Mul(left.(FloatResult).Float, right.(FloatResult).Float)}, nil
		case left.Kind() == ResInteger && right.Kind() == ResFloat:
			leftFloat := new(big.Float).SetInt(left.(IntResult).Int)
			return FloatResult{new(big.Float).Mul(leftFloat, right.(FloatResult).Float)}, nil
		case left.Kind() == ResFloat && right.Kind() == ResInteger:
			rightFloat := new(big.Float).SetInt(right.(IntResult).Int)
			return FloatResult{new(big.Float).Mul(left.(FloatResult).Float, rightFloat)}, nil
		}
	case TokenDiv:
		switch {
		case right.Kind() == ResInteger && right.(IntResult).Cmp(new(big.Int).SetInt64(0)) == 0,
			right.Kind() == ResFloat && right.(FloatResult).Cmp(new(big.Float).SetInt64(0)) == 0:
			return nil, LangError{
				ErrorDomain,
				node.Position(),
				"division by zero",
			}
		case left.Kind() == ResInteger && right.Kind() == ResInteger:
			leftFloat := new(big.Float).SetInt(left.(IntResult).Int)
			rightFloat := new(big.Float).SetInt(right.(IntResult).Int)
			return FloatResult{new(big.Float).Quo(leftFloat, rightFloat)}, nil
		case left.Kind() == ResFloat && right.Kind() == ResFloat:
			return FloatResult{new(big.Float).Quo(left.(FloatResult).Float, right.(FloatResult).Float)}, nil
		case left.Kind() == ResInteger && right.Kind() == ResFloat:
			leftFloat := new(big.Float).SetInt(left.(IntResult).Int)
			return FloatResult{new(big.Float).Quo(leftFloat, right.(FloatResult).Float)}, nil
		case left.Kind() == ResFloat && right.Kind() == ResInteger:
			rightFloat := new(big.Float).SetInt(right.(IntResult).Int)
			return FloatResult{new(big.Float).Quo(left.(FloatResult).Float, rightFloat)}, nil
		}
	}

	return nil, LangError{
		ErrorType,
		node.Position(),
		fmt.Sprintf("cannot do binary operation %s on types %s and %s", node.Op.Kind, left.Kind(), right.Kind()),
	}
}

func (r *Runtime) EvaluateLiteral(lit LiteralNode) (Result, error) {
	switch tok := lit.Token; tok.Kind {
	case TokenInteger:
		if intVal, ok := new(big.Int).SetString(tok.Value, 0); ok {
			return IntResult{intVal}, nil
		} else {
			return nil, LangError{
				ErrorSyntax,
				tok.Position,
				fmt.Sprintf("invalid integer literal: %q", tok.Value),
			}
		}
	case TokenFloat:
		if floatVal, ok := new(big.Float).SetString(tok.Value); ok {
			return FloatResult{floatVal}, nil
		} else {
			return nil, LangError{
				ErrorSyntax,
				tok.Position,
				fmt.Sprintf("invalid float literal: %q", tok.Value),
			}
		}
	case TokenString:
		return StringResult(tok.Value), nil
	case TokenIdentifier:
		return IdentResult(tok.Value), nil
	default:
		return nil, LangError{
			ErrorSyntax,
			tok.Position,
			fmt.Sprintf("unexpected literal type %s", tok.Kind),
		}
	}
}

func (r *Runtime) EvaluateList(listNode ListNode) (Result, error) {
	list := ListResult{}

	for _, itemNode := range listNode.Items {
		item, err := r.EvaluateExpr(itemNode)
		if err != nil {
			return nil, err
		}

		list = append(list, item)
	}

	return list, nil
}

func (r *Runtime) EvaluateMap(mapNode MapNode) (Result, error) {
	mapping := MapResult{}

	for keyNode, valueNode := range mapNode.Items {
		key, err := r.EvaluateExpr(keyNode)
		if err != nil {
			return nil, err
		}

		value, err := r.EvaluateExpr(valueNode)
		if err != nil {
			return nil, err
		}

		mapping[key] = value
	}

	return mapping, nil
}

func (r *Runtime) EvaluateUnaryOp(unary UnaryOpNode) (Result, error) {
	expr, err := r.EvaluateExpr(unary.Node)
	if err != nil {
		return nil, err
	}

	switch unary.Op.Kind {
	case TokenMinus:
		switch ex := expr.(type) {
		case IntResult:
			return IntResult{new(big.Int).Neg(ex.Int)}, nil
		case FloatResult:
			return FloatResult{new(big.Float).Neg(ex.Float)}, nil
		}
	case TokenPlus:
		switch ex := expr.(type) {
		case IntResult:
		case FloatResult:
			return ex, nil
		}
	case TokenBitwiseNot:
		switch ex := expr.(type) {
		case IntResult:
			return IntResult{new(big.Int).Not(ex.Int)}, nil
		}
	}

	return nil, LangError{
		ErrorType,
		unary.Position(),
		fmt.Sprintf("%s does not implement unary operation %s", expr.Kind(), unary.Op.Kind),
	}
}

func (r *Runtime) EvaluateExpr(expr Node) (Result, error) {
	switch expr.Type() {
	case NodeBinOp:
		return r.EvaluateBinOp(*expr.(*BinOpNode))
	case NodeUnaryOp:
		return r.EvaluateUnaryOp(*expr.(*UnaryOpNode))
	case NodeMap:
		return r.EvaluateMap(*expr.(*MapNode))
	case NodeList:
		return r.EvaluateList(*expr.(*ListNode))
	case NodeLiteral:
		return r.EvaluateLiteral(*expr.(*LiteralNode))
	default:
		return nil, LangError{
			ErrorRuntime,
			expr.Position(),
			fmt.Sprintf("cannot evaluate %s expression yet", expr.Type()),
		}
	}
}

func (r *Runtime) EvaluateExprStmt(stmt ExprStmt) (Result, error) {
	return r.EvaluateExpr(stmt.Expr)
}

func (r *Runtime) EvaluateMetaStmt(stmt MetaStmt) (Result, error) {
	metaMap, err := r.EvaluateMap(*stmt.Metadata)
	if err != nil {
		return nil, err
	}

	metadata, err := ParseMetadata(metaMap.(MapResult))
	if err != nil {
		return nil, err
	}
	r.Metadata = *metadata

	return MetaResult(r.Metadata), nil
}

func (r *Runtime) EvaluateStmt(stmt Node) (Result, error) {
	switch stmt.Type() {
	case StmtExpr:
		return r.EvaluateExprStmt(*stmt.(*ExprStmt))
	case StmtMeta:
		return r.EvaluateMetaStmt(*stmt.(*MetaStmt))
	default:
		return nil, LangError{
			ErrorRuntime,
			stmt.Position(),
			fmt.Sprintf("cannot evaluate %s statement yet", stmt.Type()),
		}
	}
}

func (r *Runtime) Run(statements []Node) ([]Result, error) {
	results := []Result{}
	for _, stmt := range statements {
		result, err := r.EvaluateStmt(stmt)
		if err != nil {
			return nil, err
		}
		results = append(results, result)
	}

	return results, nil
}
