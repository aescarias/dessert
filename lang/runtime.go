package lang

import (
	"fmt"
	"math/big"
)

type Metadata struct {
	Dessert    string
	Name       string
	Extensions []string
	Mime       []string
	Docs       string
}

func NewMetadata(meta map[string]Result) (*Metadata, error) {
	dessert, err := GetMapKey[StringResult](meta, "dessert", true)
	if err != nil {
		return nil, fmt.Errorf("meta: %s", err)
	}

	name, err := GetMapKey[StringResult](meta, "name", true)
	if err != nil {
		return nil, fmt.Errorf("meta: %s", err)
	}

	exts, err := GetMapKey[ListResult](meta, "exts", false)
	if err != nil {
		return nil, fmt.Errorf("meta: %s", err)
	}

	extensions, err, errIdx := ToStringList(exts)
	if err != nil {
		return nil, fmt.Errorf("meta > exts[%d]: %w", errIdx, err)
	}

	mimes, err := GetMapKey[ListResult](meta, "mime", false)
	if err != nil {
		return nil, fmt.Errorf("meta: %s", err)
	}

	mimeTypes, err, errIdx := ToStringList(mimes)
	if err != nil {
		return nil, fmt.Errorf("meta > mime[%d]: %w", errIdx, err)
	}

	docs, err := GetMapKey[StringResult](meta, "docs", false)
	if err != nil {
		return nil, fmt.Errorf("meta: %w", err)
	}

	return &Metadata{
		Dessert:    string(dessert),
		Name:       string(name),
		Extensions: extensions,
		Mime:       mimeTypes,
		Docs:       string(docs),
	}, nil
}

type Runtime struct {
	Globals  map[string]Result
	Metadata Metadata
}

func NewRuntime() *Runtime {
	runtime := &Runtime{Globals: map[string]Result{}}
	runtime.addTypes()

	return runtime
}

func (r *Runtime) addTypes() {
	for _, typeName := range AvailableTypeNames {
		r.Globals[string(typeName)] = TypeResult{
			Name:   typeName,
			Params: nil,
		}
	}
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
		if global, ok := r.Globals[tok.Value]; ok {
			return global, nil
		} else {
			return nil, LangError{
				ErrorAccess,
				tok.Position,
				fmt.Sprintf("%q is not defined", tok.Value),
			}
		}
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

func (r *Runtime) EvaluateSubscript(subscript SubscriptNode) (Result, error) {
	expr, err := r.EvaluateExpr(subscript.Expr)
	if err != nil {
		return nil, fmt.Errorf("while evaluating subscript expr: %w", err)
	}

	param, err := r.EvaluateExpr(subscript.Item)
	if err != nil {
		return nil, fmt.Errorf("while evaluating subscript item: %w", err)
	}

	switch res := expr.(type) {
	case ListResult:
		index, isInteger := param.(*IntResult)
		if !isInteger {
			return nil, fmt.Errorf("list index [%v] must be integer", param)
		}

		if idx := int(index.Int64()); idx > len(res) {
			return nil, fmt.Errorf("list index [%d] is out of range (list length is %d)", index.Int64(), len(res))
		} else {
			return res[idx], nil
		}
	case MapResult:
		if value, exists := res[param]; exists {
			return value, nil
		} else {
			return nil, fmt.Errorf("mapping %v does not have key %v", res, param)
		}
	case TypeResult:
		arrayLen, isInteger := param.(IntResult)
		if !isInteger {
			return nil, fmt.Errorf("array length [%v] must be integer", param)
		}

		switch res.Name {
		case TpByte:
			return TypeResult{Name: TpByte, Params: []Result{arrayLen}}, nil
		default:
			return TypeResult{Name: TpArray, Params: []Result{res, arrayLen}}, nil
		}
	case StructResult:
		arrayLen, isInteger := param.(IntResult)
		if !isInteger {
			return nil, fmt.Errorf("array length [%v] must be integer", param)
		}

		return TypeResult{
			Name: TpArray,
			Params: []Result{
				TypeResult{Name: TpStruct, Params: []Result{res}},
				arrayLen,
			},
		}, nil
	default:
		return nil, fmt.Errorf("subscript is not supported for object of type %v", expr.Kind())
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
	case NodeSubscript:
		return r.EvaluateSubscript(*expr.(*SubscriptNode))
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
	metaMap := map[string]Result{}
	for key, valueNode := range stmt.Metadata {
		value, err := r.EvaluateExpr(valueNode)
		if err != nil {
			return nil, err
		}
		metaMap[key] = value
	}

	metadata, err := NewMetadata(metaMap)
	if err != nil {
		return nil, err
	}
	r.Metadata = *metadata

	return MetaResult(r.Metadata), nil
}

func (r *Runtime) EvaluateModifierList(list map[string]Node) (map[string]Result, error) {
	modifiers := map[string]Result{}

	for key, valueNode := range list {
		if valueNode == nil {
			modifiers[key] = BooleanResult(true)
			continue
		}

		value, err := r.EvaluateExpr(valueNode)
		if err != nil {
			return nil, err
		}

		modifiers[key] = value
	}

	return modifiers, nil
}

func (r *Runtime) EvaluateStructStmt(stmt StructStmt) (Result, error) {
	modifiers, err := r.EvaluateModifierList(stmt.Modifiers)
	if err != nil {
		return nil, err
	}

	fields := []StructField{}

	for _, field := range stmt.Fields {
		fieldVal, err := r.EvaluateExpr(field.Value)
		if err != nil {
			return nil, fmt.Errorf("%v > %s: %w", stmt.Name, field.Name, err)
		}

		fieldMod, err := r.EvaluateModifierList(field.Modifiers)
		if err != nil {
			return nil, fmt.Errorf("%v > modifiers for %s: %w", stmt.Name, field.Name, err)
		}

		var fieldItem Result
		switch item := fieldVal.(type) {
		case TypeResult:
			fieldItem = item
		case StringResult:
			fieldItem = TypeResult{Name: TpMatch, Params: []Result{item}}
		case StructResult:
			fieldItem = TypeResult{Name: TpStruct, Params: []Result{item}}
		default:
			return nil, fmt.Errorf("%v > %s: %v is not allowed in this context", stmt.Name, field.Name, fieldVal)
		}

		fields = append(fields, StructField{Name: field.Name, Value: fieldItem, Modifiers: fieldMod})
	}

	r.Globals[stmt.Name] = StructResult{Name: stmt.Name, Fields: fields, Modifiers: modifiers}
	return r.Globals[stmt.Name], nil
}

func (r *Runtime) EvaluateStmt(stmt Node) (Result, error) {
	switch stmt.Type() {
	case StmtExpr:
		return r.EvaluateExprStmt(*stmt.(*ExprStmt))
	case StmtMeta:
		return r.EvaluateMetaStmt(*stmt.(*MetaStmt))
	case StmtStruct:
		return r.EvaluateStructStmt(*stmt.(*StructStmt))
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
