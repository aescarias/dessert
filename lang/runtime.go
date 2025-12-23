package lang

import (
	"fmt"
	"math/big"
)

type Runtime struct {
	Environment map[Node]Node
}

type ResultKind string

const (
	ResInteger ResultKind = "Integer"
	ResFloat   ResultKind = "Float"
	ResString  ResultKind = "String"
)

type Result interface {
	Kind() ResultKind
}

type IntResult struct{ *big.Int }
type FloatResult struct{ *big.Float }
type StringResult string

func (ir IntResult) Kind() ResultKind    { return ResInteger }
func (fr FloatResult) Kind() ResultKind  { return ResFloat }
func (sr StringResult) Kind() ResultKind { return ResString }

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
	default:
		return nil, LangError{
			ErrorSyntax,
			tok.Position,
			fmt.Sprintf("unexpected literal type %s", tok.Kind),
		}
	}
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
	case NodeLiteral:
		return r.EvaluateLiteral(*expr.(*LiteralNode))
	case NodeUnaryOp:
		return r.EvaluateUnaryOp(*expr.(*UnaryOpNode))
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

func (r *Runtime) EvaluateStmt(stmt Node) (Result, error) {
	switch stmt.Type() {
	case StmtExpr:
		return r.EvaluateExprStmt(*stmt.(*ExprStmt))
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
