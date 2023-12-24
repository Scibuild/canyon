// Bug in the parser library's macro
#![allow(unused_braces)]
#![allow(clippy::match_single_binding)]
#![allow(clippy::redundant_closure_call)]
#![allow(clippy::ptr_arg)]

use crate::ast::*;
use crate::lexer::*;
use crate::lexer::Token::*;
use plex::parser;

pub fn mk_binop(span: Span, lhs: PExpr, rhs: PExpr, op: BinOp) -> PExpr {
    Node::mk(span, Expr_::BinOp(lhs, op, rhs))
}

parser! {
    fn parse_(Token, Span);

    // How to combine spans
    (a,b) {
        Span {
            lo: a.lo,
            hi: b.hi,
        }
    }

// E - expressions requiring a semicolon after them if another expression follows
// S - expressions not requiring a semicolon after them
// 
// ex
// S S E
// S E; S
// E; E; E
// E; E; E;
// S E; S;
// ε
// 
// nonex
// E S
// 
// 
// A -> ε | E ; A | S A | E | S
// except           ^         ^  has reduce reduce conflict when A = ε
// so
// A -> E B | E | S | S A | S B
// B -> ; C
// C -> A | ε
//
//
// Now what about ; ; ; ; C


    program: PExpr {
        statements[mut s] => {
            s.reverse();
            Node::mk(span!(), Expr_::Block(s))
        }
    }

    statements: Vec<Option<PExpr>> {
        nonemptyStatements[s] => s,
        => vec![None]
    }

    semiStatements: Vec<Option<PExpr>> {
        Semi statements[s] => s,
        Semi semiStatements[s] => s
    }

    nonemptyStatements: Vec<Option<PExpr>> {
        nonOptSemiExpr[e] => vec![Some(e)],
        nonOptSemiExpr[e] semiStatements[mut ss] => { ss.push(Some(e)); ss },
        optSemiExpr[e] => vec![Some(e)],
        optSemiExpr[e] semiStatements[mut ss] => { ss.push(Some(e)); ss },
        optSemiExpr[e] nonemptyStatements[mut ss] => { ss.push(Some(e)); ss }
    }

    statementsOptSemi: Vec<Option<PExpr>> {
        maybeStatementOptSemi[e] => vec![e],
        statementsOptSemi[mut st] maybeStatementOptSemi[e] => {
            st.push(e); st
        },
        statements[mut st] Semi maybeStatementOptSemi[e] => {
            st.push(e); st
        }
    }

    maybeStatement: Option<PExpr> {
        nonOptSemiExpr[e] => Some(e),
        => None
    }

    maybeStatementOptSemi: Option<PExpr> {
        optSemiExpr[e] => Some(e),
    }

    block: PExpr {
        Obrace statements[mut s] Cbrace => {
            s.reverse(); 
            Node::mk(span!(), Expr_::Block(s))
        }
    }

    // Semicolons are optional after these expressions in statement form
    optSemiExpr: PExpr {
        If expr1[cond] block[block] 
            => Node::mk(span!(), Expr_::If(cond, block, None)),
        If expr1[cond] block[block] Else block[else_block] 
            => Node::mk(span!(), Expr_::If(cond, block, Some(else_block))),
        While expr1[cond] block[block] => Node::mk(span!(), Expr_::While(cond, block)),
    }

    nonOptSemiExpr: PExpr {
        expr1[lhs] Eq expr1[rhs] => Node::mk(span!(), Expr_::Assign(lhs, rhs)),
        Let Ident(name) Colon typ[ty] => Node::mk(span!(), Expr_::VarDecl(name, Some(ty), None)),
        Let Ident(name) Eq expr[e] => Node::mk(span!(), Expr_::VarDecl(name, None, Some(e))),
        Let Ident(name) Colon typ[ty] Eq expr[e] => Node::mk(span!(), Expr_::VarDecl(name, Some(ty), Some(e))),
        Fn Oparen params[params] Cparen typ[ret_ty] block[block] => Node::mk(span!(), Expr_::Lambda(params, ret_ty, block)),
        expr1[e] => e,
    }

    expr: PExpr {
        optSemiExpr[e] => e,
        nonOptSemiExpr[e] => e,
    }

    params: Vec<(String, PSynTy)> {
        => vec![],
        nonEmptyParams[p] => p,
        nonEmptyParams[p] Comma => p,
    }

    nonEmptyParams: Vec<(String, PSynTy)> {
        param[p] => vec![p],
        nonEmptyParams[mut ps] Comma param[p] => { ps.push(p); ps }
    }
    
    param: (String, PSynTy) {
        Ident(p) Colon typ[ty] => (p, ty)
    }

    expr1: PExpr {
        expr1[lhs] Or expr2[rhs] => mk_binop(span!(), lhs, rhs, BinOp::Or),
        expr2[e] => e
    }

    expr2: PExpr {
        expr2[lhs] And expr3[rhs] => mk_binop(span!(), lhs, rhs, BinOp::And),
        expr3[e] => e
    }

    expr3op: BinOp {
            Eqq => BinOp::Eqq,
            Ne => BinOp::Ne,
            Le => BinOp::Le,
            Lt => BinOp::Lt,
            Ge => BinOp::Ge,
            Gt => BinOp::Gt,
    }

    expr3: PExpr {
        expr3[lhs] expr3op[op] expr4[rhs] => mk_binop(span!(), lhs, rhs, op),
        expr4[e] => e
    }

    expr4op: BinOp {
        Add => BinOp::Add,
        Sub => BinOp::Sub,
    }

    expr4: PExpr {
        expr4[lhs] expr4op[op] expr5[rhs] => mk_binop(span!(), lhs, rhs, op),
        expr5[e] => e
    }

    expr5op: BinOp {
        Mul => BinOp::Mul,
        Div => BinOp::Div,
    }

    expr5: PExpr {
        expr5[lhs] expr5op[op] expr6[rhs] => mk_binop(span!(), lhs, rhs, op),
        expr6[e] => e
    }

    expr6op: UnOp {
        Sub => UnOp::Neg,
        Not => UnOp::Not,
    }

    expr6: PExpr {
        expr6op[op] expr6[rhs] => Node::mk(span!(), Expr_::UnOp(op, rhs)),
        expr7[e] => e
    }

    expr7: PExpr {
        expr7[e] Oparen fnArgs[args] Cparen => Node::mk(span!(), Expr_::FnCall(e, args)),
        expr7[e] Obrack expr[index] Cbrack => Node::mk(span!(), Expr_::Subscript(e, index)),
        expr7[e] Dot Ident(name) => Node::mk(span!(), Expr_::Selector(e, name)),
        expr8[e] => e
    }

    fnArgs: Vec<PExpr> {
        => vec![],
        nonemptyFnArgs[f] => f,
        nonemptyFnArgs[f] Comma => f,
    }

    nonemptyFnArgs: Vec<PExpr> {
        expr[e] => vec![e],
        nonemptyFnArgs[mut args] Comma expr[e] => {
            args.push(e); args
        }
    }

    expr8: PExpr {
        Ident(name) => Node::mk(span!(), Expr_::Ident(name)),
        True => Node::mk(span!(), Expr_::Bool(true)),
        False => Node::mk(span!(), Expr_::Bool(false)),
        Int(val) => Node::mk(span!(), Expr_::Int(val)),
        Oparen expr[e] Cparen => e,
    }

    typ: PSynTy {
        Ident(name) => Node::mk(span!(), SynTy::Ident(name)),
        Ident(name) Obrack typArgs[args] Cbrack => Node::mk(span!(), SynTy::Parameterised(name, args)),
        Oparen typArgs[args] Cparen typ[ret_ty] => Node::mk(span!(), SynTy::Function(args, ret_ty)),
    }

    typArgs: Vec<PSynTy> {
        => vec![],
        nonemptyTypArgs[a] => a,
        nonemptyTypArgs[a] Comma => a,
    }

    nonemptyTypArgs: Vec<PSynTy> {
        typ[ty] => vec![ty],
        nonemptyTypArgs[mut args] Comma typ[ty] => {
            args.push(ty); args
        }
    }
}

pub fn parse<I: Iterator<Item = (Token, Span)>>(i: I)
    -> Result<PExpr, (Option<(Token, Span)>, &'static str)> {
    parse_(i)
}
