use crate::lexer::{Span, Token};
use std::fmt;
use crate::types;

pub type PExpr = Box<Node<Expr_>>;
pub type PSynTy = Box<Node<SynTy>>;

impl<T> Node<T> {
    pub fn mk(span: Span, node: T) -> Box<Node<T>> {
        Box::new(Node::<T> { span, node, ty: None })
    }

}

impl<T> fmt::Display for Node<T> 
    where T: fmt::Display {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.node)
    }
}

#[derive(Debug)]
pub struct Node<T> {
    pub span: Span,
    pub node: T,
    pub ty: Option<types::Ty>,
}

#[derive(Debug)]
pub enum BinOp {
    Add, Sub, Mul, Div, Eqq, Ne, Le, Lt, Ge, Gt, And, Or
}

impl TryFrom<Token> for BinOp {
    type Error = ();

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            Token::Add => Ok(BinOp::Add),
            Token::Sub => Ok(BinOp::Sub),
            Token::Mul => Ok(BinOp::Mul),
            Token::Div => Ok(BinOp::Div),
            Token::Eqq => Ok(BinOp::Eqq),
            Token::Ne  => Ok(BinOp::Ne),
            Token::Le  => Ok(BinOp::Le),
            Token::Lt  => Ok(BinOp::Lt),
            Token::Ge  => Ok(BinOp::Ge),
            Token::Gt  => Ok(BinOp::Gt),
            Token::And => Ok(BinOp::And),
            Token::Or  => Ok(BinOp::Or),
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub enum UnOp {
    Neg, Not
}

#[derive(Debug)]
pub enum Expr_ {
    BinOp(PExpr, BinOp, PExpr),
    UnOp(UnOp, PExpr),
    Ident(String),
    Int(String),
    Bool(bool),
    Assign(PExpr, PExpr),
    FnCall(PExpr, Vec<PExpr>),
    Subscript(PExpr, PExpr),
    Selector(PExpr, String),
    If(PExpr, PExpr, Option<PExpr>),
    While(PExpr, PExpr),
    Block(Vec<Option<PExpr>>),
    Lambda(Vec<(String, PSynTy)>, PSynTy, PExpr),
    VarDecl(String, Option<PSynTy>, Option<PExpr>),
    ArrayLiteral(Vec<PExpr>),
}

impl fmt::Display for Expr_ {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Expr_::*;
        match self {
            BinOp(lhs, op, rhs) => write!(f, "({} {:?} {})", lhs, op, rhs),
            UnOp(op, rhs) => write!(f, "({:?} {})", op, rhs),
            Ident(s) => write!(f, "{}", s),
            Int(s) => write!(f, "{}", s),
            Bool(s) => write!(f, "{}", s),
            Assign(lhs, rhs) => write!(f, "{} = {}", lhs, rhs),
            FnCall(e, args) => {
                write!(f, "{}(", e)?;
                for arg in args {
                    write!(f, "{}, ", arg)?;
                }
                write!(f, ")")
            }
            Subscript(e, index) => write!(f, "{}[{}]", e, index),
            Selector(e, field) => write!(f, "{}.{}", e, field),
            If(cond, block, Some(elblock)) => write!(f, "if {} {} else {}", cond, block, elblock),
            If(cond, block, None) => write!(f, "if {} {}", cond, block),
            While(cond, block) => write!(f, "while {} {}", cond, block),
            Block(stmts) => {
                writeln!(f, "{{")?;
                for stmt in stmts {
                    if let Some(stmt) = stmt {
                        write!(f, "{}", stmt)?;
                    } 
                    writeln!(f, ";")?;
                }
                write!(f, "}}")
            }
            Lambda(params, retty, body) => {
                write!(f, "fn(")?;
                for param in params {
                    write!(f, "{}: {}, ", param.0, param.1)?;
                }
                write!(f, ") {} {}", retty, body)
            }
            VarDecl(name, ty, expr) => {
                write!(f, "let {}", name)?;
                if let Some(ty) = ty {
                    write!(f, ": {}", ty)?;
                }
                if let Some(expr) = expr {
                    write!(f, " = {}", expr)?;
                }
                Ok(())
            }
            ArrayLiteral(items) => {
                write!(f, "[")?;
                for item in items {
                    write!(f, "{}, ", item)?;
                }
                write!(f, "]")
            }
        }
    }
}

/// "Syntax Type" to distinguish from the system for types internally
#[derive(Debug)]
pub enum SynTy {
    Ident(String),
    Parameterised(String, Vec<PSynTy>),
    Function(Vec<PSynTy>, PSynTy)
}

impl fmt::Display for SynTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SynTy::Ident(s) => write!(f, "{}", s),
            SynTy::Parameterised(name, args) => {
                write!(f, "{}[", name)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, "]")
            },
            SynTy::Function(args, ret_ty) => {
                write!(f, "(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ") {}", ret_ty)
            }
        }
    }
}
