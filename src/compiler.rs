#![allow(unused_must_use)]

use crate::ast;
use std::io::Write;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Sym<'a> {
    Gen(usize),
    User(&'a str),
}

impl<'a> std::fmt::Display for Sym<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Sym::Gen(i) => write!(f, "gen_{}", i),
            Sym::User(i) => write!(f, "user_{}", i),
        }
    }
}

pub struct Compiler {
    output: Box<dyn Write>,
    sym_int: usize
}

impl Compiler {
    pub fn new(output: Box<dyn Write>) -> Compiler {
        Compiler {
            output,
            sym_int: 0,
        }
    }

    fn gensym<'b>(&mut self) -> Sym<'b> {
        self.sym_int += 1;
        Sym::Gen(self.sym_int - 1)
    }

    fn declare_local(&mut self, sym: &Sym) {
        writeln!(self.output, "local {};", sym);
    }

    /// Write a store to local statement
    fn store_local(&mut self, sym: &Sym, s: &dyn std::fmt::Display) {
        write!(self.output, "local ");
        self.store_existing(sym, s);
    }

    fn store_existing(&mut self, sym: &Sym, s: &'_ dyn std::fmt::Display) {
        writeln!(self.output, "{} = {} ;", sym, s);
    }

    // The result of the expression is stored in the returned symbol
    pub fn compile_expr<'b>(&mut self, expr: &'b ast::PExpr) -> Sym<'b> {
        use ast::Expr_::*;
        use ast::BinOp::*;

        let result = self.gensym();

        match &expr.node {
            BinOp(lhs, And, rhs) => {
                let lhs_sym = self.compile_expr(lhs);
                self.declare_local(&result);

                writeln!(self.output, "if {} then", lhs_sym);
                let rhs_sym = self.compile_expr(rhs);
                self.store_existing(&result, &rhs_sym);

                writeln!(self.output, "else");
                self.store_existing(&result, &lhs_sym);
                writeln!(self.output, "end");
            }
            BinOp(lhs, Or, rhs) => {
                let lhs_sym = self.compile_expr(lhs);
                self.declare_local(&result);

                writeln!(self.output, "if {} then", lhs_sym);
                self.store_existing(&result, &lhs_sym);

                writeln!(self.output, "else");
                let rhs_sym = self.compile_expr(rhs);
                self.store_existing(&result, &rhs_sym);
                writeln!(self.output, "end");
            }
            BinOp(lhs, op, rhs) => {
                let lhs_sym = self.compile_expr(lhs);
                let rhs_sym = self.compile_expr(rhs);
                let op_lua = match op {
                    Add => "+",
                    Sub => "-",
                    Mul => "*",
                    Div => "/",
                    Gt =>  ">",
                    Lt => "<",
                    Ge => ">=",
                    Le => "<=",
                    Ne => "~=",
                    Eqq => "==",
                    And | Or => unreachable!(),
                };
                let res_val = format!("({} {} {})", lhs_sym, op_lua, rhs_sym);
                self.store_local(&result, &res_val);
            },
            UnOp(op, rhs) => {
                let rhs_sym = self.compile_expr(rhs);
                let op_lua = match op {
                    ast::UnOp::Neg => "-",
                    ast::UnOp::Not => "not",
                };
                let res_val = format!("({} {})", op_lua, rhs_sym);
                self.store_local(&result, &res_val);
            },
            Int(s) => { 
                self.store_local(&result, s); 
            }
            Bool(b) => {
                self.store_local(&result, b);
            }
            Ident(name) => {
                return Sym::User(name);
                // self.store_local(&result, &Sym::User(name));
            }
            VarDecl(name, _opt_ty, expr) => {
                if let Some(expr) = expr {
                    let e = self.compile_expr(expr);
                    self.store_local(&Sym::User(name), &e);
                }
            }
            Block(exprs) => {
                self.declare_local(&result);
                writeln!(self.output, "do");
                for (i, e) in exprs.iter().enumerate() {
                    if let Some(e) = e {
                        if i == exprs.len() - 1 {
                            let expr_res = self.compile_expr(e);
                            self.store_existing(&result, &expr_res);
                        } else {
                            self.compile_expr(e);
                        }
                    }
                }
                writeln!(self.output, "end");
            }
            If(cond, block, else_block) => {
                let cond_res = self.compile_expr(cond);
                self.declare_local(&result);

                writeln!(self.output, "if {} then", cond_res);
                let block_res = self.compile_expr(block);
                self.store_existing(&result, &block_res);

                if let Some(else_block) = else_block {
                    writeln!(self.output, "else");
                    let else_res = self.compile_expr(else_block);
                    self.store_existing(&result, &else_res);
                }
                writeln!(self.output, "end");
            }
            While(cond, block) => {
                writeln!(self.output, "while true do");
                let cond_res = self.compile_expr(cond);
                writeln!(self.output, "if not ({}) then break end", cond_res);
                self.compile_expr(block);
                writeln!(self.output, "end");
                self.declare_local(&result);
            }
            Assign(lval, rval) => {
                match &lval.node {
                    Ident(name) => {
                        let e = self.compile_expr(rval);
                        self.store_existing(&Sym::User(name), &e);
                    }
                    _ => unreachable!(),
                }
            }
            FnCall(fun, args) => {
                let fun_res = self.compile_expr(fun);
                let arg_ress = args.iter().map(|arg| self.compile_expr(arg)).collect::<Vec<_>>();
                // TODO: Don't do this manually here
                write!(self.output, "local {} = {}(", result, fun_res);
                let mut first = true;
                for arg in arg_ress {
                    if !first {
                        write!(self.output, ", ");
                    } 
                    first = false;
                    write!(self.output, "{}", arg);
                }
                writeln!(self.output, ");");
            }
            Subscript(array, index) => {
                let array_res = self.compile_expr(array);
                let index_res = self.compile_expr(index);
                // TODO: our own safety checks
                self.store_local(&result, &format!("{}[{}]", array_res, index_res))
            }
            Lambda(args, _ret_ty, block) => {
                write!(self.output, "local {} = function(", result);
                let mut first = true;
                for arg in args {
                    if !first {
                        write!(self.output, ", ");
                    } 
                    first = false;
                    write!(self.output, "{}", Sym::User(&arg.0));
                }
                writeln!(self.output, ")");
                let ret_val = self.compile_expr(block);
                writeln!(self.output, "return {}\nend;", ret_val);
            }
            Selector(_expr, _field) => {
                unimplemented!()
            }
        }
        result
    }
}
