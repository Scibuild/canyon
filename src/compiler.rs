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

    pub fn compile_lval(&mut self, expr: &ast::PExpr) -> String {
        use ast::Expr_::*;
        match &expr.node {
            Ident(name) => {
                format!("{}", Sym::User(name))
            }
            Subscript(array, index) => {
                let array_res = self.compile_expr(array);
                let index_res = self.compile_expr(index);
                format!("{}[1+({})]", array_res, index_res)
            }
            Selector(subexpr, field) => {
                let subexpr_res = self.compile_expr(subexpr);
                format!("{}.{}", subexpr_res, field)
            }
            _ => unreachable!()
        }
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
                let sym = Sym::User(name);
                if let Some(expr) = expr {
                    let e = self.compile_expr(expr);
                    self.store_local(&sym, &e);
                } else {
                    self.declare_local(&sym);
                }
            }
            Block(exprs) => {
                self.declare_local(&result);
                for e in exprs.iter() {
                    if let Some(e) = e {
                        if let FnDecl(name, _) = &e.node {
                            self.declare_local(&Sym::User(&name));
                        }
                    }
                }

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
                let lval_str = self.compile_lval(lval);
                let rval_res = self.compile_expr(rval);
                // TODO: this probably shouldnt be done manually here either
                writeln!(self.output, "{} = {} ;", lval_str, rval_res);
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
                self.store_local(&result, &format!("{}[1+({})]", array_res, index_res))
            }
            Lambda(l) => {
                self.declare_local(&result);
                self.compile_lambda(l, &result);
            }
            Selector(subexpr, field) => {
                let subexpr_res = self.compile_expr(subexpr);
                self.store_local(&result, &format!("{}.{}", subexpr_res, field))
            }
            ArrayLiteral(items) => {
                let item_ress = items.iter().map(|i| self.compile_expr(i)).collect::<Vec<_>>();
                write!(self.output, "local {} = {{", result);
                let mut first = true;
                for item in item_ress {
                    if !first {
                        write!(self.output, ", ");
                    } 
                    first = false;
                    write!(self.output, "{}", item);
                }
                writeln!(self.output, "}};");
            }
            FnDecl(name, l) => {
                // FnDecls can only appear in blocks so the fn has already been declared in the
                // scope, i think this works need to think harder, and make sure that first bit is
                // true in the parser
                self.compile_lambda(l, &Sym::User(&name));
            },
            TypeDecl(_, _) => {},
            Return(expr) => {
                let expr_res = self.compile_expr(expr);
                writeln!(self.output, "return {};", expr_res);
            }

        }
        result
    }

    /// result must be declared before calling
    pub fn compile_lambda<'b>(&mut self, l: &ast::Lambda, result: &Sym) {
        write!(self.output, "{} = function(", result);
        let mut first = true;
        for param in &l.params {
            if !first {
                write!(self.output, ", ");
            } 
            first = false;
            write!(self.output, "{}", Sym::User(&param.0));
        }
        writeln!(self.output, ")");
        let ret_val = self.compile_expr(&l.body);
        writeln!(self.output, "return {}\nend;", ret_val);
    }
}
