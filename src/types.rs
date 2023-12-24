use crate::ast;
use crate::lexer::Span;

#[derive(Debug, PartialEq, Clone)]
pub enum Ty {
    Int,
    Bool,
    Unit,
    Bottom,
    Array(Box<Ty>),
    Fn(Vec<Ty>, Box<Ty>),
}

pub struct Elaborator {
    vars: Vec<(String, Ty)>,
    scopes: Vec<usize>,
}

pub struct TypeError {
    pub span: Span,
    pub tag: TypeError_,
}

#[derive(Debug)]
pub enum TypeError_ {
    UnableToUnify(Ty, Ty),
    CannotSubscriptType(Ty),
    CannotCallType(Ty),
    VariableNotInScope(String),
    TypeNotInScope(String),
    //                  Expected Found
    FunctionExpectsNArgs(usize, usize),
    ParamTyExpectsNArgs(usize, usize),
    InvalidLvalue,
}
use TypeError_::*;

impl TypeError {
    fn mk(span: Span, tag: TypeError_) -> TypeError {
        TypeError {span, tag}
    }
}

impl Elaborator {
    pub fn new() -> Self {
        Elaborator {
            vars: vec![],
            scopes: vec![0],
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(self.vars.len());
    }

    fn pop_scope(&mut self) {
        let newlen = self.scopes.pop().unwrap();
        while newlen < self.vars.len() {
            self.vars.pop();
        }
    }

    fn lookup_var(&self, name: &str) -> Option<Ty> {
        self.vars.iter().rfind(|(s, _)| s == name).map(|x| x.1.clone())
    }

    // For when you want to generate an error if the variable cannot be found
    fn lookup_var_expect(&self, name: &str, span: Span) -> Result<Ty, TypeError> {
        self.lookup_var(name).ok_or_else(|| TypeError::mk(span, VariableNotInScope(name.to_string())))
    }

    fn insert_var(&mut self, name: &str, ty: Ty) {
        self.vars.push((name.to_owned(), ty));
    }

    fn unify(&mut self, ty1: &Ty, ty2: &Ty, span: Span) -> Result<Ty, TypeError> {
        if ty1 == ty2 {
            Ok(ty1.clone())
        } else {
            Err(TypeError::mk(span, UnableToUnify(ty1.clone(), ty2.clone())))
        }

    }

    fn unify_expr<'a>(&mut self, expr: &'a mut ast::PExpr, ty: &Ty) -> Result<&'a Ty, TypeError> {
        if expr.ty.is_none() {
            self.elaborate_expr(expr)?;
        }
        let res_ty = self.unify(expr.ty.as_ref().unwrap(), ty, expr.span)?;
        expr.ty = Some(res_ty);
        Ok(expr.ty.as_ref().unwrap())
    }

    pub fn elaborate_expr<'a>(&mut self, expr: &'a mut ast::PExpr) -> Result<&'a Ty, TypeError> {
        use ast::Expr_::*;
        expr.ty = Some(match &mut expr.node {
            BinOp(lhs, op, rhs) => {
                use ast::BinOp::*;
                match op {
                    Add | Sub | Mul | Div  => {
                        self.unify_expr(lhs, &Ty::Int)?;
                        self.unify_expr(rhs, &Ty::Int)?;
                        Ty::Int
                    },
                    | Le | Lt | Ge | Gt => {
                        self.unify_expr(lhs, &Ty::Int)?;
                        self.unify_expr(rhs, &Ty::Int)?;
                        Ty::Bool
                    },
                    And | Or => {
                        self.unify_expr(lhs, &Ty::Bool)?;
                        self.unify_expr(rhs, &Ty::Bool)?;
                        Ty::Bool
                    },
                    Eqq | Ne => {
                        self.elaborate_expr(lhs)?;
                        self.unify_expr(rhs, lhs.ty.as_ref().unwrap())?;
                        Ty::Bool
                    }
                }
            },
            UnOp(op, rhs) => {
                use ast::UnOp::*;
                match op {
                    Neg => {
                        self.unify_expr(rhs, &Ty::Int)?;
                        Ty::Int
                    }
                    Not => {
                        self.unify_expr(rhs, &Ty::Bool)?;
                        Ty::Bool
                    }
                }
            },
            Int(_) => Ty::Int,
            Bool(_) => Ty::Bool,
            Ident(name) => self.lookup_var_expect(name, expr.span)?,
            VarDecl(name, optty, expr) => {

                let ty = if let Some(expr) = expr {
                    if let Some(ty) = optty {
                        self.unify_expr(expr, &self.elab_ty(ty)?)?.clone()
                    } else {
                        self.elaborate_expr(expr)?.clone()
                    }
                } else if let Some(ty) = optty {
                        self.elab_ty(ty)?
                } else {
                        unreachable!();
                };

                self.insert_var(name, ty);

                Ty::Unit
            }
            Block(exprs) => {
                self.push_scope();
                let mut res = Ty::Unit;
                for expr in exprs {
                    res = if let Some(expr) = expr {
                        self.elaborate_expr(expr)?.clone()
                    } else { 
                        Ty::Unit
                    };
                }
                self.pop_scope();
                res
            }
            If(cond, block, else_block) => {
                self.unify_expr(cond, &Ty::Bool)?;
                if let Some(else_block) = else_block {
                    let resty = self.elaborate_expr(block)?;
                    self.unify_expr(else_block, resty)?;
                    resty.clone()
                } else {
                    self.unify_expr(block, &Ty::Unit)?.clone()
                }
            }
            While(cond, block) => {
                self.unify_expr(cond, &Ty::Bool)?;
                self.unify_expr(block, &Ty::Unit)?.clone()
            }
            Assign(lval, rval) => {
                let lval_ty = self.elab_lvalue(lval)?;
                self.unify_expr(rval, lval_ty)?;
                Ty::Unit
            }
            FnCall(fun, args) => {
                let fun_ty = self.elaborate_expr(fun)?;
                let (fun_args_ty, fun_ret_ty) = match fun_ty {
                    Ty::Fn(a, b) => (a,b),
                    ty =>  return Err(TypeError::mk(expr.span, 
                            CannotCallType(ty.clone()))),
                };
                if fun_args_ty.len() != args.len() {
                    return Err(TypeError::mk(expr.span, FunctionExpectsNArgs(fun_args_ty.len(), args.len())));
                }
                for (fun_arg, arg) in std::iter::zip(fun_args_ty.iter(), 
                        args.iter_mut()) {
                    self.unify_expr(arg, fun_arg)?;
                }

                *fun_ret_ty.to_owned()
            }
            Subscript(array, index) => self.elab_subscript(expr.span, array, index)?,
            Lambda(args, ret_ty, block) => {
                self.push_scope();
                let mut args_elabbed: Vec<Ty> = vec![];
                for (arg_name, arg_ty) in args.iter() {
                    let arg_ty_elabbed = self.elab_ty(arg_ty)?;
                    args_elabbed.push(arg_ty_elabbed.clone());
                    self.insert_var(arg_name, arg_ty_elabbed);
                }
                let ret_ty_elabbed = self.elab_ty(ret_ty)?;
                self.unify_expr(block, &ret_ty_elabbed)?;
                self.pop_scope();
                Ty::Fn(args_elabbed, Box::new(ret_ty_elabbed))
            }
            Selector(_expr, _field) => {
                unimplemented!()
            }
            ArrayLiteral(items) => {
                if let Some((head, tail)) = items.split_first_mut() {
                    let item_ty = self.elaborate_expr(head)?;
                    for item in tail {
                        self.unify_expr(item, item_ty)?;
                    }

                    Ty::Array(Box::new(item_ty.clone()))
                } else {
                    Ty::Array(Box::new(Ty::Bottom))
                }
            }
        });
        Ok(expr.ty.as_ref().unwrap())
    }

    fn elab_subscript(&mut self, span: Span, array: &mut ast::PExpr, index: &mut ast::PExpr) -> Result<Ty, TypeError> {
                self.unify_expr(index, &Ty::Int)?;
                let item_ty = match self.elaborate_expr(array)? {
                    Ty::Array(item) => item,
                    ty => return Err(TypeError::mk(span, 
                            CannotSubscriptType(ty.clone())))
                };
                Ok(*item_ty.clone())
    }

    fn elab_lvalue<'a>(&mut self, expr: &'a mut ast::PExpr) -> Result<&'a Ty, TypeError> {
        use ast::Expr_::*;

        expr.ty = Some(match &mut expr.node {
            Ident(name) => self.lookup_var_expect(name, expr.span)?,
            Subscript(array, index) => self.elab_subscript(expr.span, array, index)?,
            _ => return Err(TypeError::mk(expr.span, InvalidLvalue)),
        });
        Ok(expr.ty.as_ref().unwrap())
    }

    fn elab_ty(&self, synty: &ast::PSynTy) -> Result<Ty, TypeError> {
        use ast::SynTy::*;
        match &synty.node {
            Ident(s) if s == "int" => Ok(Ty::Int),
            Ident(s) if s == "bool" => Ok(Ty::Bool),
            Ident(s) if s == "unit" => Ok(Ty::Unit),
            Parameterised(s, args) if s == "arr" => if args.len() == 1 {
                Ok(Ty::Array(Box::new(self.elab_ty(&args[0])?)))
            } else {
                Err(TypeError::mk(synty.span, ParamTyExpectsNArgs(1, args.len())))
            }
            Function(args, ret_ty) => Ok(Ty::Fn(
                    args.iter().map(|t| self.elab_ty(t)).collect::<Result<Vec<_>, _>>()?, 
                    Box::new(self.elab_ty(ret_ty)?))),
            Ident(s) => Err(TypeError::mk(synty.span, TypeNotInScope(s.to_string()))),
            Parameterised(s, _) => Err(TypeError::mk(synty.span, TypeNotInScope(s.to_string()))),
        }

    }

}
