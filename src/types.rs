use crate::ast;
use crate::lexer::Span;

#[derive(Debug, PartialEq, Clone)]
pub enum Ty {
    Int,
    Bool,
    Unit,
    // The type of expressions that do not return
    Bottom,
    Array(Box<Ty>),
    Fn(Vec<Ty>, Box<Ty>),
    Record(Vec<(String, Ty)>),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Var {
    Type, Const, Variable
}

pub struct Elaborator {
    vars: Vec<(String, Ty, Var)>,
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
    CannotSelectFieldFromType(String, Ty),
    VariableNotInScope(String),
    TypeNotInScope(String),
    //                  Expected Found
    FunctionExpectsNArgs(usize, usize),
    ParamTyExpectsNArgs(usize, usize),
    InvalidLvalue,
    CannotAssignToConstant
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

    fn lookup_var(&self, name: &str) -> Option<(Ty, Var)> {
        self.vars.iter().rfind(|(s, _, _)| s == name).map(|x| (x.1.clone(), x.2))
    }

    // For when you want to generate an error if the variable cannot be found
    fn lookup_var_expect(&self, name: &str, span: Span) -> Result<(Ty, Var), TypeError> {
        self.lookup_var(name).ok_or_else(|| TypeError::mk(span, VariableNotInScope(name.to_string())))
    }

    fn insert_var(&mut self, name: &str, ty: Ty, var: Var) {
        self.vars.push((name.to_owned(), ty, var));
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
            Ident(name) => self.lookup_var_expect(name, expr.span)?.0,
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

                self.insert_var(name, ty, Var::Variable);

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
            Lambda(l) => {
                self.push_scope();
                let mut args_elabbed: Vec<Ty> = vec![];
                for (arg_name, arg_ty) in l.params.iter() {
                    let arg_ty_elabbed = self.elab_ty(arg_ty)?;
                    args_elabbed.push(arg_ty_elabbed.clone());
                    self.insert_var(arg_name, arg_ty_elabbed, Var::Const);
                }
                let ret_ty_elabbed = self.elab_ty(&l.ret_ty)?;
                self.unify_expr(&mut l.body, &ret_ty_elabbed)?;
                self.pop_scope();
                Ty::Fn(args_elabbed, Box::new(ret_ty_elabbed))
            }
            Selector(subexpr, field) => self.elab_selector(expr.span, subexpr, field)?,
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

            FnDecl(name, lambda) => {
                // TODO: Mutual recursion properly (probably factor a bunch of this out to happen
                // before everything else)
                let mut args_elabbed: Vec<Ty> = vec![];
                for (arg_name, arg_ty) in &lambda.params {
                    let arg_ty_elabbed = self.elab_ty(&arg_ty)?;
                    args_elabbed.push(arg_ty_elabbed.clone());
                    self.insert_var(&arg_name, arg_ty_elabbed, Var::Const);
                }
                let ret_ty_elabbed = self.elab_ty(&lambda.ret_ty)?;
                let fn_ty = Ty::Fn(args_elabbed.clone(), Box::new(ret_ty_elabbed.clone()));
                self.insert_var(name, fn_ty, Var::Const);

                self.push_scope();
                for ((arg_name, _), arg_ty_elabbed) in std::iter::zip(lambda.params.iter(), args_elabbed.iter()) {
                    self.insert_var(&arg_name, arg_ty_elabbed.clone(), Var::Const);
                }
                self.unify_expr(&mut lambda.body, &ret_ty_elabbed)?;
                self.pop_scope();
                Ty::Unit
            }
            TypeDecl(name, synty) => {
                let ty = self.elab_ty(&synty)?;
                self.insert_var(name, ty, Var::Type);
                Ty::Unit
            }
            // TODO:
            // - Subtyping so that return can return Bottom which can be upcast into any type
            // - Passing through the return type in the context or as a parameter
            Return(_) => todo!(),
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

    fn elab_selector(&mut self, span: Span, subexpr: &mut ast::PExpr, field: &str) -> Result<Ty, TypeError> {
                let ty = self.elaborate_expr(subexpr)?;
                match ty {
                    Ty::Record(fields) => {
                        Ok(fields.iter().find(|(fname, _)| fname == field)
                            .ok_or(TypeError::mk(span, CannotSelectFieldFromType(field.to_string(), ty.clone())))?
                            .1.clone())
                    }
                    ty => return Err(TypeError::mk(span, 
                            CannotSelectFieldFromType(field.to_string(), ty.clone())))
                }
    }


    fn elab_lvalue<'a>(&mut self, expr: &'a mut ast::PExpr) -> Result<&'a Ty, TypeError> {
        use ast::Expr_::*;

        expr.ty = Some(match &mut expr.node {
            Ident(name) => {
                let (ty, var) = self.lookup_var_expect(name, expr.span)?;
                if var == Var::Variable {
                    ty
                } else {
                    return Err(TypeError::mk(expr.span, CannotAssignToConstant));
                }
            }
            Subscript(array, index) => self.elab_subscript(expr.span, array, index)?,
            Selector(subexpr, field) => self.elab_selector(expr.span, subexpr, field)?,
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
            Ident(s) => {
                if let Some((ty, Var::Type)) = self.lookup_var(s) {
                    Ok(ty)
                } else {
                    Err(TypeError::mk(synty.span, TypeNotInScope(s.to_string())))
                }
            }
            Parameterised(s, args) if s == "arr" => if args.len() == 1 {
                Ok(Ty::Array(Box::new(self.elab_ty(&args[0])?)))
            } else {
                Err(TypeError::mk(synty.span, ParamTyExpectsNArgs(1, args.len())))
            }
            Function(args, ret_ty) => Ok(Ty::Fn(
                    args.iter().map(|t| self.elab_ty(t)).collect::<Result<Vec<_>, _>>()?, 
                    Box::new(self.elab_ty(ret_ty)?))),
            Parameterised(s, _) => Err(TypeError::mk(synty.span, TypeNotInScope(s.to_string()))),
            Record(fields) => Ok(Ty::Record(fields.iter().map::<Result<_, _>, _>(
                        |(name, synty)| Ok((name.clone(), self.elab_ty(synty)?))).collect::<Result<Vec<_>,_>>()?))
        }
    }
}
