use std::mem;

use crate::{ast::*, lexer::*, msg::*, reader::Reader, token::*};

pub struct Parser<'a> {
    lexer: Lexer,
    token: Token,
    ast: &'a mut Vec<P<Expr>>,
}
use crate::P;

macro_rules! expr {
    ($e:expr,$pos:expr) => {
        P(Expr {
            pos: $pos,
            decl: $e,
        })
    };
}

type EResult = Result<P<Expr>, MsgWithPos>;

impl<'a> Parser<'a> {
    pub fn new(reader: Reader, ast: &'a mut Vec<P<Expr>>) -> Parser<'a> {
        Self {
            lexer: Lexer::new(reader),
            token: Token::new(TokenKind::End, Position::new(1, 1)),
            ast,
        }
    }

    fn init(&mut self) -> Result<(), MsgWithPos> {
        self.advance_token()?;

        Ok(())
    }

    pub fn parse(&mut self) -> Result<(), MsgWithPos> {
        self.init()?;
        while !self.token.is_eof() {
            self.parse_top_level()?;
        }
        Ok(())
    }

    fn expect_token(&mut self, kind: TokenKind) -> Result<Token, MsgWithPos> {
        if self.token.kind == kind {
            let token = self.advance_token()?;

            Ok(token)
        } else {
            Err(MsgWithPos::new(
                self.lexer.path(),
                self.token.position,
                Msg::ExpectedToken(kind.name().into(), self.token.name()),
            ))
        }
    }

    fn parse_top_level(&mut self) -> Result<(), MsgWithPos> {
        let expr = self.parse_expression()?;
        self.ast.push(expr);
        Ok(())
    }

    fn parse_function_param(&mut self) -> Result<String, MsgWithPos> {
        let name = self.expect_identifier()?;
        Ok(name)
    }

    fn parse_import(&mut self) -> EResult {
        unimplemented!()
        /*let pos = self.expect_token(TokenKind::Import)?.position;
        if let ExprDecl::ConstStr(s) = self.lit_str()?.expr {
            return Ok(expr!(ExprDecl::Import(s.clone()), pos));
        } else {
            unreachable!()
        }*/
    }

    fn parse_include(&mut self) -> EResult {
        let _pos = self.expect_token(TokenKind::Include)?.position;
        if let ExprDecl::Const(Constant::Str(_s)) = &self.lit_str()?.decl {
            unimplemented!()
        //return Ok(expr!(ExprDecl::Include(s.clone()), pos));
        } else {
            unreachable!()
        }
    }

    fn parse_function(&mut self) -> EResult {
        let pos = self.expect_token(TokenKind::Fun)?.position;

        //self.expect_identifier()?;
        self.expect_token(TokenKind::LParen)?;
        let params = if self.token.kind == TokenKind::RParen {
            vec![]
        } else {
            let mut tmp = vec![];
            while !self.token.is(TokenKind::RParen) {
                tmp.push(self.expect_identifier()?);
                if !self.token.is(TokenKind::RParen) {
                    self.expect_token(TokenKind::Comma)?;
                }
            }
            tmp
        };
        self.expect_token(TokenKind::RParen)?;
        let block = self.parse_expression()?;
        Ok(expr!(ExprDecl::Function(params, block), pos))
    }

    fn parse_let(&mut self) -> EResult {
        let reassignable = self.token.is(TokenKind::Var);

        let pos = self.advance_token()?.position;
        let ident = self.expect_identifier()?;
        let expr = if self.token.is(TokenKind::Eq) {
            self.expect_token(TokenKind::Eq)?;
            let expr = self.parse_expression()?;
            Some(expr)
        } else {
            None
        };
        Ok(expr!(ExprDecl::Var(reassignable, ident, expr), pos))
    }

    fn parse_return(&mut self) -> EResult {
        let pos = self.expect_token(TokenKind::Return)?.position;
        let expr = self.parse_expression()?;
        Ok(expr!(ExprDecl::Return(Some(expr)), pos))
    }

    fn parse_expression(&mut self) -> EResult {
        match self.token.kind {
            TokenKind::Fun => self.parse_function(),

            TokenKind::Match => self.parse_match(),
            TokenKind::Let | TokenKind::Var => self.parse_let(),
            TokenKind::LBrace => self.parse_block(),
            TokenKind::If => self.parse_if(),
            TokenKind::For => self.parse_for(),
            TokenKind::While => self.parse_while(),
            TokenKind::Break => self.parse_break(),
            TokenKind::Continue => self.parse_continue(),
            TokenKind::Return => self.parse_return(),
            TokenKind::Throw => self.parse_throw(),
            TokenKind::Import => self.parse_import(),
            _ => self.parse_binary(0),
        }
    }

    fn parse_self(&mut self) -> EResult {
        let pos = self.expect_token(TokenKind::This)?.position;
        Ok(expr!(ExprDecl::Const(Constant::This), pos))
    }

    fn parse_break(&mut self) -> EResult {
        let pos = self.expect_token(TokenKind::Break)?.position;
        let expr = if self.token.is(TokenKind::LParen) {
            Some(self.parse_expression()?)
        } else {
            None
        };
        Ok(expr!(ExprDecl::Break(expr), pos))
    }
    fn parse_continue(&mut self) -> EResult {
        let pos = self.expect_token(TokenKind::Continue)?.position;
        Ok(expr!(ExprDecl::Continue, pos))
    }

    fn parse_throw(&mut self) -> EResult {
        let pos = self.token.position;
        if let TokenKind::String(s) = self.token.clone().kind {
            self.advance_token()?;
            return Ok(expr!(ExprDecl::Throw(s.clone()), pos));
        } else {
            panic!("String expected at {}", pos)
        }
    }

    fn parse_for(&mut self) -> EResult {
        let pos = self.expect_token(TokenKind::For)?.position;

        let decl = self.parse_expression()?;
        if self.token.is(TokenKind::In) {
            self.advance_token()?;
            let in_ = self.parse_expression()?;
            let block = self.parse_expression()?;
            let name = self.expect_identifier()?;
            Ok(expr!(ExprDecl::ForIn(name, in_, block), pos))
        } else {
            self.expect_token(TokenKind::Semicolon)?;

            let cond = self.parse_expression()?;
            self.expect_token(TokenKind::Semicolon)?;
            let then = self.parse_expression()?;

            let block = self.parse_expression()?;
            Ok(expr!(ExprDecl::For(decl, cond, then, block), pos))
        }
    }

    fn parse_while(&mut self) -> EResult {
        let pos = self.expect_token(TokenKind::While)?.position;
        let cond = self.parse_expression()?;
        let block = self.parse_block()?;
        Ok(expr!(ExprDecl::While(cond, block), pos))
    }

    fn parse_match(&mut self) -> EResult {
        let pos = self.expect_token(TokenKind::Match)?.position;
        let value = self.parse_expression()?;
        self.expect_token(TokenKind::LBrace)?;
        let mut data = vec![];
        let mut or = None;
        while !self.token.is(TokenKind::RBrace) && !self.token.is_eof() {
            if self.token.is(TokenKind::Underscore) {
                self.expect_token(TokenKind::Underscore)?;
                self.expect_token(TokenKind::Arrow)?;
                let expr = self.parse_expression()?;
                or = Some(expr);
                continue;
            }
            let cond = self.parse_expression()?;
            self.expect_token(TokenKind::Arrow)?;
            let expr = self.parse_expression()?;
            data.push((cond, expr));
        }

        self.expect_token(TokenKind::RBrace)?;

        Ok(expr!(ExprDecl::Switch(value, data, or), pos))
    }

    fn parse_if(&mut self) -> EResult {
        let pos = self.expect_token(TokenKind::If)?.position;
        let cond = self.parse_expression()?;
        let then_block = self.parse_expression()?;
        let else_block = if self.token.is(TokenKind::Else) {
            self.advance_token()?;

            if self.token.is(TokenKind::If) {
                let if_block = self.parse_if()?;
                let block = expr!(ExprDecl::Block(vec![if_block]), if_block.pos);

                Some(block)
            } else {
                Some(self.parse_expression()?)
            }
        } else {
            None
        };

        Ok(expr!(ExprDecl::If(cond, then_block, else_block), pos))
    }

    fn parse_block(&mut self) -> EResult {
        let pos = self.expect_token(TokenKind::LBrace)?.position;
        let mut exprs = vec![];
        while !self.token.is(TokenKind::RBrace) && !self.token.is_eof() {
            let expr = self.parse_expression()?;
            exprs.push(expr);
        }
        self.expect_token(TokenKind::RBrace)?;
        Ok(expr!(ExprDecl::Block(exprs), pos))
    }

    fn create_binary(&mut self, tok: Token, left: P<Expr>, right: P<Expr>) -> P<Expr> {
        let op = match tok.kind {
            TokenKind::Eq => return expr!(ExprDecl::Assign(left, right), tok.position),
            TokenKind::Or => "||",
            TokenKind::And => "&&",
            TokenKind::BitOr => "|",
            TokenKind::BitAnd => "&",
            TokenKind::EqEq => "==",
            TokenKind::Ne => "!=",
            TokenKind::Lt => "<",
            TokenKind::Gt => ">",
            TokenKind::Le => "<=",
            TokenKind::Ge => ">=",
            TokenKind::Caret => "^",
            TokenKind::Add => "+",
            TokenKind::Sub => "-",
            TokenKind::Mul => "*",
            TokenKind::Div => "/",
            TokenKind::LtLt => "<<",
            TokenKind::GtGt => ">>",
            TokenKind::Mod => "%",
            _ => unimplemented!(),
        };

        expr!(ExprDecl::Binop(op.to_owned(), left, right), tok.position)
    }

    fn parse_binary(&mut self, precedence: u32) -> EResult {
        let mut left = self.parse_unary()?;
        loop {
            let right_precedence = match self.token.kind {
                TokenKind::Or => 1,
                TokenKind::And => 2,
                TokenKind::Eq => 3,
                TokenKind::EqEq
                | TokenKind::Ne
                | TokenKind::Lt
                | TokenKind::Le
                | TokenKind::Gt
                | TokenKind::Ge => 4,
                TokenKind::BitOr | TokenKind::BitAnd | TokenKind::Caret => 6,
                TokenKind::LtLt | TokenKind::GtGt | TokenKind::Add | TokenKind::Sub => 8,
                TokenKind::Mul | TokenKind::Div | TokenKind::Mod => 9,
                _ => {
                    return Ok(left);
                }
            };
            if precedence >= right_precedence {
                return Ok(left);
            }

            let tok = self.advance_token()?;
            left = {
                let right = self.parse_binary(right_precedence)?;
                self.create_binary(tok, left, right)
            };
        }
    }

    pub fn parse_unary(&mut self) -> EResult {
        match self.token.kind {
            TokenKind::Add | TokenKind::Sub | TokenKind::Not => {
                let tok = self.advance_token()?;
                let op = match tok.kind {
                    TokenKind::Add => String::from("+"),
                    TokenKind::Sub => String::from("-"),
                    TokenKind::Not => String::from("!"),
                    _ => unreachable!(),
                };
                let expr = self.parse_primary()?;
                Ok(expr!(ExprDecl::Unop(op, expr), tok.position))
            }
            _ => self.parse_primary(),
        }
    }

    /*pub fn parse_expression(&mut self) -> EResult {
        self.parse_binary(0)
    }*/

    /*fn parse_call(&mut self) -> EResult
    {
        let expr = self.parse_expression()?;
        self.expect_token(TokenKind::LParen)?;
        let args = self.parse_comma_list(TokenKind::RParen, |p| p.parse_expression())?;
        Ok(expr!(ExprDecl::Call(expr, args), expr.pos))
    }*/

    pub fn parse_primary(&mut self) -> EResult {
        let mut left = self.parse_factor()?;
        loop {
            left = match self.token.kind {
                TokenKind::Dot => {
                    let tok = self.advance_token()?;
                    let ident = self.expect_identifier()?;
                    expr!(ExprDecl::Field(left, ident), tok.position)
                }

                TokenKind::LBracket => {
                    let tok = self.advance_token()?;
                    let val_or_index = self.parse_expression()?;
                    if self.token.is(TokenKind::Comma) {
                        unimplemented!()
                    /*self.advance_token()?;
                    let mut vals = vec![val_or_index];

                    while !self.token.is(TokenKind::RBracket) {
                        vals.push(self.parse_expression()?);
                    }
                    self.expect_token(TokenKind::RBracket)?;
                    expr!(ExprDecl::Array(vals), tok.position)*/
                    } else {
                        self.expect_token(TokenKind::RBracket)?;
                        expr!(ExprDecl::Array(left, val_or_index), tok.position)
                    }
                }
                _ => {
                    if self.token.is(TokenKind::LParen) {
                        let expr = left;

                        self.expect_token(TokenKind::LParen)?;

                        let args =
                            self.parse_comma_list(TokenKind::RParen, |p| p.parse_expression())?;

                        expr!(ExprDecl::Call(expr, args), expr.pos)
                    } else {
                        return Ok(left);
                    }
                }
            }
        }
    }

    fn expect_identifier(&mut self) -> Result<String, MsgWithPos> {
        let tok = self.advance_token()?;

        if let TokenKind::Identifier(ref value) = tok.kind {
            Ok(value.to_owned())
        } else {
            Err(MsgWithPos::new(
                self.lexer.path(),
                tok.position,
                Msg::ExpectedIdentifier(tok.name()),
            ))
        }
    }

    fn parse_comma_list<F, R>(
        &mut self,
        stop: TokenKind,
        mut parse: F,
    ) -> Result<Vec<R>, MsgWithPos>
    where
        F: FnMut(&mut Parser) -> Result<R, MsgWithPos>,
    {
        let mut data = vec![];
        let mut comma = true;

        while !self.token.is(stop.clone()) && !self.token.is_eof() {
            if !comma {
                return Err(MsgWithPos::new(
                    self.lexer.path(),
                    self.token.position,
                    Msg::ExpectedToken(TokenKind::Comma.name().into(), self.token.name()),
                ));
            }

            let entry = parse(self)?;
            data.push(entry);

            comma = self.token.is(TokenKind::Comma);
            if comma {
                self.advance_token()?;
            }
        }

        self.expect_token(stop)?;

        Ok(data)
    }

    fn advance_token(&mut self) -> Result<Token, MsgWithPos> {
        let tok = self.lexer.read_token()?;

        Ok(mem::replace(&mut self.token, tok))
    }

    fn parse_lambda(&mut self) -> EResult {
        unimplemented!()
        /*let tok = self.advance_token()?;
        let params = if tok.kind == TokenKind::Or {
            vec![]
        } else {
            self.parse_comma_list(TokenKind::BitOr, |f| f.parse_function_param())?
        };

        let block = self.parse_expression()?;
        Ok(expr!(ExprDecl::Lambda(params, block), tok.position))*/
    }

    pub fn parse_factor(&mut self) -> EResult {
        let expr = match self.token.kind {
            TokenKind::Fun => self.parse_function(),

            TokenKind::LParen => self.parse_parentheses(),
            TokenKind::LitChar(_) => self.lit_char(),
            TokenKind::LitInt(_, _, _) => self.lit_int(),
            TokenKind::LitFloat(_) => self.lit_float(),
            TokenKind::String(_) => self.lit_str(),
            TokenKind::Builtin(_) => self.parse_builtin(),
            TokenKind::Identifier(_) => self.ident(),
            TokenKind::This => self.parse_self(),
            TokenKind::BitOr | TokenKind::Or => self.parse_lambda(),
            TokenKind::True => self.parse_bool_literal(),
            TokenKind::False => self.parse_bool_literal(),
            TokenKind::Nil => self.parse_nil(),

            _ => Err(MsgWithPos::new(
                self.lexer.path(),
                self.token.position,
                Msg::ExpectedFactor(self.token.name().clone()),
            )),
        };

        expr
    }

    fn parse_builtin(&mut self) -> EResult {
        let b = if let TokenKind::Builtin(b) = &self.token.kind {
            b.clone()
        } else {
            unreachable!()
        };
        let pos = self.advance_token()?.position;

        Ok(expr!(ExprDecl::Const(Constant::Builtin(b.clone())), pos))
    }

    fn parse_parentheses(&mut self) -> EResult {
        let pos = self.advance_token()?.position;
        let expr = self.parse_expression()?;
        self.expect_token(TokenKind::RParen)?;
        Ok(expr!(ExprDecl::Paren(expr), pos))
    }

    fn parse_nil(&mut self) -> EResult {
        let tok = self.advance_token()?;
        let pos = tok.position;
        if let TokenKind::Nil = tok.kind {
            Ok(expr!(ExprDecl::Const(Constant::Null), pos))
        } else {
            unreachable!()
        }
    }

    fn parse_bool_literal(&mut self) -> EResult {
        let tok = self.advance_token()?;
        let value = tok.is(TokenKind::True);
        if value {
            Ok(expr!(ExprDecl::Const(Constant::True), tok.position))
        } else {
            Ok(expr!(ExprDecl::Const(Constant::False), tok.position))
        }
    }

    fn lit_int(&mut self) -> EResult {
        let tok = self.advance_token()?;
        let pos = tok.position;
        if let TokenKind::LitInt(i, _, _) = tok.kind {
            Ok(expr!(
                ExprDecl::Const(Constant::Int(i.parse().unwrap())),
                pos
            ))
        } else {
            unreachable!()
        }
    }

    fn lit_char(&mut self) -> EResult {
        let tok = self.advance_token()?;
        let _pos = tok.position;
        if let TokenKind::LitChar(_c) = tok.kind {
            unimplemented!()
        } else {
            unreachable!()
        }
    }

    fn lit_float(&mut self) -> EResult {
        let tok = self.advance_token()?;
        let pos = tok.position;
        if let TokenKind::LitFloat(c) = tok.kind {
            Ok(expr!(
                ExprDecl::Const(Constant::Float(c.parse().unwrap())),
                pos
            ))
        } else {
            unreachable!()
        }
    }

    fn lit_str(&mut self) -> EResult {
        let tok = self.advance_token()?;
        let pos = tok.position;
        if let TokenKind::String(s) = tok.kind {
            Ok(expr!(ExprDecl::Const(Constant::Str(s)), pos))
        } else {
            unreachable!()
        }
    }

    fn ident(&mut self) -> EResult {
        let pos = self.token.position;
        let ident = self.expect_identifier()?;

        Ok(expr!(ExprDecl::Const(Constant::Ident(ident)), pos))
    }
}
