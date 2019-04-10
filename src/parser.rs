use std::mem;

use crate::{ast::*, lexer::*, msg::*, reader::Reader};

pub struct Parser<'a> {
  lexer: Lexer,
  token: Token,
  ast: &'a mut Vec<Box<Expr>>,
}

macro_rules! expr {
  ($e:expr,$pos:expr) => {
    Box::new(Expr {
      pos: $pos,
      decl: $e,
    })
  };
}

type EResult = Result<Box<Expr>, MsgWithPos>;

impl<'a> Parser<'a> {
  pub fn new(reader: Reader, ast: &'a mut Vec<Box<Expr>>) -> Parser<'a> {
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
        self.token.pos,
        Msg::ExpectedToken(kind.name().into(), self.token.kind.name().to_owned()),
      ))
    }
  }

  fn parse_top_level(&mut self) -> Result<(), MsgWithPos> {
    let expr = self.parse_expression()?;
    self.ast.push(expr);
    Ok(())
  }

  fn parse_import(&mut self) -> EResult {
    unimplemented!()
    /*let pos = self.expect_token(TokenKind::Import)?.pos;
    if let ExprDecl::ConstStr(s) = self.lit_str()?.expr {
        return Ok(expr!(ExprDecl::Import(s.clone()), pos));
    } else {
        unreachable!()
    }*/
  }

  fn parse_function(&mut self) -> EResult {
    let pos = self.expect_token(TokenKind::Function)?.pos;

    //self.expect_identifier()?;
    let rec = if self.token.is(TokenKind::Rec) {
      self.advance_token()?;
      true
    } else {
      false
    };
    let name = if let TokenKind::Const(Const::Ident(s)) = &self.token.kind {
      Some(s.to_owned())
    } else {
      None
    };

    if name.is_some() {
      self.advance_token()?;
    };

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

    let body = self.parse_expression()?;
    Ok(expr!(ExprDecl::Function(rec, name, params, body), pos))
  }

  fn parse_let(&mut self) -> EResult {
    let pos = self.advance_token()?.pos;
    let ident = self.expect_identifier()?;
    let expr = if self.token.is(TokenKind::Eq) {
      self.expect_token(TokenKind::Eq)?;
      let expr = self.parse_expression()?;
      Some(expr)
    } else {
      None
    };
    Ok(expr!(ExprDecl::Var(true, ident, expr), pos))
  }

  fn parse_return(&mut self) -> EResult {
    let pos = self.advance_token()?.pos;
    let e = self.parse_expression()?;

    Ok(expr!(ExprDecl::Return(Some(e)), pos))
  }

  fn parse_expression(&mut self) -> EResult {
    match self.token.kind {
      TokenKind::Function => self.parse_function(),

      TokenKind::Match => self.parse_match(),
      TokenKind::Var => self.parse_let(),
      TokenKind::Return => self.parse_return(),
      TokenKind::LBrace => self.parse_block(),
      TokenKind::If => self.parse_if(),
      TokenKind::While => self.parse_while(),
      TokenKind::Break => self.parse_break(),
      TokenKind::Continue => self.parse_continue(),

      _ => self.parse_binary(0),
    }
  }

  fn parse_break(&mut self) -> EResult {
    let pos = self.expect_token(TokenKind::Break)?.pos;
    let expr = if self.token.is(TokenKind::LParen) {
      self.advance_token()?;
      let e = Some(self.parse_expression()?);
      self.expect_token(TokenKind::RParen)?;
      e
    } else {
      None
    };
    Ok(expr!(ExprDecl::Break(expr), pos))
  }
  fn parse_continue(&mut self) -> EResult {
    let pos = self.expect_token(TokenKind::Continue)?.pos;
    Ok(expr!(ExprDecl::Continue, pos))
  }

  fn parse_while(&mut self) -> EResult {
    let pos = self.expect_token(TokenKind::While)?.pos;
    let cond = self.parse_expression()?;
    let block = self.parse_block()?;
    Ok(expr!(ExprDecl::While(cond, block), pos))
  }

  fn parse_match(&mut self) -> EResult {
    let pos = self.expect_token(TokenKind::Match)?.pos;
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
    let pos = self.expect_token(TokenKind::If)?.pos;
    let cond = self.parse_expression()?;
    self.expect_token(TokenKind::Then)?;
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
    let pos = self.expect_token(TokenKind::LBrace)?.pos;
    let mut exprs = vec![];
    while !self.token.is(TokenKind::RBrace) && !self.token.is_eof() {
      let expr = self.parse_expression()?;
      exprs.push(expr);
    }
    self.expect_token(TokenKind::RBrace)?;
    Ok(expr!(ExprDecl::Block(exprs), pos))
  }

  fn create_binary(&mut self, tok: Token, left: Box<Expr>, right: Box<Expr>) -> Box<Expr> {
    let op = match tok.kind {
      TokenKind::Eq => return expr!(ExprDecl::Assign(left, right), tok.pos),
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
      TokenKind::GtGtGt => ">>>",
      TokenKind::GtGt => ">>",
      TokenKind::Mod => "%",
      _ => unimplemented!(),
    };

    expr!(ExprDecl::Binop(op.to_owned(), left, right), tok.pos)
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
        TokenKind::LtLt | TokenKind::GtGt | TokenKind::GtGtGt | TokenKind::Add | TokenKind::Sub => {
          8
        }
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
        Ok(expr!(ExprDecl::Unop(op, expr), tok.pos))
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
          expr!(ExprDecl::Field(left, ident), tok.pos)
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
          expr!(ExprDecl::Array(vals), tok.pos)*/
          } else {
            self.expect_token(TokenKind::RBracket)?;
            expr!(ExprDecl::Array(left, val_or_index), tok.pos)
          }
        }
        _ => {
          if self.token.is(TokenKind::LParen) {
            let expr = left;

            self.expect_token(TokenKind::LParen)?;

            let args = self.parse_comma_list(TokenKind::RParen, |p| p.parse_expression())?;

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

    if let TokenKind::Const(Const::Ident(ref value)) = tok.kind {
      Ok(value.to_owned())
    } else {
      Err(MsgWithPos::new(
        self.lexer.path(),
        tok.pos,
        Msg::ExpectedIdentifier(tok.kind.name().to_owned()),
      ))
    }
  }

  fn parse_comma_list<F, R>(&mut self, stop: TokenKind, mut parse: F) -> Result<Vec<R>, MsgWithPos>
  where
    F: FnMut(&mut Parser) -> Result<R, MsgWithPos>,
  {
    let mut data = vec![];
    let mut comma = true;

    while !self.token.is(stop.clone()) && !self.token.is_eof() {
      if !comma {
        return Err(MsgWithPos::new(
          self.lexer.path(),
          self.token.pos,
          Msg::ExpectedToken(
            TokenKind::Comma.name().into(),
            self.token.kind.name().to_owned(),
          ),
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
    Ok(expr!(ExprDecl::Lambda(params, block), tok.pos))*/
  }

  pub fn parse_factor(&mut self) -> EResult {
    let expr = match self.token.kind {
      TokenKind::Function => self.parse_function(),

      TokenKind::LParen => self.parse_parentheses(),
      TokenKind::Const(Const::Char(_)) => self.lit_char(),
      TokenKind::Const(Const::Int(_)) => self.lit_int(),
      TokenKind::Const(Const::Float(_)) => self.lit_float(),
      TokenKind::Const(Const::Str(_)) => self.lit_str(),
      TokenKind::Const(Const::Ident(_)) => self.ident(),

      TokenKind::BitOr | TokenKind::Or => self.parse_lambda(),
      TokenKind::Const(Const::Bool(_)) => self.parse_bool_literal(),

      _ => Err(MsgWithPos::new(
        self.lexer.path(),
        self.token.pos,
        Msg::ExpectedFactor(self.token.kind.name().clone().to_owned()),
      )),
    };

    expr
  }

  fn parse_parentheses(&mut self) -> EResult {
    let pos = self.advance_token()?.pos;
    let expr = self.parse_expression()?;
    if self.token.is(TokenKind::Comma) {
      self.advance_token()?;
      let mut elems = vec![expr];
      while self.token.kind != TokenKind::RParen && !self.token.is_eof() {
        elems.push(self.parse_expression()?);
      }
      return Ok(expr!(ExprDecl::Tuple(elems), pos));
    }
    self.expect_token(TokenKind::RParen)?;
    Ok(expr!(ExprDecl::Paren(expr), pos))
  }

  fn parse_bool_literal(&mut self) -> EResult {
    let tok = self.advance_token()?;
    let value = tok.is(TokenKind::Const(Const::Bool(true)));
    if value {
      Ok(expr!(ExprDecl::Const(Const::Bool(true)), tok.pos))
    } else {
      Ok(expr!(ExprDecl::Const(Const::Bool(false)), tok.pos))
    }
  }

  fn lit_int(&mut self) -> EResult {
    let tok = self.advance_token()?;
    let pos = tok.pos;
    if let TokenKind::Const(Const::Int(i)) = tok.kind {
      Ok(expr!(ExprDecl::Const(Const::Int(i)), pos))
    } else {
      unreachable!()
    }
  }

  fn lit_char(&mut self) -> EResult {
    let tok = self.advance_token()?;
    let pos = tok.pos;
    if let TokenKind::Const(Const::Char(c)) = tok.kind {
      Ok(expr!(ExprDecl::Const(Const::Char(c)), pos))
    } else {
      unreachable!()
    }
  }

  fn lit_float(&mut self) -> EResult {
    let tok = self.advance_token()?;
    let pos = tok.pos;
    if let TokenKind::Const(Const::Float(f)) = tok.kind {
      Ok(expr!(ExprDecl::Const(Const::Float(f)), pos))
    } else {
      unreachable!()
    }
  }

  fn lit_str(&mut self) -> EResult {
    let tok = self.advance_token()?;
    let pos = tok.pos;
    if let TokenKind::Const(Const::Str(s)) = tok.kind {
      Ok(expr!(ExprDecl::Const(Const::Str(s)), pos))
    } else {
      unreachable!()
    }
  }

  fn ident(&mut self) -> EResult {
    let pos = self.token.pos;
    let ident = self.expect_identifier()?;

    Ok(expr!(ExprDecl::Const(Const::Ident(ident)), pos))
  }
}
