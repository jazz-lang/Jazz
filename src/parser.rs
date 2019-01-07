use crate::ast::*;
use crate::error::*;
use crate::lexer::position::*;
use crate::lexer::reader::Reader;
use crate::lexer::token::*;
use crate::lexer::*;
pub struct Parser<'a> {
    lexer: Lexer,
    ast: &'a mut AST,
    token: Token,
}

pub type ExprResult = Result<Box<Expr>, MsgWithPos>;
pub type StmtResult = Result<Box<Stmt>, MsgWithPos>;

impl<'a> Parser<'a> {
    pub fn new(reader: Reader, ast: &'a mut AST) -> Parser<'a> {
        let token = Token::new(TokenKind::End, Position::new(1, 1));
        let lexer = Lexer::new(reader);
        Parser { lexer, ast, token }
    }

    fn parse_statement(&mut self) -> StmtResult {
        match self.token.kind {
            TokenKind::While => self.parse_while(),
            TokenKind::If => self.parse_if(),
            TokenKind::Var | TokenKind::Let => self.parse_var(),
            TokenKind::Return => self.parse_return(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_block(&mut self) -> StmtResult {
        let pos = self.expect_token(TokenKind::LBrace)?.position;
        let mut stmts = vec![];

        while !self.token.is(TokenKind::RBrace) && !self.token.is_eof() {
            let stmt = self.parse_statement()?;
            stmts.push(stmt);
        }

        self.expect_token(TokenKind::RBrace)?;

        Ok(Box::new(Stmt::new(StmtKind::Block(stmts), pos)))
    }

    fn parse_var(&mut self) -> StmtResult {
        let reassignable = if self.token.is(TokenKind::Let) {
            false
        } else if self.token.is(TokenKind::Var) {
            true
        } else {
            panic!("let or var expected")
        };

        let pos = self.advance_token()?.position;
        let ident = self.expect_identifier()?;
        let data_type = self.parse_var_type()?;
        let expr = self.parse_var_assignment()?;

        self.expect_semicolon()?;

        if reassignable {
            Ok(Box::new(Stmt::new(
                StmtKind::Var(ident, data_type, expr),
                pos,
            )))
        } else {
            Ok(Box::new(Stmt::new(
                StmtKind::Let(ident, data_type, expr),
                pos,
            )))
        }
    }

    fn parse_var_type(&mut self) -> Result<Option<Type>, MsgWithPos> {
        if self.token.is(TokenKind::Colon) {
            self.advance_token()?;

            Ok(Some(self.parse_type()?))
        } else {
            Ok(None)
        }
    }

    fn parse_var_assignment(&mut self) -> Result<Option<Box<Expr>>, MsgWithPos> {
        if self.token.is(TokenKind::Eq) {
            self.expect_token(TokenKind::Eq)?;
            let expr = self.parse_expression()?;

            Ok(Some(expr))
        } else {
            Ok(None)
        }
    }

    fn parse_while(&mut self) -> StmtResult {
        let pos = self.expect_token(TokenKind::While)?.position;

        let mut opts = ExprParsingOpts::new();
        opts.parse_struct_lit(false);
        let expr = self.parse_expression_with_opts(&opts)?;

        let block = self.parse_block()?;

        Ok(Box::new(Stmt::new(StmtKind::While(expr, block), pos)))
    }

    fn parse_return(&mut self) -> StmtResult {
        let pos = self.expect_token(TokenKind::Return)?.position;
        let expr = if self.token.is(TokenKind::Semicolon) {
            None
        } else {
            let expr = self.parse_expression()?;
            Some(expr)
        };

        self.expect_semicolon()?;

        Ok(Box::new(Stmt::new(StmtKind::Return(expr), pos)))
    }

    fn parse_if(&mut self) -> StmtResult {
        let pos = self.expect_token(TokenKind::If)?.position;

        let mut opts = ExprParsingOpts::new();
        opts.parse_struct_lit(false);
        let cond = self.parse_expression_with_opts(&opts)?;

        let then_block = self.parse_block()?;

        let else_block = if self.token.is(TokenKind::Else) {
            self.advance_token()?;

            if self.token.is(TokenKind::If) {
                let if_block = self.parse_if()?;
                let block = Stmt::new(StmtKind::Block(vec![if_block]), pos);

                Some(Box::new(block))
            } else {
                Some(self.parse_block()?)
            }
        } else {
            None
        };

        Ok(Box::new(Stmt::new(
            StmtKind::If(cond, then_block, else_block),
            pos,
        )))
    }
    fn parse_expression_with_opts(&mut self, opts: &ExprParsingOpts) -> ExprResult {
        self.parse_binary(0, opts)
    }

    pub fn parse(&mut self) -> Result<(), MsgWithPos> {
        self.init()?;
        let mut elems = vec![];
        while !self.token.is_eof() {
            self.parse_top_level(&mut elems)?;
        }

        self.ast.extend(elems);
        Ok(())
    }

    pub fn parse_top_level(&mut self, elements: &mut Vec<Elem>) -> Result<(), MsgWithPos> {
        match self.token.kind {
            TokenKind::Fun => {
                let fct = self.parse_function()?;
                elements.push(Elem::Func(fct));
            }
            // TokenKind::Struct => Err(MsgWithPos::new(self.token.position, Msg::Unimplemented)),
            _ => {
                let msg = Msg::ExpectedTopLevelElement(self.token.name());
                return Err(MsgWithPos::new(self.token.position, msg));
            }
        }

        Ok(())
    }

    fn parse_function(&mut self) -> Result<Function, MsgWithPos> {
        let pos = self.expect_token(TokenKind::Fun)?.position;
        let ident = self.expect_identifier()?;
        let params = self.parse_function_params()?;
        let return_ty = self.parse_function_type()?;
        let block = self.parse_function_block()?;

        let is_internal = block.is_some();

        Ok(Function {
            name: ident,
            pos: pos,
            params,
            block,
            return_ty,
            is_pub: true,
            abi: None,
            external: is_internal,
            is_const: false,
            is_static: false,
        })
    }

    fn parse_function_block(&mut self) -> Result<Option<Box<Stmt>>, MsgWithPos> {
        if self.token.is(TokenKind::Semicolon) {
            self.advance_token()?;

            Ok(None)
        } else {
            let block = self.parse_block()?;

            Ok(Some(block))
        }
    }

    fn parse_function_type(&mut self) -> Result<Type, MsgWithPos> {
        if self.token.is(TokenKind::Arrow) || self.token.is(TokenKind::Colon) {
            self.advance_token()?;
            let ty = self.parse_type()?;

            Ok(ty)
        } else {
            Ok(Type::Basic("void".into()))
        }
    }

    fn parse_function_params(&mut self) -> Result<Vec<Param>, MsgWithPos> {
        self.expect_token(TokenKind::LParen)?;

        let params = self.parse_comma_list(TokenKind::RParen, |p| p.parse_function_param())?;

        Ok(params)
    }

    fn parse_function_param(&mut self) -> Result<Param, MsgWithPos> {
        let pos = self.token.position;

        let reassignable = if self.token.is(TokenKind::Var) {
            self.advance_token()?;

            true
        } else {
            false
        };

        let name = self.expect_identifier()?;

        self.expect_token(TokenKind::Colon)?;
        let data_type = self.parse_type()?;

        Ok(Param {
            reassignable,
            name: name,
            pos: pos,
            typ: data_type,
        })
    }

    fn advance_token(&mut self) -> Result<Token, MsgWithPos> {
        let tok = self.lexer.read_token()?;

        Ok(std::mem::replace(&mut self.token, tok))
    }
    fn expect_token(&mut self, kind: TokenKind) -> Result<Token, MsgWithPos> {
        if self.token.kind == kind {
            let token = self.advance_token()?;
            Ok(token)
        } else {
            Err(MsgWithPos::new(
                self.token.position,
                Msg::ExpectedToken(kind.name().into(), self.token.name()),
            ))
        }
    }

    fn parse_expression_statement(&mut self) -> StmtResult {
        let pos = self.token.position;
        let expr = self.parse_expression()?;
        self.expect_semicolon()?;
        Ok(Box::new(Stmt::new(StmtKind::Expr(expr), pos)))
    }

    fn parse_expression(&mut self) -> ExprResult {
        let opts = ExprParsingOpts::new();
        self.parse_binary(0, &opts)
    }
    pub fn parse_binary(&mut self, precedence: u32, opts: &ExprParsingOpts) -> ExprResult {
        let mut left = self.parse_unary(opts)?;
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
                TokenKind::EqEqEq | TokenKind::NeEqEq => 5,
                TokenKind::BitOr | TokenKind::BitAnd | TokenKind::Caret => 6,
                TokenKind::LtLt | TokenKind::GtGt | TokenKind::GtGtGt => 7,
                TokenKind::Add | TokenKind::Sub => 8,
                TokenKind::Mul | TokenKind::Div | TokenKind::Mod => 9,
                TokenKind::Is | TokenKind::As => 10,
                _ => {
                    return Ok(left);
                }
            };

            if precedence >= right_precedence {
                return Ok(left);
            }

            let tok = self.advance_token()?;

            left = match tok.kind {
                TokenKind::Is | TokenKind::As => {
                    //let is = tok.is(TokenKind::Is);

                    let right = self.parse_type()?;
                    let expr = Expr::new(ExprKind::Conv(Box::new(right), left), tok.position);

                    Box::new(expr)
                }

                _ => {
                    let right = self.parse_binary(right_precedence, opts)?;
                    self.create_binary(tok, left, right)?
                }
            };
        }
    }

    pub fn create_binary(&mut self, tok: Token, left: Box<Expr>, right: Box<Expr>) -> ExprResult {
        use self::TokenKind::*;
        let op = match tok.kind {
            Eq => return Ok(Box::new(Expr::new(ExprKind::Assign(left,right), tok.position))),
            Or => BinaryOp::Or,
            BitOr => BinaryOp::BitOr,
            And => BinaryOp::And,
            BitAnd => BinaryOp::BitAnd,
            Ne => BinaryOp::Neq,
            EqEq => BinaryOp::Eq,
            Gt => BinaryOp::Gt,
            Ge => BinaryOp::Ge,
            Caret => BinaryOp::BitXor,
            Add => BinaryOp::Add,
            Sub => BinaryOp::Sub,
            Mul => BinaryOp::Mul,
            Div => BinaryOp::Div,
            Mod => BinaryOp::Mod,
            LtLt => BinaryOp::Shl,
            GtGt => BinaryOp::Shr,
            _ => panic!("Unimplemented token: {:?}", tok),
        };

        return Ok(Box::new(Expr::new(
            ExprKind::Binary(left, op, right),
            tok.position,
        )));
    }

    pub fn parse_primary(&mut self, opts: &ExprParsingOpts) -> ExprResult {
        let mut left = self.parse_factor(opts)?;
        loop {
            left = match self.token.kind {
                TokenKind::Dot => {
                    let tok = self.advance_token()?;
                    let ident = self.expect_identifier()?;
                    if self.token.is(TokenKind::LParen) {
                        self.parse_call(tok.position, Some(left), Path::new(ident))?
                    } else {
                        Box::new(Expr::new(ExprKind::Field(left, ident), tok.position))
                    }
                }
                TokenKind::LBracket => {
                    let tok = self.advance_token()?;
                    let index = self.parse_expression()?;
                    self.expect_token(TokenKind::RBracket)?;
                    Box::new(Expr::new(ExprKind::Array(left, index), tok.position))
                }
                _ => return Ok(left),
            }
        }
    }

    pub fn parse_unary(&mut self, opts: &ExprParsingOpts) -> ExprResult {
        match self.token.kind {
            TokenKind::Add | TokenKind::Sub | TokenKind::Not => {
                let tok = self.advance_token()?;
                let op = match tok.kind {
                    TokenKind::Not => UnaryOp::Not,
                    TokenKind::Sub => UnaryOp::Neg,
                    TokenKind::Add => UnaryOp::Plus,
                    _ => unreachable!(),
                };

                let expr = self.parse_primary(opts)?;
                Ok(Box::new(Expr::new(ExprKind::Unary(op, expr), tok.position)))
            }
            TokenKind::Cast => {
                let pos = self.advance_token()?.position;
                self.expect_token(TokenKind::LParen)?;
                let ty = self.parse_type()?;
                self.expect_token(TokenKind::RParen)?;
                let expr = self.parse_primary(opts)?;
                return Ok(Box::new(Expr::new(
                    ExprKind::Unary(UnaryOp::Cast(ty), expr),
                    pos,
                )));
            }
            _ => self.parse_primary(opts),
        }
    }

    fn parse_lit_int(&mut self) -> ExprResult {
        let tok = self.advance_token()?;
        let pos = tok.position;

        if let TokenKind::LitInt(value, base, suffix) = tok.kind {
            let filtered = value.chars().filter(|&ch| ch != '_').collect::<String>();
            let parsed = u64::from_str_radix(&filtered, base.num());

            match parsed {
                Ok(num) => {
                    let expr = Expr::new(ExprKind::IntLiteral(num as i64, base, suffix), pos);
                    Ok(Box::new(expr))
                }

                _ => {
                    let bits = match suffix {
                        IntSuffix::Byte => "byte",
                        IntSuffix::Int => "int",
                        IntSuffix::Long => "long",
                    };

                    Err(MsgWithPos::new(pos, Msg::NumberOverflow(bits.into())))
                }
            }
        } else {
            unreachable!();
        }
    }

    fn parse_lit_float(&mut self) -> ExprResult {
        let tok = self.advance_token()?;
        let pos = tok.position;

        if let TokenKind::LitFloat(value, suffix) = tok.kind {
            let filtered = value.chars().filter(|&ch| ch != '_').collect::<String>();
            let parsed = filtered.parse::<f64>();

            if let Ok(num) = parsed {
                let expr = Expr::new(ExprKind::FloatLiteral(num, suffix), pos);
                return Ok(Box::new(expr));
            }
        }

        unreachable!()
    }

    fn parse_string(&mut self) -> ExprResult {
        let string = self.advance_token()?;

        if let TokenKind::String(value) = string.kind {
            Ok(Box::new(Expr::new(
                ExprKind::StrLiteral(value),
                string.position,
            )))
        } else {
            unreachable!();
        }
    }

    fn parse_bool_literal(&mut self) -> ExprResult {
        let tok = self.advance_token()?;
        let value = tok.is(TokenKind::True);

        Ok(Box::new(Expr::new(
            ExprKind::BoolLiteral(value),
            tok.position,
        )))
    }

    fn parse_nil(&mut self) -> ExprResult {
        let tok = self.advance_token()?;

        Ok(Box::new(Expr::new(ExprKind::NullLiteral, tok.position)))
    }

    fn parse_parentheses(&mut self) -> ExprResult {
        self.advance_token()?;
        let exp = self.parse_expression()?;
        self.expect_token(TokenKind::RParen)?;

        Ok(exp)
    }

    fn parse_factor(&mut self, opts: &ExprParsingOpts) -> ExprResult {
        match self.token.kind {
            TokenKind::LParen => self.parse_parentheses(),
            TokenKind::Identifier(_) => self.parse_identifier_or_call(opts),
            TokenKind::LitInt(_, _, _) => self.parse_lit_int(),
            TokenKind::LitFloat(_, _) => self.parse_lit_float(),
            TokenKind::LitChar(_) => self.parse_lit_char(),
            TokenKind::String(_) => self.parse_string(),
            TokenKind::True => self.parse_bool_literal(),
            TokenKind::False => self.parse_bool_literal(),
            TokenKind::Nil => self.parse_nil(),
            _ => {Err(MsgWithPos::new(
                self.token.position,
                Msg::ExpectedFactor(self.token.name().clone()),
            ))},
        }
    }

    fn parse_identifier_or_call(&mut self, opts: &ExprParsingOpts) -> ExprResult {
        let pos = self.token.position;
        let mut path = vec![self.expect_identifier()?];
        while self.token.is(TokenKind::Sep) {
            self.advance_token()?;
            let ident = self.expect_identifier()?;
            path.push(ident);
        }
        if self.token.is(TokenKind::LParen) {
            self.parse_call(pos, None, Path { path })
        } else if self.token.is(TokenKind::LBrace) && opts.parse_struct_lit {
            self.parse_lit_struct(pos, Path { path })
        } else {
            assert_eq!(1, path.len());
            let name = &path[0];
            Ok(Box::new(Expr::new(ExprKind::Ident(name.clone()), pos)))
        }
    }

    fn parse_call(&mut self, pos: Position, object: Option<Box<Expr>>, path: Path) -> ExprResult {
        self.expect_token(TokenKind::LParen)?;

        let args = self.parse_comma_list(TokenKind::RParen, |p| p.parse_expression())?;

        Ok(Box::new(Expr::new(ExprKind::Call(path, object, args), pos)))
    }

    fn parse_lit_struct(&mut self, pos: Position, path: Path) -> ExprResult {
        self.expect_token(TokenKind::LBrace)?;
        let args = self.parse_comma_list(TokenKind::RBrace, |p| p.parse_lit_struct_arg())?;

        Ok(Box::new(Expr::new(ExprKind::StructInit(path, args), pos)))
    }

    fn parse_lit_struct_arg(&mut self) -> Result<StructArg, MsgWithPos> {
        let pos = self.token.position;
        let name = self.expect_identifier()?;

        self.expect_token(TokenKind::Colon)?;

        let expr = self.parse_expression()?;

        Ok(StructArg {
            pos: pos,
            name: name,
            expr: expr,
        })
    }

    fn parse_lit_char(&mut self) -> ExprResult {
        let tok = self.advance_token()?;
        let pos = tok.position;

        if let TokenKind::LitChar(val) = tok.kind {
            Ok(Box::new(Expr::new(ExprKind::CharLiteral(val), pos)))
        } else {
            unreachable!();
        }
    }

    fn expect_semicolon(&mut self) -> Result<Token, MsgWithPos> {
        self.expect_token(TokenKind::Semicolon)
    }

    fn expect_identifier(&mut self) -> Result<Name, MsgWithPos> {
        let tok = self.advance_token()?;
        if let TokenKind::Identifier(ref value) = tok.kind {
            Ok(value.to_owned())
        } else {
            Err(MsgWithPos::new(
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

    pub fn init(&mut self) -> Result<Token, MsgWithPos> {
        self.advance_token()
    }

    pub fn parse_type(&mut self) -> Result<Type, MsgWithPos> {
        match self.token.kind {
            TokenKind::Identifier(_) => {
                let name = self.expect_identifier()?;
                Ok(Type::Basic(name))
            }
            TokenKind::BitAnd => {
                self.advance_token()?;
                let ty = self.parse_type()?;
                Ok(Type::Ref(Box::new(ty)))
            }
            TokenKind::Mul => {
                self.advance_token()?;
                let ty = self.parse_type()?;
                Ok(Type::Ptr(Box::new(ty)))
            }
            _ => Err(MsgWithPos::new(
                self.token.position,
                Msg::ExpectedType(self.token.name()),
            )),
        }
    }
}

pub struct ExprParsingOpts {
    parse_struct_lit: bool,
}

impl ExprParsingOpts {
    pub fn new() -> ExprParsingOpts {
        ExprParsingOpts {
            parse_struct_lit: true,
        }
    }

    pub fn parse_struct_lit(&mut self, val: bool) -> &mut ExprParsingOpts {
        self.parse_struct_lit = val;
        self
    }
}
