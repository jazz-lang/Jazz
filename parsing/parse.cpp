#include "parse.h"
#include <forward_list>
#include <sstream>
#include <vector>
#include <iostream>
#include <llvm/ADT/APSInt.h>
#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/Support/ErrorHandling.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/MemoryBuffer.h>

#include "lex.h"
#include "ast/decl.h"
#include "ast/module.h"
#include "ast/token.h"
#include "utils.h"

using namespace jazz;

static std::unique_ptr<llvm::MemoryBuffer> getFileMemoryBuffer(llvm::StringRef filePath) {
    auto buffer = llvm::MemoryBuffer::getFile(filePath);
    if (!buffer) errorExit("couldn't open file '", filePath, "'");
    return std::move(*buffer);
}

Parser::Parser(llvm::StringRef filePath, Module& module, llvm::ArrayRef<std::string> importPaths, llvm::ArrayRef<std::string> frameworkSearchPaths)
: Parser(getFileMemoryBuffer(filePath), module, importPaths, frameworkSearchPaths) {}

Parser::Parser(std::unique_ptr<llvm::MemoryBuffer> input, Module& module, llvm::ArrayRef<std::string> importPaths,
               llvm::ArrayRef<std::string> frameworkSearchPaths)
: lexer(std::move(input)), currentModule(&module), currentTokenIndex(0), importPaths(importPaths),
  frameworkSearchPaths(frameworkSearchPaths) {
    tokenBuffer.emplace_back(lexer.nextToken());
}

Token Parser::currentToken() {
    jazz_assert(currentTokenIndex < tokenBuffer.size());
    return tokenBuffer[currentTokenIndex];
}

Position Parser::getLocation() {
    return currentToken().getLocation();
}

Token Parser::lookAhead(int offset) {
    if (int(currentTokenIndex) + offset < 0) return Token(Token::None, Position());
    int count = int(currentTokenIndex) + offset - int(tokenBuffer.size()) + 1;
    while (count-- > 0) {
        tokenBuffer.emplace_back(lexer.nextToken());
    }
    return tokenBuffer[currentTokenIndex + offset];
}

Token Parser::advanceToken() {
    Token token = currentToken();
    if (++currentTokenIndex == tokenBuffer.size()) {
        tokenBuffer.emplace_back(lexer.nextToken());
    }
    return token;
}

/// Adds quotes around the string representation of the given token unless
/// it's an identifier, numeric literal, string literal, or end-of-file.
static std::string quote(Token::Kind tokenKind) {
    std::ostringstream stream;
    if (tokenKind < Token::Break) {
        stream << tokenKind;
    } else {
        stream << '\'' << tokenKind << '\'';
    }
    return stream.str();
}

static std::string formatList(llvm::ArrayRef<Token::Kind> tokens) {
    std::string result;

    for (auto& token : tokens) {
        result += quote(token);

        if (tokens.size() > 2 && &token != &tokens.back()) {
            result += ", ";
        }

        if (&token == &tokens.back() - 1) {
            if (tokens.size() == 2) {
                result += " ";
            }
            result += "or ";
        }
    }

    return result;
}

[[noreturn]] static void unexpectedToken(Token token, llvm::ArrayRef<Token::Kind> expected = {}, const char* contextInfo = nullptr) {
    if (expected.size() == 0) {
        error(token.getLocation(), "unexpected ", quote(token), contextInfo ? " " : "", contextInfo ? contextInfo : "");
    } else {
        error(token.getLocation(), "expected ", formatList(expected), contextInfo ? " " : "", contextInfo ? contextInfo : "", ", got ", quote(token));
    }
}

void Parser::expect(llvm::ArrayRef<Token::Kind> expected, const char* contextInfo) {
    if (!llvm::is_contained(expected, currentToken())) {
        unexpectedToken(currentToken(), expected, contextInfo);
    }
}

Token Parser::parse(llvm::ArrayRef<Token::Kind> expected, const char* contextInfo) {
    expect(expected, contextInfo);
    return advanceToken();
}

void Parser::checkTerminator(Token::Kind currentTerminator, llvm::function_ref<Position()> getLocation) {
    static Token::Kind previousTerminator = Token::None;
    static const char* filePath = nullptr;

    if (filePath != lexer.getFilePath()) {
        filePath = lexer.getFilePath();
        previousTerminator = Token::None;
    }

    if (previousTerminator == Token::None) {
        previousTerminator = currentTerminator;
    } else if (previousTerminator != currentTerminator) {
        //warning(getLocation(), "inconsistent statement terminator, expected ", quote(previousTerminator));
    }
}

void Parser::parseTerminator(const char* contextInfo) {
    if (getLocation().line != lookAhead(-1).getLocation().line) {
        checkTerminator(Token::Newline, [this] {
            // TODO: Use pre-existing buffer instead of reading from file here.
            readLineFromFile(lookAhead(-1).getLocation());
            std::ifstream file(getLocation().file);
            for (auto line = lookAhead(-1).getLocation().line; --line;) {
                file.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
            }
            std::string line;
            std::getline(file, line);
            return Position(getLocation().file, lookAhead(-1).getLocation().line,
                                  static_cast<Position::IntegerType>(line.size() + 1));
        });
        return;
    }

    switch (currentToken()) {
        case Token::RightBrace:
            checkTerminator(Token::Newline, std::bind(&Parser::getLocation, this));
            return;
        case Token::Semicolon:
            checkTerminator(Token::Semicolon, std::bind(&Parser::getLocation, this));
            advanceToken();
            return;
        default:
            unexpectedToken(currentToken(), { Token::Newline, Token::Semicolon }, contextInfo);
    }
}

/// argument-list ::= '(' ')' | '(' nonempty-argument-list ')'
/// nonempty-argument-list ::= argument | nonempty-argument-list ',' argument
/// argument ::= (id ':')? expr
std::vector<NamedValue> Parser::parseArgumentList() {
    parse(Token::LeftParen);
    std::vector<NamedValue> args;
    while (currentToken() != Token::RightParen) {
        std::string name;
        Position location = Position();
        if (lookAhead(1) == Token::Colon) {
            auto result = parse(Token::Identifier);
            name = std::move(result.getString());
            location = result.getLocation();
            advanceToken();
        }
        auto value = parseExpr();
        if (!location.isValid()) location = value->getLocation();
        args.push_back({ std::move(name), std::move(value), location });
        if (currentToken() != Token::RightParen) parse(Token::Comma);
    }
    advanceToken();
    return args;
}

/// var-expr ::= id
std::unique_ptr<VarExpr> Parser::parseIdentifier() {
    jazz_assert(currentToken().is(Token::Identifier, Token::Init));
    auto id = advanceToken();
    return llvm::make_unique<VarExpr>(id.getString(), id.getLocation());
}

std::unique_ptr<VarExpr> Parser::parseThis() {
    jazz_assert(currentToken() == Token::This);
    auto expr = llvm::make_unique<VarExpr>("this", getLocation());
    advanceToken();
    return expr;
}

static std::string replaceEscapeChars(llvm::StringRef literalContent, Position literalStartLocation) {
    std::string result;
    result.reserve(literalContent.size());

    for (auto it = literalContent.begin(), end = literalContent.end(); it != end; ++it) {
        if (*it == '\\') {
            ++it;
            jazz_assert(it != end);
            switch (*it) {
                case '0':
                    result += '\0';
                    break;
                case 'a':
                    result += '\a';
                    break;
                case 'b':
                    result += '\b';
                    break;
                case 'n':
                    result += '\n';
                    break;
                case 'r':
                    result += '\r';
                    break;
                case 't':
                    result += '\t';
                    break;
                case 'v':
                    result += '\v';
                    break;
                case '"':
                    result += '"';
                    break;
                case '\'':
                    result += '\'';
                    break;
                case '\\':
                    result += '\\';
                    break;
                default:
                    auto itColumn = literalStartLocation.column + 1 + (it - literalContent.begin());
                    Position itLocation(literalStartLocation.file, literalStartLocation.line, itColumn);
                    error(itLocation, "unknown escape character '\\", *it, "'");
            }
            continue;
        }
        result += *it;
    }
    return result;
}

std::unique_ptr<StringLiteralExpr> Parser::parseString() {
    jazz_assert(currentToken() == Token::StringLiteral);
    auto content = replaceEscapeChars(currentToken().getString().drop_back().drop_front(), getLocation());
    auto expr = llvm::make_unique<StringLiteralExpr>(std::move(content), getLocation());
    advanceToken();
    return expr;
}

std::unique_ptr<CharacterLiteralExpr> Parser::parseChar() {
    jazz_assert(currentToken() == Token::CharacterLiteral);
    auto content = replaceEscapeChars(currentToken().getString().drop_back().drop_front(), getLocation());
    if (content.size() != 1) error(getLocation(), "character literal must consist of a single UTF-8 byte");
    auto expr = llvm::make_unique<CharacterLiteralExpr>(content[0], getLocation());
    advanceToken();
    return expr;
}

std::unique_ptr<IntLiteralExpr> Parser::parseInt() {
    jazz_assert(currentToken() == Token::IntegerLiteral);
    auto expr = llvm::make_unique<IntLiteralExpr>(currentToken().getIntegerValue(), getLocation());
    advanceToken();
    return expr;
}

std::unique_ptr<FloatLiteralExpr> Parser::parseFloat() {
    jazz_assert(currentToken() == Token::FloatLiteral);
    auto expr = llvm::make_unique<FloatLiteralExpr>(currentToken().getFloatingPointValue(), getLocation());
    advanceToken();
    return expr;
}

std::unique_ptr<BoolLiteralExpr> Parser::parseBool() {
    std::unique_ptr<BoolLiteralExpr> expr;
    switch (currentToken()) {
        case Token::True:
            expr = llvm::make_unique<BoolLiteralExpr>(true, getLocation());
            break;
        case Token::False:
            expr = llvm::make_unique<BoolLiteralExpr>(false, getLocation());
            break;
        default:
            llvm_unreachable("all cases handled");
    }
    advanceToken();
    return expr;
}

std::unique_ptr<NullLiteralExpr> Parser::parseNullLiteral() {
    jazz_assert(currentToken() == Token::Null);
    auto expr = llvm::make_unique<NullLiteralExpr>(getLocation());
    advanceToken();
    return expr;
}

std::unique_ptr<UndefinedLiteralExpr> Parser::parseUndefinedLiteral() {
    jazz_assert(currentToken() == Token::Undefined);
    auto expr = llvm::make_unique<UndefinedLiteralExpr>(getLocation());
    advanceToken();
    return expr;
}

/// array-literal ::= '[' expr-list ']'
std::unique_ptr<ArrayLiteralExpr> Parser::parseArrayInit() {
    jazz_assert(currentToken() == Token::LeftBracket);
    auto location = getLocation();
    advanceToken();
    auto elements = parseExprList();
    parse(Token::RightBracket);
    return llvm::make_unique<ArrayLiteralExpr>(std::move(elements), location);
}

/// tuple-literal ::= '(' tuple-literal-elements ')'
/// tuple-literal-elements ::= tuple-literal-element | tuple-literal-elements ',' tuple-literal-element
/// tuple-literal-element ::= id ':' expr
std::unique_ptr<TupleExpr> Parser::parseTuple() {
    jazz_assert(currentToken() == Token::LeftParen);
    auto location = getLocation();
    auto elements = parseArgumentList();
    for (auto& element : elements) {
        if (element.getName().empty()) {
            if (auto* varExpr = llvm::dyn_cast<VarExpr>(element.getValue())) {
                element.setName(varExpr->getIdentifier());
            } else {
                error(element.getLocation(), "tuple elements must have names");
            }
        }
    }
    return llvm::make_unique<TupleExpr>(std::move(elements), location);
}

/// non-empty-type-list ::= type | type ',' non-empty-type-list
std::vector<Type> Parser::parseTypeList() {
    std::vector<Type> types;

    while (true) {
        types.push_back(parseType());

        if (currentToken() == Token::Comma) {
            advanceToken();
            continue;
        }

        if (currentToken() == Token::RightShift) {
            tokenBuffer[currentTokenIndex] = Token(Token::Greater, currentToken().getLocation());
            tokenBuffer.insert(tokenBuffer.begin() + currentTokenIndex + 1, Token(Token::Greater, currentToken().getLocation().nextColumn()));
        }

        break;
    }

    return types;
}

/// generic-argument-list ::= '<' non-empty-type-list '>'
std::vector<Type> Parser::parseGenericArgumentList() {
    jazz_assert(currentToken() == Token::Less);
    advanceToken();
    std::vector<Type> genericArgs = parseTypeList();
    parse(Token::Greater);
    return genericArgs;
}

int64_t Parser::parseArraySizeInBrackets() {
    jazz_assert(currentToken() == Token::LeftBracket);
    advanceToken();
    int64_t arraySize;

    switch (currentToken()) {
        case Token::IntegerLiteral:
            arraySize = advanceToken().getIntegerValue().getExtValue();
            break;
        case Token::RightBracket:
            arraySize = ArrayType::runtimeSize;
            break;
        case Token::QuestionMark:
            advanceToken();
            arraySize = ArrayType::unknownSize;
            break;
        default:
            error(getLocation(), "non-literal array bounds not implemented yet");
    }

    parse(Token::RightBracket);
    return arraySize;
}

/// simple-type ::= id | id generic-argument-list | id '[' int-literal? ']'
Type Parser::parseBasicType(bool isMutable) {
    auto identifier = parse(Token::Identifier);
    std::vector<Type> genericArgs;

    switch (currentToken()) {
        case Token::Less:
            genericArgs = parseGenericArgumentList();
            LLVM_FALLTHROUGH;
        default:
            return BasicType::get(identifier.getString(), std::move(genericArgs), isMutable, identifier.getLocation());
        case Token::LeftBracket:
            auto bracketLocation = getLocation();
            Type elementType = BasicType::get(identifier.getString(), {}, isMutable, identifier.getLocation());
            return ArrayType::get(elementType, parseArraySizeInBrackets(), false, bracketLocation);
    }
}

/// tuple-type ::= '(' tuple-type-elements ')'
/// tuple-type-elements ::= tuple-type-element | tuple-type-elements ',' tuple-type-element
/// tuple-type-element ::= id ':' type
Type Parser::parseTupleType() {
    jazz_assert(currentToken() == Token::LeftParen);
    auto location = getLocation();
    advanceToken();
    std::vector<TupleElement> elements;

    while (currentToken() != Token::RightParen) {
        std::string name = parse(Token::Identifier).getString();
        parse(Token::Colon);
        elements.push_back({ std::move(name), parseType() });
        if (currentToken() != Token::RightParen) parse(Token::Comma);
    }

    advanceToken();
    return TupleType::get(std::move(elements), false, location);
}

/// function-type ::= param-type-list '->' type
/// param-type-list ::= '(' param-types ')'
/// param-types ::= '' | non-empty-param-types
/// non-empty-param-types ::= type | type ',' non-empty-param-types
Type Parser::parseFunctionType() {
    jazz_assert(currentToken() == Token::LeftParen);
    auto location = getLocation();
    advanceToken();
    std::vector<Type> paramTypes;

    while (currentToken() != Token::RightParen) {
        paramTypes.emplace_back(parseType());
        if (currentToken() != Token::RightParen) parse(Token::Comma);
    }

    advanceToken();
    parse(Token::RightArrow);
    Type returnType = parseType();
    return FunctionType::get(returnType, std::move(paramTypes), false, location);
}

/// type ::= simple-type | 'mutable' simple-type | type 'mutable'? '*' | type 'mutable'? '?' |
///          function-type | tuple-type
Type Parser::parseType() {
    Type type;
    auto location = getLocation();

    switch (currentToken()) {
        case Token::Identifier:
            type = parseBasicType(false);
            break;
        case Token::Mutable:
            advanceToken();
            type = parseBasicType(true);
            type.setMutable(true);
            break;
        case Token::LeftParen:
            if (arrowAfterParentheses()) {
                type = parseFunctionType();
            } else {
                type = parseTupleType();
            }
            break;
        default:
            unexpectedToken(currentToken(), { Token::Identifier, Token::Mutable, Token::LeftParen });
    }

    while (true) {
        switch (currentToken()) {
            case Token::Star:
                type = PointerType::get(type, false, getLocation());
                advanceToken();
                break;
            case Token::QuestionMark:
                type = OptionalType::get(type, false, getLocation());
                advanceToken();
                break;
            case Token::Mutable:
                advanceToken();
                switch (currentToken()) {
                    case Token::Star:
                        type = PointerType::get(type, true, getLocation());
                        advanceToken();
                        break;
                    case Token::QuestionMark:
                        type = OptionalType::get(type, true, getLocation());
                        advanceToken();
                        break;
                    default:
                        unexpectedToken(currentToken(), { Token::Star, Token::QuestionMark }, "after 'mutable'");
                }
                break;
            case Token::LeftBracket:
                type = ArrayType::get(type, parseArraySizeInBrackets(), type.isMutable(), getLocation());
                break;
            case Token::And:
                error(getLocation(), "References not implemented");
            default:
                return type.withLocation(location);
        }
    }
}

std::unique_ptr<TypenameExpr> Parser::parseTypenameExpr() {
    assert(currentToken() == Token::Typename);
    auto location = getLocation();
    advanceToken();
    parse(Token::LeftParen);
    auto type = parseType();
    parse(Token::RightParen);
    return llvm::make_unique<TypenameExpr>(type,location);
}

/// sizeof-expr ::= 'sizeof' '(' type ')'
std::unique_ptr<SizeofExpr> Parser::parseSizeofExpr() {
    assert(currentToken() == Token::Sizeof);
    auto location = getLocation();
    advanceToken();
    parse(Token::LeftParen);
    auto type = parseType();
    parse(Token::RightParen);
    return llvm::make_unique<SizeofExpr>(type, location);
}

/// addressof-expr ::= 'addressof' '(' expr ')'
std::unique_ptr<AddressofExpr> Parser::parseAddressofExpr() {
    assert(currentToken() == Token::Addressof || currentToken() == Token::And);
    auto location = getLocation();
    advanceToken();
    parse(Token::LeftParen);
    auto operand = parseExpr();
    parse(Token::RightParen);
    return llvm::make_unique<AddressofExpr>(std::move(operand), location);
}

/// member-expr ::= expr '.' id
std::unique_ptr<MemberExpr> Parser::parseMemberExpr(std::unique_ptr<Expr> lhs) {
    auto location = getLocation();
    llvm::StringRef member;

    if (currentToken().is(Token::Identifier, Token::Init, Token::Deinit)) {
        member = advanceToken().getString();
    } else {
        unexpectedToken(currentToken(), Token::Identifier);
    }

    return llvm::make_unique<MemberExpr>(std::move(lhs), member, location);
}

/// subscript-expr ::= expr '[' expr ']'
std::unique_ptr<SubscriptExpr> Parser::parseSubscript(std::unique_ptr<Expr> operand) {
    jazz_assert(currentToken() == Token::LeftBracket);
    auto location = getLocation();
    advanceToken();
    auto index = parseExpr();
    parse(Token::RightBracket);
    return llvm::make_unique<SubscriptExpr>(std::move(operand), std::move(index), location);
}

/// unwrap-expr ::= expr '!'
std::unique_ptr<UnwrapExpr> Parser::parseUnwrapExpr(std::unique_ptr<Expr> operand) {
    jazz_assert(currentToken() == Token::Not);
    auto location = getLocation();
    advanceToken();
    return llvm::make_unique<UnwrapExpr>(std::move(operand), location);
}

/// call-expr ::= expr generic-argument-list? argument-list
std::unique_ptr<CallExpr> Parser::parseCallExpr(std::unique_ptr<Expr> callee) {
    std::vector<Type> genericArgs;
    if (currentToken() == Token::Less) {
        genericArgs = parseGenericArgumentList();
    }
    auto location = getLocation();
    auto args = parseArgumentList();
    return llvm::make_unique<CallExpr>(std::move(callee), std::move(args), std::move(genericArgs), location);
}

std::unique_ptr<LambdaExpr> Parser::parseLambdaExpr() {
    jazz_assert(currentToken().is(Token::LeftParen, Token::Identifier));
    auto location = getLocation();
    std::vector<ParamDecl> params;

    if (currentToken() == Token::Identifier) {
        params.push_back(parseParam(true));
    } else {
        params = parseParamList(nullptr, true);
    }

    parse(Token::RightArrow);
    auto body = parseExpr();
    return llvm::make_unique<LambdaExpr>(std::move(params), std::move(body), location);
}

/// paren-expr ::= '(' expr ')'
std::unique_ptr<Expr> Parser::parseParenExpr() {
    jazz_assert(currentToken() == Token::LeftParen);
    advanceToken();
    auto expr = parseExpr();
    parse(Token::RightParen);
    return expr;
}

/// if-expr ::= expr '?' expr ':' expr
std::unique_ptr<IfExpr> Parser::parseIfExpr(std::unique_ptr<Expr> condition) {
    jazz_assert(currentToken() == Token::QuestionMark);
    auto location = getLocation();
    advanceToken();
    auto thenExpr = parseExpr();
    parse(Token::Colon);
    auto elseExpr = parseExpr();
    return llvm::make_unique<IfExpr>(std::move(condition), std::move(thenExpr), std::move(elseExpr), location);
}

bool Parser::shouldParseGenericArgumentList() {
    // Temporary hack: use spacing to determine whether to parse a generic argument list
    // of a less-than binary expression. Zero spaces on either side of '<' will cause it
    // to be interpreted as a generic argument list, for now.
    return lookAhead(0).getLocation().column + int(lookAhead(0).getString().size()) == lookAhead(1).getLocation().column ||
           lookAhead(1).getLocation().column + 1 == lookAhead(2).getLocation().column;
}

/// Returns true if a right-arrow token immediately follows the current set of parentheses.
bool Parser::arrowAfterParentheses() {
    jazz_assert(currentToken() == Token::LeftParen);
    int offset = 1;

    for (int parenDepth = 1; parenDepth > 0; ++offset) {
        switch (lookAhead(offset)) {
            case Token::LeftParen:
                ++parenDepth;
                break;
            case Token::RightParen:
                --parenDepth;
                break;
            default:
                break;
        }
    }

    return lookAhead(offset) == Token::RightArrow;
}

/// postfix-expr ::= postfix-expr postfix-op | call-expr | variable-expr | string-literal |
///                  int-literal | float-literal | bool-literal | null-literal |
///                  paren-expr | array-literal | tuple-literal | subscript-expr |
///                  member-expr | unwrap-expr | lambda-expr | sizeof-expr | addressof-expr | comptime-expr
std::unique_ptr<Expr> Parser::parsePostfixExpr() {
    std::unique_ptr<Expr> expr;
    switch (currentToken()) {
        case Token::Comptime: 
            
            advanceToken();
            expr = parseExpr();
            break;
        case Token::Identifier:
        case Token::Init:
            switch (lookAhead(1)) {
                case Token::LeftParen:
                    expr = parseCallExpr(parseIdentifier());
                    break;
                case Token::Less:
                    if (shouldParseGenericArgumentList()) {
                        expr = parseCallExpr(parseIdentifier());
                        break;
                    }
                    LLVM_FALLTHROUGH;
                default:
                    expr = parseIdentifier();
                    break;
            }
            break;
        case Token::StringLiteral:
            expr = parseString();
            break;
        case Token::CharacterLiteral:
            expr = parseChar();
            break;
        case Token::IntegerLiteral:
            expr = parseInt();
            break;
        case Token::FloatLiteral:
            expr = parseFloat();
            break;
        case Token::True:
        case Token::False:
            expr = parseBool();
            break;
        case Token::Null:
            expr = parseNullLiteral();
            break;
        case Token::This:
            expr = parseThis();
            break;
        case Token::LeftParen:
            if (lookAhead(1) == Token::RightParen && lookAhead(2) == Token::RightArrow) {
                expr = parseLambdaExpr();
            } else if (lookAhead(1) == Token::Identifier && lookAhead(2).is(Token::Colon, Token::Comma)) {
                if (arrowAfterParentheses()) {
                    expr = parseLambdaExpr();
                } else {
                    expr = parseTuple();
                }
            } else {
                expr = parseParenExpr();
            }
            break;
        case Token::LeftBracket:
            expr = parseArrayInit();
            break;
        case Token::Typename: 
            expr = parseTypenameExpr();
            break;
        case Token::Sizeof:
            expr = parseSizeofExpr();
            break;
        case Token::Addressof:
            expr = parseAddressofExpr();
            break;
        case Token::Undefined:
            expr = parseUndefinedLiteral();
            break;
        default:
           
            unexpectedToken(currentToken());
            break;
    }
    while (true) {
        switch (currentToken()) {
            case Token::LeftBracket:
                expr = parseSubscript(std::move(expr));
                break;
            case Token::LeftParen:
                expr = parseCallExpr(std::move(expr));
                break;
            case Token::Dot:
                advanceToken();
                expr = parseMemberExpr(std::move(expr));
                break;
            case Token::Increment:
            case Token::Decrement:
                expr = parseIncrementOrDecrementExpr(std::move(expr));
                break;
            case Token::Not:
                expr = parseUnwrapExpr(std::move(expr));
                break;
            case Token::QuestionMark:
                expr = parseIfExpr(std::move(expr));
                break;
            default:
                return expr;
        }
    }
}

std::unique_ptr<Expr> Parser::parsePrefixExpr() {
    jazz_assert(isUnaryOperator(currentToken()));
    auto op = currentToken();
    
    auto location = getLocation();
    advanceToken();
    if (op == Token::Comptime) {
        return llvm::make_unique<ComptimeExpr>(parseExpr(),location);
    }
    return llvm::make_unique<UnaryExpr>(op, parsePreOrPostfixExpr(), location);
}

std::unique_ptr<Expr> Parser::parsePreOrPostfixExpr() {
    auto comptime = currentToken() == Token::Comptime;

    auto val =  isUnaryOperator(currentToken()) ? parsePrefixExpr() : parsePostfixExpr();
    if (comptime) {
        return llvm::make_unique<ComptimeExpr>(std::move(val),val->getLocation());
    } else {
        return val;
    }
}

std::unique_ptr<UnaryExpr> Parser::parseIncrementOrDecrementExpr(std::unique_ptr<Expr> operand) {
    auto location = getLocation();
    return llvm::make_unique<UnaryExpr>(parse({ Token::Increment, Token::Decrement }), std::move(operand), location);
}

std::unique_ptr<Expr> Parser::parseBinaryExpr(std::unique_ptr<Expr> lhs, int minPrecedence) {
    while (isBinaryOperator(currentToken()) && getPrecedence(currentToken()) >= minPrecedence) {
        auto backtrackLocation = currentTokenIndex;
        auto op = advanceToken();
        auto expr = parsePreOrPostfixExpr();
        if (isAssignmentOperator(currentToken())) {
            currentTokenIndex = backtrackLocation;
            break;
        }
        if (isBinaryOperator(currentToken()) && getPrecedence(currentToken()) > getPrecedence(op)) {
            expr = parseBinaryExpr(std::move(expr), getPrecedence(op) + 1);
        }
        lhs = llvm::make_unique<BinaryExpr>(op, std::move(lhs), std::move(expr), op.getLocation());
    }
    return lhs;
}

std::unique_ptr<Expr> Parser::parseExpr() {
    return parseBinaryExpr(parsePreOrPostfixExpr(), 0);
}

std::vector<std::unique_ptr<Expr>> Parser::parseExprList() {
    std::vector<std::unique_ptr<Expr>> exprs;

    if (currentToken() == Token::Semicolon || currentToken() == Token::RightBrace) {
        return exprs;
    }

    while (true) {
        exprs.emplace_back(parseExpr());
        if (currentToken() != Token::Comma) return exprs;
        advanceToken();
    }
}

std::unique_ptr<ReturnStmt> Parser::parseReturnStmt() {
    jazz_assert(currentToken() == Token::Return);
    auto location = getLocation();
    advanceToken();

    std::unique_ptr<Expr> returnValue;
    if (!currentToken().is(Token::Semicolon, Token::RightBrace)) {
        returnValue = parseExpr();
    }

    parseTerminator();
    return llvm::make_unique<ReturnStmt>(std::move(returnValue), location);
}

std::unique_ptr<VarDecl> Parser::parseVarDecl(bool requireInitialValue, Decl* parent, AccessLevel accessLevel) {
    auto keyword = parse({ Token::Var, Token::Const });
    auto name = parse(Token::Identifier);

    Type type;
    if (currentToken() == Token::Colon) {
        advanceToken();
        auto typeLocation = getLocation();
        type = parseType();
        if (type.isMutable()) error(typeLocation, "type specifier cannot specify mutability");
    }
    type.setMutable(keyword == Token::Var);

    std::unique_ptr<Expr> initializer;

    if (requireInitialValue) {
        parse(Token::Assignment);
        initializer = parseExpr();
        parseTerminator();
    }

    return llvm::make_unique<VarDecl>(type, name.getString(), std::move(initializer), parent, accessLevel, *currentModule, name.getLocation());
}

std::unique_ptr<VarStmt> Parser::parseVarStmt(Decl* parent) {
    return llvm::make_unique<VarStmt>(parseVarDecl(true, parent, AccessLevel::None));
}

std::unique_ptr<ExprStmt> Parser::parseExprStmt(std::unique_ptr<Expr> expr) {
    auto stmt = llvm::make_unique<ExprStmt>(std::move(expr));
    parseTerminator();
    return stmt;
}



std::vector<std::unique_ptr<Stmt>> Parser::parseBlockOrStmt(Decl* parent) {
    std::vector<std::unique_ptr<Stmt>> stmts;
    if (currentToken() == Token::LeftBrace) {
        advanceToken();
        stmts = parseStmtsUntil(Token::RightBrace, parent);
        advanceToken();
    } else {
        stmts.emplace_back(parseStmt(parent));
    }
    return stmts;
}

std::unique_ptr<DeferStmt> Parser::parseDeferStmt() {
    jazz_assert(currentToken() == Token::Defer);
    advanceToken();
    auto stmt = llvm::make_unique<DeferStmt>(parseExpr());
    parseTerminator();
    return stmt;
}

std::unique_ptr<IfStmt> Parser::parseIfStmt(Decl* parent) {
    jazz_assert(currentToken() == Token::If);
    advanceToken();
    if (currentToken() == Token::LeftParen) 
        parse(Token::LeftParen);
    auto condition = parseExpr();
    if (currentToken() == Token::RightParen)
        parse(Token::RightParen);
    auto thenStmts = parseBlockOrStmt(parent);
    std::vector<std::unique_ptr<Stmt>> elseStmts;
    if (currentToken() == Token::Else) {
        advanceToken();
        elseStmts = parseBlockOrStmt(parent);
    }
    return llvm::make_unique<IfStmt>(std::move(condition), std::move(thenStmts), std::move(elseStmts));
}

std::unique_ptr<WhileStmt> Parser::parseWhileStmt(Decl* parent,bool inlineWhile) {
    jazz_assert(currentToken() == Token::While);
    advanceToken();
    if (currentToken() == Token::LeftParen) 
        parse(Token::LeftParen);
    auto condition = parseExpr();
    if (currentToken() == Token::RightParen) 
        parse(Token::RightParen);
    auto body = parseBlockOrStmt(parent);
    return llvm::make_unique<WhileStmt>(std::move(condition), std::move(body), nullptr,inlineWhile);
}

std::unique_ptr<ForStmt> Parser::parseForStmt(Decl* parent,bool inlineFor) {
    jazz_assert(currentToken() == Token::For);
    auto location = getLocation();
    advanceToken();
    if (currentToken() == Token::LeftParen) 
        parse(Token::LeftParen);
    auto variable = parseVarDecl(false, parent, AccessLevel::None);
    parse(Token::In);
    auto range = parseExpr();
    if (currentToken() == Token::RightParen) 
        parse(Token::RightParen);
    auto body = parseBlockOrStmt(parent);
    return llvm::make_unique<ForStmt>(std::move(variable), std::move(range), std::move(body), location,inlineFor);
}

std::unique_ptr<SwitchStmt> Parser::parseSwitchStmt(Decl* parent) {
    jazz_assert(currentToken() == Token::Switch);
    advanceToken();
    if (currentToken() == Token::LeftParen) 
        parse(Token::LeftParen);
    auto condition = parseExpr();
    if (currentToken() == Token::RightParen)
        parse(Token::RightParen);
    parse(Token::LeftBrace);
    std::vector<SwitchCase> cases;
    std::vector<std::unique_ptr<Stmt>> defaultStmts;
    bool defaultSeen = false;
    while (true) {
        if (currentToken() == Token::Case) {
            advanceToken();
            auto value = parseExpr();
            parse(Token::Colon);
            auto stmts = parseStmtsUntilOneOf(Token::Case, Token::Default, Token::RightBrace, parent);
            cases.push_back({ std::move(value), std::move(stmts) });
        } else if (currentToken() == Token::Default) {
            if (defaultSeen) {
                error(getLocation(), "switch-statement may only contain one 'default' case");
            }
            advanceToken();
            parse(Token::Colon);
            defaultStmts = parseStmtsUntilOneOf(Token::Case, Token::Default, Token::RightBrace, parent);
            defaultSeen = true;
        } else {
            error(getLocation(), "expected 'case' or 'default'");
        }
        if (currentToken() == Token::RightBrace) break;
    }
    advanceToken();
    return llvm::make_unique<SwitchStmt>(std::move(condition), std::move(cases), std::move(defaultStmts));
}

/// break-stmt ::= 'break' ('\n' | ';')
std::unique_ptr<BreakStmt> Parser::parseBreakStmt() {
    auto location = getLocation();
    advanceToken();
    parseTerminator();
    return llvm::make_unique<BreakStmt>(location);
}

/// continue-stmt ::= 'continue' ('\n' | ';')
std::unique_ptr<ContinueStmt> Parser::parseContinueStmt() {
    auto location = getLocation();
    advanceToken();
    parseTerminator();
    return llvm::make_unique<ContinueStmt>(location);
}

/// stmt ::= var-stmt | return-stmt | expr-stmt | defer-stmt | if-stmt |
///          switch-stmt | while-stmt | for-stmt | break-stmt | continue-stmt
std::unique_ptr<Stmt> Parser::parseStmt(Decl* parent) {

    auto shouldInline = false;
start: 
    switch (currentToken()) {
        case Token::Inline: {
            advanceToken();
            shouldInline = true;
            goto start;
        }
        case Token::LeftBrace: {
            auto statements = parseBlockOrStmt(parent);

            return llvm::make_unique<CompoundStmt>(std::move(statements));
        }

        case Token::Comptime: 
            advanceToken();
            
            return llvm::make_unique<ComptimeStmt>(parseStmt(parent));
        case Token::Return:
            return parseReturnStmt();
        case Token::Var:
            return parseVarStmt(parent);
        case Token::Defer:
            return parseDeferStmt();
        case Token::If:
            return parseIfStmt(parent);
        case Token::While:
            return parseWhileStmt(parent,shouldInline);
        case Token::For:
            return parseForStmt(parent,shouldInline);
        case Token::Switch:
            return parseSwitchStmt(parent);
        case Token::Break:
            return parseBreakStmt();
        case Token::Continue:
            return parseContinueStmt();
        case Token::Underscore: {
            advanceToken();
            parse(Token::Assignment);
            auto stmt = llvm::make_unique<ExprStmt>(parseExpr());
            parseTerminator();
            return std::move(stmt);
        }
        default:
            auto stmt = llvm::make_unique<ExprStmt>(parseExpr());
            parseTerminator();
            return std::move(stmt);
    }
    std::unique_ptr<Expr> expr = parseExpr();

    if (expr->isCallExpr() || expr->isIncrementOrDecrementExpr() || expr->isAssignment() || expr->isBinaryExpr()) {
        return parseExprStmt(std::move(expr));
    } else {
        unexpectedToken(currentToken());
    }
}

std::vector<std::unique_ptr<Stmt>> Parser::parseStmtsUntil(Token::Kind end, Decl* parent) {
    std::vector<std::unique_ptr<Stmt>> stmts;
    while (currentToken() != end) {
        stmts.emplace_back(parseStmt(parent));
    }
    return stmts;
}

std::vector<std::unique_ptr<Stmt>> Parser::parseStmtsUntilOneOf(Token::Kind end1, Token::Kind end2, Token::Kind end3, Decl* parent) {
    std::vector<std::unique_ptr<Stmt>> stmts;
    while (currentToken() != end1 && currentToken() != end2 && currentToken() != end3) {
        stmts.emplace_back(parseStmt(parent));
    }
    return stmts;
}

ParamDecl Parser::parseParam(bool withType) {
    auto comptime = false;
    auto var = false;

    if (currentToken() == Token::Var) {
        var = true;
        advanceToken();
    }

    if (currentToken() == Token::Comptime) {
        comptime = true;
        advanceToken();
    }
    

    auto name = parse(Token::Identifier);
    Type type;
    std::unique_ptr<Expr>* defaultValue;
    if (withType) {
        parse(Token::Colon);
        type = parseType();

        if (currentToken() == Token::Assignment) {
            advanceToken();
            defaultValue = new std::unique_ptr<Expr>(parseExpr());
        }
    }

    auto decl =  ParamDecl(std::move(type), std::move(name.getString()), name.getLocation());
    decl.setCompileTime(comptime);
    decl.setVar(var);
    return decl;
}

std::vector<ParamDecl> Parser::parseParamList(bool* isVariadic, bool withTypes) {
    parse(Token::LeftParen);
    std::vector<ParamDecl> params;
    while (currentToken() != Token::RightParen) {
        if (isVariadic && currentToken() == Token::DotDotDot) {
            advanceToken();
            *isVariadic = true;
            break;
        }
        params.emplace_back(parseParam(withTypes));
        if (currentToken() != Token::RightParen) parse(Token::Comma);
    }
    parse(Token::RightParen);
    return params;
}

void Parser::parseGenericParamList(std::vector<GenericParamDecl>& genericParams) {
    parse(Token::Less);
    while (true) {
        auto genericParamName = parse(Token::Identifier);
        genericParams.emplace_back(genericParamName.getString(), genericParamName.getLocation());

        if (currentToken() == Token::Colon) { // Generic type constraint.
            advanceToken();
            auto identifier = parse(Token::Identifier);
            genericParams.back().addConstraint(BasicType::get(identifier.getString(), {}, false, identifier.getLocation()));
            if (currentToken() == Token::Plus) {
                advanceToken();
                while (true) {
                    
                    auto identifier = parse(Token::Identifier);
                    genericParams.back().addConstraint(BasicType::get(identifier.getString(),{},false,identifier.getLocation()));
                    if (currentToken() == Token::Greater) {
                        break;
                    }
                    if (currentToken() == Token::Comma) {
                        break;
                    }
                    parse(Token::Plus);
                }
            }
        }
        if (currentToken() == Token::Greater) break;
        parse(Token::Comma);
    }
    parse(Token::Greater);
}

std::unique_ptr<FunctionDecl> Parser::parseFunctionProto(bool isExtern, TypeDecl* receiverTypeDecl, AccessLevel accessLevel,bool should_inline,bool no_inline,
                                                         bool no_mangle,
                                                         std::vector<GenericParamDecl>* genericParams) {
    jazz_assert(currentToken() == Token::Fun);
    advanceToken();

    bool isValidFunctionName = currentToken() == Token::Identifier || isOverloadable(currentToken()) ||
                               (currentToken() == Token::LeftBracket && lookAhead(1) == Token::RightBracket);
    if (!isValidFunctionName) unexpectedToken(currentToken(), {}, "as function name");

    Position nameLocation = getLocation();
    llvm::StringRef name;
    if (currentToken() == Token::Identifier) {
        name = advanceToken().getString();
    } else if (currentToken() == Token::RightArrow) {
        advanceToken();
        name = "->";
    } else if (currentToken() == Token::LeftBracket) {
        advanceToken();
        parse(Token::RightBracket);
        name = "[]";
    } else if (receiverTypeDecl) {
        error(nameLocation, "operator functions must be non-member functions");
    } else {
        name = toString(advanceToken().getKind());
    }

    if (currentToken() == Token::Less) {
        parseGenericParamList(*genericParams);
    }

    bool isVariadic = false;
    auto params = parseParamList(isExtern ? &isVariadic : nullptr, true);

    Type returnType = Type::getVoid();
    if (currentToken() == Token::Colon) {
        advanceToken();
        returnType = parseType();
    }

    FunctionProto proto(std::move(name), std::move(params), std::move(returnType), isVariadic, isExtern);
    
    if (receiverTypeDecl) {
        auto method = llvm::make_unique<MethodDecl>(std::move(proto), *receiverTypeDecl, std::vector<Type>(), accessLevel, nameLocation);
        method->no_inline = no_inline;
        method->should_inline = should_inline;
        method->no_mangle = no_mangle;
        return method;
    } else {
        auto function = llvm::make_unique<FunctionDecl>(std::move(proto), std::vector<Type>(), accessLevel, *currentModule, nameLocation);
        function->no_inline = no_inline;
        function->no_mangle = no_mangle;
        function->should_inline = should_inline;

        return function;
    }
}

std::unique_ptr<FunctionTemplate> Parser::parseFunctionTemplateProto(TypeDecl* receiverTypeDecl, AccessLevel accessLevel,bool should_inline,bool no_inline,bool no_mangle) {
    std::vector<GenericParamDecl> genericParams;
    auto decl = parseFunctionProto(false, receiverTypeDecl, accessLevel, should_inline,no_inline,no_mangle,&genericParams);
    return llvm::make_unique<FunctionTemplate>(std::move(genericParams), std::move(decl), accessLevel);
}

std::unique_ptr<FunctionDecl> Parser::parseFunctionDeclaration(TypeDecl* receiverTypeDecl, AccessLevel accessLevel, bool requireBody,bool should_inline,bool no_inline,bool no_mangle) {
    auto decl = parseFunctionProto(false, receiverTypeDecl, accessLevel, should_inline,no_inline,no_mangle,nullptr);
    if (requireBody && currentToken() == Token::Assignment) {
        parse(Token::Assignment);
        std::unique_ptr<Stmt> returnStmt;
        if (!decl->getReturnType().isVoid()) 
            returnStmt = llvm::make_unique<ReturnStmt>(parseExpr(),getLocation());
        else     
            returnStmt = llvm::make_unique<ExprStmt>(parseExpr());
        
        decl->body = llvm::Optional<std::vector<std::unique_ptr<Stmt>>>(std::vector<std::unique_ptr<Stmt>>());
        decl->body->push_back(std::move(returnStmt));
        parseTerminator();
    } else if (requireBody || currentToken() == Token::LeftBrace) {
        parse(Token::LeftBrace);
        decl->setBody(parseStmtsUntil(Token::RightBrace, decl.get()));
        parse(Token::RightBrace);
    }

    if (lookAhead(-1) != Token::RightBrace) {
        parseTerminator();
    }   
    decl->should_inline = should_inline;
    decl->no_inline = no_inline;

    return decl;
}

std::unique_ptr<FunctionTemplate> Parser::parseFunctionTemplate(TypeDecl* receiverTypeDecl, AccessLevel accessLevel,bool should_inline,bool no_inline,bool no_mangle) {
    auto decl = parseFunctionTemplateProto(receiverTypeDecl, accessLevel,should_inline,no_inline,no_mangle);
    
    parse(Token::LeftBrace);
    decl->getFunctionDecl()->setBody(parseStmtsUntil(Token::RightBrace, decl.get()));
    parse(Token::RightBrace);
    return decl;
}

std::unique_ptr<FunctionDecl> Parser::parseExternFunctionDecl() {
    jazz_assert(currentToken() == Token::Extern);
    advanceToken();
    llvm::StringRef api = "C";
    if (currentToken() == Token::StringLiteral) {
        api = llvm::StringRef(parseString()->getValue());
    }
    
    auto decl = parseFunctionProto(true, nullptr, AccessLevel::Default,false,false,false,nullptr);
    parseTerminator();
    return decl;
}

std::unique_ptr<InitDecl> Parser::parseInitDecl(TypeDecl& receiverTypeDecl, AccessLevel accessLevel) {
    auto initLocation = parse(Token::Init).getLocation();
    auto params = parseParamList(nullptr, true);
    auto decl = llvm::make_unique<InitDecl>(receiverTypeDecl, std::move(params), accessLevel, initLocation);
    parse(Token::LeftBrace);
    decl->setBody(parseStmtsUntil(Token::RightBrace, decl.get()));
    parse(Token::RightBrace);
    return decl;
}

/// deinit-decl ::= 'deinit' '(' ')' '{' stmt* '}'
std::unique_ptr<DeinitDecl> Parser::parseDeinitDecl(TypeDecl& receiverTypeDecl) {
    auto deinitLocation = parse(Token::Deinit).getLocation();
    parse(Token::LeftParen);
    auto expectedRParenLocation = getLocation();
    if (advanceToken() != Token::RightParen) error(expectedRParenLocation, "deinitializers cannot have parameters");
    auto decl = llvm::make_unique<DeinitDecl>(receiverTypeDecl, deinitLocation);
    parse(Token::LeftBrace);
    decl->setBody(parseStmtsUntil(Token::RightBrace, decl.get()));
    parse(Token::RightBrace);
    return decl;
}

/// field-decl ::= 'var' id ':' type ('\n' | ';')
FieldDecl Parser::parseFieldDecl(TypeDecl& typeDecl, AccessLevel accessLevel) {
    expect(Token::Var, "in field declaration");
    bool isMutable = advanceToken() == Token::Var;
    auto name = parse(Token::Identifier);

    parse(Token::Colon);
    auto typeLocation = getLocation();
    Type type = parseType();
    if (type.isMutable()) error(typeLocation, "type specifier cannot specify mutability");
    type.setMutable(isMutable);

    parseTerminator();
    return FieldDecl(type, std::move(name.getString()), typeDecl, accessLevel, name.getLocation());
}

/// type-template-decl ::= ('struct' | 'interface') id generic-param-list? '{' member-decl* '}'
std::unique_ptr<TypeTemplate> Parser::parseTypeTemplate(AccessLevel accessLevel) {
    std::vector<GenericParamDecl> genericParams;
    auto typeDecl = parseTypeDecl(&genericParams, accessLevel);
    return llvm::make_unique<TypeTemplate>(std::move(genericParams), std::move(typeDecl), accessLevel);
}

Token Parser::parseTypeDefinition(std::vector<Type>& interfaces, std::vector<GenericParamDecl>* genericParams) {
    auto name = parse(Token::Identifier);

    if (currentToken() == Token::Less) {
        parseGenericParamList(*genericParams);
    }

    if (currentToken() == Token::Colon) {
        advanceToken();
        interfaces = parseTypeList();
    }

    return name;
}


std::unique_ptr<TypeDecl> Parser::parseTypeDecl(std::vector<GenericParamDecl>* genericParams, AccessLevel typeAccessLevel) {
    TypeTag tag;
    switch (advanceToken()) {
        case Token::Struct:
            tag = TypeTag::Struct;
            break;
        case Token::Interface:
            tag = TypeTag::Interface;
            break;
        default:
            llvm_unreachable("invalid token");
    }

    std::vector<Type> interfaces;
    auto name = parseTypeDefinition(interfaces, genericParams);

    auto typeDecl = llvm::make_unique<TypeDecl>(tag, name.getString(), std::vector<Type>(), std::move(interfaces), typeAccessLevel,
                                                *currentModule, name.getLocation());
    parse(Token::LeftBrace);

    while (currentToken() != Token::RightBrace) {
        AccessLevel accessLevel = AccessLevel::Default;
        bool isMutating = false;
        bool should_inline = false;
        bool no_inline = false;
        bool no_mangle = false;
        bool override_ = false;
    start:
        
        switch (currentToken()) {
            case Token::Override: 
                if (override_) {
                    warning(getLocation(),"duplicate 'override specifier");
                }
                override_ = true;
                advanceToken();
                goto start;
            case Token::NoMangle: 
                if (no_mangle) {
                    warning(getLocation(),"duplicate 'nomangle' specifier");
                }
                no_mangle = true;
                advanceToken();
                goto start;
            case Token::Mutable:
                if (isMutating) {
                    warning(getLocation(), "duplicate 'mut' specifier");
                }
                isMutating = true;
                advanceToken();
                goto start;
            case Token::Private:
                if (tag == TypeTag::Interface) {
                    warning(getLocation(), "interface members cannot be private");
                }
                if (accessLevel != AccessLevel::Default) {
                    warning(getLocation(), "duplicate access specifier");
                }
                accessLevel = AccessLevel::Private;
                advanceToken();
                goto start;
            
            case Token::Inline: 
                if (should_inline) {
                    warning(getLocation(),"duplicate 'inline' specifier");
                }
                if (no_inline) {
                    error(getLocation(),"Adding 'inline' to 'no inline' function");
                }
                should_inline = true;
                advanceToken();
                goto start;
            case Token::NoInline: 
                if (should_inline) {
                    error(getLocation(),"Function marked as 'inline' but you trying to mark it as 'no inline'");
                }

                if (no_inline) {
                    warning(getLocation(),"duplicate 'no inline' specifier");
                }
                no_inline = true;
                advanceToken();
                goto start;
            case Token::Fun: {
                auto requireBody = tag != TypeTag::Interface;
                
                if (lookAhead(2) == Token::Less) {
                    auto decl = parseFunctionTemplate(typeDecl.get(), accessLevel,should_inline,no_inline,no_mangle);
                    llvm::cast<MethodDecl>(decl->getFunctionDecl())->setMutating(isMutating);
                    llvm::cast<MethodDecl>(decl->getFunctionDecl())->setOverriding(override_);
                    typeDecl->addMethod(std::move(decl));
                } else {
                    auto decl = llvm::cast<MethodDecl>(parseFunctionDeclaration(typeDecl.get(), accessLevel, requireBody,should_inline,no_inline,no_mangle));
                    decl->should_inline = should_inline;
                    decl->no_inline = no_inline;
                    decl->setMutating(isMutating);
                    decl->setOverriding(override_);
                    typeDecl->addMethod(std::move(decl));
                }
                break;
            }
            case Token::Init:
                typeDecl->addMethod(parseInitDecl(*typeDecl, accessLevel));
                break;
            case Token::Deinit:
                if (accessLevel != AccessLevel::Default) {
                    warning(lookAhead(-1).getLocation(), "deinitializers cannot be ", accessLevel);
                }
                typeDecl->addMethod(parseDeinitDecl(*typeDecl));
                break;
            case Token::Var:
                typeDecl->addField(parseFieldDecl(*typeDecl, accessLevel));
                break;
            default:
                unexpectedToken(currentToken());
        }
    }

    advanceToken();
    return typeDecl;
}

std::unique_ptr<EnumDecl> Parser::parseEnumeration(std::vector<GenericParamDecl>* genericParams, AccessLevel typeAccessLevel) {
    jazz_assert(currentToken() == Token::Enum);
    advanceToken();

    std::vector<Type> interfaces;
    auto name = parseTypeDefinition(interfaces, genericParams);

    parse(Token::LeftBrace);
    std::vector<EnumCase> cases;
    auto valueCounter = llvm::APSInt::get(0);

    while (currentToken() != Token::RightBrace) {
        parse(Token::Case);  
        auto caseName = parse(Token::Identifier);
        auto value = llvm::make_unique<IntLiteralExpr>(valueCounter, caseName.getLocation());
        cases.emplace_back(caseName.getString(), std::move(value), caseName.getLocation());
        parseTerminator("after enum case");
        ++valueCounter;
    }

    advanceToken();
    return llvm::make_unique<EnumDecl>(name.getString(), std::move(cases), typeAccessLevel, *currentModule, name.getLocation());
}

std::unique_ptr<ImportDecl> Parser::parseImportDecl() {
    jazz_assert(currentToken() == Token::Import);
    advanceToken();

    std::string importTarget;
    auto location = getLocation();

    if (currentToken() == Token::StringLiteral) {
        importTarget = parseString()->getValue();
    } else {
        importTarget = parse({ Token::Identifier, Token::StringLiteral }, "after 'import'").getString();
    }

    parseTerminator("after 'import' declaration");
    return llvm::make_unique<ImportDecl>(std::move(importTarget), *currentModule, location);
}

void Parser::parseIfdefBody(std::vector<std::unique_ptr<Decl>>* activeDecls) {
    if (currentToken() == Token::HashIf) {
        parseIfdef(activeDecls);
    } else {
        if (activeDecls) {
            activeDecls->emplace_back(parseTopLevelDecl(true));
        } else {
            parseTopLevelDecl(false);
        }
    }
}

void Parser::parseIfdef(std::vector<std::unique_ptr<Decl>>* activeDecls) {
    jazz_assert(currentToken() == Token::HashIf);
    advanceToken();
    bool negate = currentToken() == Token::Not;
    if (negate) advanceToken();
    auto identifier = parse(Token::Identifier);

    bool condition = false;
    if (identifier.getString() == "hasInclude") {
        parse(Token::LeftParen);
        auto header = parse(Token::StringLiteral);
        parse(Token::RightParen);

        for (auto& path : llvm::concat<const std::string>(importPaths, frameworkSearchPaths)) {
            auto headerPath = llvm::Twine(path) + "/" + header.getString().drop_back().drop_front();
            if (llvm::sys::fs::exists(headerPath) && !llvm::sys::fs::is_directory(headerPath)) {
                condition = true;
                break;
            }
        }
    } else {
        condition = currentModule->isDefined(identifier.getString());
    }

    if (negate) condition = !condition;

    while (!currentToken().is(Token::HashElse, Token::HashEndif)) {
        parseIfdefBody(condition ? activeDecls : nullptr);
    }

    if (currentToken() == Token::HashElse) {
        advanceToken();
        while (currentToken() != Token::HashEndif) {
            parseIfdefBody(condition ? nullptr : activeDecls);
        }
    }

    advanceToken();
}

std::unique_ptr<Decl> Parser::parseTopLevelDecl(bool addToSymbolTable) {
    AccessLevel accessLevel = AccessLevel::Default;
    std::unique_ptr<Decl> decl;
    bool should_inline = false;
    bool no_inline = false;
    bool no_mangle = false;
start:
    switch (currentToken()) {
        case Token::NoMangle: 
            if (no_mangle) {
                warning(getLocation(),"duplicate 'nomangle' specifier");
            }
            no_mangle = true;
            advanceToken();
            goto start;
        case Token::Inline: 
            if (should_inline) {
                warning(getLocation(),"duplicate 'inline' specifier");
            }
            if (no_inline) {
                error(getLocation(),"Adding 'inline' to 'no inline' function");
            }
            
            should_inline = true;
            advanceToken();
            goto start;
        case Token::NoInline: 
            if (should_inline) {
                error(getLocation(),"Function marked as 'inline' but you trying to mark it 'no inline'");
            }
            if (no_inline) {
                warning(getLocation(),"duplicate 'no inline' specifier");
            }
            no_inline = true;
            advanceToken();
            goto start;
        case Token::Private:
            if (accessLevel != AccessLevel::Default) warning(getLocation(), "duplicate access specifier");
            accessLevel = AccessLevel::Private;
            advanceToken();
            goto start;
        case Token::Fun: {
            if (currentToken() == Token::Inline) {
                advanceToken();
                should_inline = true;
            }

            if (currentToken() == Token::NoInline) {
                advanceToken();
                no_inline = true;
            }
        
            if (lookAhead(2) == Token::Less) {
                decl = parseFunctionTemplate(nullptr, accessLevel,should_inline,no_inline,no_mangle);
                
                if (addToSymbolTable) currentModule->addToSymbolTable(llvm::cast<FunctionTemplate>(*decl));
            } else {
                auto fdecl = parseFunctionDeclaration(nullptr, accessLevel,true,should_inline,no_inline,no_mangle);
                
                fdecl->should_inline = should_inline;
                fdecl->no_inline = no_inline;
                decl = std::move(fdecl);
                if (addToSymbolTable) currentModule->addToSymbolTable(llvm::cast<FunctionDecl>(*decl));
            }
            break;
        }
        case Token::Link: {
            advanceToken();
            auto name = parseString()->getValue();
            decl = llvm::make_unique<LinkDecl>(std::move(name));
            break;
        }
        case Token::Extern:
            if (accessLevel != AccessLevel::Default) {
                warning(lookAhead(-1).getLocation(), "extern functions cannot have access specifiers");
            }
            decl = parseExternFunctionDecl();
            if (addToSymbolTable) currentModule->addToSymbolTable(llvm::cast<FunctionDecl>(*decl));
            break;
        case Token::Struct:
        case Token::Interface:
            if (lookAhead(2) == Token::Less) {
                decl = parseTypeTemplate(accessLevel);
                if (addToSymbolTable) currentModule->addToSymbolTable(llvm::cast<TypeTemplate>(*decl));
            } else {
                decl = parseTypeDecl(nullptr, accessLevel);
                if (addToSymbolTable) currentModule->addToSymbolTable(llvm::cast<TypeDecl>(*decl));
            }
            break;
        case Token::Enum:
            if (lookAhead(2) == Token::Less) {
                error(getLocation(), "generic enums not implemented yet");
            } else {
                decl = parseEnumeration(nullptr, accessLevel);
                if (addToSymbolTable) currentModule->addToSymbolTable(llvm::cast<EnumDecl>(*decl));
            }
            break;
        case Token::Var:
        case Token::Const:
            decl = parseVarDecl(true, nullptr, accessLevel);
            if (addToSymbolTable) currentModule->addToSymbolTable(llvm::cast<VarDecl>(*decl), true);
            break;
        case Token::Import:
            if (accessLevel != AccessLevel::Default) {
                warning(lookAhead(-1).getLocation(), "imports cannot have access specifiers");
            }
            return parseImportDecl();
        default:
            unexpectedToken(currentToken());
    }

    return decl;
}

void Parser::parse() {
    std::vector<std::unique_ptr<Decl>> topLevelDecls;
    SourceFile sourceFile(lexer.getFilePath());

    while (currentToken() != Token::None) {
        if (currentToken() == Token::HashIf) {
            parseIfdef(&topLevelDecls);
        } else {
            topLevelDecls.emplace_back(parseTopLevelDecl(true));
        }
    }

    sourceFile.setDecls(std::move(topLevelDecls));
    currentModule->addSourceFile(std::move(sourceFile));
}
