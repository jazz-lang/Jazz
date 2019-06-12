#include "token.h"
#include <cerrno>
#include <ostream>

#include <llvm/ADT/STLExtras.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/Support/ErrorHandling.h>

#include "utils.h"

using namespace jazz;

namespace {

enum class PrecedenceGroup {
    Assignment,
    LogicalOr,
    LogicalAnd,
    Bitwise,
    Comparison,
    Range,
    AddSub,
    MulDiv,
};

PrecedenceGroup getPrecedenceGroup(Token::Kind tokenKind) {
    switch (tokenKind) {
        case Token::Equal:
        case Token::NotEqual:
        case Token::PointerEqual:
        case Token::PointerNotEqual:
            return PrecedenceGroup::Comparison;
        case Token::Less:
        case Token::LessOrEqual:
        case Token::Greater:
        case Token::GreaterOrEqual:
            return PrecedenceGroup::Comparison;
        case Token::DotDot:
        case Token::DotDotDot:
            return PrecedenceGroup::Range;
        case Token::Plus:
        case Token::Minus:
            return PrecedenceGroup::AddSub;
        case Token::Star:
        case Token::Slash:
        case Token::Modulo:
            return PrecedenceGroup::MulDiv;
        case Token::AndAnd:
            return PrecedenceGroup::LogicalAnd;
        case Token::OrOr:
            return PrecedenceGroup::LogicalOr;
        case Token::And:
        case Token::Or:
        case Token::Xor:
            return PrecedenceGroup::Bitwise;
        case Token::LeftShift:
        case Token::RightShift:
            return PrecedenceGroup::Bitwise;
        default:
            if (isAssignmentOperator(tokenKind)) return PrecedenceGroup::Assignment;
            llvm_unreachable("invalid binary operator");
    }
}

} // namespace

Token::Token(Token::Kind kind, Position location, llvm::StringRef string) : kind(kind), string(string), location(location) {
    jazz_assert(!string.empty() || kind == Token::None || kind >= Token::Break);
    jazz_assert(location.isValid());
#ifndef NDEBUG
    if (kind == Token::IntegerLiteral) (void) getIntegerValue(); 
    if (kind == Token::FloatLiteral) (void) getFloatingPointValue();
#endif
}

bool jazz::isBinaryOperator(Token::Kind tokenKind) {
    switch (tokenKind) {
        case Token::Equal:
        case Token::NotEqual:
        case Token::PointerEqual:
        case Token::PointerNotEqual:
        case Token::Less:
        case Token::LessOrEqual:
        case Token::Greater:
        case Token::GreaterOrEqual:
        case Token::Plus:
        case Token::Minus:
        case Token::Star:
        case Token::Slash:
        case Token::Modulo:
        case Token::And:
        case Token::AndAnd:
        case Token::Or:
        case Token::OrOr:
        case Token::Xor:
        case Token::LeftShift:
        case Token::RightShift:
        case Token::DotDot:
        case Token::DotDotDot:
            return true;
        default:
            return isAssignmentOperator(tokenKind);
    }
}

bool jazz::isUnaryOperator(Token::Kind tokenKind) {
    switch (tokenKind) {
        case Token::Plus:
        case Token::Minus:
        case Token::Star:
        case Token::And:
        case Token::Not:
        case Token::Tilde:
        case Token::Increment:
        case Token::Decrement:
            return true;
        default:
            return false;
    }
}

bool jazz::isAssignmentOperator(Token::Kind tokenKind) {
    return tokenKind == Token::Assignment || isCompoundAssignmentOperator(tokenKind);
}

bool jazz::isCompoundAssignmentOperator(Token::Kind tokenKind) {
    switch (tokenKind) {
        case Token::PlusEqual:
        case Token::MinusEqual:
        case Token::StarEqual:
        case Token::SlashEqual:
        case Token::ModuloEqual:
        case Token::AndEqual:
        case Token::AndAndEqual:
        case Token::OrEqual:
        case Token::OrOrEqual:
        case Token::XorEqual:
        case Token::LeftShiftEqual:
        case Token::RightShiftEqual:
            return true;
        default:
            return false;
    }
}

bool jazz::isOverloadable(Token::Kind tokenKind) {
    switch (tokenKind) {
        case Token::Equal:
        case Token::NotEqual:
        case Token::Less:
        case Token::LessOrEqual:
        case Token::Greater:
        case Token::GreaterOrEqual:
        case Token::Plus:
        case Token::Minus:
        case Token::Star:
        case Token::Slash:
        case Token::LeftShift:
        case Token::RightShift:
        case Token::RightArrow:
        case Token::Modulo:
            return true;
        default:
            return false;
    }
}

int jazz::getPrecedence(Token::Kind tokenKind) {
    return int(getPrecedenceGroup(tokenKind));
}

llvm::APSInt Token::getIntegerValue() const {
    llvm::APInt value;
    bool fail = string.getAsInteger(0, value);
    jazz_assert(!fail, "invalid integer literal");
    return llvm::APSInt(value);
}

long double Token::getFloatingPointValue() const {
    long double value = std::strtold(string.str().c_str(), nullptr);
    jazz_assert(errno != ERANGE, "invalid floating-point literal");
    return value;
}

UnaryOperator::UnaryOperator(Token token) : kind(token) {
    jazz_assert(isUnaryOperator(token));
}

BinaryOperator::BinaryOperator(Token token) : kind(token) {
    jazz_assert(isBinaryOperator(token));
}

bool jazz::isComparisonOperator(Token::Kind tokenKind) {
    switch (tokenKind) {
        case Token::Equal:
        case Token::NotEqual:
        case Token::PointerEqual:
        case Token::PointerNotEqual:
        case Token::Less:
        case Token::LessOrEqual:
        case Token::Greater:
        case Token::GreaterOrEqual:
            return true;
        default:
            return false;
    }
}

bool jazz::isBitwiseOperator(Token::Kind tokenKind) {
    switch (tokenKind) {
        case Token::And:
        case Token::AndEqual:
        case Token::Or:
        case Token::OrEqual:
        case Token::Xor:
        case Token::XorEqual:
        case Token::Tilde:
        case Token::LeftShift:
        case Token::LeftShiftEqual:
        case Token::RightShift:
        case Token::RightShiftEqual:
            return true;
        default:
            return false;
    }
}

std::string jazz::getFunctionName(Token::Kind tokenKind) {
    switch (tokenKind) {
        case Token::DotDot:
            return "Range";
        case Token::DotDotDot:
            return "ClosedRange";
        default:
            return toString(isCompoundAssignmentOperator(tokenKind) ? withoutCompoundEqSuffix(tokenKind) : tokenKind);
    }
}

const char* jazz::toString(Token::Kind tokenKind) {
    static const char* const tokenStrings[] = {
        "end-of-file",
        "newline",
        "identifier",
        "number",
        "float literal",
        "string literal",
        "character literal",
        "addressof",
        "break",
        "case",
        "const",
        "continue",
        "default",
        "defer",
        "deinit",
        "else",
        "enum",
        "extern",
        "false",
        "for",
        "inline",
        "noinline",
        "nomangle",
        "comptime",
        "fun",
        "override",
        "typename",
        "if",
        "link",
        "import",
        "in",
        "init",
        "module",
        "interface",
        "mut",
        "null",
        "private",
        "return",
        "sizeof",
        "struct",
        "switch",
        "this",
        "true",
        "undefined",
        "var",
        "while",
        "_",
        "#if",
        "#else",
        "#endif",
        "==",
        "!=",
        "===",
        "!==",
        "<",
        "<=",
        ">",
        ">=",
        "+",
        "+=",
        "-",
        "-=",
        "*",
        "*=",
        "/",
        "/=",
        "%",
        "%=",
        "++",
        "--",
        "!",
        "&",
        "&=",
        "&&",
        "&&=",
        "|",
        "|=",
        "||",
        "||=",
        "^",
        "^=",
        "~",
        "<<",
        "<<=",
        ">>",
        ">>=",
        "=",
        "(",
        ")",
        "[",
        "]",
        "{",
        "}",
        ".",
        "..",
        "...",
        ",",
        ":",
        ";",
        "->",
        "?",
    };
    static_assert(llvm::array_lengthof(tokenStrings) == int(Token::TokenCount), "tokenStrings array not up-to-date");
    return tokenStrings[int(tokenKind)];
}

std::ostream& jazz::operator<<(std::ostream& stream, Token::Kind tokenKind) {
    return stream << toString(tokenKind);
}
