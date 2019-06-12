#include "lex.h"
#include <cctype>
#include <string>
#include <vector>

#include <llvm/ADT/StringMap.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/Support/ErrorHandling.h>
#include <llvm/Support/MemoryBuffer.h>

#include "parse.h"
#include "ast/token.h"
#include "utils.h"

using namespace jazz;

std::vector<std::unique_ptr<llvm::MemoryBuffer>> Lexer::fileBuffers;

Lexer::Lexer(std::unique_ptr<llvm::MemoryBuffer> input)
: firstLocation(input->getBufferIdentifier().data(), 1, 0), lastLocation(input->getBufferIdentifier().data(), 1, 0) {
    fileBuffers.emplace_back(std::move(input));
    currentFilePosition = fileBuffers.back()->getBufferStart() - 1;
}

const char* Lexer::getFilePath() const {
    return fileBuffers.back()->getBufferIdentifier().data();
}

Position Lexer::getLocation() const {
    return Position(getFilePath(), firstLocation.line, firstLocation.column);
}

char Lexer::readChar() {
    char ch = *++currentFilePosition;
    if (ch != '\n') {
        lastLocation.column++;
    } else {
        lastLocation.line++;
        lastLocation.column = 0;
    }
    return ch;
}

void Lexer::unreadChar(char ch) {
    if (ch != '\n') {
        lastLocation.column--;
    } else {
        lastLocation.line--;
    }
    currentFilePosition--;
}

void Lexer::readBlockComment(Position startLocation) {
    int nestLevel = 1;

    while (true) {
        char ch = readChar();

        if (ch == '*') {
            char next = readChar();

            if (next == '/') {
                nestLevel--;
                if (nestLevel == 0) return;
            } else {
                unreadChar(next);
            }
        } else if (ch == '/') {
            char next = readChar();

            if (next == '*') {
                nestLevel++;
            } else {
                unreadChar(next);
            }
        } else if (ch == '\0') {
            error(startLocation, "unterminated block comment");
        }
    }
}

Token Lexer::readQuotedLiteral(char delimiter, Token::Kind literalKind) {
    const char* begin = currentFilePosition;
    const char* end = begin + 2;
    int ch;

    while ((ch = readChar()) != delimiter || *(end - 2) == '\\') {
        if (ch == '\n' || ch == '\r') {
            Position newlineLocation = firstLocation;
            newlineLocation.column += end - begin - 1;
            error(newlineLocation, "newline inside ", toString(literalKind));
        }
        end++;
    }

    return Token(literalKind, getLocation(), llvm::StringRef(begin, end - begin));
}

Token Lexer::readNumber() {
    const char* const begin = currentFilePosition;
    const char* end = begin + 1;
    bool isFloat = false;
    char ch = readChar();

    switch (ch) {
        case 'b':
            if (begin[0] != '0') goto end;
            end++;
            while (true) {
                ch = readChar();
                if (ch >= '0' && ch <= '1') {
                    end++;
                    continue;
                }
                if (std::isalnum(ch)) error(lastLocation, "invalid digit '", ch, "' in binary literal");
                if (end == begin + 2) error(firstLocation, "binary literal must have at least one digit after '0b'");
                goto end;
            }
            break;
        case 'o':
            if (begin[0] != '0') goto end;
            end++;
            while (true) {
                ch = readChar();
                if (ch >= '0' && ch <= '7') {
                    end++;
                    continue;
                }
                if (std::isalnum(ch)) error(lastLocation, "invalid digit '", ch, "' in octal literal");
                if (end == begin + 2) error(firstLocation, "octal literal must have at least one digit after '0o'");
                goto end;
            }
            break;
        default:
            if (std::isdigit(ch) && begin[0] == '0') {
                error(firstLocation, "numbers cannot start with 0[0-9], use 0o prefix for octal literal");
            }

            while (true) {
                if (ch == '.') {
                    if (isFloat) goto end;
                    isFloat = true;
                } else if (!std::isdigit(ch)) {
                    goto end;
                }
                end++;
                ch = readChar();
            }
            break;
        case 'x':
            if (begin[0] != '0') goto end;
            end++;
            int lettercase = 0; 
            while (true) {
                ch = readChar();

                if (std::isdigit(ch)) {
                    end++;
                } else if (ch >= 'a' && ch <= 'f') {
                    if (lettercase > 0) error(lastLocation, "mixed letter case in hex literal");
                    end++;
                    lettercase = -1;
                } else if (ch >= 'A' && ch <= 'F') {
                    if (lettercase < 0) error(lastLocation, "mixed letter case in hex literal");
                    end++;
                    lettercase = 1;
                } else {
                    if (std::isalnum(ch)) error(lastLocation, "invalid digit '", ch, "' in hex literal");
                    if (end == begin + 2) error(firstLocation, "hex literal must have at least one digit after '0x'");
                    goto end;
                }
            }
            break;
    }

end:
    unreadChar(ch);

    jazz_assert(begin != end);
    if (end[-1] == '.') {
        unreadChar('.'); 
        isFloat = false;
        end--;
    }

    return Token(isFloat ? Token::FloatLiteral : Token::IntegerLiteral, getLocation(), llvm::StringRef(begin, end - begin));
}

static const llvm::StringMap<Token::Kind> keywords = {
    { "addressof", Token::Addressof },
    { "break", Token::Break },
    { "case", Token::Case },
    { "const", Token::Const },
    { "continue", Token::Continue },
    { "default", Token::Default },
    { "defer", Token::Defer },
    { "deinit", Token::Deinit },
    { "else", Token::Else },
    { "enum", Token::Enum },
    { "extern", Token::Extern },
    { "false", Token::False },
    { "for", Token::For },
    { "inline", Token::Inline },
    { "noinline", Token::NoInline },
    { "nomangle", Token::NoMangle },
    { "comptime", Token::Comptime},
    { "fun", Token::Fun },
    { "override", Token::Override },
    { "typename", Token::Typename },
    { "if", Token::If },
    { "link", Token::Link },
    { "import", Token::Import },
    { "in", Token::In },
    { "init", Token::Init },
    { "interface", Token::Interface },
    { "mut", Token::Mutable },
    { "module", Token::Module },
    { "null", Token::Null },
    { "private", Token::Private },
    { "return", Token::Return },
    { "sizeof", Token::Sizeof },
    { "struct", Token::Struct },
    { "switch", Token::Switch },
    { "this", Token::This },
    { "true", Token::True },
    { "undefined", Token::Undefined },
    { "var", Token::Var },
    { "while", Token::While },
    { "_", Token::Underscore },
    { "#if", Token::HashIf },
    { "#else", Token::HashElse },
    { "#endif", Token::HashEndif },
};

Token Lexer::nextToken() {
    while (true) {
        char ch = readChar();
        firstLocation.line = lastLocation.line;
        firstLocation.column = lastLocation.column;

        switch (ch) {
            case ' ':
            case '\t':
            case '\r':
            case '\n':
                break; 
            case '/':
                ch = readChar();
                if (ch == '/') {
                   
                    while (true) {
                        char ch = readChar();
                        if (ch == '\n') break;
                        if (ch == '\0') goto end;
                    }
                } else if (ch == '*') {
                    readBlockComment(firstLocation);
                } else if (ch == '=') {
                    return Token(Token::SlashEqual, getLocation());
                } else {
                    unreadChar(ch);
                    return Token(Token::Slash, getLocation());
                }
                break;
            case '+':
                ch = readChar();
                if (ch == '+') return Token(Token::Increment, getLocation());
                if (ch == '=') return Token(Token::PlusEqual, getLocation());
                unreadChar(ch);
                return Token(Token::Plus, getLocation());
            case '-':
                ch = readChar();
                if (ch == '-') return Token(Token::Decrement, getLocation());
                if (ch == '>') return Token(Token::RightArrow, getLocation());
                if (ch == '=') return Token(Token::MinusEqual, getLocation());
                unreadChar(ch);
                return Token(Token::Minus, getLocation());
            case '*':
                ch = readChar();
                if (ch == '=') return Token(Token::StarEqual, getLocation());
                unreadChar(ch);
                return Token(Token::Star, getLocation());
            case '%':
                ch = readChar();
                if (ch == '=') return Token(Token::ModuloEqual, getLocation());
                unreadChar(ch);
                return Token(Token::Modulo, getLocation());
            case '<':
                ch = readChar();
                if (ch == '=') return Token(Token::LessOrEqual, getLocation());
                if (ch == '<') {
                    ch = readChar();
                    if (ch == '=') return Token(Token::LeftShiftEqual, getLocation());
                    unreadChar(ch);
                    return Token(Token::LeftShift, getLocation());
                }
                unreadChar(ch);
                return Token(Token::Less, getLocation());
            case '>':
                ch = readChar();
                if (ch == '=') return Token(Token::GreaterOrEqual, getLocation());
                if (ch == '>') {
                    ch = readChar();
                    if (ch == '=') return Token(Token::RightShiftEqual, getLocation());
                    unreadChar(ch);
                    return Token(Token::RightShift, getLocation());
                }
                unreadChar(ch);
                return Token(Token::Greater, getLocation());
            case '=':
                ch = readChar();
                if (ch == '=') {
                    ch = readChar();
                    if (ch == '=') return Token(Token::PointerEqual, getLocation());
                    unreadChar(ch);
                    return Token(Token::Equal, getLocation());
                }
                unreadChar(ch);
                return Token(Token::Assignment, getLocation());
            case '!':
                ch = readChar();
                if (ch == '=') {
                    ch = readChar();
                    if (ch == '=') return Token(Token::PointerNotEqual, getLocation());
                    unreadChar(ch);
                    return Token(Token::NotEqual, getLocation());
                }
                unreadChar(ch);
                return Token(Token::Not, getLocation());
            case '&':
                ch = readChar();
                if (ch == '&') {
                    ch = readChar();
                    if (ch == '=') return Token(Token::AndAndEqual, getLocation());
                    unreadChar(ch);
                    return Token(Token::AndAnd, getLocation());
                }
                if (ch == '=') return Token(Token::AndEqual, getLocation());
                unreadChar(ch);
                return Token(Token::And, getLocation());
            case '|':
                ch = readChar();
                if (ch == '|') {
                    ch = readChar();
                    if (ch == '=') return Token(Token::OrOrEqual, getLocation());
                    unreadChar(ch);
                    return Token(Token::OrOr, getLocation());
                }
                if (ch == '=') return Token(Token::OrEqual, getLocation());
                unreadChar(ch);
                return Token(Token::Or, getLocation());
            case '^':
                ch = readChar();
                if (ch == '=') return Token(Token::XorEqual, getLocation());
                unreadChar(ch);
                return Token(Token::Xor, getLocation());
            case '~':
                return Token(Token::Tilde, getLocation());
            case '(':
                return Token(Token::LeftParen, getLocation());
            case ')':
                return Token(Token::RightParen, getLocation());
            case '[':
                return Token(Token::LeftBracket, getLocation());
            case ']':
                return Token(Token::RightBracket, getLocation());
            case '{':
                return Token(Token::LeftBrace, getLocation());
            case '}':
                return Token(Token::RightBrace, getLocation());
            case '.':
                ch = readChar();
                if (ch == '.') {
                    char ch = readChar();
                    if (ch == '.') return Token(Token::DotDotDot, getLocation());
                    unreadChar(ch);
                    return Token(Token::DotDot, getLocation());
                }
                unreadChar(ch);
                return Token(Token::Dot, getLocation());
            case ',':
                return Token(Token::Comma, getLocation());
            case ';':
                return Token(Token::Semicolon, getLocation());
            case ':':
                return Token(Token::Colon, getLocation());
            case '?':
                return Token(Token::QuestionMark, getLocation());
            case '\0':
                goto end;
            case '"':
                return readQuotedLiteral('"', Token::StringLiteral);
            case '\'':
                return readQuotedLiteral('\'', Token::CharacterLiteral);
            default:
                if (std::isdigit(ch)) return readNumber();

                if (!std::isalpha(ch) && ch != '_' && ch != '#') {
                    error(firstLocation, "unknown token '", (char) ch, "'");
                }

                const char* begin = currentFilePosition;
                const char* end = begin;
                do {
                    end++;
                } while (std::isalnum(ch = readChar()) || ch == '_');
                unreadChar(ch);

                llvm::StringRef string(begin, end - begin);

                auto it = keywords.find(string);
                if (it != keywords.end()) {
                    return Token(it->second, getLocation(), string);
                }

                return Token(Token::Identifier, getLocation(), string);
        }
    }

end:
    return Token(Token::None, getLocation());
}
