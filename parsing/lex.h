#pragma once

#include <memory>
#include <vector>
#include "ast/token.h"

namespace llvm {
class MemoryBuffer;
}

namespace jazz {

struct Position;

class Lexer {
public:
    Lexer(std::unique_ptr<llvm::MemoryBuffer> input);
    Token nextToken();
    const char* getFilePath() const;

private:
    Position getLocation() const;
    char readChar();
    void unreadChar(char ch);
    void readBlockComment(Position startLocation);
    Token readQuotedLiteral(char delimiter, Token::Kind literalKind);
    Token readNumber();

private:
    const char* currentFilePosition;
    Position firstLocation;
    Position lastLocation;
    static std::vector<std::unique_ptr<llvm::MemoryBuffer>> fileBuffers;
};

} // namespace jazz
