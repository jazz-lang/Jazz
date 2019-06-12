#pragma once

#include <limits>

namespace jazz {

struct Position {
    using IntegerType = short;

    const char* file;
    IntegerType line;
    IntegerType column;

    Position() : Position(nullptr, std::numeric_limits<IntegerType>::min(), std::numeric_limits<IntegerType>::min()) {}
    Position(const char* file, IntegerType line, IntegerType column) : file(file), line(line), column(column) {}
    Position nextColumn() const { return Position(file, line, column + 1); }
    bool isValid() const { return line > 0 && column > 0; }
};

} // namespace jazz
