#pragma once

#include <string>

namespace jazz {

class FunctionDecl;

std::string mangleName(const FunctionDecl& functionDecl);

} // namespace jazz
