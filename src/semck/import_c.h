
#pragma once

#include <string>

namespace llvm {
class StringRef;
template<typename T>
class ArrayRef;
} // namespace llvm

namespace jazz {

class SourceFile;

bool includeC(SourceFile& importer, llvm::StringRef headerName, llvm::ArrayRef<std::string> importPaths,
                   llvm::ArrayRef<std::string> frameworkSearchPaths);

} // namespace jazz
