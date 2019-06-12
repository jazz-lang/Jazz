#include "utils.h"
#include <algorithm>
#include <cctype>

#include <llvm/Support/ErrorOr.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/Program.h>

using namespace jazz;

WarningMode jazz::warningMode = WarningMode::Default;

std::string jazz::readLineFromFile(Position location) {
    std::ifstream file(location.file);

    while (--location.line) {
        file.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    }

    std::string lineContent;
    std::getline(file, lineContent);
    return lineContent;
}

void jazz::renameFile(llvm::Twine sourcePath, llvm::Twine targetPath) {
    if (auto error = llvm::sys::fs::rename(sourcePath, targetPath)) {
        errorExit("couldn't rename '", sourcePath, "' to '", targetPath, "': ", error.message());
    }
}

void jazz::printDiagnostic(Position location, llvm::StringRef type, llvm::raw_ostream::Colors color, llvm::StringRef message) {
    if (llvm::outs().has_colors()) {
        llvm::outs().changeColor(llvm::raw_ostream::SAVEDCOLOR, true);
    }

    if (location.file && *location.file) {
       
        if ((location.file[0] != '.' && location.file[1] != '/') && location.file[0] != '/' ) {
            llvm::outs() << "./";
        }
        llvm::outs() << location.file;
        if (location.isValid()) {
            llvm::outs() << ':' << location.line << ':' << location.column;
        }
        llvm::outs() << ": ";
    }

    printColored(type, color);
    printColored(": ", color);
    printColored(message, llvm::raw_ostream::SAVEDCOLOR);

    if (location.file && *location.file && location.isValid()) {
        auto line = readLineFromFile(location);
        llvm::outs() << '\n' << line << '\n';

        for (char ch : line.substr(0, location.column - 1)) {
            llvm::outs() << (ch != '\t' ? ' ' : '\t');
        }
        printColored('^', llvm::raw_ostream::GREEN);
    }

    llvm::outs() << '\n';
}

void CompileError::print() const {
    printDiagnostic(location, "error", llvm::raw_ostream::RED, message);

    for (auto& note : notes) {
        printDiagnostic(note.getLocation(), "note", llvm::raw_ostream::BLUE, note.getMessage());
    }

    llvm::outs().flush();
}

std::string jazz::getCCompilerPath() {
    for (const char* compiler : { "cc", "gcc", "clang", "cl.exe" }) {
        if (auto path = llvm::sys::findProgramByName(compiler)) {
            return std::move(*path);
        }
    }
    return "";
}
