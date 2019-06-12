#pragma once

#include <cassert>
#include <cstdlib> 
#include <fstream>
#include <memory>
#include <ostream>
#include <string>
#include <utility> 
#include <vector>

#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/StringExtras.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/raw_ostream.h>

#include "ast/location.h"

#ifndef NDEBUG
#define jazz_assert(condition, ...) assert(condition)
#else
#define jazz_assert(condition, ...) ((void) sizeof(condition))
#endif

namespace jazz {

struct Type;

inline std::ostream& operator<<(std::ostream& stream, llvm::StringRef string) {
    return stream.write(string.data(), string.size());
}

template<typename SourceContainer, typename Mapper>
auto map(const SourceContainer& source, Mapper mapper) -> std::vector<decltype(mapper(*source.begin()))> {
    std::vector<decltype(mapper(*source.begin()))> result;
    result.reserve(source.size());
    for (auto& element : source) {
        result.emplace_back(mapper(element));
    }
    return result;
}

template<typename T>
std::vector<T> instantiate(llvm::ArrayRef<T> elements, const llvm::StringMap<Type>& genericArgs) {
    return map(elements, [&](const T& element) { return element->instantiate(genericArgs); });
}

template<typename TargetContainer, typename SourceContainer>
static void append(TargetContainer& target, const SourceContainer& source) {
    target.insert(target.end(), source.begin(), source.end());
}

template<typename T>
struct StateSaver {
    StateSaver(T& state) : state(state), savedState(std::move(state)) {}
    ~StateSaver() { state = std::move(savedState); }

private:
    T& state;
    T savedState;
};

template<typename T>
StateSaver<T> makeStateSaver(T& state) {
    return StateSaver<T>(state);
}

#define CONCAT_IMPL(a, b) a##b
#define CONCAT(a, b) CONCAT_IMPL(a, b)
#define _SAVE_(state) const auto CONCAT(stateSaver, __COUNTER__) = makeStateSaver(state)

#define NOTNULL(x) (jazz_assert(x), x)

std::string readLineFromFile(Position location);
void renameFile(llvm::Twine sourcePath, llvm::Twine targetPath);
void printDiagnostic(Position location, llvm::StringRef type, llvm::raw_ostream::Colors color, llvm::StringRef message);

class Note {
public:
    template<typename... Args>
    Note(Position location, Args&&... args) : location(location), message(llvm::join_items("", std::forward<Args>(args)...)) {}
    Position getLocation() const { return location; }
    llvm::StringRef getMessage() const { return message; }

private:
    Position location;
    std::string message;
};

class CompileError {
public:
    CompileError(Position location, std::string&& message, std::vector<Note>&& notes)
    : location(location), message(std::move(message)), notes(std::move(notes)) {}
    void print() const;

private:
    Position location;
    std::string message;
    std::vector<Note> notes;
};

template<typename T>
void printColored(const T& text, llvm::raw_ostream::Colors color) {
    if (llvm::outs().has_colors()) llvm::outs().changeColor(color, true);
    llvm::outs() << text;
    if (llvm::outs().has_colors()) llvm::outs().resetColor();
}

template<typename... Args>
[[noreturn]] void errorExit(Args&&... args) {
    printColored("error: ", llvm::raw_ostream::RED);
    using expander = int[];
    (void) expander{ 0, (void(void(printColored(std::forward<Args>(args), llvm::raw_ostream::SAVEDCOLOR))), 0)... };
    llvm::outs() << '\n';
    exit(1);
}

template<typename... Args>
[[noreturn]] void errorWithNotes(Position location, std::vector<Note>&& notes, Args&&... args) {
    std::string message;
    llvm::raw_string_ostream messageStream(message);
    using expander = int[];
    (void) expander{ 0, (void(void(messageStream << std::forward<Args>(args))), 0)... };
    throw CompileError(location, std::move(messageStream.str()), std::move(notes));
}

template<typename... Args>
[[noreturn]] void error(Position location, Args&&... args) {
    errorWithNotes(location, std::vector<Note>(), std::forward<Args>(args)...);
}

enum class WarningMode {
    Default,
    Suppress,
    TreatAsErrors
};
extern WarningMode warningMode;

template<typename... Args>
void warning(Position location, Args&&... args) {
    switch (warningMode) {
        case WarningMode::Default:
            break;
        case WarningMode::Suppress:
            return;
        case WarningMode::TreatAsErrors:
            return error(location, std::forward<Args>(args)...);
    }

    std::string message;
    llvm::raw_string_ostream messageStream(message);
    using expander = int[];
    (void) expander{ 0, (void(void(messageStream << std::forward<Args>(args))), 0)... };
    printDiagnostic(location, "warning", llvm::raw_ostream::YELLOW, messageStream.str());
}

std::string getCCompilerPath();

} // namespace jazz
