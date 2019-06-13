#include <csignal>
#include <cstdlib>
#include <string>
#include <vector>
#ifndef _WIN32
#include <cstring>
#include <execinfo.h>
#include <unistd.h>
#endif
#include <llvm/ADT/ArrayRef.h>
#include <cstdio>
#include <iostream>
#include <ostream>
#include <string>
#include <system_error>
#include <filesystem>
#include <vector>
#include <llvm/Analysis/TargetTransformInfo.h>
#include <llvm/Analysis/TargetTransformInfo.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/ADT/StringSet.h>
#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/Linker/Linker.h>
#include <llvm/LTO/LTO.h>
#include <llvm/LTO/Config.h>
#include <llvm/Support/TargetParser.h>
#include <llvm/InitializePasses.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
//#include <llvm/Transforms/Vectorize/LoadStoreVectorizer.h> This file doesn't exists in LLVM from Guix packages
#include <llvm/Transforms/IPO/PassManagerBuilder.h>
#include <llvm/Transforms/IPO/AlwaysInliner.h>
#include <llvm/Transforms/IPO.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Utils.h>
#include <llvm/Transforms/Vectorize/LoopVectorize.h>
#include <llvm/Transforms/IPO/AlwaysInliner.h>
#include <llvm/Support/ErrorOr.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/Path.h>
#include <llvm/Support/Program.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/raw_ostream.h>
#include "ast/dump-ast.h"
#include "ast/module.h"
#include "llvm/llvm.h"
#include "package-manager/manifest.h"
#include "package-manager/package-manager.h"
#include "parse.h"
#include "semck/typecheck.h"
#include "utils.h"
#include "eval/eval.h"
#include "jit/jit_backend.h"

using namespace jazz;

namespace llvm {
template<typename T>
class ArrayRef;
class StringRef;
} // namespace llvm

class Manifest;

bool checkFlag(llvm::StringRef flag, std::vector<llvm::StringRef>& args);
int buildProject(llvm::StringRef packageRoot, const char* argv0, std::vector<llvm::StringRef>& args, bool run);
int buildExe(llvm::ArrayRef<std::string> files, const jazz::Manifest* manifest, const char* argv0,
                    std::vector<llvm::StringRef>& args, llvm::StringRef outputDirectory, llvm::StringRef outputFileName, bool run);


static void printHelp() {
    llvm::outs() << "OVERVIEW: Jazz compiler\n"
                    "\n"
                    "USAGE: Jazz [options] <inputs>\n"
                    "\n"
                    "OPTIONS:\n"
                    "  new                   - Create new empty project\n"
                    "  build                 - Compile some file or project\n"
                    "  run                   - Run file or project\n"
                    "  -c                    - Output .o file without linking\n"
                    "  -emit=<>              - Emit <assembly, llvm ir,llvm bitcode>\n"
                    "  -O<level>             - Set optimization level,possible values: fast,release,debug,Less\n"
                    "  -F<directory>         - Add a search path for C framework header import\n"
                    "  -fPIC                 - Emit position-independent code\n"
                    "  -fLTO                 - Enable link-time optimizations\n"
                    "  -shared               - Create shared library\n"
                    "  -static               - Create statically linked binary\n"
                    "  -musl                 - Use musl libc (no backtraces and may not work on windows)\n"
                    "  -help                 - Display this help\n"
                    "  -I<directory>         - Add a search path for modules and C headers import\n"
                    "  -o<filename>          - Set output file name\n"
                    "  -parse                - Only parse file/project\n"
                    "  -v                    - Verbose output\n"
                    "  -no-std               - Compile without standart library\n"
                    "  -dump-ast             - Print the AST to stdout\n"
                    "  -dump-ir              - Print LLVM IR to stdout\n"
                    "  -typecheck            - Only parse and typecheck given code/project\n"
                    "  -Werror               - Make all warning errors\n";
}

extern "C" void signalHandler(int signal) {
#ifndef _WIN32
    void* stacktrace[128];
    int size = backtrace(stacktrace, 128);
    llvm::errs() << strsignal(signal) << '\n';
    backtrace_symbols_fd(stacktrace, size, STDERR_FILENO);
#endif
    std::exit(signal);
}

bool verbose = false;

int main(int argc, const char** argv) {
    std::signal(SIGINT, signalHandler);
    std::signal(SIGILL, signalHandler);
    std::signal(SIGABRT, signalHandler);
    std::signal(SIGFPE, signalHandler);
    std::signal(SIGSEGV, signalHandler);
    std::signal(SIGTERM, signalHandler);

    const char* argv0 = argv[0];

    --argc;
    ++argv;


    llvm::StringRef command = argv[0];
    bool build = command == "build";
    bool run = command == "run";
    bool create = command == "new";

    if (build || run || create) {
        --argc;
        ++argv;
    }



    std::vector<std::string> inputs;
    std::vector<llvm::StringRef> args;

    for (int i = 0; i < argc; ++i) {
        llvm::StringRef arg = argv[i];

        if (arg == "help" || arg == "-help" || arg == "--help" || arg == "-h") {
            printHelp();
            return 0;
        } else if (arg.startswith("-")) {
            args.push_back(arg);
        } else {
            inputs.push_back(arg);
        }
    }

    try {
        if (create) {
            auto error = llvm::sys::fs::create_directory(argv[0]);
            if (error) {
                std::cout << error;
                return 0;
            }

            error = llvm::sys::fs::create_directory(std::string(argv[0]) + "/src");
            if (error) {
                std::cout << error;
                return 0;
            }

            
            std::stringstream ss = std::stringstream();
            ss << "var name = \"" << argv[0] << "\"" << std::endl;
            ss << "var version = \"0.1.0\"" << std::endl;
            ss << std::endl;
            ss << "//uncomment next declaration to declare dependencies "<<  std::endl << "// var dependencies = [ /* your dependencies here */]; " << std::endl;
            std::ofstream path_build(std::string(argv[0]) + "/build.jazz");
            path_build.write(ss.str().c_str(),ss.str().size());
            path_build.close();
            ss = std::stringstream();
            ss << "fun main() {" << std::endl;
            ss << "\tvar stdout = getStdout();" << std::endl;
            ss << "\tstdout << \"Hello,World!\"" << std::endl;
            ss << "}" << std::endl;

            std::ofstream path_main(std::string(argv[0]) + "/src/main.jazz");
            path_main.write(ss.str().c_str(),ss.str().size());
            path_main.close();
            return 1;
        }

        if (inputs.empty()) {
            llvm::SmallString<128> currentPath;
            if (auto error = llvm::sys::fs::current_path(currentPath)) {
                errorExit(error.message());
            }
            return buildProject(currentPath, argv0, args, run);
        } else {
            return buildExe(inputs, nullptr, argv0, args, ".", "", run);
        }
    } catch (const CompileError& error) {
        error.print();
        return 1;
    }
}


bool checkFlag(llvm::StringRef flag, std::vector<llvm::StringRef>& args) {
    const auto it = std::find(args.begin(), args.end(), flag);
    const bool contains = it != args.end();
    if (contains) args.erase(it);
    return contains;
}


std::vector<std::string> getOptionValues(llvm::StringRef flagPrefix, std::vector<llvm::StringRef>& args) {
    std::vector<std::string> values;
    for (auto arg = args.begin(); arg != args.end();) {
        if (arg->startswith(flagPrefix)) {
            values.push_back(arg->substr(flagPrefix.size()));
            arg = args.erase(arg);
        } else {
            ++arg;
        }
    }
    return values;
}

std::string getOptionValue(llvm::StringRef flagPrefix, std::vector<llvm::StringRef>& args) {
    auto values = getOptionValues(flagPrefix, args);
    if (values.empty()) {
        return "";
    } else {
        return std::move(values.back());
    }
}

void addHeadersSearchFromEnv(std::vector<std::string>& importPaths, const char* name) {
    if (const char* pathList = std::getenv(name)) {
        llvm::SmallVector<llvm::StringRef, 16> paths;
        llvm::StringRef(pathList).split(paths, llvm::sys::EnvPathSeparator, -1, false);

        for (llvm::StringRef path : paths) {
            importPaths.push_back(path);
        }
    }
}

void addHeadersSearchFromCC(std::vector<std::string>& importPaths) {
    auto compilerPath = getCCompilerPath();
    if (compilerPath.empty()) return;

    if (llvm::StringRef(compilerPath).endswith_lower("cl.exe")) {
        addHeadersSearchFromEnv(importPaths, "INCLUDE");
    } else {
        std::string command = "echo | " + compilerPath + " -E -v - 2>&1 | grep '^ /'";
        std::shared_ptr<FILE> process(popen(command.c_str(), "r"), pclose);

        while (!std::feof(process.get())) {
            std::string path;

            while (true) {
                int ch = std::fgetc(process.get());

                if (ch == EOF || ch == '\n') {
                    break;
                } else if (!path.empty() || ch != ' ') {
                    path += (char) ch;
                }
            }

            if (llvm::sys::fs::is_directory(path)) {
                importPaths.push_back(path);
            }
        }
    }
}

void addBuiltinImportPaths(std::vector<std::string>& importPaths, llvm::ArrayRef<std::string> inputFiles) {
    llvm::StringSet<> relativeImportSearchPaths;

    for (llvm::StringRef filePath : inputFiles) {
        auto directoryPath = llvm::sys::path::parent_path(filePath);
        if (directoryPath.empty()) directoryPath = ".";
        relativeImportSearchPaths.insert(directoryPath);
    }

    for (auto& keyValue : relativeImportSearchPaths) {
        importPaths.push_back(keyValue.getKey());
    }
    auto home = std::getenv("HOME");
    if (!home) {
        errorExit("environment variable HOME not set");
    }

    importPaths.push_back(std::string(home) + ".jazz");
    importPaths.push_back("/usr/include");
    importPaths.push_back("/usr/local/include");
    addHeadersSearchFromEnv(importPaths, "CPATH");
    addHeadersSearchFromEnv(importPaths, "C_INCLUDE_PATH");
    addHeadersSearchFromCC(importPaths);


}
using namespace llvm::CodeGenOpt;

static void addDiscriminatorsPass(const llvm::PassManagerBuilder& Builder,llvm::legacy::PassManagerBase &PM) {
    PM.add(llvm::createAddDiscriminatorsPass());
}

void emitCode(llvm::Module& module, llvm::StringRef fileName, llvm::TargetMachine::CodeGenFileType fileType, llvm::Reloc::Model relocModel,Level opt_level,bool isDebug) {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();
    llvm::InitializeNativeTargetDisassembler();
    llvm::Triple triple(llvm::sys::getDefaultTargetTriple());
    const std::string& targetTriple = triple.str();
    module.setTargetTriple(targetTriple);

    std::string errorMessage;
    auto* target = llvm::TargetRegistry::lookupTarget(targetTriple, errorMessage);
    if (!target) errorExit(errorMessage);

    llvm::TargetOptions options;

    auto* targetMachine = target->createTargetMachine(targetTriple, "generic", "", options, relocModel);
    targetMachine->setOptLevel(opt_level);

    module.setDataLayout(targetMachine->createDataLayout());

    std::error_code error;
    llvm::raw_fd_ostream file(fileName, error, llvm::sys::fs::F_None);
    if (error) errorExit(error.message());

    llvm::PassManagerBuilder PMBuilder;
    PMBuilder.SizeLevel = static_cast<int>(opt_level) > 2 ? 2 : 0;
    PMBuilder.OptLevel = static_cast<int>(opt_level);
    PMBuilder.DisableTailCalls = isDebug;
    PMBuilder.DisableUnrollLoops = isDebug;
    PMBuilder.SLPVectorize = !isDebug;
    PMBuilder.LoopVectorize = !isDebug;
    PMBuilder.RerollLoops = !isDebug;
    PMBuilder.PerformThinLTO = true;
    PMBuilder.PrepareForLTO = true;
    const auto triple_pass = llvm::Triple(module.getTargetTriple());
    llvm::TargetLibraryInfoImpl tlii = llvm::TargetLibraryInfoImpl(triple_pass);
    if (isDebug) {
        PMBuilder.Inliner = llvm::createAlwaysInlinerLegacyPass(false);
    } else {
        targetMachine->adjustPassManager(PMBuilder);
        PMBuilder.addExtension(llvm::PassManagerBuilder::EP_EarlyAsPossible,addDiscriminatorsPass);
        PMBuilder.Inliner = llvm::createFunctionInliningPass(PMBuilder.OptLevel,PMBuilder.SizeLevel,false);
    }

    llvm::legacy::FunctionPassManager FPM = llvm::legacy::FunctionPassManager(&module);
    auto tliwp = new llvm::TargetLibraryInfoWrapperPass(tlii);

    FPM.add(tliwp);
    FPM.add(llvm::createTargetTransformInfoWrapperPass(targetMachine->getTargetIRAnalysis()));

    PMBuilder.populateFunctionPassManager(FPM);
    llvm::legacy::PassManager passManager;
    passManager.add(llvm::createTargetTransformInfoWrapperPass(targetMachine->getTargetIRAnalysis()));
    PMBuilder.populateModulePassManager(passManager);

    if (targetMachine->addPassesToEmitFile(passManager, file, nullptr, fileType)) {
        errorExit("TargetMachine can't emit a file of this type");
    }

    FPM.doInitialization();
    for (llvm::Function& F : module) {
        if (!F.isDeclaration()) 
            FPM.run(F);
    }
    FPM.doFinalization();
    passManager.run(module);
    file.flush();
}

void emitLLVMBitcode(const llvm::Module& module, llvm::StringRef fileName) {
    std::error_code error;
    llvm::raw_fd_ostream file(fileName, error, llvm::sys::fs::F_None);
    if (error) errorExit(error.message());
    llvm::WriteBitcodeToFile(module, file);
    file.flush();
}


int buildProject(llvm::StringRef packageRoot, const char* argv0, std::vector<llvm::StringRef>& args, bool run) {
    auto manifestPath = (packageRoot + "/" + jazz::Manifest::manifestFileName).str();
    jazz::Manifest manifest(packageRoot);
    fetchDependencies(packageRoot);

    printColored("\tCompiling ",llvm::raw_ostream::GREEN);
    std::cout << "" << manifest.getPackageName() << " (" << packageRoot << ")" << std::endl;
    for (auto& targetRootDir : manifest.getTargetRootDirectories()) {
        llvm::StringRef outputFileName;
        if (manifest.isMultiTarget() || manifest.getPackageName().empty()) {
            outputFileName = llvm::sys::path::filename(targetRootDir);
        } else {
            outputFileName = manifest.getPackageName();
        }
        int exitStatus = buildExe(getSourceFiles(targetRootDir, manifestPath), &manifest, argv0, args, manifest.getOutputDirectory(),
                                         outputFileName, run);
        if (exitStatus != 0) return exitStatus;
    }

    return 0;
}

int invokeLinker(std::vector<std::string> args,std::vector<std::string> linkLibraries,bool static_link,bool musl) {

    std::string command;
    if (musl) {
        command = "musl-clang ";

    } else {
        command = "clang ";
    }

    if (verbose) {
        command = command + "-v ";
    }
    if (static_link) {
        command = (command + "-static ");
    }



    args.erase(args.begin());

    for (auto& str : linkLibraries) {
        if (!str.empty()) {
            command.push_back('-');
            command.push_back('l');
            command.push_back(' ');
            for (char c : str) {
                command.push_back(c);
            }
            command.push_back(' ');
        }
    }
    for (auto& str : args) {
        for (char c : str) {
            command.push_back(c);
        }
        command.push_back(' ');
    }

    return system(command.c_str());
}

void diagfun(const llvm::DiagnosticInfo& info) {

}

int buildExe(llvm::ArrayRef<std::string> files, const jazz::Manifest* manifest, const char* argv0,
                           std::vector<llvm::StringRef>& args, llvm::StringRef outputDirectory, llvm::StringRef outputFileName, bool run) {
    bool parse = checkFlag("-parse", args);
    bool typecheck = checkFlag("-typecheck", args);
    bool compileOnly = checkFlag("-c", args);
    bool printAST = checkFlag("-dump-ast", args);
    
    
    bool emitPositionIndependentCode = checkFlag("-fPIC", args);
    bool no_std = checkFlag("-no-std",args);
    bool static_link = checkFlag("-static",args);
    
    bool shared = checkFlag("-shared",args);
    bool use_musl = checkFlag("-musl",args);
    verbose = checkFlag("-v",args);
#ifdef __linux__
    emitPositionIndependentCode = true;
#endif
    if (checkFlag("-w", args)) warningMode = WarningMode::Suppress;
    if (checkFlag("-Werror", args)) warningMode = WarningMode::TreatAsErrors;
    auto disabledWarnings = getOptionValues("-Wno-", args);
    auto defines = getOptionValues("-D", args);
    std::string emit = getOptionValue("-emit=",args);
    bool emitAssembly = emit == "assembly" || checkFlag("-S", args);
    bool emitLLVMBitcode = emit == "llvm-bitcode";
    bool dumpIR = checkFlag("-dump-ir", args) || emit == "llvm";
    auto opt_level = getOptionValue("-O",args);
#ifdef _WIN32
    defines.push_back("Windows");
#endif
    if (use_musl) {
        defines.push_back("musl");
    }
    auto importPaths = getOptionValues("-I", args);
    auto frameworkSearchPaths = getOptionValues("-F", args);
    auto specifiedOutputFileName = getOptionValue("-o", args);
    auto linkLibraries = getOptionValues("-l",args);

    Level level = Less;


    bool isDebug = opt_level == "debug";
    if (opt_level == "release") {
        level = Default;
        isDebug = false;
    } else if (opt_level == "debug") {
        level = None;
        isDebug = true;
    } else if (opt_level == "fast") {
        level = Aggressive;
        isDebug = false;
    } else if (opt_level == "less") {
        isDebug = true;
    } else {
        assert(opt_level.size() == 0);
        level = Default;
        isDebug = false;
    }
    if (!specifiedOutputFileName.empty()) {
        outputFileName = specifiedOutputFileName;
    }

    for (llvm::StringRef arg : args) {
        if (arg.startswith("-")) {
            errorExit("unsupported option '", arg, "'");
        }
    }

    if (files.empty()) {
        errorExit("no input files");
    }


    addBuiltinImportPaths(importPaths, files);
    Module module("main", std::move(defines));
    for (llvm::StringRef filePath : files) {
        Parser parser(filePath, module, importPaths, frameworkSearchPaths);
        parser.parse();
    }

    if (printAST) {
        ASTDumper dump(std::cout);
        dump.dumpModule(module);
        return 0;
    }

    if (parse) return 0;

    Typechecker typechecker(std::move(disabledWarnings),no_std);
    for (auto& importedModule : module.getImportedModules()) {
        typechecker.typecheckModule(*importedModule, nullptr, importPaths, frameworkSearchPaths);
    }
    typechecker.typecheckModule(module, manifest, importPaths, frameworkSearchPaths);

    bool treatAsLibrary = !module.getSymbolTable().contains("main") && !run;
    if (treatAsLibrary) {
        compileOnly = true;
    }

    if (typecheck) return 0;

    Codegen irGenerator;

    for (auto* module : Module::getAllImportedModules()) {
        irGenerator.compile(*module);
    }




    auto& mainModule = irGenerator.compile(module);

    if (dumpIR) {
        mainModule.setModuleIdentifier("");
        mainModule.setSourceFileName("");
        mainModule.print(llvm::outs(), nullptr);
        return 0;
    }
    llvm::Module linkedModule("", irGenerator.getLLVMContext());
    llvm::Linker linker(linkedModule);

    llvm::StringSet linked;
    if (verbose)
        std::cout << "Started linking procedure" << std::endl;

    for (auto& module : irGenerator.getGeneratedModules()) {
        if (verbose)
            std::cout << "Linking '" << module->getName() << "' module" << std::endl;
        auto pos = linked.find(module->getName());

        if (pos == linked.end()) {
            linked.insert(module->getName());
            bool error = linker.linkInModule(std::move(module));

            if (error)
            {
                printf("FAIL\n");
                errorExit("LLVM module linking failed");
            }

        }
    }


    if (emitLLVMBitcode) {
        ::emitLLVMBitcode(linkedModule, "output.bc");
        return 0;
    }

    auto ccPath = getCCompilerPath();
    bool msvc = llvm::StringRef(ccPath).endswith_lower("cl.exe");

    llvm::SmallString<128> temporaryOutputFilePath;
    auto* outputFileExtension = emitAssembly ? "s" : msvc ? "obj" : "o";
    if (auto error = llvm::sys::fs::createTemporaryFile("jazz", outputFileExtension, temporaryOutputFilePath)) {
        errorExit(error.message());
    }

    auto fileType = emitAssembly ? llvm::TargetMachine::CGFT_AssemblyFile : llvm::TargetMachine::CGFT_ObjectFile;
    auto relocModel = emitPositionIndependentCode ? llvm::Reloc::Model::PIC_ : llvm::Reloc::Model::Static;
    emitCode(linkedModule, temporaryOutputFilePath, fileType, relocModel,level,isDebug);

    if (!outputDirectory.empty()) {
        auto error = llvm::sys::fs::create_directories(outputDirectory);
        if (error) errorExit(error.message());
    }

    if (compileOnly || emitAssembly) {
        llvm::SmallString<128> outputFilePath = outputDirectory;
        llvm::sys::path::append(outputFilePath, llvm::Twine("output.") + outputFileExtension);
        auto command = std::string("mv ");
        command = (command + temporaryOutputFilePath + " " + outputFilePath).str();
        system(command.c_str());
        return 0;
    }

    llvm::SmallString<128> temporaryExecutablePath;
    const char* executableNamePattern = msvc ? "jazz-%%%%%%%%.exe" : "jazz-%%%%%%%%.out";

    if (auto error = llvm::sys::fs::createUniqueFile(executableNamePattern, temporaryExecutablePath)) {
        errorExit(error.message());
    }

    std::vector<std::string> ccArgs = {
        msvc ? ccPath.c_str() : argv0,
        temporaryOutputFilePath.c_str(),
    };

    std::string outputPathFlag = ((msvc ? "-Fe" : "-o") + temporaryExecutablePath).str();
    ccArgs.push_back(outputPathFlag.c_str());
    ccArgs.push_back("-lc");
    ccArgs.push_back("-lpthread");
    if (shared) ccArgs.push_back("-shared");
    if (manifest) {
        for (auto& library : manifest->getLinkLibraries()) {
            ccArgs.push_back(std::string("-l" + library));
        }
    }

    if (msvc) {
        ccArgs.push_back("-link");
        ccArgs.push_back("-DEBUG");
        ccArgs.push_back("legacy_stdio_definitions.lib");
        ccArgs.push_back("ucrt.lib");
        ccArgs.push_back("msvcrt.lib");
    }


    llvm::StringRef out = "c-compiler-stdout.txt";
    llvm::StringRef err = "c-compiler-stderr.txt";

    llvm::Optional<llvm::StringRef> redirects[3] = { llvm::None, out, err };

    std::vector<llvm::StringRef> ccArgStringRefs(ccArgs.begin(), ccArgs.end());

    int ccExitStatus = msvc ? llvm::sys::ExecuteAndWait(ccArgs[0], ccArgStringRefs, llvm::None, redirects) : invokeLinker(ccArgs,linkLibraries,static_link,use_musl);

    llvm::sys::fs::remove(temporaryOutputFilePath);
    uint64_t fileSize;
    if (!llvm::sys::fs::file_size(out, fileSize) && fileSize == 0) llvm::sys::fs::remove(out);
    if (!llvm::sys::fs::file_size(err, fileSize) && fileSize == 0) llvm::sys::fs::remove(err);
    if (ccExitStatus != 0) return ccExitStatus;

    if (run) {

        std::string error;
        llvm::StringRef executableArgs[] = { temporaryExecutablePath };
        printColored("\tRunning ",llvm::raw_ostream::GREEN);
        std::cout << "'" << executableArgs[0] << "'" << std::endl;
        int executableExitStatus = llvm::sys::ExecuteAndWait(executableArgs[0], executableArgs, llvm::None, {}, 0, 0, &error);
        llvm::sys::fs::remove(temporaryExecutablePath);

        if (msvc) {
            auto path = temporaryExecutablePath;
            llvm::sys::path::replace_extension(path, "ilk");
            llvm::sys::fs::remove(path);
            llvm::sys::path::replace_extension(path, "pdb");
            llvm::sys::fs::remove(path);
        }

        if (!error.empty()) {
            llvm::outs() << error << '\n';
        }

        return executableExitStatus;
    }

    llvm::SmallString<128> outputPathPrefix = outputDirectory;
    if (!outputPathPrefix.empty()) {
        outputPathPrefix.append(llvm::sys::path::get_separator());
    }

    if (outputFileName.empty()) {
        outputFileName = "a";
        auto command = std::string("mv  ");
        command = (command + temporaryExecutablePath).str();
        command = (command + " " + outputPathPrefix + outputFileName + (msvc ? ".exe" : ".out")).str();

        std::system(command.c_str());
    } else {
        auto command = std::string("mv  ");
        command = (command + temporaryExecutablePath).str();
        command = (command + " " + outputPathPrefix + outputFileName + (msvc ? ".exe" : "")).str();
        std::system(command.c_str());
    }
    if (msvc) {
        auto path = temporaryExecutablePath;
        llvm::sys::path::replace_extension(path, "ilk");
        llvm::sys::path::replace_extension(path, "pdb");
        renameFile(path, outputPathPrefix + outputFileName + ".pdb");
    }

    return 0;
}
