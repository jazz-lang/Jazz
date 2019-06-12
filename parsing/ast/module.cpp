#include "module.h"
#include "mangle.h"

using namespace jazz;

llvm::StringMap<std::shared_ptr<Module>> Module::allImportedModules;

std::vector<Module*> Module::getAllImportedModules() {
    return map(allImportedModules, [](auto& p) { return p.second.get(); });
}

llvm::ArrayRef<std::shared_ptr<Module>> Module::getStdlibModules() {
    auto it = allImportedModules.find("std");
    if (it == allImportedModules.end()) return {};
    return it->second;
}

static std::vector<std::unique_ptr<Decl>> nonASTDecls;

void Module::addToSymbolTableWithName(Decl& decl, llvm::StringRef name, bool global) {
    if (getSymbolTable().contains(name)) {
        
        error(decl.getLocation(), "redefinition of '", name, "'");
    }

    if (global) {
        getSymbolTable().addGlobal(name, &decl);
    } else {
        getSymbolTable().add(name, &decl);
    }
}

void Module::addToSymbolTable(FunctionTemplate& decl) {
    if (getSymbolTable().findWithMatchingPrototype(*decl.getFunctionDecl())) {
        error(decl.getLocation(), "redefinition of '", decl.getRefinedName(), "'");
    }
    getSymbolTable().addGlobal(decl.getRefinedName(), &decl);
}

void Module::addToSymbolTable(FunctionDecl& decl) {
    if (auto proto = getSymbolTable().findWithMatchingPrototype(decl)) {
        if (proto->isMethodDecl()) {
            if (llvm::cast<MethodDecl>(proto)->isOverriding()) {
                getSymbolTable().addGlobal(decl.getRefinedName(),&decl);
                return;
            } 
        }
        error(decl.getLocation(), "redefinition of '", decl.getRefinedName(), "'");
    }
    getSymbolTable().addGlobal(decl.getRefinedName(), &decl);
}

void Module::addToSymbolTable(TypeTemplate& decl) {
    addToSymbolTableWithName(decl, decl.getTypeDecl()->getName(), true);
}

void Module::addToSymbolTable(TypeDecl& decl) {
    llvm::cast<BasicType>(decl.getType().getBase())->setDecl(&decl);
    addToSymbolTableWithName(decl, decl.getRefinedName(), true);

    for (auto& memberDecl : decl.getMemberDecls()) {
        if (auto* nonTemplateMethod = llvm::dyn_cast<MethodDecl>(memberDecl.get())) {
            addToSymbolTable(*nonTemplateMethod);
        }
    }
}

void Module::addToSymbolTable(EnumDecl& decl) {
    llvm::cast<BasicType>(decl.getType().getBase())->setDecl(&decl);
    addToSymbolTableWithName(decl, decl.getName(), true);
}

void Module::addToSymbolTable(VarDecl& decl, bool global) {
    addToSymbolTableWithName(decl, decl.getName(), global);
}

template<typename DeclT>
void Module::addToSymbolTableNonAST(DeclT& decl) {
    std::string name = decl.getName();
    nonASTDecls.push_back(llvm::make_unique<DeclT>(std::move(decl)));
    getSymbolTable().add(std::move(name), nonASTDecls.back().get());

    if (std::is_same<DeclT, TypeDecl>::value) {
        auto& typeDecl = llvm::cast<TypeDecl>(*nonASTDecls.back());
        llvm::cast<BasicType>(*typeDecl.getType()).setDecl(&typeDecl);
    }
}

void Module::addToSymbolTable(FunctionDecl&& decl) {
    addToSymbolTableNonAST(decl);
}

void Module::addToSymbolTable(TypeDecl&& decl) {
    addToSymbolTableNonAST(decl);
}

void Module::addToSymbolTable(VarDecl&& decl) {
    addToSymbolTableNonAST(decl);
}

void Module::addIdentifierReplacement(llvm::StringRef source, llvm::StringRef target) {
    jazz_assert(!target.empty());
    getSymbolTable().addIdentifierReplacement(source, target);
}
