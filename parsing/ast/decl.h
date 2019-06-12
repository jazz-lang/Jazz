#pragma once

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include <llvm/ADT/Optional.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/Support/Casting.h>

#include "expr.h"
#include "location.h"
#include "stmt.h"
#include "type.h"
#include "utils.h"

namespace std {
template<>
struct hash<std::vector<jazz::Type>> {
    size_t operator()(llvm::ArrayRef<jazz::Type> types) const {
        jazz_assert(!types.empty());
        size_t hashValue = reinterpret_cast<size_t>(types[0].getBase());

        for (auto type : types.drop_front()) {
            hashValue ^= reinterpret_cast<size_t>(type.getBase());
        }

        return hashValue;
    }
};
} // namespace std

namespace llvm {
class StringRef;
}

namespace jazz {

class Module;
class TypeDecl;
class FieldDecl;
class FunctionDecl;

enum class DeclKind {
    GenericParamDecl,
    FunctionDecl,
    MethodDecl,
    InitDecl,
    DeinitDecl,
    FunctionTemplate,
    TypeDecl,
    TypeTemplate,
    EnumDecl,
    VarDecl,
    FieldDecl,
    ParamDecl,
    ImportDecl,
    LinkDecl,
};

enum class AccessLevel {
    None,
    Private,
    Default,
};

inline llvm::raw_ostream& operator<<(llvm::raw_ostream& stream, AccessLevel accessLevel) {
    switch (accessLevel) {
        case AccessLevel::None:
            llvm_unreachable("invalid access level");
        case AccessLevel::Private:
            return stream << "private";
        case AccessLevel::Default:
            return stream << "public";
    }
    llvm_unreachable("all cases handled");
}

class Decl {
public:
    virtual ~Decl() = 0;

    bool isVariableDecl() const { return getKind() >= DeclKind::VarDecl && getKind() <= DeclKind::ParamDecl; }
    bool isParamDecl() const { return getKind() == DeclKind::ParamDecl; }
    bool isFunctionDecl() const { return getKind() >= DeclKind::FunctionDecl && getKind() <= DeclKind::DeinitDecl; }
    bool isMethodDecl() const { return getKind() >= DeclKind::MethodDecl && getKind() <= DeclKind::DeinitDecl; }
    bool isGenericParamDecl() const { return getKind() == DeclKind::GenericParamDecl; }
    bool isInitDecl() const { return getKind() == DeclKind::InitDecl; }
    bool isDeinitDecl() const { return getKind() == DeclKind::DeinitDecl; }
    bool isFunctionTemplate() const { return getKind() == DeclKind::FunctionTemplate; }
    bool isTypeDecl() const { return getKind() == DeclKind::TypeDecl || getKind() == DeclKind::EnumDecl; }
    bool isTypeTemplate() const { return getKind() == DeclKind::TypeTemplate; }
    bool isEnumDecl() const { return getKind() == DeclKind::EnumDecl; }
    bool isVarDecl() const { return getKind() == DeclKind::VarDecl; }
    bool isFieldDecl() const { return getKind() == DeclKind::FieldDecl; }
    bool isImportDecl() const { return getKind() == DeclKind::ImportDecl; }

    DeclKind getKind() const { return kind; }
    virtual Module* getModule() const = 0;
    virtual Position getLocation() const = 0;
    virtual llvm::StringRef getName() const = 0;
    bool isMain() const { return getName() == "main"; }
    AccessLevel getAccessLevel() const { return accessLevel; }
    virtual bool isReferenced() const { return referenced; }
    void setReferenced(bool referenced) { this->referenced = referenced; }
    bool hasBeenMoved() const;
    std::unique_ptr<Decl> instantiate(const llvm::StringMap<Type>& genericArgs, llvm::ArrayRef<Type> genericArgsArray) const;

protected:
    Decl(DeclKind kind, AccessLevel accessLevel) : kind(kind), accessLevel(accessLevel), referenced(false) {}

private:
    const DeclKind kind;
    AccessLevel accessLevel;
    bool referenced;
};

inline Decl::~Decl() {}

class Movable {
public:
    bool isMoved() const { return moved; }
    void setMoved(bool moved,Position location = Position()) { this->moved = moved;this->moveLocation = location; }
    Position getLocation() const {
        return moveLocation;
    }
private:
    bool moved = false;
    Position moveLocation = Position();
};

class LinkDecl: public Decl {
public:
    static bool classof(const Decl* d) {
        return d->getKind() == DeclKind::LinkDecl;
    }

    DeclKind getKind() const {
        return DeclKind::LinkDecl;
    }

    Module* getModule() const {
        return nullptr;
    }
    Position getLocation() const {
        return Position();
    }
    llvm::StringRef getName() const {return llvm::StringRef(libname);}
    LinkDecl(llvm::StringRef name): Decl(DeclKind::LinkDecl,AccessLevel::Default),libname(name) {}    
private: 
    std::string libname;
};


class VariableDecl : public Decl {
public:
    Type getType() const { return type; }
    void setType(Type type) { this->type = type; }
    static bool classof(const Decl* d) { return d->isVariableDecl(); }

protected:
    VariableDecl(DeclKind kind, AccessLevel accessLevel, Type type) : Decl(kind, accessLevel), type(type) {}

private:
    Type type;
};

class ParamDecl : public VariableDecl, public Movable {
public:
    ParamDecl(Type type, std::string&& name, Position location,std::unique_ptr<Expr>* defaultValue = nullptr,bool compile_time = false)
    : VariableDecl(DeclKind::ParamDecl, AccessLevel::None, type), name(std::move(name)), location(location),compile_time(compile_time) {}
    llvm::StringRef getName() const override { return name; }
    bool isVar() const {return var;}
    void setVar(bool value) {var = value;}
    Module* getModule() const override { return nullptr; }
    Expr* getDefaultValue() {
        if (defaultValue) {
            return defaultValue->get();
        } else {
            return nullptr;
        }
    }
    void setCompileTime(bool value) {compile_time = value;}
    bool isCompileTime() const {return compile_time;}
    Position getLocation() const override { return location; }
    static bool classof(const Decl* d) { return d->getKind() == DeclKind::ParamDecl; }
    bool operator==(const ParamDecl& other) const { return getType() == other.getType() && getName() == other.getName(); }

private:
    std::string name;
    Position location;
    std::unique_ptr<Expr>* defaultValue;
    bool compile_time = false;
    bool var;
};

std::vector<ParamDecl> instantiateParams(llvm::ArrayRef<ParamDecl> params, const llvm::StringMap<Type>& genericArgs);

class GenericParamDecl : public Decl {
public:
    GenericParamDecl(std::string&& name, Position location)
    : Decl(DeclKind::GenericParamDecl, AccessLevel::None), name(std::move(name)), location(location) {}
    llvm::StringRef getName() const override { return name; }
    llvm::ArrayRef<Type> getConstraints() const { return constraints; }
    void addConstraint(Type constraint) { constraints.push_back(constraint); }
    Module* getModule() const override { return nullptr; }
    Position getLocation() const override { return location; }
    static bool classof(const Decl* d) { return d->getKind() == DeclKind::GenericParamDecl; }

private:
    std::string name;
    llvm::SmallVector<Type, 1> constraints;
    Position location;
};

class FunctionProto {
public:
    FunctionProto(std::string&& name, std::vector<ParamDecl>&& params, Type returnType, bool isVarArg, bool isExtern)
    : name(std::move(name)), params(std::move(params)), returnType(returnType), varArg(isVarArg), external(isExtern) {}
    llvm::StringRef getName() const { return name; }
    llvm::ArrayRef<ParamDecl> getParams() const { return params; }
    llvm::MutableArrayRef<ParamDecl> getParams() { return params; }
    Type getReturnType() const { return returnType; }
    bool isVarArg() const { return varArg; }
    bool isExtern() const { return external; }
    FunctionProto instantiate(const llvm::StringMap<Type>& genericArgs) const;

private:
    std::string name;
    std::vector<ParamDecl> params;
    Type returnType;
    bool varArg;
    bool external;
};

std::string getRefinedFunctionName(Type receiver, llvm::StringRef name, llvm::ArrayRef<Type> genericArgs,llvm::ArrayRef<ParamDecl> params = llvm::ArrayRef<ParamDecl>());


class FunctionDecl : public Decl {
public:
    FunctionDecl(FunctionProto&& proto, std::vector<Type>&& genericArgs, AccessLevel accessLevel, Module& module, Position location)
    : FunctionDecl(DeclKind::FunctionDecl, std::move(proto), std::move(genericArgs), accessLevel, module, location) {}
    bool isExtern() const { return getProto().isExtern(); }
    bool isVariadic() const { return getProto().isVarArg(); }
    llvm::StringRef getName() const override { return getProto().getName(); }
    std::string getRefinedName() const;
    llvm::ArrayRef<Type> getGenericArgs() const { return genericArgs; }
    Type getReturnType() const { return getProto().getReturnType(); }
    llvm::ArrayRef<ParamDecl> getParams() const { return getProto().getParams(); }
    llvm::MutableArrayRef<ParamDecl> getParams() { return getProto().getParams(); }
    const FunctionProto& getProto() const { return proto; }
    FunctionProto& getProto() { return proto; }
    virtual TypeDecl* getTypeDecl() const { return nullptr; }
    virtual bool isMutating() const { return false; }

    std::vector<ParamDecl>& getConstParams() {
        return constParams;
    }

    const std::vector<ParamDecl>& getConstParams() const {
        return constParams;
    }
    bool hasBody() const { return body.hasValue(); }
    llvm::ArrayRef<std::unique_ptr<Stmt>> getBody() const { return *NOTNULL(body); }
    llvm::MutableArrayRef<std::unique_ptr<Stmt>> getBody() { return *NOTNULL(body); }
    void setBody(std::vector<std::unique_ptr<Stmt>>&& body) { this->body = std::move(body); }
    void setBody(std::vector<std::unique_ptr<Stmt>>& body) {this->body = std::move(body);}
    Position getLocation() const override { return location; }
    FunctionType* getFunctionType() const;
    bool signatureMatches(const FunctionDecl& other, bool matchReceiver = true) const;
    Module* getModule() const override { return &module; }
    std::unique_ptr<FunctionDecl> instantiate(const llvm::StringMap<Type>& genericArgs, llvm::ArrayRef<Type> genericArgsArray);
    bool isTypechecked() const { return typechecked; }
    void setTypechecked(bool typechecked) { this->typechecked = typechecked; }
    static bool classof(const Decl* d) { return d->isFunctionDecl(); }

protected:
    FunctionDecl(DeclKind kind, FunctionProto&& proto, std::vector<Type>&& genericArgs, AccessLevel accessLevel, Module& module, Position location)
    : Decl(kind, accessLevel), proto(std::move(proto)), genericArgs(std::move(genericArgs)), location(location), module(module),
    typechecked(false)
    {}

public: 
    llvm::Optional<std::vector<std::unique_ptr<Stmt>>> body;
private:
    
    FunctionProto proto;
    std::vector<Type> genericArgs;
    std::vector<ParamDecl> constParams = std::vector<ParamDecl>();
    
    Position location;
    Module& module;
    
    bool typechecked = false;

public: 
    bool no_inline = false;
    bool should_inline = false;
    bool no_mangle = false;
};

class MethodDecl : public FunctionDecl {
public:
    MethodDecl(FunctionProto proto, TypeDecl& receiverTypeDecl, std::vector<Type>&& genericArgs, AccessLevel accessLevel, Position location,bool override_ = false)
    : MethodDecl(DeclKind::MethodDecl, std::move(proto), receiverTypeDecl, std::move(genericArgs), accessLevel, location,override_) {}
    bool isMutating() const override { return mutating; }
    bool isOverriding() const {return override_;}
    void setOverriding(bool val) {
        override_ = val;
    }
    void setMutating(bool mutating) { this->mutating = mutating; }
    TypeDecl* getTypeDecl() const override { return typeDecl; }

    std::unique_ptr<MethodDecl> instantiate(const llvm::StringMap<Type>& genericArgs, TypeDecl& typeDecl);
    static bool classof(const Decl* d) { return d->isMethodDecl(); }

protected:
    MethodDecl(DeclKind kind, FunctionProto proto, TypeDecl& typeDecl, std::vector<Type>&& genericArgs, AccessLevel accessLevel, Position location,bool override_ = false);

private:
    TypeDecl* typeDecl;
    bool mutating;
    bool override_;
};

class InitDecl : public MethodDecl {
public:
    InitDecl(TypeDecl& receiverTypeDecl, std::vector<ParamDecl>&& params, AccessLevel accessLevel, Position location)
    : MethodDecl(DeclKind::InitDecl, FunctionProto("init", std::move(params), Type::getVoid(), false, false), receiverTypeDecl, {},
                 accessLevel, location) {}
    bool isMutating() const override { return true; }
    static bool classof(const Decl* d) { return d->getKind() == DeclKind::InitDecl; }
};

class DeinitDecl : public MethodDecl {
public:
    DeinitDecl(TypeDecl& receiverTypeDecl, Position location)
    : MethodDecl(DeclKind::DeinitDecl, FunctionProto("deinit", {}, Type::getVoid(), false, false), receiverTypeDecl, {}, AccessLevel::None, location) {}
    bool isMutating() const override { return true; }
    static bool classof(const Decl* d) { return d->getKind() == DeclKind::DeinitDecl; }
};

class FunctionTemplate : public Decl {
public:
    FunctionTemplate(std::vector<GenericParamDecl>&& genericParams, std::unique_ptr<FunctionDecl> functionDecl, AccessLevel accessLevel)
    : Decl(DeclKind::FunctionTemplate, accessLevel), genericParams(std::move(genericParams)), functionDecl(std::move(functionDecl)) {}
    llvm::StringRef getName() const override { return getFunctionDecl()->getName(); }
    std::string getRefinedName() const { return getFunctionDecl()->getRefinedName(); }
    bool isReferenced() const override;
    static bool classof(const Decl* d) { return d->isFunctionTemplate(); }
    llvm::ArrayRef<GenericParamDecl> getGenericParams() const { return genericParams; }
    FunctionDecl* getFunctionDecl() const { return functionDecl.get(); }
    FunctionDecl* instantiate(const llvm::StringMap<Type>& genericArgs);
    Module* getModule() const override { return functionDecl->getModule(); }
    Position getLocation() const override { return functionDecl->getLocation(); }

private:
    std::vector<GenericParamDecl> genericParams;
    std::unique_ptr<FunctionDecl> functionDecl;
    std::unordered_map<std::vector<Type>, std::unique_ptr<FunctionDecl>> instantiations;
};

enum class TypeTag { Struct, Interface, Union, Enum };

class TypeDecl : public Decl {
public:
    TypeDecl(TypeTag tag, std::string&& name, std::vector<Type>&& genericArgs, std::vector<Type>&& interfaces, AccessLevel accessLevel,
             Module& module, Position location)
    : Decl(DeclKind::TypeDecl, accessLevel), tag(tag), name(std::move(name)), genericArgs(std::move(genericArgs)),
      interfaces(std::move(interfaces)), location(location), module(module) {}
    TypeTag getTag() const { return tag; }
    llvm::StringRef getName() const { return name; }
    std::string getRefinedName() const;
    llvm::ArrayRef<FieldDecl> getFields() const { return fields; }
    std::vector<FieldDecl>& getFields() { return fields; }
    llvm::ArrayRef<std::unique_ptr<Decl>> getMethods() const { return methods; }
    llvm::ArrayRef<Type> getGenericArgs() const { return genericArgs; }
    llvm::ArrayRef<Type> getInterfaces() const { return interfaces; }
    bool hasInterface(const TypeDecl& interface) const;
    bool isCopy() const;
    Position getLocation() const { return location; }
    void addField(FieldDecl&& field);
    void addMethod(std::unique_ptr<Decl> decl);
    llvm::ArrayRef<std::unique_ptr<Decl>> getMemberDecls() const { return methods; }
    DeinitDecl* getDeinitializer() const;
    Type getType(bool isMutable = false) const;
    Type getTypeForPassing(bool isMutable = false) const;
    bool passByValue() const { return (isStruct() && isCopy()) || isUnion(); }
    bool isStruct() const { return tag == TypeTag::Struct; }
    bool isInterface() const { return tag == TypeTag::Interface; }
    bool isUnion() const { return tag == TypeTag::Union; }
    unsigned getFieldIndex(llvm::StringRef fieldName) const;
    Module* getModule() const { return &module; }
    static bool classof(const Decl* d) { return d->isTypeDecl(); }

protected:
    TypeDecl(DeclKind kind, TypeTag tag, std::string&& name, AccessLevel accessLevel, Module& module, Position location)
    : Decl(kind, accessLevel), tag(tag), name(std::move(name)), location(location), module(module) {}

private:
    TypeTag tag;
    std::string name;
    std::vector<Type> genericArgs;
    std::vector<Type> interfaces;
    std::vector<FieldDecl> fields;
    std::vector<std::unique_ptr<Decl>> methods;
    Position location;
    Module& module;
};

class TypeTemplate : public Decl {
public:
    TypeTemplate(std::vector<GenericParamDecl>&& genericParams, std::unique_ptr<TypeDecl> typeDecl, AccessLevel accessLevel)
    : Decl(DeclKind::TypeTemplate, accessLevel), genericParams(std::move(genericParams)), typeDecl(std::move(typeDecl)) {}
    llvm::ArrayRef<GenericParamDecl> getGenericParams() const { return genericParams; }
    llvm::StringRef getName() const override { return getTypeDecl()->getName(); }
    TypeDecl* getTypeDecl() const { return typeDecl.get(); }
    TypeDecl* instantiate(const llvm::StringMap<Type>& genericArgs);
    TypeDecl* instantiate(llvm::ArrayRef<Type> genericArgs);
    Module* getModule() const override { return typeDecl->getModule(); }
    Position getLocation() const override { return typeDecl->getLocation(); }
    static bool classof(const Decl* d) { return d->getKind() == DeclKind::TypeTemplate; }

private:
    std::vector<GenericParamDecl> genericParams;
    std::unique_ptr<TypeDecl> typeDecl;
    std::unordered_map<std::vector<Type>, std::unique_ptr<TypeDecl>> instantiations;
};

class EnumCase {
public:
    EnumCase(std::string&& name, std::unique_ptr<Expr> value, Position location)
    : name(std::move(name)), value(std::move(value)), location(location) {}
    llvm::StringRef getName() const { return name; }
    Expr* getValue() const { return value.get(); }
    Position getLocation() const { return location; }

private:
    std::string name;
    std::unique_ptr<Expr> value;
    Position location;
};

class EnumDecl : public TypeDecl {
public:
    EnumDecl(std::string&& name, std::vector<EnumCase>&& cases, AccessLevel accessLevel, Module& module, Position location)
    : TypeDecl(DeclKind::EnumDecl, TypeTag::Enum, std::move(name), accessLevel, module, location), cases(std::move(cases)) {}
    llvm::ArrayRef<EnumCase> getCases() const { return cases; }
    const EnumCase* getCaseByName(llvm::StringRef name) const;
    Type getUnderlyingType() const { return Type::getInt(); }
    static bool classof(const Decl* d) { return d->getKind() == DeclKind::EnumDecl; }

private:
    std::vector<EnumCase> cases;
};

class VarDecl : public VariableDecl, public Movable {
public:
    VarDecl(Type type, std::string&& name, std::unique_ptr<Expr> initializer, Decl* parent, AccessLevel accessLevel, Module& module, Position location)
    : VariableDecl(DeclKind::VarDecl, accessLevel, type), name(std::move(name)), initializer(std::move(initializer)), parent(parent),
      location(location), module(module) {}
    llvm::StringRef getName() const override { return name; }
    Expr* getInitializer() const { return initializer.get(); }
    Decl* getParent() const { return parent; }
    Position getLocation() const override { return location; }
    Module* getModule() const override { return &module; }
    static bool classof(const Decl* d) { return d->getKind() == DeclKind::VarDecl; }

private:
    std::string name;
    std::unique_ptr<Expr> initializer;
    Decl* parent;
    Position location;
    Module& module;
};

class FieldDecl : public VariableDecl {
public:
    FieldDecl(Type type, std::string&& name, TypeDecl& parent, AccessLevel accessLevel, Position location)
    : VariableDecl(DeclKind::FieldDecl, accessLevel, type), name(std::move(name)), location(location), parent(parent) {}
    llvm::StringRef getName() const override { return name; }
    std::string getRefinedName() const;
    Module* getModule() const override { return parent.getModule(); }
    Position getLocation() const override { return location; }
    TypeDecl* getParent() const { return &parent; }
    static bool classof(const Decl* d) { return d->getKind() == DeclKind::FieldDecl; }

private:
    std::string name;
    Position location;
    TypeDecl& parent;
};

class ImportDecl : public Decl {
public:
    ImportDecl(std::string&& target, Module& module, Position location)
    : Decl(DeclKind::ImportDecl, AccessLevel::None), target(std::move(target)), location(location), module(module) {}
    llvm::StringRef getName() const override { return ""; }
    llvm::StringRef getTarget() const { return target; }
    Position getLocation() const override { return location; }
    Module* getModule() const override { return &module; }
    static bool classof(const Decl* d) { return d->getKind() == DeclKind::ImportDecl; }

private:
    std::string target;
    Position location;
    Module& module;
};

} // namespace jazz
