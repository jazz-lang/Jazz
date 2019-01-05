use crate::lexer::position::Position;

#[derive(Clone,Debug,PartialEq,Eq,)]
pub enum Msg {
Unimplemented,
    UnknownClass(String),
    UnknownType(String),
    UnknownIdentifier(String),
    UnknownStruct(String),
    UnknownFunction(String),
    UnknownField(String, String),
    UnknownMethod(String, String, Vec<String>),
    UnknownStaticMethod(String, String, Vec<String>),
    UnknownCtor(String, Vec<String>),
    MethodExists(String, String, Position),
    IncompatibleWithNil(String),
    IdentifierExists(String),
    ShadowFunction(String),
    ShadowParam(String),
    ShadowClass(String),
    ShadowStruct(String),
    ShadowTrait(String),
    ShadowField(String),
    ShadowGlobal(String),
    ShadowConst(String),
    VarNeedsTypeInfo(String),
    ParamTypesIncompatible(String, Vec<String>, Vec<String>),
    WhileCondType(String),
    IfCondType(String),
    ReturnType(String, String),
    LvalueExpected,
    AssignType(String, String, String),
    AssignField(String, String, String, String),
    UnOpType(String, String),
    BinOpType(String, String, String),
    ConstValueExpected,
    OutsideLoop,
    NoReturnValue,
    MainNotFound,
    WrongMainDefinition,
    ThisUnavailable,
    SelfTypeUnavailable,
    SuperUnavailable,
    SuperNeedsMethodCall,
    ReferenceTypeExpected(String),
    ThrowNil,
    CatchOrFinallyExpected,
    LetMissingInitialization,
    LetReassigned,
    UnderivableType(String),
    CycleInHierarchy,
    SuperfluousOverride(String),
    SuperfluousOpen(String),
    MissingOverride(String),
    ThrowsDifference(String),
    MethodNotOverridable(String),
    TypesIncompatible(String, String),
    ReturnTypeMismatch(String, String),
    UnresolvedInternal,
    UnclosedComment,
    UnknownChar(char),
    UnclosedChar,
    UnclosedString,
    NumberOverflow(String),
    ExpectedClass(String),
    ExpectedFactor(String),
    ExpectedToken(String, String),
    ExpectedTopLevelElement(String),
    ExpectedTrait(String),
    ExpectedType(String),
    ExpectedIdentifier(String),
    MisplacedElse,
    IoError,
    ExpectedClassElement(String),
    RedundantModifier(String),
    MisplacedModifier(String),
    InvalidEscapeSequence(char),
    MissingFctBody,
    FctCallExpected,
    ThisOrSuperExpected(String),
    NoSuperDelegationWithPrimaryCtor(String),
    NoSuperClass(String),
    RecursiveStructure,
    TraitMethodWithBody,
    TryNeedsCall,
    TryCallNonThrowing,
    ThrowingCallWithoutTry,
    TypeParamsExpected,
    TypeParamNameNotUnique(String),
    StaticMethodNotInTrait(String, String, Vec<String>),
    MethodNotInTrait(String, String, Vec<String>),
    StaticMethodMissingFromTrait(String, String, Vec<String>),
    MethodMissingFromTrait(String, String, Vec<String>),
    WrongNumberTypeParams(usize, usize),
    ClassExpected(String),
    ClassExpectedAsTypeParam,
    AssignmentToConst,
    BoundExpected,
    NoTypeParamsExpected,
    MultipleClassBounds,
    DuplicateTraitBound,
    ClassBoundNotSatisfied(String, String),
    TraitBoundNotSatisfied(String, String),
    AbstractMethodNotInAbstractClass,
    AbstractMethodWithImplementation,
    NewAbstractClass,
    MissingAbstractOverride(String, String),
    ModifierNotAllowedForStaticMethod(String),
    GlobalInitializerNotSupported,
    MakeIteratorReturnType(String),
    UnknownStructField(String, String),
    StructFieldNotInitialized(String, String),
}

#[derive(Clone,Debug)]
pub struct MsgWithPos {
    pub msg: Msg,
    pub pos: Position,
}

impl MsgWithPos {
    pub fn new(pos: Position,msg: Msg) -> MsgWithPos {
        MsgWithPos {
            msg,
            pos,
        }
    }
}

use std::fmt;

impl fmt::Display for MsgWithPos {
    fn fmt(&self,f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,"error at {}: {:?}",self.pos,self.msg)
    }
}
