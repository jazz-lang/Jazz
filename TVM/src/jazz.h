#pragma once

#if defined(_WIN32)
#define JAZZ_WIN
#endif

#if defined(__APPLE__) || defined(macintosh)
#include <TargetConditionals.h>
#define JAZZ_APPLE
#endif

#if defined(__linux__) || defined(linux)
#define JAZZ_LINUX
#endif

#if defined(_64BITS) || defined(__x86_64__) || defined(_M_X64) || \
    defined(__LP64__)
#define JAZZ_X64
#endif

#if defined(_MSC_VER)
#define JAZZ_VCC
#endif

#if defined(__llvm__)
#define JAZZ_LLVM
#endif

#if defined(__clang__)
#define JAZZ_CLANG
#endif

#if defined(JAZZ_VCC) || defined(__MINGW32__)
#define JAZZ_CALLCONV_WIN
#endif

#ifdef _DEBUG
#define JAZZ_DEBUG
#endif
#ifndef JAZZ_VCC
#include <stdint.h>
#endif

#if defined(JAZZ_VCC) || defined(__MINGW32__)
#define EXPORT __declspec(dllexport)
#define IMPORT __declspec(dllimport)
#else
#define EXPORT
#define IMPORT extern
#endif

#ifdef JAZZ_X64
#define WORD_SIZE 8
#define IS_X64 1
#else
#define WORD_SIZE 4
#define IS_X64 0
#endif

#ifdef __cplusplus
#define C_FUNCTION_BEGIN \
    extern "C"           \
    {
#define C_FUNCTION_END \
    }                  \
    ;
#else
#define C_FUNCTION_BEGIN
#define C_FUNCTION_END
#ifndef true
#define true 1
#define false 0
typedef unsigned char bool;
#endif
#endif

typedef intptr_t int_val;
typedef int64_t i64;
typedef uint64_t u64;

#include <memory.h>
#include <stdio.h>
#include <stdlib.h>
#if defined(JAZZ_WIN) && !defined(JAZZ_LLVM)
#if defined(JAZZ_WIN) && !defined(__MINGW32__)
#include <Windows.h>
#elif defined(JAZZ_WIN) && defined(__MINGW32__)
#include <windows.h>
#else
#include <xdk.h>
#endif
#include <wchar.h>
typedef wchar_t uchar;
#define USTR(str) L##str
#define NATIVE_UCHAR_FUN
#define usprintf swprintf
#define uprintf wprintf
#define ustrlen wcslen
#define ustrdup _wcsdup
extern int
uvszprintf(uchar *out, int out_size, const uchar *fmt, va_list arglist);
#define utod(s, end) wcstod(s, end)
#define utoi(s, end) wcstol(s, end, 10)
#define ucmp(a, b) wcscmp(a, b)
#define utostr(out, size, str) wcstombs(out, str, size)
#elif defined(JAZZ_MAC)
typedef uint16_t uchar;
#undef USTR
#define USTR(str) u##str
#else
#include <stdarg.h>
#if defined(JAZZ_APPLE)
#include <stddef.h>
#include <stdint.h>
typedef uint16_t char16_t;
typedef uint32_t char32_t;
#else
#include <uchar.h>
#endif
typedef char16_t uchar;
#undef USTR
#define USTR(str) u##str
#endif

#ifdef JAZZ_VCC
#define no_return(f) __declspec(noreturn) f
#define unreachable()
#else
#define no_return(f) f __attribute__((noreturn))
#define unreachable() __builtin_unreachable()
#endif

typedef struct Type Type;
typedef struct RuntimeObj RuntimeObj;
typedef struct GcBlock GcBlock;
typedef struct
{
    GcBlock *current;
} GcAlloc;

#define TYPES_BEGIN \
    typedef enum    \
    {

#define TYPES_END \
    }             \
    TypeKind;
#define TYPE(type, idx) TYPE_##type = idx,

#include "jazz.h"

TYPES_BEGIN

TYPE(VOID, 0)
TYPE(I8, 1)
TYPE(I16, 2)
TYPE(I32, 3)
TYPE(I64, 4)
TYPE(F32, 5)
TYPE(F64, 6)
TYPE(BOOL, 7)
TYPE(BYTES, 8)
TYPE(HYBRID, 9)
TYPE(FUN, 10)
TYPE(OBJ, 11)
TYPE(ARRAY, 12)
TYPE(TYPE, 13)
TYPE(REF, 14)
TYPE(VIRTUAL, 15)
TYPE(HYBRID_OBJ, 16)
TYPE(ABSTRACT, 17)
TYPE(ENUM, 18)
TYPE(FUN_METHOD, 19)
TYPE(NULL, 20)
TYPE(LAST, 21)

TYPES_END
typedef struct _FieldLookup FieldLookup;

typedef struct
{
    GcAlloc alloc;
    void **function_addrs;
    Type **functions_sigs;
} ModuleCtx;
typedef struct
{
    Type **params;
    Type *ret;
    int argc;
    Type *parent;
    struct
    {
        TypeKind kind;
        void *p;
    } closure_sig;
    struct
    {
        Type **params;
        Type *ret;
        int argc;
        Type *parent;
    } closure;
} FuncSig;

typedef struct
{
    uchar *name;
    Type *t;
    int hashed_name;
} ObjField;

typedef struct
{
    const uchar *name;
    int findex;
    int pindex;
    int hash;
} ObjProto;

typedef struct
{
    int nfields;
    int nprotos;
    int nbindings;
    uchar *name;
    Type *super;
    ObjField *fields;
    ObjProto *proto;
    int *bindings;
    void **global_value;
    ModuleCtx *module;
    RuntimeObj *rt;
} ObjSig;

typedef struct
{
    ObjField *fields;
    int nfields;
    int size;
    int *indexes;
    FieldLookup *lookup;
} VirtualSig;

typedef struct
{
    uchar *name;
    int nparams;
    Type **params;
    int size;
    bool ptr;
    int *offsets;
} EnumValue;

typedef struct
{
    uchar *name;
    int nvalues;
    EnumValue *values;
    void **global_value;
} EnumType;

struct Type
{
    TypeKind kind;
    union {
        uchar *abs_name;
        FuncSig *fun;
        ObjSig *obj;
        EnumType *enumt;
        VirtualSig *virt;
        Type *param;
    };
    void **proto;
    uint32_t *mark_bits;
};

extern EXPORT int
type_size(Type *t);
#define PAD_SIZE(size, t) \
    ((t)->kind == TYPE_VOID ? 0 : ((-(size)) & (type_size(t) - 1)))
extern EXPORT int
pad_struct(int size, Type *t);
extern EXPORT RuntimeObj *
jazz_get_runtime_obj(Type *t);
extern EXPORT RuntimeObj *
jazz_get_proto(Type *t);
extern EXPORT void
flush_proto(Type *t);
extern EXPORT void
jazz_init_enum(Type *et, ModuleCtx *module);

typedef unsigned char byte;

typedef struct
{
    Type *t;
    int __pad;
    union {
        bool b;
        unsigned char u8;
        unsigned short u16;
        int i;
        float f;
        double d;
        byte *bytes;
        void *ptr;
        i64 i64;
    } v;
} ValueDynamic;
typedef struct
{
    Type *t;
} ValueObj;

typedef struct _ValVirtual ValueVirtual;
struct _ValVirtual
{
    Type *t;
    ValueDynamic *value;
    ValueVirtual *next;
};
typedef struct
{
    Type *t;
    Type *at;
    int size;
    int __pad;
} ValueArray;

typedef struct
{
    Type *t;
    void *func;
    int has_value;
#ifdef JAZZ_X64
    int __pad;
#endif
    void *value;
} ValueClosure;

typedef struct
{
    ValueClosure closure;
    ValueClosure *func;
} ValueClosureWrap;

struct _FieldLookup
{
    Type *t;
    int hash;
    int index;
};
typedef struct
{
    void *ptr;
    Type *closure;
    int idx;
} RuntimeBinding;

struct RuntimeObj
{
    Type *t;
    int nfields;
    int nproto;
    int size;
    int nmethods;
    bool ptr;
    void **methods;
    int *fields_index;
    RuntimeBinding *bindings;
    RuntimeObj *parent;
    uchar **(*toString)(ValueDynamic *val);
    int (*cmp_func)(ValueDynamic *a, ValueDynamic *b);
    ValueDynamic *(*cast_func)(ValueDynamic *val, Type *t);
    ValueDynamic *(*get_field)(ValueDynamic *val, int field);
    int nlookup;
    FieldLookup *lookup;
};

typedef struct
{
    Type *t;
    FieldLookup *lookup;
    char *raw;
    void **values;
    int nfields;
    int raw_size;
    int nvalues;
    ValueVirtual *virtuals;
} ValueDynObj;

typedef struct
{
    Type *t;
    int index;
} ValueEnum;
extern EXPORT Type TypeVOID;
extern EXPORT Type TypeI32;
extern EXPORT Type TypeI64;
extern EXPORT Type TypeF64;
extern EXPORT Type TypeF32;
extern EXPORT Type TypeDyn;
extern EXPORT Type TypeArray;
extern EXPORT Type TypeBytes;
extern EXPORT Type TypeDynObj;
extern EXPORT Type TypeBool;
extern EXPORT Type TypeAbstract;

extern EXPORT double
jazz_nan(void);
extern EXPORT bool
ty_is_dynamic(Type *t);
#define val_is_ptr(v) ((v)->kind >= TYPE_BYTES)
extern EXPORT bool
is_same_type(Type *x, Type *y);
extern EXPORT bool
jazz_cast(Type *from, Type *to);

#define jazz_arr_ptr(a, t) ((t *)(((ValueArray *)(a)) + 1))

extern EXPORT ValueArray *
alloc_array(Type *t, int size);
extern EXPORT ValueDynamic *
alloc_dyn(Type *t);
extern EXPORT ValueDynamic *
alloc_obj(Type *t);
extern EXPORT ValueDynamic *
alloc_bool(bool b);
extern EXPORT ValueEnum *
alloc_enum(Type *t, int index);
extern EXPORT ValueDynObj *
alloc_dynobj();
extern EXPORT ValueVirtual *
alloc_virtual(Type *t);
extern EXPORT byte *
alloc_bytes(int size);
extern EXPORT byte *
copy_bytes(byte *bytes, int size);
extern EXPORT int
str_utf8_len(const byte *str, int at);
extern EXPORT int
str_from_utf8(uchar *out, int out_len, const char *str);
extern EXPORT char *
str_to_utf8(const uchar *bytes);
extern EXPORT uchar *
str_to_utf16(const char *str);
extern EXPORT ValueDynamic *
virtual_to_dyn(ValueVirtual *v);
extern EXPORT ObjField *
obj_get_field(Type *t, int fid);

extern EXPORT int
hash(byte *s);
extern EXPORT int
hash_utf8(char *str);
extern EXPORT int
hash_gen(uchar *name, bool cache);
extern EXPORT byte *
str_from_hash(int hash);

#define throw_error(msg, ...) \
    jazz_throw(alloc_strbytes(USTR(msg), ##__VA_ARGS__))
extern EXPORT ValueDynamic *
alloc_strbytes(const uchar *fmt, ...);
extern EXPORT
jazz_assert(void);
extern EXPORT
no_return(void jazz_throw(ValueDynamic *v));
extern EXPORT
no_return(void jazz_rethrow(ValueDynamic *v));
extern EXPORT void
setup_jump(void *j);
extern EXPORT void
prepare_exception(void *symbol, void *stack);
extern EXPORT void
dump_stack();
extern EXPORT ValueArray *
exception_stack();

extern EXPORT ValueVirtual *
cast_to_virtual(Type *ty, ValueDynamic *obj);
