project('Jazz','cpp','c')

cxx = meson.get_compiler('cpp')



llvm =  dependency('llvm')

deps = [
    llvm,
    cxx.find_library('clang'),
    cxx.find_library('clangAST'),
    cxx.find_library('clangAnalysis'),
    cxx.find_library('clangBasic'),
    cxx.find_library('clangDriver'),
    cxx.find_library('clangEdit'),
    cxx.find_library('clangFrontend'),
    cxx.find_library('clangIndex'),
    cxx.find_library('clangLex'),
    cxx.find_library('clangParse'),
    cxx.find_library('clangRewrite'),
    cxx.find_library('clangSema'),
    cxx.find_library('clangSerialization')
]

inc_dir = include_directories(
    [
        'package-manager',
        'mil',
        'src/eval',
        'parsing/ast',
        'src/semck',
        'backend/llvm',
        'backend/jit',
        'backend/',
        'parsing/',
        'src/'
    ]

)

src = [
    'parsing/ast/dump-ast.cpp',
    'parsing/ast/decl.cpp',
    'parsing/ast/expr.cpp',
    'parsing/ast/mangle.cpp',
    'parsing/ast/module.cpp',
    'parsing/ast/stmt.cpp',
    'parsing/ast/token.cpp',
    'parsing/ast/type.cpp',
    'package-manager/manifest.cpp',
    'package-manager/package-manager.cpp',
    'parsing/parse.cpp',
    'parsing/lex.cpp',
    'src/semck/decl.cpp',
    'src/semck/expr.cpp',
    'src/semck/stmt.cpp',
    'src/semck/typecheck.cpp',
    'src/semck/import_c.cpp',
    'src/utils.cpp',
    'backend/llvm/decl.cpp',
    'backend/llvm/stmt.cpp',
    'backend/llvm/gen.cpp',
    'backend/llvm/expr.cpp',
    'backend/llvm/value.cpp',
    'backend/llvm/eval.cpp',
    'src/eval/eval.cpp',
    'src/main.cpp'
]

add_global_arguments('-std=c++17', language : 'cpp')
executable('jazz',src,include_directories: inc_dir,dependencies: deps)
