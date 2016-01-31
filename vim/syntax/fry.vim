if exists("b:current_syntax")
  finish
endif

syn region    fryString        start=+\(L\|u\|u8\|U\|R\|LR\|u8R\|uR\|UR\)\="+ skip=+\\\\\|\\"+ end=+"+ contains=cSpecial,cFormat,@Spell extend

syn keyword fryKeyword fn while if else end of var for struct this typeclass instance data switch case as package blackjack hookers
syn keyword fryStorage static public private native
syn keyword fryStatement return
syn keyword fryType auto

syn region fryInfix start=+infix\(r\|l\)+ end=+\n+ contains=fryInfixFunction,fryInfixKeyword,fryOperator,fryNumber
syn keyword fryInfixKeyword infixr infixl contained
syn match fryInfixFunction +[A-Za-z_][A-Za-z0-9_]*+ contained

hi link fryInfixKeyword fryKeyword
hi link fryInfixFunction fryFunction

syn match fryAnnotation +@\w\+\(([^)]*)\)\?+ contains=fryAnnotationName
syn match fryAnnotationName +@\w\++ contained
syn match fryAnnotationValue +([^)]*)+ contained
syn match fryHasType +[^"A-Za-z0-9_][A-Z]\(\w\|\d\|_\)*+ contains=fryNamedType
syn match fryNamedType +[A-Z]\(\w\|\d\|_\)*+ contained
syn match fryImportRegion +import\s\+[A-Za-z:]\+\(\s\+as\s\+[A-Za-z:]\+\|(\s*\w\+\s*\(,\s*\w\+\s*\)*)\)\?+ contains=fryImport,fryImportPathSep,fryImportPath,fryNamedType
syn match fryFnCall +[a-z_][A-Za-z0-9_]*(+ contains=fryFunction
syn match fryHasType2 +:+
syn match fryNumber +\(0x[0-9]\+\|[0-9]\+\)\(L\|l\)\?+

syn match fryLambdaFn +lam\s\+\(\w\+\(\s\+\w\+\)*\s*\)\?\.+ contains=fryLambdaDelim
syn match fryLambdaDelim +[.()]\|lam+ contained

syn match fryOperator +[+\-><~!#$%=|*&@]+

syn keyword fryImport import as contained
syn match fryImportPathSep +::+ contained
syn match fryImportPath +[A-Za-z]\++ contained
syn match fryFunction +[a-z_][A-Z0-9a-z_]*+ contained

syn region fryComment start=+#+ end=+\n+

hi link fryComment Comment

hi link fryNumber Number
hi link fryAnnotationValue PreProc
hi link fryAnnotationName Function
hi link fryAnnotation PreProc
hi link fryStorage StorageClass
hi link fryFunction Function
hi link fryLambdaDelim Delimiter
hi link fryHasType2 Delimiter
hi link fryOperator Operator
hi link fryNamedType fryType
hi link fryImport Keyword
hi link fryImportPathSep Delimiter
hi link fryImportPath Include


" syn region    qbString        start=+\(L\|u\|u8\|U\|R\|LR\|u8R\|uR\|UR\)\="+ skip=+\\\\\|\\"+ end=+"+ contains=cSpecial,cFormat,@Spell extend
"
" syn match qbPackageAccess +/*\w\+/\w\++ contains=qbPackageAccessHighlight
" syn match qbPackageAccessHighlight +\w\+/+ contained
" syn match qbOpenBrace +[{]+ contained
"
" syn keyword qbTypeKeyword data type contained
" syn keyword qbFunctionKeyword let extern private contained
" syn keyword qbWhere where
"
" hi def link qbWhere qbFunctionKeyword
" syn match qbDot +\.+ contained
"
" syn match qbOperator +[->+<!@#$%^&*,:.=/]\+\|\~+
" syn match qbAnonymousType +'\w+
"
" syn cluster qbContained contains=qbOperator,qbAnonymousType,qbPackageAccess
"
" " {{{
" syn match qbLetStmt +\(let\|type\)\(\s\+\w\+\)\+\s*:+ nextgroup=qbTypeSignature contains=qbFunctionRegion,qbFunctionKeyword,qbTypeKeyword
" syn match qbUntypedLetStmt +\(let\|type\)\(\s\+\w\+\)\+\s*=+ contains=qbFunctionRegion,qbFunctionKeyword,qbTypeKeyword
" syn match qbFunctionRegion +\w\+\s*\(:\|=\)+ contains=qbFunctionName contained
"
" syn match qbFunctionName +\w\++ contained
" syn match qbTypeSignature +[^;=]\++ contains=@qbContained,qbKind,qbTypeContext contained
" syn match qbTypeContext +[^|]\+|+ contained contains=qbTypeContextFirst,qbTypeContextDelimiter
" syn match qbTypeContextDelimiter +[,|]+ contained
"
" syn match qbTypeContextFirst +\w\++ contained nextgroup=qbTypeContextFirstParams
" syn match qbTypeContextFirstParams +\(\s\+\w\+\)*+ contained nextgroup=qbTypeContextRest
"
" syn match qbTypeContextRest +\s*,\s*\w\++ contained nextgroup=qbTypeContextRestParams contains=qbTypeContextDelimiter
" syn match qbTypeContextRestParams +\(\s\+\w\+\)*+ contained nextgroup=qbTypeContextRest
"
" syn match qbKind +\*+ contained
"
" hi link qbTypeContextFirst qbTypeContextRest
" hi link qbTypeContextRest Special
"
" hi link qbTypeContextFirstParams qbTypeContextRestParams
" hi link qbTypeContextRestParams TypeThe
" " }}}
"
" " {{{
" syn match qbDataDeclaration +data[^.]\+[.{]+ contains=qbDataStmt,qbKindSignature,@qbContained,qbKind,qbDot,qbOpenBrace
" syn match qbDataStmt +\(data\|type\)\(\s\+\w\+\)\s*:+ nextgroup=qbDataKindSignature contains=qbTypeRegion,qbTypeKeyword contained
" syn match qbTypeRegion +\w\+\s*:+ contains=qbDeclaredTypename contained
" syn match qbDeclaredTypename +\w\++ contained
" " }}}
"
" " {{{
" syn match qbCaseStmtGlobal +case[^.]\+[.{]+ contains=qbCase,qbDot,qbOpenBrace,@qbContained
" syn keyword qbCase case contained
" " }}}
"
" " {{{
" syn match qbLambdaFunction +\\[^.]\+[.{]+ contains=qbDot,qbOpenBrace,qbLambdaBackslash,qbPatternMatch
" syn match qbLambdaBackslash +\\+ contained
"
" syn region qbPatternMatch start=+\(\w\|_\)\+@(+ end=+)+ transparent contains=qbPatternMatch,qbPatternBind
" syn match qbPatternBind +\(\w\+\|_\)@(+ nextgroup=TypeWord contains=qbBind,qbAt,qbBindParen
" syn match TypeWord +\w\++ contained
" syn match qbAt +@+ contained
" syn match qbBind +^\w\++ contained
" syn match qbBindParen +[()]+
" hi def link qbBindParen qbLambdaFunction
" hi def link qbBind qbLambdaFunction
" " }}}
"
" " {{{
" syn region qbComment start=+/\*+ end=+\*/+
" syn region qbComment start=+\*\*\*+ end=+\n+
" " }}}
"
" " {{{
" syn match qbClassDeclaradion +class[^.]\+[.{]+ contains=qbDot,qbOpenBrace,qbClass
" syn keyword qbClass class contained nextgroup=qbClassContext
" syn match qbClassContext +[^|]\+|\|+ contained contains=qbTypeContext nextgroup=qbClassName
" syn match qbClassName +\s\+\(\w\|/\|_\)\++ contained nextgroup=qbClassParam
" syn match qbClassParam +\s\+\(\w\|/\|_\)\++ contained nextgroup=qbClassParam
"
" hi link qbClass qbTypeKeyword
" hi link qbClassName qbTypeContextFirst
" hi link qbClassParam qbTypeContextFirstParams
" " }}}
" "
" " {{{
" syn match qbInstaceDeclaration +instance[^.]\+[.{]+ contains=qbDot,qbOpenBrace,qbInstance
" syn keyword qbInstance instance contained nextgroup=qbInstanceContext
" syn match qbInstanceContext +[^|]\+|\|+ contained contains=qbTypeContext nextgroup=qbClassName
"
" hi link qbInstance qbTypeKeyword
" " }}}
"
" " {{{
"
" syn keyword qbInfix infixr infixl nextgroup=qbInfixName
" syn match qbInfixName +\s\+\w\++ contained nextgroup=qbInfixOperator
" syn match qbInfixOperator +\s\+(\([->+<!@#$%^&*=]\+\|\~\)\+)+ contained
"
" hi link qbInfix Keyword
" hi link qbInfixName qbFunctionName
" hi link qbInfixOperator qbOperator
"
" " }}}
"
" " {{{
" syn match qbImportStmt +import[^.]\+[.{]+ contains=qbImport,qbDot,qbOpenBrace
" syn keyword qbImport import contained nextgroup=qbImportPath
" syn match qbImportPath +\s\+\(\w\|/\)\+\s*+ contained nextgroup=qbImportAs
" syn keyword qbImportAs as contained nextgroup=qbImportAsWhat
" syn keyword qbImportAsWhat +\w\++ contained
"
"
" hi link qbImport Include
" hi link qbImportAs qbImport
" hi link qbImportAsWhat TypeKeyword
"
" syn match qbPackageStmt +package[^.]\+[.{]+ contains=qbPackage,qbDot,qbOpenBrace
" syn keyword qbPackage package contained nextgroup=qbPackagePath
" syn match qbPackagePath +\s\+\(\w\|/\)\+\s*+ contained
"
" hi link qbPackage Statement
" " }}
"
" syn keyword qbExport export nextgroup=qbExportPackage
" syn match qbExportPackage +\s\+package+ contains=qbPackage nextgroup=qbImportPath
" hi link qbExport qbImport
"
"
" syn match qbNumber +[1-9][0-9]*+
" syn match qbNumber +0[1-9][0-9]*+
" syn match qbNumber +0x[1-9][0-9]*+
" syn match qbNumber +0+
"
" hi link qbTypeContextFirstParams Type
" hi link qbOperator Operator
" hi link qbFunctionKeyword Keyword
" hi link qbTypeKeyword Structure
" hi link qbStatement Statement
" hi link qbFunctionName Function
" hi link qbKind Type
" hi link qbDot Delimiter
" hi link qbDeclaredTypename Type
" hi link qbAnonymousType Type
" hi link qbCase Statement
" hi link qbLambdaBackslash qbDot
" hi link qbOpenBrace Normal
" hi link TypeWord Type
" hi link qbAt Operator
" hi link qbLambdaFunction Constant
" hi link qbTypeContextDelimiter Delimiter
" hi link qbComment Comment
" hi link qbNumber Number
hi link fryString String
hi link fryKeyword Keyword
hi link fryStatement Statement
hi link fryType Type
" hi link qbPackageAccessHighlight Include
