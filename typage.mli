exception Type_Error of Ast.loc*string

val typage : Ast.loc Ast.file -> Ast.c_type Ast.file
