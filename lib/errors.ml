
(*Frontend*)
exception ParseError of string
exception IncludeError of string
exception RecCheckError of string
exception TypeError of string
exception VecCheckError of string
exception InterpError of string
exception LowerMonoTASTError of string
exception PrintError of string

(*Mir*)
exception MirError of string
exception MirSimError of string

(*Codegen*)
exception CodegenError of string
exception LlvmgenError of string
