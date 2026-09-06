(*

  Collection of specific Exceptions for different parts of the compiler

*)


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

(*Backend*)
exception LlvmgenError of string
