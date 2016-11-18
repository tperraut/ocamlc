%{

  open Astcommon
  open Ast

  let current_pos () =
    Parsing.symbol_start_pos (),
    Parsing.symbol_end_pos ()

  let mk_for id e_start e_end b =
    Iblock [ Idecl_var (id, Tint);
	     Iassign (id, e_start);
	     Iwhile (Ebinop(Le, Eident id, e_end),
		     b @ [ Iassign (id, Ebinop(Plus,
					       Eident id,
					       Econst(Cint 1))) ]
	     )
	   ]
%}

%token <int> CONST_INT
%token PLUS MINUS MULT DIV
%token <bool> CONST_BOOL
%token AND OR NOT
%token EQ NEQ
%token GE GT LE LT
%token IF THEN ELSE
%token LPAREN RPAREN
%token COMMA
%token <Ast.ident> IDENT
%token VAR ASSIGN
%token FUN RETURN
%token PRINT NEWLINE EXIT
%token SEMI
%token BEGIN END
%token WHILE FOR TO
%token LBRACKET RBRACKET
%token COLON
%token INT BOOL ARRAY
%token EOF

%nonassoc ELSE
%left OR
%left AND
%left NOT
%left EQ NEQ GE GT LE LT
%left PLUS MINUS
%left MULT DIV
%left LBRACKET

%start prog
%type <Ast.prog> prog

%%

prog:
| instrs=list(instr); EOF { instrs }
;
  
block:
| BEGIN; instrs=list(instr); END      { instrs              }
;

typed_ident:
| id=IDENT; COLON; ty=typ             { id, ty              }
;
    
typ:
| INT           { Tint    }
| BOOL          { Tbool   }
| ty=typ ARRAY  { Tarr ty }
;

opt_typ:
| COLON ty=typ  { Some ty }
| (* nothing *) { None    }
    
instr:
| VAR; tid=typed_ident SEMI  { let id, ty = tid in Idecl_var (id, ty)  }
| FUN; id=IDENT;
  LPAREN; typed_params=separated_list(COMMA, typed_ident); RPAREN;
  oty=opt_typ;
  b=block                             {
    let params = List.map fst typed_params
    and param_types = List.map snd typed_params
    in  Idecl_fun (id, params, { return_ty=oty; params_ty=param_types }, b)
  }
| id=IDENT; ASSIGN; e=expr; SEMI      { Iassign (id, e)     }
| f=field_expr; ASSIGN; e=expr; SEMI
                    { let e1, e2 = f in Isetarr (e1, e2, e) }
| PRINT; e=expr; SEMI                 { Iprint e            }
| b=block                             { Iblock b            }
| WHILE; e=expr; b=block              { Iwhile (e, b)       }
| IF; c=expr; THEN; b1=block; ELSE; b2=block
                                      { Iif (c, b1, b2)     }
| FOR; id=IDENT; ASSIGN; e_start=expr; TO; e_end=expr; b=block
                                      { mk_for id e_start e_end b }
| NEWLINE; SEMI                       { Inewline            }
| c=call; SEMI                        { Icall c             }
| RETURN; e=expr; SEMI                { Ireturn e           }
| EXIT; SEMI                          { Iexit               }
;
    
expr:
| c=const                                  { c                   }
| id=IDENT                                 { Eident id           }
| LPAREN; s=expr; RPAREN                   { s                   }
| op=unop; e=expr                          { Eunop (op, e)       }
| e1=expr; op=binop; e2=expr               { Ebinop (op, e1, e2) }
| IF; c=expr; THEN; e1=expr; ELSE; e2=expr { Eif (c, e1, e2)     }
| ty=typ; LBRACKET; e=expr; RBRACKET       { Enewarr (ty, e)     }
| f=field_expr           { let e1, e2 = f in Egetarr (e1, e2)    }
| c=call                                   { Ecall c             }
;

call:
| id=IDENT; LPAREN; params=separated_list(COMMA, expr); RPAREN;
                                           { id, params          }
;
					     
					   
field_expr:
| e1=expr; LBRACKET; e2=expr; RBRACKET     { (e1, e2)            }
;

const:
| i=CONST_INT  { Econst (Cint i)  }
| b=CONST_BOOL { Econst (Cbool b) }
;

%inline binop:
| PLUS  { Plus  }
| MINUS { Minus }
| MULT  { Mult  }
| DIV   { Div   }
| AND   { And   }
| OR    { Or    }
| EQ    { Eq    }
| NEQ   { Neq   }
| LT    { Lt    }
| LE    { Le    }
| GT    { Gt    }
| GE    { Ge    }
;

%inline unop:
| MINUS { Uminus }
| NOT   { Not    }
;
