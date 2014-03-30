module ErlHaskExamples where

start = unlines [
  "module 'start' ['main'/0,",
  "        'module_info'/0,",
  "        'module_info'/1]",
  "    attributes []",
  "'main'/0 =",
  "    %% Line 5",
  "    fun () ->",
  "    let <X> =",
  "        %% Line 6",
  "        call 'simple':'do'",
  "        (1)",
  "    in  %% Line 7",
  "        call 'erlang':'display'",
  "        (X)",
  "'module_info'/0 =",
  "    fun () ->",
  "    call 'erlang':'get_module_info'",
  "        ('start')",
  "'module_info'/1 =",
  "    fun (_cor0) ->",
  "    call 'erlang':'get_module_info'",
  "        ('start', _cor0)",
  "end  "
  ]

twoLetMod = unlines [
  "module 'simple' ['main'/0]",
  "    attributes []",
  "'main'/0 = ",
  "%% Line 5",
  "fun () ->",
  "    let <X> =",
  "        %% Line 6",
  "        call 'random':'uniform'",
  "            ()",
  "    in  let <Y> =",
  "            %% Line 7",
  "            call 'erlang':'+'",
  "                (X, 2)",
  "        in  %% Line 8",
  "             call 'erlang':'display'",
  "                 (Y)",
  "end"]
            -- "            call 'io':'format'",
    -- "                ([126|[112]], [Y|[]])",

simpleModule = unlines [
    "module 'simple' ['main'/0]",
    "    attributes []",
    "'main'/0 =",
    "    fun () ->",
    "       let <X> =",
    "           call 'erlang':'date'",
    "               ()",
    "       in",
    "           call 'erlang':'display'",
    "               (X)",
    "end"]

-- Constr (Module (Atom "simple")
--         [Function (Atom "main",0)]
--         []
--         [FunDef (Constr (Function (Atom "main",0)))
--          (Constr (Lambda []
--                   (Exp (Constr (Let (["X"],
--                                      Exp (Constr
--                                           (ModCall (Exp (Constr (Lit (LAtom (Atom "mod")))),
--                                                     Exp (Constr (Lit (LAtom (Atom "read"))))) [])))
--                                 (Exp (Constr
--                                       (ModCall (Exp (Constr (Lit (LAtom (Atom "erlang")))),
--                                                 Exp (Constr (Lit (LAtom (Atom "display")))))
--                                        [Exp (Constr (Var "X"))])
--                                      ))
--                                )))
--                  ))
--         ])



-- (Module (Atom "simple")
--  [Function (Atom "main",0)]
--  []
--  [FunDef (Constr (Function (Atom "main",0)))
--   (Constr (Lambda [] (Exp
--                       (Constr
--                        (Let (["X"],
--                              Exp (Constr (ModCall (Exp (Constr (Lit (LAtom (Atom "random")))),
--                                                    Exp (Constr (Lit (LAtom (Atom "uniform"))))) [])))
--                         (Exp (Constr
--                               (Let (["Y"],
--                                     Exp (Constr (ModCall (Exp (Constr (Lit (LAtom (Atom "erlang")))),
--                                                           Exp (Constr (Lit (LAtom (Atom "+")))))
--                                                  [Exp (Constr (Var "X")),
--                                                   Exp (Constr (Lit (LInt 2)))]
--                                                 )))
--                                (Exp (Constr
--                                      (ModCall (Exp (Constr (Lit (LAtom (Atom "erlang")))),
--                                                Exp (Constr (Lit (LAtom (Atom "display")))))
--                                       [Exp (Constr (Var "X"))])))))))))))])
