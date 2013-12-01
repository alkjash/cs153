open Cfg
open Cfg_ast

let block1 = [Label "blk1"; Move (Var "x", Int 6);
Store (Reg(Mips.R4), 0, Var "x"); Load (Var "y",Reg(Mips.R4),0);
Store (Reg(Mips.R2),0,Var "y"); Return];;

let (gen,kill) = calc_gen_kill [block1] (empty_env,empty_env);;

VS.elements (gen block1 1);;

VS.elements (kill block1 0);; 