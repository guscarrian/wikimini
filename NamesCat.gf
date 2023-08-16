--concrete NamesCat of Names = CatCat ** { lincat LN = PN; }

concrete NamesCat of Names = CatCat ** open Prelude, ResCat, CommonRomance, PhonoCat in {

param
  HasArt = NoArt | UseArt | AlwaysArt ;


lincat LN = {s  : Str;
             p  : Compl;
             art : HasArt;
             g : Gender;
             num : Number;
             } ;


lin PlainLN n = heavyNP {
      s = \\c => n.s; 
      a = {g = n.g ; n = n.num ; p = P3}
      } ;


lin InLN n = {
      s = n.p.s ++ case n.art of {
        AlwaysArt => artDef True n.g n.num n.p.c ++ n.s;
        _         => prepCase n.p.c ++ n.s
        } ;
  } ;

lin UseLN n = heavyNP {
      s = \\c => case n.art of {
        AlwaysArt | UseArt => artDef True n.g n.num c ++ n.s ;
        _      => n.s
        } ;
      a = {g = n.g ; n = n.num ; p = P3}
  } ;


oper
  --mkLN = overload {
  --  mkLN : Str -> Gender -> LN = \s,g ->
  --    lin LN {s = s ;
  --            p = "en" ;
  --            art = False ;
  --            g = g ;
  --            num = Sg} ;

  mkLN = overload {
    mkLN : Str -> Gender -> LN = \s,g ->
      lin LN {s = s ;
              p =  {s=""; c=CPrep P_a; isDir=True} ; --this
              art = NoArt ;
              g = g ;
              num = Sg} ;

    mkLN : Str -> Gender -> Number -> LN = \s,g,num ->
      lin LN {s = s ;
              p = {s=""; c=CPrep P_a; isDir=True} ; --this
              art = NoArt ;
              g = g ;
              num = num} ;
  } ;

--    mkLN : Str -> Gender -> Number -> LN = \s,g,num ->
--      lin LN {s = s ;
--              p = "en" ;
--              art = False ;
--              g = g ;
--              num = num} ;
--  } ;




--FRE
defLN : LN -> LN = \n -> n ** {art = AlwaysArt} ;
useDefLN : LN -> LN = \n -> n ** {art = UseArt} ;
prepLN : LN -> Compl -> LN = \n,s -> n ** {p = s} ;

--lin afghanistan_LN = prepLN (useDefLN (mkLN "Afghanistan" masculine)) (mkPrep "en") ;
--lin united_states_LN = prepLN (defLN (mkLN "États-Unis" masculine plural)) (dative) ;



}
