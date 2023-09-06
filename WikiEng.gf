--# -path=.:../gf-wordnet

concrete WikiEng of Wiki =
  GrammarEng [
  Phr, Utt, Pol, ListNP, Adv, Comp, VPSlash, Tense, Card, Cl, Voc, AP,
  Num, S, Conj, Det, NP, Temp, Ant, Quant, Dig, CN, Digits, VP, PConj, Pron,
  Prep, A, V2, N, PN, ListS, AdV, ListAdv, DAP, Decimal, RP, Ord, RCl, RS,
  LN, GN, SN, AdA, V3, MU,
  ASimul,
  AdAP,
  AdVVP,
  AdjCN,
  AdjOrd,
  AdvAP,
  AdvCN,
  AdvNP,
  AdvVP,
  AdvVPSlash,
  ApposCN,
  BaseNP,
  BaseS,
  CompAP,
  CompAdv,
  CompNP,
  ComplSlash,
  CompoundN,
  ConjNP,
  ConjS,
  ConsNP,
  D_0,
  D_1,
  D_2,
  D_3,
  D_4,
  D_5,
  D_6,
  D_7,
  D_8,
  D_9,
  DefArt,
  DetCN,
  DetDAP,
  DetQuant,
  ExtAdvS,
  ExtRelNP,
  FullName,
  FullStop,
  GivenName,
  IDig,
  IFrac,
  IIDig,
  IdRP,
  IndefArt,
  MaleSurname,
  MassNP,
  NegDecimal,
  NoPConj,
  NoVoc,
  NumCard,
  NumDecimal,
  NumPl,
  NumSg,
  OrdSuperl,
  PNeg,
  PPos,
  PassAgentVPSlash,
  PassVPSlash,
  PhrUtt,
  PhrUttMark,
  PosDecimal,
  PositA,
  PossNP,
  PossPron,
  PredVP,
  PrepNP,
  QuantityNP,
  RelVP,
  Slash2V3,
  SlashV2a,
  TPast,
  TPres,
  TTAnt,
  UseCl,
  UseComp,
  UseDAP,
  UseLN,
  UseN,
  UsePN,
  UsePron,
  UseRCl,
  UttS,
  above_Prep,
  after_Prep,
  also_AdV,
  and_Conj,
  as_Prep,
  but_1_Conj,
  dollar_MU,
  european_union_NP,
  for_Prep,
  from_Prep,
  gen_Quant,
  he_Pron,
  in_1_Prep,
  it_Pron,
  of_1_Prep,
  out_of_3_Prep,
  partly_AdA,
  per_Prep,
  per_capita_Adv,
  percent_MU,
  she_Pron,
  this_Quant,
  to_1_Prep,
  to_2_Prep,
  very_AdA,
  with_Prep
{- now in Mini
  also_AdV,
  area_6_N,
  but_1_Conj,
  capital_3_N,
  coat_of_arms_N,
  east_4_N,
  flag_1_N,
  have_1_V2,
  in_1_Prep,
  inhabitant_1_N,
  island_1_N,
  kilometre_1_N,
  language_1_N,
  nordic_2_A,
  north_3_N,
  north_east_N,
  northwest_2_N,
  official_1_A,
  on_1_Prep,
  south_3_N,
  south_east_N,
  south_west_N,
  speak_3_V2,
  spoken_A,
  square_1_A,
  state_3_N,
  to_1_Prep,
  west_2_N,
  with_Prep
-}
  ],
ExtendEng [
  N, VP, VPSlash,
  CompoundN,
  PassVPSlash
  ],
-- MiniEng
WordsEng **
open
  Prelude in {
lincat Mark = {s : Str} ;

lin PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;
lin FullStop  = {s = "."} ;

}
