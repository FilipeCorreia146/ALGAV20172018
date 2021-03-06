:- module(alphabeta, [alphabeta/6]).

alphabeta([X,play,Pos], Alpha, Beta, GoodPos, Val, Depth) :-
  Depth > 0,
  Depth1 is Depth-1,
  moves([X,play,Pos], PosList),!,
  PosList \=[],
  boundedbest(PosList, Alpha, Beta, GoodPos, Val, Depth1),!.

alphabeta(Pos,_,_,_,Val,0):-
   utility(Pos, Val).

alphabeta(Pos,_,_,_,Val,_):-
  utility(Pos,Val).

boundedbest([Pos|PosList], Alpha, Beta, GoodPos, GoodVal, Depth) :-
  alphabeta(Pos, Alpha, Beta, _, Val, Depth),
  goodenough(PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal, Depth).

goodenough([], _, _, Pos, Val, Pos, Val, _) :- !.

goodenough(_, Alpha, Beta, Pos, Val, Pos, Val, _) :-
  min_to_move(Pos), Val > Beta, !
  ;
  max_to_move(Pos), Val < Alpha, !.

goodenough(PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal, Depth) :-
  newbounds(Alpha, Beta, Pos, Val, NewAlpha, NewBeta),
  boundedbest(PosList, NewAlpha, NewBeta, Pos1, Val1, Depth),
  betterof(Pos, Val, Pos1, Val1, GoodPos, GoodVal).

newbounds(Alpha, Beta, Pos, Val, Val, Beta) :-
  min_to_move(Pos), Val > Alpha,!.

newbounds(Alpha, Beta, Pos, Val, Alpha, Val) :-
  max_to_move(Pos), Val < Beta, !.

newbounds(Alpha, Beta, _, _, Alpha, Beta).

betterof(Pos, Val, _Pos1, Val1, Pos, Val) :-
  min_to_move(Pos), Val > Val1, !
  ;
  max_to_move(Pos), Val < Val1, !.

betterof(_, _, Pos1, Val1, Pos1, Val1).
