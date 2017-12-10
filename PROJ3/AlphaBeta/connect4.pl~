:- module(connect4, [moves/2,min_to_move/1,max_to_move/1,utility/2,winPos/2,drawPos/2, isWin/2]).

moves(Pos,NextPosList):-
 findall(NextPos, move(Pos, NextPos), NextPosList).

move([X1, play, Board], Ret) :-
    nextPlayer(X1, X2),
    move_aux(X1,7,Board, NextBoard),
    (winMove(X1,X2,NextBoard, Ret);
     threeMove(X1,X2,NextBoard, Ret);
     twoMove(X1,X2,NextBoard,Ret);
     oneMove(X1,X2,NextBoard,Ret);
     drawMove(X1,X2,NextBoard,Ret);
    Ret = [X2, play, NextBoard]).


winMove(X1,X2,NextBoard,[X2,win,NextBoard]):-
 isWin(X1, NextBoard),!.

threeMove(X1,X2,NextBoard,[X2,threeInLine, S, NextBoard]):-
 threeInLine(X1,NextBoard,S),!.

twoMove(X1,X2,NextBoard,[X2,twoInLine, S, NextBoard]):-
 twoInLine(X1,NextBoard,S),!.

oneMove(X1,X2,NextBoard,[X2,one,S, NextBoard]):-
 one(X1,NextBoard,S),!.

one(P,L,S):-
 testOne(P,L,S1),
 nextPlayer(P,X),
 testOne(X,L,S2),
 ST is S1-S2,
 ST \= 0,
 S is ST;
 fail.

testOne(_,[],0).

testOne(P,[H|T],S1):-
 testOne(P,T,S),
 ( H == P,
 S1 is S +1;
  S1 is S).

drawMove(X1,X2,NextBoard,[X2,draw,NextBoard]):-
 drawPos(X1,NextBoard),!.

%move([X1, play, Board], [X2, threeInLine, S, NextBoard]) :-
%    nextPlayer(X1, X2),
%    move_aux(X1,7,Board, NextBoard),
%    threeInLine(X1,NextBoard,S), !.

%move([X1, play, Board], [X2, draw, NextBoard]) :-
%    nextPlayer(X1, X2),
%    move_aux(X1,7,Board, NextBoard),
%    drawPos(X1,NextBoard), !.

%move([X1, play, Board], [X2, play, NextBoard]) :-
%    nextPlayer(X1, X2),
%    move_aux(X1,7,Board, NextBoard).

 %True if NextBoard is Board whith an empty case replaced by Player mark.
move_aux(P,1,L,LRet):-
 placeAtBottom(P,1,L,LRet),!.

move_aux(P,Pos,L,LRet):-
 Pos>1,
 placeAtBottom(P,Pos,L,LRet);
 Pos1 is Pos -1,
 move_aux(P,Pos1,L,LRet).

placeAtBottom(P,Pos, L, LRet):-
   (Pos1 is Pos +35,
    nth1(Pos1,L,X,_),
    X == 0,!;
    Pos1 is Pos + 28,
    nth1(Pos1,L, X,_),
    X ==0,!;
    Pos1 is Pos +21,
     nth1(Pos1,L, X,_),
    X ==0,!;
    Pos1 is Pos + 14,
     nth1(Pos1, L, X,_),
    X ==0,!;
    Pos1 is Pos + 7,
     nth1(Pos1, L, X,_),
    X ==0,!;
    Pos1 is Pos ,
     nth1(Pos1, L, X,_),
    X ==0, !),
   place(P,Pos1,L,LRet).

place(P,1,[_|L],[P|L]).

place(P,Pos,[H|T],[H|LRet]):-
  Pos1 is Pos-1,
  place(P,Pos1,T,LRet).

% min_to_move(+Pos)
% True if the next player to play is the MIN player.
min_to_move([o, _, _]).

% max_to_move(+Pos)
% True if the next player to play is the MAX player.
max_to_move([x, _, _]).


% utility(+Pos, -Val) :-
% True if Val the the result of the evaluation function at Pos.
% We will only evaluate for final position.
% So we will only have MAX win, MIN win or draw.
% We will use  1 when MAX win
%             -1 when MIN win
%              0 otherwise.
utility([o, win, _], 999).       % Previous player (MAX) has win.
utility([x, win, _], -999).      % Previous player (MIN) has win.
utility([_, draw, _], 0).
utility([_,threeInLine,S,_],R):-
 R is S*10.
utility([_,twoInLine,S,_],R):-
 R is S*5.
utility([_,one,S,_],R):-
 R is S.
utility([_,_,_],0).

isWin(P,L):-
 LPos = [1,2,3,4,8,9,10,11,15,16,17,18],
 testWin(P,L,LPos).

testWin(_,_,[]):- fail.

testWin(P,L,[H|T]):-
 allElements(H,[],LE,4,4,16),
 smallerList(LE,L,RL),
 winPos(P,RL);
 testWin(P,L,T).

% winPos(+Player, +Board)
% True if Player win in Board.
winPos(P, [X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16]) :-
 equal(X1, X2, X3, X4, P) ; % 1st line
 equal(X5, X6, X7, X8, P) ; % 2nd line
 equal(X9, X10, X11, X12, P) ; % 3rd line
 equal(X13, X14, X15, X16, P); % 4th line
 equal(X1, X5, X9, X13, P) ; % 1st col
 equal(X2, X6, X10, X14, P) ; % 2nd col
 equal(X3, X7, X11, X15, P) ; % 3rd col
 equal(X4, X8, X12, X16, P) ; % 4th col
 equal(X1, X6, X11, X16, P) ; % 1st diag
 equal(X4, X7, X10, X13, P). % 2nd diag

% drawPos(+Player, +Board)
% True if the game is a draw.
drawPos(_,Board) :-
    \+ member(0, Board).


% equal(+W, +X, +Y, +Z, +T).
% True if W = X = Y = Z = T.
equal(X, X, X, X, X).

threeInLine(P,L,S):-
 LPos = [1,2,3,4,5,8,9,10,11,12,15,16,17,18,19,22,23,24,25,26],
 testThreeInLine(P,L,LPos,S1),
 nextPlayer(P,X),
 testThreeInLine(X,L,LPos,S2),
 ST is S1-S2,
 ST\=0,
 S is ST;
 fail.

testThreeInLine(_,_,[],0).

testThreeInLine(P,L,[H|T],S1):-
   testThreeInLine(P,L,T,S),
  allElements(H,[],LE,3,3,9),
  smallerList(LE,L,RL),
  (   threeInLinePos(P,RL),
  S1 is S+1;
  S1 is S).

threeInLinePos(P, [X1,X2,X3,X4,X5,X6,X7,X8,X9]):-
 equal(X1,X2,X3,P);
 equal(X4,X5,X6,P);
 equal(X7,X8,X9,P);
 equal(X1,X5,X9,P);
 equal(X7,X5,X4,P);
 equal(X1,X4,X7,P);
 equal(X2,X5,X8,P);
 equal(X3,X6,X9,P).

equal(X,X,X,X).

twoInLine(P,L,S):-
 LPos = [1,2,3,4,5,6,8,9,10,11,12,13,15,16,17,18,19,20,22,23,24,25,26,27,29,30,31,32,33,34],
 testTwoInLine(P,L,LPos,S1),
 nextPlayer(P,X),
 testTwoInLine(X,L,LPos,S2),
 ST is S1-S2,
 ST \= 0,
 S is ST;
 fail.

testTwoInLine(_,_,[],0).

testTwoInLine(P,L,[H|T],S1):-
  testTwoInLine(P,L,T,S),
  allElements(H,[],LE,2,2,4),
  smallerList(LE,L,RL),
  ( twoInLinePos(P,RL),
  S1 is S+1;
  S1 is S).

twoInLinePos(P, [X1,X2,X3,X4]):-
 equal(X1,X2,P);
 equal(X3,X4,P);
 equal(X1,X3,P);
 equal(X2,X4,P);
 equal(X1,X4,P);
 equal(X2,X3,P).

equal(X,X,X).

smallerList([],_,RL):-
 RL = [].

smallerList([H|T],L,[E|RL]):-
  smallerList(T,L,RL),
  nth1(H,L,E).

allElements(_,List,List,0,_,0).

allElements(S,List,LE,0,C,T):-
 ST is S-C,
 S1 is ST+7,
 L1 is C-1,
 T1 is T-1,
 S2 is S1+1,
 allElements(S2,[S1|List],LE,L1,C,T1).

allElements(S,List,LE,L,C,T):-
 S1 is S+1,
 L1 is L-1,
 T1 is T-1,
 allElements(S1,[S|List],LE,L1,C,T1),!.






























