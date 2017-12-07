:- module(connect4, [moves/2,min_to_move/1,max_to_move/1,utility/2,winPos/2,drawPos/2, isWin/2]).

moves(Pos,NextPosList):-
 findall(NextPos, move(Pos, NextPos), NextPosList).

move([X1, play, Board], [X2, win, NextBoard]) :-
    nextPlayer(X1, X2),
    move_aux(X1,7,Board, NextBoard),
    isWin(X1, NextBoard), !.

move([X1, play, Board], [X2, draw, NextBoard]) :-
    nextPlayer(X1, X2),
    move_aux(X1,7,Board, NextBoard),
    drawPos(X1,NextBoard), !.

move([X1, play, Board], [X2, play, NextBoard]) :-
    nextPlayer(X1, X2),
    move_aux(X1,7,Board, NextBoard).

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
utility([o, win, _], 1).       % Previous player (MAX) has win.
utility([x, win, _], -1).      % Previous player (MIN) has win.
utility([_, draw, _], 0).

isWin(P,L):-
 LPos = [1,2,3,4,8,9,10,11,15,16,17,18],
 testWin(P,L,LPos).

testWin(_,_,[]):- fail.

testWin(P,L,[H|T]):-
  Pos2 is H +1, Pos3 is H+2, Pos4 is H+3,
 Pos5 is H+7, Pos6 is H+8, Pos7 is H+9, Pos8 is H+10,
 Pos9 is H+14, Pos10 is H+15, Pos11 is H+16, Pos12 is H+17,
 Pos13 is H+21, Pos14 is H+22, Pos15 is H+23, Pos16 is H+24,

 nth1(H,L,V1),nth1(Pos2,L,V2), nth1(Pos3,L,V3), nth1(Pos4,L,V4),
 nth1(Pos5,L,V5), nth1(Pos6,L,V6), nth1(Pos7,L,V7), nth1(Pos8,L,V8),
 nth1(Pos9,L,V9), nth1(Pos10,L,V10), nth1(Pos11,L,V11), nth1(Pos12,L,V12),
 nth1(Pos13,L,V13), nth1(Pos14,L,V14), nth1(Pos15,L,V15), nth1(Pos16,L,V16),
 winPos(P,[V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16]);
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





































