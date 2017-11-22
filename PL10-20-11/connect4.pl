:- module(connect4, [moves/2,min_to_move/1,max_to_move/1,utility/2,winPos/2,drawPos/2]).

moves(Pos,NextPosList):-
 %findall(NextPos, move(Pos, NextPos), NextPosList).
 %DEPTH == 3
 findnsols(3, NextPos, move(Pos, NextPos), NextPosList).

move([X1, play, Board], [X2, win, NextBoard]) :-
    nextPlayer(X1, X2),
    move_aux(X1, Board, NextBoard),
    winPos(X1, NextBoard), !.

move([X1, play, Board], [X2, draw, NextBoard]) :-
    nextPlayer(X1, X2),
    move_aux(X1, Board, NextBoard),
    drawPos(X1,NextBoard), !.

move([X1, play, Board], [X2, play, NextBoard]) :-
    nextPlayer(X1, X2),
    move_aux(X1, Board, NextBoard).

% True if NextBoard is Board whith an empty case replaced by Player mark.
move_aux(P, [0|Bs], [P|Bs]).

move_aux(P, [B|Bs], [B|B2s]) :-
    move_aux(P, Bs, B2s).


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







