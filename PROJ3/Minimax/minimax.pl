:- module(minimax, [minimax/4]).

% minimax(Pos, BestNextPos, Val)
% Pos is a position, Val is its minimax value.
% Best move from Pos leads to position BestNextPos.
minimax([X,play,List], BestNextPos, Val, Depth) :-
    Depth > 0,
    Depth1 is  Depth-1,
    moves([X,play,List],NextPosList),
    best(NextPosList, BestNextPos, Val, Depth1),!.

minimax(Pos,_,Val,0):-
    utility(Pos,Val).

minimax(Pos,_,Val,_):-
    utility(Pos,Val).

best([Pos], Pos, Val, Depth) :-
    minimax(Pos, _, Val, Depth), !.

best([Pos1 | PosList], BestPos, BestVal, Depth) :-
    minimax(Pos1, _, Val1, Depth),
    best(PosList, Pos2, Val2, Depth),
    betterOf(Pos1, Val1, Pos2, Val2, BestPos, BestVal).



betterOf(Pos0, Val0, _, Val1, Pos0, Val0) :-   % Pos0 better than Pos1
    min_to_move(Pos0),                         % MIN to move in Pos0
    Val0 > Val1, !                             % MAX prefers the value
    ;
    max_to_move(Pos0),                         % MAX to move in Pos0
    Val0 < Val1, !.                            % MIN prefers the lesser value

betterOf(_, _, Pos1, Val1, Pos1, Val1).        % Otherwise Pos1 better than Pos0









