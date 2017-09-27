aminhalista([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 , 11, 12 ]).

%a)
contar([],0).
contar([_|T],C):-
    contar(T,C1),
    C is C1+1.

somar([],0).
somar([H|T], S):-
    somar(T,S1),
    S is S1+H.

media(L,M):-
    contar(L,C),
    somar(L,S),
    M is S/C.

%b)

menor([X | R], M):-
    menor(R, X, M).

menor([], X, X).

menor([Y | R], X, M):-
    X<Y, !,
    menor(R, X, M).

menor([Y | R], _, M):-
    menor(R, Y, M).


%c)

contarPares([],0):-!.

contarPares([H|T],C):-
    0 is H mod 2,
    contarPares(T,C1),
    C is C1 + 1.

contarPares([_|T],C):-
    contarPares(T,C).

contarImpares([], 0):-!.

contarImpares([H|T],C):-
    1 is H mod 2,
    contarImpares(T,C1),
    C is C1+1.

contarImpares([_|T],C):-
    contarImpares(T,C).

%d)

membro(X, [X | _]).

membro(X, [_ | T]):-
    membro(X, T).

nao_tem_repetidos([]).

nao_tem_repetidos([H | T]):-
    not(membro(H, T)),
    nao_tem_repetidos(T).

tem_repetidos([H | T]):-
    not(nao_tem_repetidos([H | T])).

%e)

novoMenor(L1,[M|L3]) :-
    menor(L1, M),
    delete(M,L1,L3).

%f)

concatena([], L, L).

concatena([H | T], L, [H | LR]):-
    concatena(T, L, LR).

%g)

flatten([],[]):-!.

%h)

deleteOne(_, [], []).

deleteOne(X, [X | R], R):-
    !.

deleteOne(X, [Y | R], [Y | R1]):-
    delete(X, R, R1).

%i)

delete(_,[],[]).
delete(X, [X|R], R1):-!,
    delete(X,R,R1).

delete(X, [Y|R], [Y|R1]):-
    delete(X, R, R1).















