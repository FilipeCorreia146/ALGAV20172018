:-include('paises.pl').

%2)

vizinho(X,Y):-
    fronteira(X,Y);
    fronteira(Y,X).

lista(C):-
    write('Continente: '),
    write(C), nl,
    write('--------------------'), nl,
    findall(X, pais(X, C, _), L),
    paises(L).

paises([H | T]):-
    descricao_pais(H),
    paises(T).

descricao_pais(P):-
    pais(P, _, X),
    write(P),
    write(', '),
    write(X),
    write(', '),
    findall(Y, vizinho(P, Y), L),
    write(L), nl.


%3)

inverte(L, LI):-
    inverte2(L, [], LI).

inverte2([], L, L).

inverte2([H | T], LA, LR):-
    inverte2(T, [H | LA], LR).

doisMaisPop(P1,P2):-
    setof([H,P],C^pais(P,C,H),L),
    inverte(L,[HP1,HP2|_]),
    getSecondElement(HP1, P1),
    getSecondElement(HP2, P2).

getSecondElement([_,P|_], P).


%4)

paisesGrandes(C, N, L):-
    setof(X-P, (pais(P, C, X), X>N), L).


%5)


somaPopViz(P,L,S):-
    findall((Y,X),(vizinho(P,X),pais(X,_,Y)),L),
    contaPop(L,S).

contaPop([],0).
contaPop([(H,_)|T], S):-
    contaPop(T,S1),
    S is S1 + H.

%7)

roteiro(O,D,F,Cam):-
    findall(C,dfs(O,D,F,C),Cam).

dfs(Orig,Dest,F,Cam):-
    dfs2(Orig,Dest,[Orig],F,Cam),
    length(Cam,F).
%condicao final: nodo actual = destino

dfs2(_,_,0,_).

dfs2(Dest,Dest,LA,F,Cam):-
    F>0,
%caminho actual esta invertido
    reverse(LA,Cam).

dfs2(Act,Dest,LA,F,Cam):-
    F1 is F-1,
    F1>0,
%testar ligacao entre ponto
%actual e um qualquer X
    vizinho(Act,X),
%testar nao circularidade p/ nao
%visitar nodos ja visitados
    \+ member(X,LA),
%chamada recursiva
    dfs2(X,Dest,[X|LA],F1,Cam).





