:-include('paises.pl').
:-dynamic cor/2.

cores(azul).
cores(amarelo).
cores(vermelho).
cores(preto).
cores(branco).
cores(verde).
cores(lilas).
cores(ciano).
cores(magenta).
cores(cinza).
cores(tijolo).

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

%6)

membro(X, [X | _]).

membro(X, [_ | T]):-
    membro(X, T).

procuraPaisesAtravessados(P1, P2, _, 1):-
    vizinho(P1, P2), !.

procuraPaisesAtravessados(P1, P2, V, Num):-
    vizinho(P1, X),
    \+ membro(X, V),
    procuraPaisesAtravessados(X, P2, [X | V], Num1),
    Num is Num1+1.

numPaisesAtravessados(P1, P2, Num):-
    procuraPaisesAtravessados(P1, P2, [P1], Num).

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


%8)

%antes de executar o 8
% set_prolog_flag(answer_write_options, [quoted(true), portray(true),
% spacing(next_argument)] ).


roteiros(O,F,Cam,Cont):-
    NF is F+1,
    findall(C,caminho(O,NF,C, Cont),Cam).

caminho(Orig,F,Cam, Cont):-
    caminho2(Orig,[Orig],F,Cam, Cont),
    length(Cam,F).

caminho2(_,0,_, _).

caminho2(_,LA,F,Cam, _):-
    F>0,
%caminho actual esta invertido
    reverse(LA,Cam).

caminho2(Act,LA,F,Cam, Cont):-
    F1 is F-1,
    F1>0,
%testar ligacao entre ponto
%actual e um qualquer X
    vizinho(Act,X),
%o pais tem de pertencer ao continente definido
    pais(X, Cont, _),
%testar nao circularidade p/ nao
%visitar nodos ja visitados
    \+ member(X,LA),
%chamada recursiva
    caminho2(X,[X|LA],F1,Cam, Cont).


	%9)

% Algoritmo Sequencial Aproximado (Welsh e Powell)

colorir_mapa(C):-
    findall((N,P),(pais(P,C,_),contaVizinhos(P,N)),LP),
    sort(LP,SLP),
    inverte(SLP,ILP),
    findall(CO,cores(CO),LC),
    atribuir_cor(ILP,LC).


atribuir_cor([(_,P1),(H1,P2)|T1],[C|T2]):-
    %assertz(cor(C,P1)),
    vizinho(P1,P2),
    atribuir_cor([(H1,P2)|T1],T2);
    atribuir_cor([(H1,P2)|T1],[C|T2]).


contaVizinhos(P,N):-
    findall((P,X),vizinho(P,X),L),
    length(L,Y),
    N is Y.

%10)

%cor(azul, portugal).
%cor(amarelo, espanha).
%cor(azul, franca).
%cor(vermelho, alemanha).
%cor(amarelo, polonia).
%cor(verde, italia).

checkCores(R):-
    findall((C, X), cor(C, X), P),
    listar(P, R).

listar([], []).

listar([(C, P) | L], [(P, C, V) | R]):-
    setof(C1, Y^P^ (vizinho(Y, P), cor(C1, Y)), V),
    listar(L, R).



caminho2(Dest,Dest,LA,F,Cam, _):-
    F>0,
%caminho actual esta invertido
    reverse(LA,Cam).

caminho2(Act,Dest,LA,F,Cam, Cont):-
    F1 is F-1,
    F1>0,
%testar ligacao entre ponto
%actual e um qualquer X
    vizinho(Act,X),
%o pais tem de pertencer ao continente definido
    pais(X, Cont, _),
%testar nao circularidade p/ nao
%visitar nodos ja visitados
    \+ member(X,LA),
%chamada recursiva
    caminho2(X,Dest,[X|LA],F1,Cam,Cont).


	
	
%11)


exportar():-
    tell('BD.txt'),
    listing,
    told.

