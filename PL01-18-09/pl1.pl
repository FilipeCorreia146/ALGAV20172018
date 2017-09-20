continente(africa) .
continente(asia) .
continente(america) .
continente(europa) .
continente(oceania) .

pais(portugal, europa, 10) .
pais(espanha, europa, 25) .
pais(franca, europa, 40) .
pais(australia, oceania, 50) .
pais(mexico, america, 70).
pais(usa, america, 120) .
pais(canada, america, 100).
pais(china, asia, 3000) .
pais(nova_zelandia, oceania, 39).
pais(india, asia, 1500) .

fronteira(portugal, espanha).
fronteira(espanha, franca).
fronteira(mexico, usa).
fronteira(usa, canada).
fronteira(china, india).


vizinho(X,Y):-
    fronteira(X,Y);
    fronteira(Y,X).

contSemPaises(C):-
    continente(C),
    not(pais(_,C,_)).

semVizinhos(L):-
    pais(L,_,_),
    not(vizinho(L,_)).

chegoLaFacil(P,J):-
    vizinho(P,J);
    pais(O,_,_),
    vizinho(P,O),
    vizinho(O,J).

pot(_,0,1).

pot(X,Y,P):-
    Y>0,
    Y1 is Y-1,
    pot(X,Y1,P1),
    P is X*P1.

sum(X,X,X).

sum(I,S,P):-
    S>I,
    I1 is I+1,
    sum(I1, S, P1),
    P is I+P1.





