% -----------------------------------------------------------------------
% Trabalho prático: factos de cidades com localização baseada em
% latitude e longitude e predicado auxiliar para calcular a distância
% entre quaisquer duas destas cidades.
% ------------------------------------------------------------------------
:-dynamic custo/2.


%city(name,latitude,longitude)
city(brussels,50.8462807,4.3547273).
city(tirana,41.33165,19.8318).
city(andorra,42.5075025,1.5218033).
city(vienna,48.2092062,16.3727778).
city(minsk,53.905117,27.5611845).
city(sarajevo,43.85643,18.41342).
city(sofia,42.6976246,23.3222924).
city(zagreb,45.8150053,15.9785014).
city(nicosia,35.167604,33.373621).
city(prague,50.0878114,14.4204598).
city(copenhagen,55.6762944,12.5681157).
city(london,51.5001524,-0.1262362).
city(helsinki,60.1698791,24.9384078).
city(paris,48.8566667,2.3509871).
city(marseille,43.296386,5.369954).
city(tbilisi,41.709981,44.792998).
city(berlin,52.5234051,13.4113999).
city(athens,37.97918,23.716647).
city(budapest,47.4984056,19.0407578).
city(reykjavik,64.135338,-21.89521).
city(dublin,53.344104,-6.2674937).
city(rome,41.8954656,12.4823243).
city(pristina,42.672421,21.164539).
city(riga,56.9465346,24.1048525).
city(vaduz,47.1410409,9.5214458).
city(vilnius,54.6893865,25.2800243).
city(luxembourg,49.815273,6.129583).
city(skopje,42.003812,21.452246).
city(valletta,35.904171,14.518907).
city(chisinau,47.026859,28.841551).
city(monaco,43.750298,7.412841).
city(podgorica,42.442575,19.268646).
city(amsterdam,52.3738007,4.8909347).
city(belfast,54.5972686,-5.9301088).
city(oslo,59.9138204,10.7387413).
city(warsaw,52.2296756,21.0122287).
city(lisbon,38.7071631,-9.135517).
city(bucharest,44.430481,26.12298).
city(moscow,55.755786,37.617633).
city(san_marino,43.94236,12.457777).
city(edinburgh,55.9501755,-3.1875359).
city(belgrade,44.802416,20.465601).
city(bratislava,48.1483765,17.1073105).
city(ljubljana,46.0514263,14.5059655).
city(madrid,40.4166909,-3.7003454).
city(stockholm,59.3327881,18.0644881).
city(bern,46.9479986,7.4481481).
city(kiev,50.440951,30.5271814).
city(cardiff,51.4813069,-3.1804979).

linearCoord(City,X,Y):-
	city(City,Lat,Lon),
	geo2linear(Lat,Lon,X,Y).

geo2linear(Lat,Lon,X,Y):-
	degrees2radians(Lat,LatR),
	degrees2radians(Lon,LonR),
	X is round(6371*cos(LatR)*cos(LonR)),
	Y is round(6371*cos(LatR)*sin(LonR)).

%  dist_cities(brussels,prague,D).
%  D = 716837.
dist_cities(C1,C2,Dist):-
    city(C1,Lat1,Lon1),
    city(C2,Lat2,Lon2),
    %linearCoord(C1,Lat1,Lon1),
    %linearCoord(C2,Lat2,Lon2),
    distance(Lat1,Lon1,Lat2,Lon2,Dist).

degrees2radians(Deg,Rad):-
	Rad is Deg*0.0174532925.

% distance(latitude_first_point,longitude_first_point,latitude_second_point,longitude_second_point,distance
% in meters)
distance(Lat1, Lon1, Lat2, Lon2, Dis2):-
	degrees2radians(Lat1,Psi1),
	degrees2radians(Lat2,Psi2),
	DifLat is Lat2-Lat1,
	DifLon is Lon2-Lon1,
	degrees2radians(DifLat,DeltaPsi),
	degrees2radians(DifLon,DeltaLambda),
	A is sin(DeltaPsi/2)*sin(DeltaPsi/2)+ cos(Psi1)*cos(Psi2)*sin(DeltaLambda/2)*sin(DeltaLambda/2),
	C is 2*atan2(sqrt(A),sqrt(1-A)),
	Dis1 is 6371000*C,
	Dis2 is round(Dis1).

% distance(50.8462807,4.3547273,50.0878114,14.4204598,D).
% Online: http://www.movable-type.co.uk/scripts/latlong.html
%

tsp1(C,L):-
    findall(X,(city(X,_,_), not(X==C)),LS),
    findall([C|E],(permutation(LS,E)),NL),
    custos(C,NL),
    setof(Custo-Caminho,custo(Caminho,Custo),[L|_]).

custos(_,[]).

custos(City,[Head | Tail]):-
    custos(City,Tail),
    custos2(City, Head, Custo),
    asserta(custo(Head,Custo)).

custos2(_, [], _).

custos2(C,[H|[]],M):-
    dist_cities(C,H,M).

custos2(C, [H1,H2|T], M):-
    custos2(C, [H2|T], M1),
    dist_cities(H1,H2,Dist),
    M is M1 + Dist.


tsp2(City,L):-
    findall(X,(city(X,_,_), not(X==City)),TempList),
    find_closest(City,TempList,_,L), !.


find_closest(City,[],List,L):-
    reverse([City|List],L),
    !.

find_closest(City,TempList,[],L):-
    append([City],[],List1),
    calc_dist(City,TempList,DistanceList),
    sort(DistanceList,OrderedList),
    getFirst(OrderedList,OtherCity),
    append([OtherCity],List1, List),
    delete(TempList,OtherCity,TempList1),
    find_closest(City,TempList1,List,L).

find_closest(City,TempList,List,L):-
    calc_dist(City,TempList,DistanceList),
    sort(DistanceList,OrderedList),
    getFirst(OrderedList,OtherCity),
    append([OtherCity],List,List1),
    delete(TempList,OtherCity,TempList1),
    find_closest(City,TempList1,List1,L).


getFirst([_-H|_],H).

calc_dist(_,[],_).

calc_dist(City,[H|[]],DistanceList):-
    dist_cities(City,H,Dist),
    append([Dist-H],[],DistanceList).

calc_dist(City,[H|T],DistanceList):-
    calc_dist(City,T,DistanceList1),
    dist_cities(City,H,Dist),
    append([Dist-H],DistanceList1,DistanceList).

% Given three colinear points p, q, r, the function checks if
% point q lies on line segment 'pr'
%onSegment(P, Q, R)
onSegment((PX,PY), (QX,QY), (RX,RY)):-
    QX =< max(PX,RX),
    QX >= min(PX,RX),
    QY =< max(PY,RY),
    QY >= min(PY,RY).


% To find orientation of ordered triplet (p, q, r).
% The function returns following values
% 0 --> p, q and r are colinear
% 1 --> Clockwise
% 2 --> Counterclockwise

orientation((PX,PY), (QX,QY), (RX,RY), Orientation):-
	Val is (QY - PY) * (RX - QX) - (QX - PX) * (RY - QY),

	(
		Val == 0, !, Orientation is 0;
		Val >0, !, Orientation is 1;
		Orientation is 2
	).

orientation4cases(P1,Q1,P2,Q2,O1,O2,O3,O4):-
    orientation(P1, Q1, P2,O1),
    orientation(P1, Q1, Q2,O2),
    orientation(P2, Q2, P1,O3),
    orientation(P2, Q2, Q1,O4).



% The main function that returns true if line segment 'p1q1'
% and 'p2q2' intersect.
doIntersect(P1,Q1,P2,Q2):-
    % Find the four orientations needed for general and
    % special cases
	orientation4cases(P1,Q1,P2,Q2,O1,O2,O3,O4),

	(
    % General case
    O1 \== O2 , O3 \== O4,!;

    % Special Cases
    % p1, q1 and p2 are colinear and p2 lies on segment p1q1
    O1 == 0, onSegment(P1, P2, Q1),!;

    % p1, q1 and p2 are colinear and q2 lies on segment p1q1
    O2 == 0, onSegment(P1, Q2, Q1),!;

    % p2, q2 and p1 are colinear and p1 lies on segment p2q2
    O3 == 0, onSegment(P2, P1, Q2),!;

     % p2, q2 and q1 are colinear and q1 lies on segment p2q2
    O4 == 0, onSegment(P2, Q1, Q2),!
    ).

%4

%transforma uma lista numa lista de segmentos
seg([_],[]).

seg([X,Y|R],[(X,Y)|R2]):-
	seg([Y|R],R2).

%verifica se o segmemto é membro da lista
seg_membro((X,Y),L):-
	member((X,Y),L);
	member((Y,X),L).

%transforma a lista de segmentos num circuito
circuito(City, [City], []):- !.

circuito(City, LF, Pares) :-
    Pares=[_|_],
    ((member((City,C),Pares),
      delete(Pares,(City,C),NovosPares));
    (member((C,City),Pares),
     delete(Pares,(C,City),NovosPares))),
    circuito(C,Circuito,NovosPares),
    append([City],Circuito,LF), !.

%retorna o circuito obtido através da heuristica do vizinho mais próximo
%(greedy) com remoção de cruzamentos
tsp3(City,LF):-
    tsp2(City,L),
    seg(L,Pares),
    verifica_cruzamentos(LI,Pares),
    circuito(City,LF,LI).

% remover pares de segmentos cruzados enquanto existirem cruzamentos
% na solucao
verifica_cruzamentos(LF,Pares):-

	%vai buscar dois segmentos
	member((P1,Q1),Pares),
	member((P2,Q2),Pares),
	%as cidades têm de ser todas diferentes
	%pois os segmentos têm de ser não colineares
	not(P1==P2),
	not(P1==Q2),
	not(Q1==P2),
	not(Q1==Q2),

	linearCoord(P1,P1x,P1y), A = (P1x,P1y),
	linearCoord(Q1,Q1x,Q1y), B = (Q1x,Q1y),
	linearCoord(P2,P2x,P2y), C = (P2x,P2y),
	linearCoord(Q2,Q2x,Q2y), D = (Q2x,Q2y),

	%verifica se os segmentos se intersetam
	%doIntersect(P1,Q1,P2,Q2),
	doIntersect(A,B,C,D),

	%caso se intersetem os segmentos sao eliminados da lista
	delete(Pares,(P1,Q1),SemP1eQ1),
	delete(SemP1eQ1,(P2,Q2),NovosPares),

	%arbitrariamente selecionar um dos segmentos e a partir de
	%um dos nodos desse segmento selecionar o nodo mais
	%proximo do outro segmento eliminado
	dist_cities(P1, P2, CustoP1P2),
	dist_cities(P1, Q2, CustoP1Q2),

	%caso P1-P2 tenha menor custo ver se ja existe na lista
	%acrescentar P1,P2 e P1,Q2
	((CustoP1P2<CustoP1Q2,
	  not(seg_membro((P1,P2),NovosPares)),
	  not(seg_membro((Q1,Q2),NovosPares)),
	  append([(P1,P2),(Q1,Q2)], NovosPares, L2));
	((not(seg_membro((P1,Q2),NovosPares)),
	  not(seg_membro((Q1,P1),NovosPares)),
	  append([(P1,Q2),(Q1,P1)], NovosPares, L2));
	  append([(P1,P2),(Q1,Q2)], NovosPares, L2))),

	verifica_cruzamentos(LF,L2).

verifica_cruzamentos(LF,LF).


tsp4(City, [City|List]):-
	initialSolution(City,L),
	totalDistance([City|L],D1),
	random(0.80,0.99,Alpha),
	It is 200,
	Temp is 1,
	ItpT is 10,
	annealling(L,D1,Alpha,It,ItpT,Temp,City,List).

annealling(L,_,_,0,_,_,_,L):-!.
annealling(L,_,_,_,_,0,_,L):-!.
annealling(L,D,Alpha,It,I,T,C,LR):-
	It1 is It-1,
	anneall(L,D,T,I,C,L1,D1),
	I1 is I+1,
	T1 is T*Alpha,
	!,
	annealling(L1,D1,Alpha,It1,I1,T1,C,LR).


anneall(L,D,_,0,_,L,D):-!.

anneall(List,Distance,Temperature,Iteration,City,L1,D1):-
	I1 is Iteration-1,
	newAdjacent(List,LTemp),
	totalDistance([City|LTemp],DTemp),
	calc(Distance, DTemp,List,LTemp,Temperature,RetDist,RetList),
	!,
	anneall(RetList,RetDist,Temperature,I1,City,L1,D1).

calc(Distance,0,List,_,_,Distance,List).
calc(0,Distance,_,List,_,Distance,List).

calc(Distance, DTemp, List,LTemp,Temperature,RetDist,RetList):-
	Distance > DTemp,
	calc(0,DTemp,_,LTemp,_,RetDist,RetList);
	CostDif is Distance - DTemp,
	calcAp(Temperature,CostDif,Ap),
	random(R),
	Ap > R,
	calc(0,DTemp,_,LTemp,_,RetDist,RetList);
	calc(Distance,0,List,_,_,RetDist,RetList).



calcAp(T,CD,Ap):-
	Exp is CD/T,
	Ap = exp(Exp).

newAdjacent(S1,Sn):-
	length(S1,T1),
	random_between(1,T1,Pos1),
	random_between(1,T1,Pos2),
	nth1(Pos1,S1,E1),
	nth1(Pos2,S1,E2),
	removeElementPos(Pos1,S1,S2),
	insertElementPos(Pos1,E2,S2,S3),
	removeElementPos(Pos2,S3,S4),
	insertElementPos(Pos2,E1,S4,Sn).

removeElementPos(1,[H|T],T):-!.
removeElementPos(Pos1,[H|T],[H|L]):-
	Pos1 >1,
	N is Pos1 -1,
	!,
	removeElementPos(N,T,L).

insertElementPos(1,E,L,[E|L]):-!.

insertElementPos(Pos1,E,[H|T],[H|S]):-
	Pos1>1,
	N is Pos1-1,
	!,
	insertElementPos(N,E,T,S).

initialSolution(Orig,L):-
	findall(X,(city(X,_,_),X\=Orig),L1),
	random_permutation(L1,L).

totalDistance([],0).
totalDistance([_],0).
totalDistance([X,Y|L],T):-
	dist_cities(X,Y,Cl),
	totalDistance([Y|L],T1),
	T is T1 + Cl.



