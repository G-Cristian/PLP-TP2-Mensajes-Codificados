:- dynamic(diccionario/1).

% Dado un nombre de archivo que contiene todas las palabras que se quieren
% agregar al diccionario (una por linea), vacia diccionario/1 y agrega
% las definiciones nuevas

cargar(NombreDeArchivo) :-
  retractall(diccionario(_)),
  atom_codes(NombreDeArchivo, Arch),
  open(Arch, read, Str),
  read_file(Str,_),
  close(Str).

read_file(Stream,[]) :- at_end_of_stream(Stream).
read_file(Stream,[X|L]) :-
    not(at_end_of_stream(Stream)),
    read_line_to_codes(Stream,Codes),
    string_codes(X, Codes),
    assertz(diccionario(X)),
    read_file(Stream,L), !.


% listar mensajes secretos de ejemplo.
ej(1, [rombo, cuadrado, espacio, perro, cuadrado, sol, cuadrado]).
% solo debería ser "la cosa" porque cuadrado != triangulo
ej(2, [rombo, cuadrado, espacio, perro, triangulo, sol, cuadrado]).

ej(3, [rombo, cuadrado, perro, cuadrado, sol, luna, triangulo, estrella, arbol, gato]).


% Ejercicio 1
% S es valido solo si es una codificacion de un predicado del diccionario.

diccionario_lista(S) :- diccionario(P), string_codes(P,S).

% Ejercicio 2
% El predicado es valido cuando tiene un solo elemento y ese elemento es el resultante.
% O el elemento resultante es la union del primer elemento de la lista, mas el J, mas el resultado
% del predicado valido con un elemento menos.

%juntar_con(?L,+J,?V)
%L y V no pueden ser libres al mismo tiempo.
juntar_con(L,J,V) :- length(L,1), member(V,L), not(member(J,V)).
juntar_con([X|L1],J,R) :- var(R), nonvar(X), append(X,[J],XJ),append(XJ,R2,R),juntar_con(L1,J,R2).
juntar_con([X|L1],J,R):-nonvar(R), append(XJ,R2,R),append(X,[J],XJ),juntar_con(L1,J,R2),not((member(J,X))).


% Ejercicio 3

palabras(S,P) :- juntar_con(P,'espacio',S).



%Ejercicio 4

%asignar_var(A, MI, MF)
asignar_var(A,[],[(A,X)]).
asignar_var(A,MI,MI):- claves(MI,C), member(A,C),length(MI,N), N>0.
asignar_var(A,MI,[(A,X)|MI]):- claves(MI,C), not(member(A,C)),length(MI,N), N>0.

claves([],[]).
claves([(A,N)|XS],[A|R]):- claves(XS,R).

%Ejercicio 5
%palabras_con_variables(P,V)

palabras_con_variable([],[]).
palabras_con_variable([X|XS],[R|RS]):- aplanar([X|XS],LP), armarDic(LP,D), reemplazar_palabras_con_variable(D,[X|XS],[R|RS]).

reemplazar_palabras_con_variable(_,[],_).
reemplazar_palabras_con_variable(D,[X|XS],[R|RS]):-reemplazarVar(D,X,R),
	reemplazar_palabras_con_variable(D,XS,RS).

aplanar([],[]).
aplanar([X|XS],RS):- append(X,R,RS),aplanar(XS,R).

armarDic([],[]).
armarDic([X|XS],RS):- armarDic(XS,R),asignar_var(X,R,RS).

reemplazarVar(D,[],[]).
reemplazarVar(D,[X|XS],[R|RS]):- dameVar(D,X,R), reemplazarVar(D,XS,RS).

dameVar([],_,_).
dameVar([(X,A)|XS],Y,A):- X==Y.
dameVar([(X,A)|XS],Y,R):- X\=Y, dameVar(XS,Y,R).


%Ejercicio 6
%quitar(E,L,R)

quitar(_, [], []).
quitar(X, [X|Xs], Y) :- quitar(X, Xs, Y).
quitar(X, [T|Xs], [T|Y]) :- atomic(T), quitar(X, Xs, Y).
quitar(X, [T|Xs], [T|Y]) :-  not(var(T)), not(var(X)), string_codes([X],C1),string_codes([T],C2) ,C1\=C2 , quitar(X, Xs, Y).
quitar(X, [T|Xs], Y) :-  atomic(T), atomic(X),X==T , quitar(X, Xs, Y).


%Ejercicio 7
cant_distintos([],0).
cant_distintos([X|XS],R):- atomic(X), member(X,XS), cant_distintos(XS,R).
cant_distintos([X|XS],R):- atomic(X), not(member(X,XS)), cant_distintos(XS,R2), R is R2 + 1 .

















