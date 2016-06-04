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
% solo deberÃ­a ser "la cosa" porque cuadrado != triangulo
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

%asignar_var(+A, +MI, -MF)
asignar_var(A,[],[(A,X)]).
asignar_var(A,MI,MI):- claves(MI,C), member(A,C),length(MI,N), N>0.
asignar_var(A,MI,[(A,X)|MI]):- claves(MI,C), not(member(A,C)),length(MI,N), N>0.

%claves(+L,-C)
claves([],[]).
claves([(A,N)|XS],[A|R]):- claves(XS,R).

%Ejercicio 5
%palabras_con_variables(+P,-V)

palabras_con_variables([],[]).
palabras_con_variables([X|XS],[R|RS]):- aplanar([X|XS],LP), armarDic(LP,D), reemplazar_palabras_con_variable(D,[X|XS],[R|RS]).

reemplazar_palabras_con_variable(_,[],[]).
reemplazar_palabras_con_variable(D,[X|XS],[R|RS]):-reemplazarVar(D,X,R),
	reemplazar_palabras_con_variable(D,XS,RS).

aplanar([],[]).
aplanar([X|XS],RS):- append(X,R,RS),aplanar(XS,R).

armarDic([],[]).
armarDic([X|XS],RS):- armarDic(XS,R),asignar_var(X,R,RS).

reemplazarVar(D,[],[]).
reemplazarVar(D,[X|XS],[R|RS]):- dameVar(D,X,R), reemplazarVar(D,XS,RS).

% quito el siguiente dameVar([],_,_) ya que si llega a ese caso tendría
% que dar false
%dameVar([],_,_).
dameVar([(X,A)|XS],Y,A):- X==Y.
dameVar([(X,A)|XS],Y,R):- X\=Y, dameVar(XS,Y,R).


%Ejercicio 6
%quitar(E,L,R)

%quitar(_, [], []).
%quitar(X, [X|Xs], Y) :- quitar(X, Xs, Y).
%quitar(X, [T|Xs], [T|Y]) :- atomic(T), quitar(X, Xs, Y).
% quitar(X, [T|Xs], [T|Y]) :- not(var(T)), not(var(X)),
% string_codes([X],C1),string_codes([T],C2) ,C1\=C2 , quitar(X, Xs, Y).
%quitar(X, [T|Xs], Y) :-  atomic(T), atomic(X),X==T , quitar(X, Xs, Y).

%quitar(?E,+L,-R)
quitar(_,[],[]).
quitar(E,[X|XS],R):-E==X,quitar(E,XS,R).
quitar(E,[X|XS],[X|R]):-E\==X,quitar(E,XS,R).


%Ejercicio 7
%cant_distintos([],0).
% cant_distintos([X|XS],R):- atomic(X), member(X,XS),
% cant_distintos(XS,R).
% cant_distintos([X|XS],R):- atomic(X), not(member(X,XS)),
% cant_distintos(XS,R2), R is R2 + 1 .

%cant_distintos(+L,-S)
cant_distintos([],0).
cant_distintos([X|XS],S):-quitar(X,XS,L),cant_distintos(L,S2),S is S2+1.

%Descifrar(S,M)
%
%
%en p tengo la lista de simbolos, ej[x o |]

descifrar([],_).
descifrar(S,M):- palabras(S,P), armar_diccionario(P,D), posibles_asignaciones(D,LD), posibles_palabras(P,LD,R), palabras_validas(R,M).

% armar_diccionario(+L, -L) recibe una lista de simbolos, instancia
% lista de tuplas, simbolo, variable

armar_diccionario([],[]).
armar_diccionario(X, D):- asignar_variables(X,[],D).

asignar_variables([],_,_).
asignar_variables([X],Y,YS):- asignar_var(X,Y,YS).
asignar_variables([X,Y|XS], YS, R):- asignar_var(X,YS,YSS), asignar_variables([Y|XS], YSS, R).

%a partir de una lista de lista de simbolos (palabras);
% un diccionario de simbolos->variables instancia una lista de
% asignaciones (una asignacion es una lista de palabras) lista de
% palabras
posibles_palabras(_,[],_).
posibles_palabras(XS,[D|DS],[Y|YS]):-  instanciar_asignaciones(XS,D,Y), posibles_palabras(XS,DS,YS).

% instanciar_asignaciones recibe una lista, de lista de simbolos, un
% diccionario de simbolo->asignacion, instancia el reemplazo de esa
% asignacion en la lista

instanciar_asignaciones([],_,_).
instanciar_asignaciones([X|XS],D,[Y|YS]):- dame_def(X, D, Y), instanciar_asignaciones(XS,D,YS).

%dame_def(+P,+D,-R)
%devuelve la definicion de una palabra
dame_def(_,[],_).
dame_def(X, [(X,Y)|XS],Y).
dame_def(X,[(A,B)|XS],Y):- X\==A, dame_def(X,XS,Y).


% devuelve una lista de diccionarios con las posibles asignaciones para
% cada variable

posibles_asignaciones([],_).

palabras_validas([],_).
palabras_validas(XS,YS):- diccionario_lista_completo(D), setof(X,(member(X,XS), todas_palabras_validas(X,D)),YS).

diccionario_lista_completo(D):- setof(X,diccionario_lista(X),D).

todas_palabras_validas([],_).
todas_palabras_validas([X|XS],YS):- string_codes(X,S), member(S,YS), todas_palabras_validas(XS,YS).
















