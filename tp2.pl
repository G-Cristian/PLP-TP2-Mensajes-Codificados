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

%Ejercicio 8
%descifrar(+S,?M)
% Primero armamos la lista con listas de variables libres usasndo
% 'palabras(S,P), palabras_con_variables(P,V)'. Luego se recorren todas
% las palabras del dicionario pasadas a codigo ascii usando
% 'diccionario_lista(D)'. Finalmente se trata de unificar la primera
% lista de V con la primer palabra de D y si unifican se sigue buscando
% recursibamente.

descifrar(S,M):-palabras(S,P),palabras_con_variables(P,V),descifrar_palabras(V),juntar_con(V,32,N),simbolos_respetan_letras(S,N),string_codes(M,N).

descifrar_palabras([]).
descifrar_palabras([V|VS]):-diccionario_lista(D),V=D,descifrar_palabras(VS).

simbolos_respetan_letras([],[]).
simbolos_respetan_letras([S|SS],[N|NS]):-simbolo_es_misma_letra_siempre(S,N,SS,NS),simbolos_respetan_letras(SS,NS).

simbolo_es_misma_letra_siempre(_,_,[],[]).
simbolo_es_misma_letra_siempre(S,L,[S2|SS],[L2|LS]):-S==S2,L==L2,simbolo_es_misma_letra_siempre(S,L,SS,LS).
simbolo_es_misma_letra_siempre(S,L,[S2|SS],[L2|LS]):-S\==S2,L\==L2,simbolo_es_misma_letra_siempre(S,L,SS,LS).


%descifrar_sin_espacios(+S, ?M)
descifrar_sin_espacios(S, M):-espacios_intercalados(S,N),descifrar(N,M).

%espacios_intercalados(+S,-M)
espacios_intercalados([S],[S]).
espacios_intercalados([S|SS],[S,espacio|MS]):-espacios_intercalados(SS,MS).
espacios_intercalados([S|SS],[S|MS]):-espacios_intercalados(SS,MS).


%mensajes_mas_parejos(S,M)
% mensajes_mas_parejos(S,M):- descifrar_sin_espacios(S,N1),
% desviacion_estandar(N1,D1), not(descifar_sin_espacios(S,N2),
% desviacion_estandar(N2,D2), D2<D1), member(N1,M).

%desviacion_estandar().
%desviacion_estandar():-

%media_de_longitudes

%cantidad_palabras:






