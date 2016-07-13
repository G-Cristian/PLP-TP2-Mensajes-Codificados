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

tests_ej_8(1, [boca, racing, espacio, newells, boca, velez, boca, independiente, sarmiento, quilmes, racing, river, newells, sanlor, chicago, espacio, rosario, almirante, estudiantes, velez, almirante, independiente, river, estudiantes, boca, chicago]).

% Ejercicio 1
%diccionario_lista(?S)
% S es reversible pues diccionario(P) instancia P y
% en string_codes(?P,?S) por lo menos uno de los argumentos debe estar
% instanciado, en este caso P.
%S es valido solo si es una codificacion
% de un predicado del diccionario.
diccionario_lista(S) :- diccionario(P), string_codes(P,S).

% Ejercicio 2
% El predicado es valido cuando tiene un solo elemento y ese elemento es el resultante.
% O el elemento resultante es la union del primer elemento de la lista, mas el J, mas el resultado
% del predicado valido con un elemento menos.

%juntar_con(?L,?J,?V)
%L y V no pueden ser libres al mismo tiempo.
%Si J est� libre, V debe estar definida.
% Si L es libre puede caer en el tercer caso. En este caso R no puede
% ser libre por el nonvar(R). El append va a darle valor a XJ y a R2
% basandose en R. El segundo append unifica X y J basandose en XJ,
% donde J es un �nico elemento. Luego se llama a juntar_con(L1,J,R2) con
% L1 libre y J y R2 unificadas. Por ultimo se llama a member con J y X
% unificadas.
%
% Si R es libre cae en caso 2 cuando L no es libre. Ahora si J est�
% libre no se unifica ni J y XJ	contiene elementos libres en el primer
% append, y en el segundo append R2 est� libre y R tiene elementos
% libres. Por �ltimo se llama a juntar_con con L1 definida, J libre y R2
% libre. Esto se repite hasta caer en el caso base donde L est�
% definida, V se unifica y el not(memeber(J,V)) con J libre y V
% unificada da false ya que existe una unificaci�n.
% En cambio si J est� definida el primer append unifica XJ y el segundo
% deja a R2 sin unificar y a R con el elemento R2 sin unificar. Luego se
% llama a juntar_con con L1 y J definidos y R2 sin unificar. Cuando se
% llega al caso base se unifica V y se chequea el not(member(J,V)) pero
% esta vez con J y V definidas. Es decir que puede dar true en cuyo caso
% vuelve con la V unificada, lo cual va unificando a los R2 anteriores.
juntar_con([L],_,L).
juntar_con([X|L1],J,R) :- var(R), nonvar(X), juntar_con(L1,J,R2),append(X,[J|R2],R).
juntar_con([X|L1],J,R):-nonvar(R), append(XJ,R2,R),append(X,[J],XJ),juntar_con(L1,J,R2),not((member(J,X))).



% Ejercicio 3
%palabras(?S,?P)
% No pueden ser las dos libres. Esto es por como funciona juntar_con.
palabras(S,P) :- juntar_con(P,'espacio',S).



%Ejercicio 4

%asignar_var(?A, ?MI, ?MF)
% asignar_var devuelve un diccionario al que cada clave le asigna una
% variable nueva en caso de que la clave no exista.
% Para ello revisa las claves que posee el diccionario de entrada, y en
% caso de que no pertenezca le asigna una nueva variable.
% Para obtener el resultados sin que se cuelgue, el diccionario
% resultante puede no estar instanciado mientras si est� instanciada MI.
% Si MI esta instanciada y A y MF no lo est�n, funciona porque se
% estar�a usando el member con MI instanciada por lo que el member
% funciona bien. Si MF esta instanciada pasa lo mismo con los member. Si
% MI y MF no estan instanciadas el primer member se queda iterando por
% todas las listas que contengan a (A,_).

asignar_var(A, MI, MI):- member((A,_), MI).
asignar_var(A, MI,[(A,_)|MI]):- not(( member( (A,_), MI) )).

%asignar_var(A,[],[(A,_)]).
%asignar_var(A,[(A,V)|MI],[(A,V)|MI]).
%asignar_var(A,[(B,V)|MI],[(B,V)|MF]):-asignar_var(A,MI,MF).

%asignar_var(A,[],[(A,_)]).
%asignar_var(A,MI,MI):- claves(MI,C), member(A,C).
% asignar_var(A,MI,[(A,_)|MI]):- claves(MI,C),
% not(member(A,C)),length(MI,N), N>0.

%claves(+L,-C)
%claves([],[]).
%claves([(A,_)|XS],[A|R]):- claves(XS,R).

%Ejercicio 5
%palabras_con_variables(+P,-V)
% Siendo P una lista de listas de atomos, instancia en V una lista de
% listas de variables.
% La idea de la solucion es armar un diccionario con las posibles
% variables para cada palabra, y luego reemplazar esas variables
% en la lista de lista de atomos.
%Funciona con la lista de lista de variables no instanciadas y no a la inversa, la lista de lista de
% atomos si debe estar instanciada. Esto ocurre porque el ultimo de los
% predicados (reemplazar_palabras_con_variable) necesita que en el
% diccionario que se arma (atomo-> variable) el atomo ya este
% instanciado. Dejar la lista de lista de atomos sin instanciar no rompe
% pero si "cuelga" por la infinidad de posibilidades de diccionarios que
% pueden armarse.


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

% quito el siguiente dameVar([],_,_) ya que si llega a ese caso tendr�a
% que dar false
%dameVar([],_,_).
dameVar([(X,A)|XS],Y,A):- X==Y.
dameVar([(X,A)|XS],Y,R):- X\=Y, dameVar(XS,Y,R).


%Ejercicio 6

%quitar(?E,+L,-R)
% siendo E un atomo y L una lista de atomos, instancia en R el resultado
% de quitar todas las apariciones de E en L.
% La solucion consiste en recorrer todos los elementos y quitarlos de la
% lista resultante en caso de que sean igual al parametro E.
%L puede% contener elementos instanciados y no instanciados. E puede no estar
% instanciado. Si E y R estan instanciados, L puede no estado, pero el
% resultado es la misma lista que se instancia en R, como si la E nunca
% hubiera pertenecido a la lista original. Si ninguna de las variables
% esta instanciada devuelve una lista vacia tanto para L como para R.


quitar(_,[],[]).
quitar(E,[X|XS],R):-E==X,quitar(E,XS,R).
quitar(E,[X|XS],[X|R]):-E\==X,quitar(E,XS,R).


%Ejercicio 7
%cant_distintos(+L,-S)
% siendo L una lista de atomos y variables, instancie en S la cantidad
% de elementos distintos que contiene L.
% Basicamente se empieza a recorrer la lista eliminando el primer
% elemento de la cola y sumando uno al resultado a instanciar. Esto nos
% asegura que el elemento no lo contaremos dos veces.
%S puede estar instanciado o no. L debe estar instanciado para que tenga sentido. Si
% S esta instanciado y L no, devuelve como primer resultado una lista de
% longitud S, con S variables distintas. Si se le pide otro resultado
% cuelga. Esto sucede porque el predicado cant_distintos usa el del item
% 6 (quitar), con las tres variables no instanciadas, por lo que no
% rompe, pero tampoco instancia nada, y la llamadas recursiva
% cantidad_distintos devuelve una lista de variables no instanciadas de
% longitud S - 1.

cant_distintos([],0).
cant_distintos([X|XS],S):-quitar(X,XS,L),cant_distintos(L,S2),S is S2+1.

%Ejercicio 8
%descifrar(+S,?M)
% Primero armamos la lista con listas de variables libres usando
% 'palabras(S,P), palabras_con_variables(P,V)'. Luego se recorren todas
% las palabras del dicionario pasadas a codigo ascii usando
% 'diccionario_lista(D)'. Finalmente se trata de unificar la primera
% lista de V con la primer palabra de D y si unifican se sigue buscando
% recursivamente.
% S debe estar instanciado y M puede o no estarlo.
% Si S no se instancia el resultado es false, ya que palabras(S,P) con S
% y P no instanciados ya devuelve false.

descifrar(S,M):-palabras(S,P),palabras_con_variables(P,V),descifrar_palabras(V),juntar_con(V,32,N),simbolos_respetan_letras(S,N),string_codes(M,N).

descifrar_palabras([]).
descifrar_palabras([V|VS]):-diccionario_lista(V),descifrar_palabras(VS).

simbolos_respetan_letras([],[]).
simbolos_respetan_letras([S|SS],[N|NS]):-simbolo_es_misma_letra_siempre(S,N,SS,NS),simbolos_respetan_letras(SS,NS).

simbolo_es_misma_letra_siempre(_,_,[],[]).
simbolo_es_misma_letra_siempre(S,L,[S2|SS],[L2|LS]):-S==S2,L==L2,simbolo_es_misma_letra_siempre(S,L,SS,LS).
simbolo_es_misma_letra_siempre(S,L,[S2|SS],[L2|LS]):-S\==S2,L\==L2,simbolo_es_misma_letra_siempre(S,L,SS,LS).

%EJERCICIO 9
%descifrar_sin_espacios(+S, ?M)
% La idea es ir metiendo espacios intercaladamente y luego utilizar el
% predicado de descifrar.
% Necesita que la lista de simbolos sin espacios este instanciada, de lo
% contrario el llamado "cuelga".
% M puede no estar instanciado.


descifrar_sin_espacios(S, M):-espacios_intercalados(S,N),descifrar(N,M).

%espacios_intercalados(+S,-M)
espacios_intercalados([S],[S]).
espacios_intercalados([S|SS],[S,espacio|MS]):-espacios_intercalados(SS,MS).
espacios_intercalados([S|SS],[S|MS]):-espacios_intercalados(SS,MS).

%EJERCICIO 10
%mensajes_mas_parejos(S,M)
% La idea es armar un prredicado que indique que no hay un mensaje con
% una desviacion estandar menor que el resultado.
% Al usar el predicado del punto anterior la lista de simbolos debe
% estar instanciada. El resultado M puede no estarlo

mensajes_mas_parejos(S,M):- descifrar_sin_espacios(S,M),
 desviacion_estandar_string(M,D1), not((descifrar_sin_espacios(S,N2),
 desviacion_estandar_string(N2,D2), D2<D1)).


%desviacion_estandar_string(+M,-DE)
%M=string
%DE=float
desviacion_estandar_string(M,DE):-string_codes(M,N),juntar_con(P,32,N),desviacion_estandar(P,DE).

%desviacion_estandar(+P,-DE)
%P=[[simbolo/codigo,simbolo/codigo,...],[simbolo/codigo,...],...]
%DE=float
desviacion_estandar(P,DE):-media(P,M),sumatoria_cuadrado_diferencias(P,M,SCD), length(P,L), DIVISION is SCD/L, DE is sqrt(DIVISION).

media(P,M):-sumatoria_longitudes(P,SL),length(P,L),M is SL/L.

sumatoria_longitudes([],0).
sumatoria_longitudes([P|PS],SL):-length(P,LP),sumatoria_longitudes(PS,SLS), SL is LP+SLS.

sumatoria_cuadrado_diferencias([],_,0).
sumatoria_cuadrado_diferencias([P|PS],M,SCD):-length(P,LP), sumatoria_cuadrado_diferencias(PS,M,SCDS), DIF is LP - M, DIFSQ is DIF*DIF, SCD is DIFSQ + SCDS.













