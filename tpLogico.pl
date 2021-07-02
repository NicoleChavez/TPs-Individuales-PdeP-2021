%BASE DE CONOCIMIENTOS

viveEnLaMansionDreadbury(tiaAgatha).
viveEnLaMansionDreadbury(mayordomo).
viveEnLaMansionDreadbury(charles).

odiaAAlguien(tiaAgatha,UnaPersona):-    
    viveEnLaMansionDreadbury(UnaPersona),
    UnaPersona\=mayordomo.
odiaAAlguien(charles,UnaPersona):-
    viveEnLaMansionDreadbury(UnaPersona),
    not(odiaAAlguien(tiaAgatha,UnaPersona)).
odiaAAlguien(mayordomo,UnaPersona):-
    odiaAAlguien(tiaAgatha,UnaPersona).

esMasRico(UnaPersona,tiaAgatha):-
    viveEnLaMansionDreadbury(UnaPersona),   
    not(odiaAAlguien(mayordomo,UnaPersona)).


%Punto 1
%a. El programa debe resolver el problema de quién mató a la tía Agatha.   

esAsesino(UnaPersona,Victima) :-
    odiaAAlguien(UnaPersona,Victima),
    not(esMasRico(UnaPersona,Victima)),
    viveEnLaMansionDreadbury(UnaPersona).

%b. Mostrar la consulta utilizada y la respuesta obtenida.
%Consulta:
%esAsesino(UnaPersona,tiaAgatha)
   

%Respuesta obtenida:
%?- UnaPersona = tiaAgatha ;
%   false. 

%Punto 2.
/** 
Consultas:
 odiaAAlguien(_,milhouse).
 false.

odiaAAlguien(charles,Persona).
Persona = mayordomo ;
false.

odiaAAlguien(Persona,tiaAgatha).
Persona = tiaAgatha ;
Persona = mayordomo.

odiaAAlguien(Odiador,Odiado).
Odiador = Odiado,       Odiado = tiaAgatha ;
Odiador = tiaAgatha,    Odiado = charles ;
Odiador = charles,      Odiado = mayordomo ;
Odiador = mayordomo,    Odiado = tiaAgatha ;
Odiador = mayordomo,    Odiado = charles.

odiaAAlguien(mayordomo,_).
true.
**/


