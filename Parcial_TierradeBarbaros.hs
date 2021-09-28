import Data.Char
import Text.Show.Functions()

{- Punto 1 19:48

Se sabe que los bárbaros tienen nombre, fuerza, habilidades y objetos, que los ayudarán más adelante en su
lucha contra el mal. 

Se pide definir los siguientes objetos y definir algunos bárbaros de ejemplo
-}

data Barbaro = Barbaro {
    nombre ::       Nombre,
    fuerza ::       Fuerza,
    habilidades ::  [Habilidad],
    objetos ::      [Objeto]
} deriving (Show)

type Objeto = Barbaro -> Barbaro
type Habilidad = String
type Nombre = String
type Fuerza = Int

astro = Barbaro "Astro" 2 ["Escribir Poesia Atroz", "Robar"] []
dave = Barbaro "Dave" 100  ["Robar"] [varitasDefectuosas]


mapFuerza :: (Int -> Fuerza) -> Barbaro -> Barbaro
mapFuerza  funcion barbaro = barbaro { fuerza = funcion . fuerza $ barbaro }

mapHabilidades :: ([Habilidad] -> [Habilidad]) -> Barbaro -> Barbaro
mapHabilidades funcion barbaro = barbaro { habilidades = funcion . habilidades $ barbaro }


--1. Las espadas aumentan la fuerza de los bárbaros en 2 unidades por cada kilogramo de peso. (Asumo que el peso vieno e kg) 

espadas:: Int -> Objeto
espadas peso  = mapFuerza (+ (peso * 2))

--2. Los amuletosMisticos puerco-marranos otorgan una habilidad dada a un bárbaro.

amuletosMisticos :: String -> Objeto
amuletosMisticos habilidad = mapHabilidades (++ [habilidad]) 

--3. Las varitasDsefectuosas, añaden la habilidad de hacer magia, pero desaparecen todos los demás objetos del bárbaro.

varitasDefectuosas :: Objeto
varitasDefectuosas barbaro =  mapHabilidades (++ ["hacerMagia"]) barbaro {objetos = []} 

--4. Una ardilla, que no hace nada.

ardilla:: Objeto
ardilla = id

--5. Una cuerda, que combina dos objetos distintos,obteniendo uno que realiza las transformaciones de los otros dos.

type Cuerda = Objeto -> Objeto -> Objeto -- no se si esta bien este type, lo hice porque Objeto era muy repetitivo y lo tenia q usar en megafonoBarbarico

cuerda :: Cuerda
cuerda unObjeto otroObjeto = unObjeto . otroObjeto

{-Punto 2
El megafono es un objeto que potencia al bárbaro, concatenando sus habilidades y poniéndolas en mayúsculas .

*Main> megafono dave
Barbaro "Dave" 100 ["TEJERESCRIBIRPOESIA"] [<function>,<function>]

Sabiendo esto, definir al megafono, y al objeto megafonoBarbarico, que está formado por una cuerda, una
ardilla y un megáfono.
-}

megafono:: Objeto --En esta funcion tengo duda de donde poner la funcion concat para que me junte todas las habilidades

megafono barbaro = barbaro {habilidades = mayusculas . habilidades $ barbaro}  

mayusculas :: [Habilidad] -> [Habilidad]
mayusculas habilidades = map (incrementarLetras) habilidades 

incrementarLetras :: Habilidad -> Habilidad
incrementarLetras [] = []
incrementarLetras (x:xs) = toUpper x : incrementarLetras xs

megafonoBarbarico :: Cuerda -> Objeto -> Objeto -> Objeto
megafonoBarbarico cuerda ardilla megafono = cuerda ardilla megafono

-----------------------------------------------------------------------------------------------------------------------------------------------------

{-Punto 3 - Aventuras

Los bárbaros suelen ir de aventuras por el reino luchando contra las fuerzas del mal, pero ahora que tienen nuestra ayuda, quieren que se les diga 
si un grupo de bárbaros puede sobrevivir a cierta aventura. Una aventura se compone de uno o más eventos, por ejemplo: 
-}

--1. invasionDeSuciosDuendes: Un bárbaro sobrevive si sabe “Escribir Poesía Atroz”

type Prueba = Barbaro -> Bool

invasionDeSuciosDuendes :: Prueba
invasionDeSuciosDuendes = any (== "Escribir Poesia Atroz"). habilidades 

--2. cremalleraDelTiempo: Un bárbaro sobrevive si no tiene pulgares. Los bárbaros llamados Faffy y Astro no tienen pulgares, los demás sí.

cremalleraDelTiempo :: Prueba
cremalleraDelTiempo barbaro= not (nombre barbaro == "Faffy" || nombre barbaro == "Astro")

cremalleraDelTiempo' = not . noTienePulgares . nombre -- hice esta solucion alternativa usando patter matching pero nose si es la mas apropiada

noTienePulgares :: Nombre -> Bool
noTienePulgares "Faffy" = True
noTienePulgares "Astro" = True
noTienePulgares _ = False

--3. ritualDeFechorias: Un bárbaro puede sobrevivir si pasa una o más pruebas como las siguientes:


ritualDeFechorias:: [Prueba] -> Prueba
ritualDeFechorias pruebas barbaro = and . map (realizarPrueba barbaro) $ pruebas

realizarPrueba :: Barbaro -> Prueba -> Bool
realizarPrueba barbaro prueba = prueba barbaro

--a. saqueo: El bárbaro debe tener la habilidad de robar y tener más de 80 de fuerza.

saqueo:: Prueba
saqueo barbaro = (any (== "Robar") . habilidades) barbaro && ((>80) . fuerza) barbaro

--b. gritoDeGuerra: El bárbaro debe tener un poder de grito de guerra igual a la cantidad de letra de sus habilidades. 
--El poder necesario para aprobar es 4 veces la cantidad de objetos del bárbaro.

gritoDeGuerra :: Prueba
gritoDeGuerra barbaro = tienePoder barbaro ==  poderDeGrito barbaro

poderDeGrito :: Barbaro -> Int
poderDeGrito = length . concat . habilidades 

tienePoder :: Barbaro -> Int
tienePoder = (*4) . length . objetos

--c. caligrafia: El bárbaro tiene caligrafía perfecta (para el estándar barbárico de la época) si 
--sus habilidades contienen más de 3 vocales y comienzan con mayúscula.


{- Esta es la funcion que no me salio q pase por ds

caligrafia :: Prueba
caligrafia = all (tieneCaligrafiaPerfecta) habilidades 

tieneCaligrafiaPerfecta :: Habilidad -> Bool
tieneCaligrafiaPerfecta habilidad = comienzaConMayuscula habilidad && contieneXVocales habilidad

comienzaConMayuscula [] = False
comienzaConMayuscula [letra: letras] = letra == toUpper letra

contieneXVocales :: Habilidad -> Bool
contieneXVocales habilidad = (>3) . length . filter (hayVocales) $ habilidad

hayVocales :: Char -> Bool
hayVocales letra  = elem letra ['a','e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U'] 

-}

--Definir la función sobrevivientes que tome una lista de bárbaros y una aventura, y diga cuáles bárbaros la sobreviven (es decir, pasan todas las pruebas)
type Aventura = [Prueba] -- lista de funciones tipo Barbaro -> Bool
 
-- En esta tengo duda si es la solucion mas apropiada tambien pero funciona jajaja

sobrevivientes :: [Barbaro] -> Aventura -> [Barbaro]
sobrevivientes barbaros aventura = map fst (filter sobrevivieron (tuplaBarbaroPrueba barbaros aventura))
                
-----------------------------------------------------------------------------------------------------------------------------------------------------
{-Punto 4 - Dinastía
A) Los bárbaros se marean cuando tienen varias habilidades iguales. Por todo esto, nos piden desarrollar una función que elimine los elementos repetidos de una lista

 > sinRepetidos [1,2,3,4,4,5,5,6,7]
[1,2,3,4,5,6,7]
-}

{-B) El descendiente de un bárbaro comparte su nombre, y un asterisco por cada generación. Por ejemplo "Dave*", "Dave**" , "Dave***" , etc.

Además, tienen en principio su mismo poder, habilidades sin repetidos, y los objetos de su padre, pero antes de
pasar a la siguiente generación, utilizan (aplican sobre sí mismos) los objetos. Por ejemplo, el hijo de Dave será equivalente a:

(ardilla.varitasDefectuosas) (Barbaro "Dave*" 100 ["tejer","escribirPoesia"] [ardilla, varitasDefectuosas])
-}

--Definir la función descendientes, que dado un bárbaro nos de sus infinitos descendientes

--me faltaria agregar la funcion sinRepetidos 

descendientes :: Barbaro -> [Barbaro]
descendientes barbaro= iterate mismoPoder barbaro

mismoPoder :: Barbaro -> Barbaro 
mismoPoder barbaro = foldl1 (.) (objetos barbaro) barbaro {nombre = nombre barbaro ++ "*", objetos = objetos barbaro } 


{-C. Pregunta: ¿Se podría aplicar sinRepetidos sobre la lista de objetos? 
Si pero habria que modificarla y agregar la funcion show para saber si un  objeto esta repetido, ya que un objeto es una funcion, y asi puedo comparar los nombres de las funciones  


¿Y sobre el nombre de un bárbaro? ¿Por qué?
Tambien la puedo usar ya que un nombre es un String al igual que una habilidad

-}