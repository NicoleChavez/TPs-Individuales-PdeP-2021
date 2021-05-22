import Text.Show.Functions

data Participante = Participante {
    nombre         :: String,
    dinero         :: Int,
    tacticaDeJuego :: String,
    propiedades    :: [Propiedad], 
    acciones       :: [Accion]
} deriving (Show)

data Propiedad = Propiedad {
    titulo :: String,
    precio :: Int
} deriving (Eq, Show)

type Accion = Participante -> Participante

--Propiedades
country :: Propiedad 
country = Propiedad {
    titulo = "country",
    precio = 10
}
hotel:: Propiedad 
hotel = Propiedad {
    titulo = "hotel",
    precio = 2000
}

--Participantes
carolina :: Participante
carolina = Participante { 
            nombre         = "Carolina",
            dinero         = 5,
            tacticaDeJuego = "Accionista",
            propiedades    = [hotel], 
            acciones       = [pagarAAccionistas]
}
manuel :: Participante
manuel = Participante {  
        nombre         = "Manuel",
        dinero         = 500,
        tacticaDeJuego = "Oferente singular",
        propiedades    = [],
        acciones       = [pasarPorElBanco, enojarse]
 }

mapDinero :: (Int -> Int) -> Participante -> Participante
mapDinero funcion participante = participante {dinero = (funcion . dinero) participante}

mapPropiedades:: ([Propiedad] -> [Propiedad]) -> Participante -> Participante
mapPropiedades funcion participante = participante {propiedades = (funcion . propiedades) participante }


--pasarPorElBanco: aumenta el dinero del jugador en $40 y cambia su táctica a “Comprador compulsivo”.
pasarPorElBanco :: Accion
pasarPorElBanco participante = mapDinero (+40) participante {tacticaDeJuego = "Comprador compulsivo"}

-- enojarse: suma $50 y agrega gritar a sus acciones.
enojarse :: Accion
enojarse participante = mapDinero (+50) participante {acciones =  acciones participante ++ [enojarse]}
                                    
-- gritar: agrega “AHHHH” al principio de su nombre.
gritar :: Accion
gritar participante = participante { nombre = "AHHHH" ++ nombre participante }


{-subastar: al momento de una subasta solo quienes tengan como tácticas “Oferente singular” o “Accionista” podrán ganar la propiedad. Ganar implica restar el precio de la
propiedad de su dinero y sumar la nueva adquisición a sus propiedades.-}

riquisitoSubasta:: Participante -> Bool
riquisitoSubasta participante = (tacticaDeJuego participante == "Oferente singular") ||  (tacticaDeJuego participante == "Accionista")

subastar :: Propiedad -> Accion
subastar propiedad participante |  riquisitoSubasta participante =  mapPropiedades (++ [propiedad]) (mapDinero (precio propiedad-) participante)
                                |  otherwise = participante

{-cobrarAlquileres: suma $10 por cada propiedad barata y $20 por cada propiedad cara obtenida.
 Las propiedades baratas son aquellas cuyo precio es menor a $150. -}

cobrarAlquileres :: Accion
cobrarAlquileres participante = mapDinero (+ (propiedadesSegun participante (<150) (*10) + propiedadesSegun participante (>=150) (*20) )) participante

propiedadesSegun :: Participante -> (Int -> Bool) -> (Int -> Int) -> Int
propiedadesSegun participante condicion multiplicarPor = (multiplicarPor. length . filter condicion. map precio . propiedades) participante

--pagarAAccionistas: resta $100 para todos los casos excepto que la táctica sea “Accionista”, en ese caso suma $200.
pagarAAccionistas :: Accion
pagarAAccionistas participante | tacticaDeJuego participante == "Accionista" = mapDinero (+200) participante 
--                             | otherwise = mapDinero (-100) participante      por el (-100) no me corria bien el programa no se cual es el error de esta linea
                               | otherwise = participante {dinero = dinero participante -100}

--hacerBerrinchePor: cuando una persona hace un berrinche por una propiedad se le suman $10 y se la hace gritar, 
--la persona sigue haciendo berrinche hasta que llegue a comprar la propiedad que quiere.

hacerBerrinchePor :: Propiedad -> Accion
hacerBerrinchePor propiedad participante | dinero participante >= precio propiedad =  mapPropiedades (++ [propiedad]) (mapDinero (precio propiedad-) participante)
                                         | otherwise = hacerBerrinchePor propiedad . gritar $ participante {dinero = dinero participante +10}

-- Modelar la función últimaRonda, que dado un participante retorna una acción equivalente a todas sus acciones.
ultimaRonda :: Participante -> Participante
ultimaRonda participante = foldl1 (.) (acciones participante) participante

dineroUltimaRonda :: Participante -> Int
dineroUltimaRonda participante = (dinero . ultimaRonda) participante

juegoFinal :: Participante -> Participante -> String
juegoFinal participante1 participante2 | dineroUltimaRonda  participante1 > dineroUltimaRonda participante2  = "Ganador " ++ nombre participante1 ++ "!!"
                                       | otherwise = "Ganador " ++ nombre participante2 ++ "!!"

                                       
