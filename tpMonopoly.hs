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

--pasarPorElBanco: aumenta el dinero del jugador en $40 y cambia su táctica a “Comprador compulsivo”.
pasarPorElBanco :: Accion
pasarPorElBanco unParticipante = unParticipante {dinero = dinero unParticipante + 40,
                                                 tacticaDeJuego = "Comprador compulsivo"
                                                }

-- enojarse: suma $50 y agrega gritar a sus acciones.
enojarse :: Accion
enojarse participante = participante { dinero = dinero participante + 50,
                                         acciones =  acciones participante ++ [enojarse]
                                    }
                                    
-- gritar: agrega “AHHHH” al principio de su nombre.
gritar :: Accion
gritar participante = participante { nombre = "AHHHH" ++ nombre participante }

esTacticaOferenteOSingular :: String -> Bool
esTacticaOferenteOSingular unaTactica = (unaTactica == "Oferente singular") ||  (unaTactica == "Accionista")

subastar :: Propiedad -> Accion
subastar propiedad participante |  (esTacticaOferenteOSingular . tacticaDeJuego) participante = participante { dinero = dinero participante - precio propiedad, propiedades = propiedades participante ++ [propiedad]}
                                |  otherwise = participante

{-cobrarAlquileres: suma $10 por cada propiedad barata y $20 por cada propiedad cara obtenida.
 Las propiedades baratas son aquellas cuyo precio es menor a $150. -}

cantidadPropiedades :: Participante -> (Int -> Bool) -> Int
cantidadPropiedades participante condicion =  length . filter condicion. map precio  $ propiedades participante

cobrarAlquileres :: Accion
cobrarAlquileres participante= participante { dinero = dinero participante + (((cantidadPropiedades participante (<150)) *10) + ((cantidadPropiedades participante (>=150)) *20)) }

--pagarAAccionistas: resta $100 para todos los casos excepto que la táctica sea “Accionista”, en ese caso suma $200.
pagarAAccionistas :: Accion
pagarAAccionistas participante | tacticaDeJuego participante == "Accionista" = participante {dinero = dinero participante + 200}
                               | otherwise = participante {dinero = dinero participante - 100}

--hacerBerrinchePor: cuando una persona hace un berrinche por una propiedad se le suman $10 y se la hace gritar, 
--la persona sigue haciendo berrinche hasta que llegue a comprar la propiedad que quiere.

hacerBerrinchePor :: Propiedad -> Accion
hacerBerrinchePor propiedad participante | dinero participante >= precio propiedad = participante {dinero = dinero participante - precio propiedad, propiedades = propiedades participante ++ [propiedad] }
                                         | otherwise = hacerBerrinchePor propiedad . gritar $ participante {dinero = dinero participante +10}

-- Modelar la función últimaRonda, que dado un participante retorna una acción equivalente a todas sus acciones.
ultimaRonda :: Participante -> Participante
ultimaRonda participante = foldl1 (.) (acciones participante) participante

juegoFinal :: Participante -> Participante -> String
juegoFinal participante1 participante2 | (dinero . ultimaRonda)  participante1 > (dinero . ultimaRonda)  participante2  = "Ganador " ++ nombre participante1 ++ "!!"
                                       | otherwise = "Ganador " ++ nombre participante2 ++ "!!"
