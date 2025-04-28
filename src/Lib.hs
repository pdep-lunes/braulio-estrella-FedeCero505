module Lib () where

import Text.Show.Functions ()

-- 0 Cura Y 1 Daña
type PoderEspecial = (String, Int) 

data Personaje = UnPersonaje
    {
    nombre :: String,
    poderBasico :: String,
    poderEspecial :: PoderEspecial,
    superPoderActivo :: Bool,
    cantidadVida :: Int
    }

vidaPersonaje :: Personaje -> Int
vidaPersonaje personaje = cantidadVida personaje


recibirDaño :: Int -> Personaje -> Personaje
recibirDaño danio personaje
    | cantidadVida personaje - danio < 0 = UnPersonaje (nombre personaje) (poderBasico personaje) (poderEspecial personaje) (superPoderActivo personaje) (0) 
    | otherwise = UnPersonaje (nombre personaje) (poderBasico personaje) (poderEspecial personaje) (superPoderActivo personaje) (cantidadVida personaje - danio)

--Funcion de cambiar nombre
--Funcion de duplicar vida (y por consecuencia dividir al usar 0.5)
--Funcion de activar poder especial



bolaEspinosa :: Personaje ->  Personaje
bolaEspinosa objetivo = recibirDaño 1000 objetivo 

lluviaDeTuercas :: Personaje -> Personaje ->  Personaje
lluviaDeTuercas atacante objetivo
    | snd.poderEspecial atacante == 0 = recibirDaño -800 objetivo
    | snd.poderEspecial atacante == 1 = recu
    | otherwise = objetivo

granadaDeEspinas :: Personaje ->  Personaje -> Personaje
granadaDeEspinas atacante objetivo
    | snd.poderEspecial atacante > 3 = UnPersonaje (nombre objetivo <> "Espina estuvo aquí") (poderBasico objetivo) (poderEspecial objetivo) (superPoderActivo objetivo) (cantidadVida objetivo)
    | snd.poderEspecial atacante > 3 && vidaPersonaje objetivo < 800 = UnPersonaje (nombre objetivo <> "Espina estuvo aquí") (poderBasico objetivo) (poderEspecial objetivo) (False) (0)
    | otherwise = bolaEspinosa objetivo

    
torretaCurativa :: Personaje -> Personaje
torretaCurativa objetivo = UnPersonaje (nombre objetivo) (poderBasico objetivo) (poderEspecial objetivo) (True) (cantidadVida objetivo*2)

