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
    | cantidadVida personaje - danio < 0 = matarPersonaje personaje
    | otherwise = UnPersonaje (nombre personaje) (poderBasico personaje) (poderEspecial personaje) (superPoderActivo personaje) (cantidadVida personaje - danio)

--Funcion de cambiar nombre
--Funcion de duplicar vida (y por consecuencia dividir al usar 0.5)
--Funcion de activar poder especial

cambiarNombrePersonaje :: String -> Personaje -> Personaje
cambiarNombrePersonaje cambio personaje = UnPersonaje (nombre personaje <> cambio) (poderBasico personaje) (poderEspecial personaje) (superPoderActivo personaje) (cantidadVida personaje)

matarPersonaje :: Personaje -> Personaje
matarPersonaje personaje = UnPersonaje (nombre personaje) (poderBasico personaje) (poderEspecial personaje) (superPoderActivo personaje) (0)

modificarVida :: Float -> Personaje -> Personaje
modificarVida modificador personaje = UnPersonaje (nombre personaje) (poderBasico personaje) (poderEspecial personaje) (superPoderActivo personaje) (div(cantidadVida personaje, modificador))

activarPoderEspecial :: Personaje -> Personaje
matarPersonaje personaje = UnPersonaje (nombre personaje) (poderBasico personaje) (poderEspecial personaje) (True) (cantidadVida personaje)

bolaEspinosa :: Personaje ->  Personaje
bolaEspinosa objetivo = recibirDaño 1000 objetivo 

lluviaDeTuercas :: Personaje -> Personaje ->  Personaje
lluviaDeTuercas atacante objetivo
    | snd.poderEspecial atacante == 0 = recibirDaño -800 objetivo
    | snd.poderEspecial atacante == 1 = modificarVida 2.0 objetivo 
    | otherwise = objetivo

granadaDeEspinas :: Personaje ->  Personaje -> Personaje
granadaDeEspinas atacante objetivo
    | snd.poderEspecial atacante > 3 = cambiarNombrePersonaje objetivo 
    | snd.poderEspecial atacante > 3 && vidaPersonaje objetivo < 800 = matarPersonaje.cambiarNombrePersonaje $ objetivo 
    | otherwise = bolaEspinosa objetivo

torretaCurativa :: Personaje -> Personaje
torretaCurativa objetivo = activarPoderEspecial $ modificarVida 2.0 personaje
