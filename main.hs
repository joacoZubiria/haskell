import Data.List
import Text.Show.Functions ()

type Material = String

data Personaje = UnPersonaje {
	nombre:: String,
	puntaje:: Int,
	inventario:: [Material]
} deriving Show

data Receta = UnaReceta{
    objetoRes::String,
    materiales::[Material],
    tiempo::Int
} deriving Show

{-
El objeto se craftea sólo si se cuenta con todos los materiales requeridos antes de comenzar la tarea. 
En caso contrario, no se altera el inventario, pero se pierden 100 puntos.
La cantidad de puntos del jugador se incrementa a razón de 10 puntos por segundo utilizado en el crafteo.
El jugador debe quedar sin los materiales usados para craftear
El jugador debe quedar con el nuevo objeto en su inventario

Por ejemplo, si un jugador con 1000 puntos tenía un sueter, una fogata y dos pollos y craftea un pollo asado, mantiene su sueter intacto, se queda con un sólo pollo, 
sin fogatas y pasa a tener un pollo asado en su inventario. Su puntaje pasa a ser 4000.
-}

craftear:: Personaje -> Receta -> Personaje
craftear personaje receta 
    | verificarMateriales personaje receta = UnPersonaje { nombre = nombre personaje, puntaje = (puntaje personaje) + (tiempo receta) * 10, inventario = nuevoInventario (inventario personaje) (materiales receta) }
    |otherwise = personaje{puntaje = (puntaje personaje)-100}

verificarMateriales:: Personaje -> Receta -> Bool
verificarMateriales personaje receta = all (flip elem (inventario personaje)) (materiales receta)

nuevoInventario :: [String] -> [String] -> [String]
nuevoInventario inventario [] = inventario
nuevoInventario inventario (o1:hs) 
    | elem o1 inventario =  nuevoInventario (filter (/= o1) inventario) hs
    | otherwise = nuevoInventario inventario []

{-
Dado un personaje y una lista de recetas:
Encontrar los objetos que podría craftear un jugador y que le permitirían como mínimo duplicar su puntaje.
Hacer que un personaje craftee sucesivamente todos los objetos indicados en la lista de recetas.
Averiguar si logra quedar con más puntos en caso de craftearlos a todos sucesivamente en el orden indicado o al revés.
-}

verSiCraftea :: Personaje -> [Receta] -> [String]
verSiCraftea personaje recetas = map objetoRes (filter (\receta -> (verificarMateriales personaje receta) && ((puntaje personaje)*2 >= ((tiempo receta)*10 + (puntaje personaje)))) recetas)

crafteoSucesivoIAD:: Personaje -> [Receta] -> Int
crafteoSucesivoIAD personaje recetas = foldl (\acc receta -> acc + puntaje (craftear personaje receta)) (puntaje personaje) recetas

crafteoSucesivoDAI:: Personaje -> [Receta] -> Int
crafteoSucesivoDAI personaje recetas = foldr (\receta acc-> acc + puntaje (craftear personaje receta)) (puntaje personaje) recetas

compararRes::Personaje -> [Receta] -> String
compararRes personaje recetas
    | crafteoSucesivoDAI personaje recetas > crafteoSucesivoIAD personaje recetas = "Mas puntos al reves"
    | otherwise = "Mas puntos izq a der"


{-
El mundo del videojuego se compone de biomas, donde cada bioma tiene muchos materiales. 
Para poder minar en un bioma particular el personaje debe tener un elemento necesario según el bioma. 
Por ejemplo, en un bioma ártico, donde hay hielo, iglues y lobos, se debe tener un suéter.

Cuando un personaje va a minar a un bioma, si cuenta con el elemento necesario, 
agrega a su inventario uno de los materiales del bioma y gana 50 puntos. 
La forma de elegir cuál es el material del bioma a conseguir, depende de la herramienta que use al minar. 
Por ejemplo, el hacha hace que se mine el último de los materiales del bioma, 
mientras que la espada actúa sobre el primero de ellos. 
Existe tambien el pico, que por ser más preciso permite apuntar 
a una determinada posición de los materiales. 
Por ejemplo, si un personaje con un sueter en su inventario mina el artico con un pico de precisión 1, 
agrega un iglú a su inventario. En caso de no poder minar por no tener lo necesario el personaje 
se va con las manos vacías y sigue como antes.

Hacer una función minar, que dada una herramienta, un personaje y un bioma, permita obtener cómo queda el personaje.
Definir las herramientas mencionadas y agregar dos nuevas. 
Mostrar ejemplos de uso. Hacerlo de manera que agregar en el futuro otras herramientas no implique modificar la función minar.
Utilizando la función composición, usar una que permita obtener un material del medio del conjunto de materiales.
Utilizando una expresión lambda, inventar una nueva herramienta, diferente a las anteriores
¿Qué pasa al intentar minar en un bioma con infinitos materiales? 
-}

data Bioma = UnBioma{
    nombreBioma :: String,
    materialesBioma :: [Material],
    necesario :: Material
} deriving Show

type Herramienta = [Material] -> Material

hacha :: Herramienta
hacha = last

espada :: Herramienta
espada = head

pico :: Int -> Herramienta
pico prec materiales = materiales !! prec 

hoz :: Int -> Herramienta -- Int indica cantidad de fuerza
hoz fuerza= last . reverse . drop fuerza

pala :: Herramienta
pala = \materiales -> (head.tail) materiales

materialMedio :: Herramienta
materialMedio materiales= (head.take(div (length materiales) 2)) materiales

minarEnBioma :: Personaje -> Herramienta -> Bioma -> Personaje
minarEnBioma personaje herramienta bioma 
    | elem (necesario bioma) (inventario personaje) = personaje{inventario = ((herramienta(materialesBioma bioma)):(inventario personaje))}
    | otherwise = personaje

biomaInfinito :: Bioma
biomaInfinito = UnBioma "cueva" (infinitosMat "antorcha") "antorcha"

infinitosMat :: Material -> [Material]
infinitosMat mat = mat:infinitosMat mat
-- nose si funciona no tengo compilador xd