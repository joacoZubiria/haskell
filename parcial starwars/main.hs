data Nave = UnaNave{
    nombre :: String,
    durabilidad :: Int,
    escudo :: Int,
    ataque :: Int,
    poder :: (Nave -> Nave)
}

turbo :: Nave -> Nave
turbo nave = nave{ataque = ataque nave + 25}

superTurbo :: Nave -> Nave
superTurbo nave = nave{durabilidad= durabilidad nave - 45, ataque = turbo.turbo.turbo $ nave}

reparacionEmergencia :: Nave -> Nave
reparacionEmergencia nave = nave{durabilidad = durabilidad nave + 50, ataque = ataque nave - 30 }

superReparacionEmergencia :: Nave -> Nave
superReparacionEmergencia nave = reparacionEmergencia nave{escudo= escudo nave + 100}

tieFighter :: Nave
tieFighter = UnaNave "TIE Fighter" 200 100 50 turbo

xWing :: Nave
xWing = UnaNave "X Wing" 300 150 100 reparacionEmergencia

naveDarthVader :: Nave
naveDarthVader = UnaNave "Nave de Darth Vader" 500 300 200 superTurbo

millenniumFalcon :: Nave
millenniumFalcon = UnaNave "Millennium Falcon" 1000 500 50 superReparacionEmergencia

durabilidadTotal :: [Nave] -> Int
durabilidadTotal naves = foldl (\acc nave -> acc + durabilidad nave ) 0 naves

atacar :: Nave -> Nave -> Nave
atacar nave1 nave2 = realizarAtaque (poder nave1 $ nave1) (poder nave2 $ nave2)

realizarAtaque :: Nave -> Nave -> Nave
realizarAtaque nave1 nave2 
    | ataque nave1  > escudo nave2 = verificarNulo nave2{durabilidad = ataque nave1 - escudo nave2, escudo = 0}
    | otherwise = nave2{escudo = ataque nave1 - escudo nave2}
    
verificarNulo :: Nave -> Nave
verificarNulo nave2 
    | durabilidad nave2 < 0 = nave2{durabilidad = 0}
    | otherwise = nave2

type Estrategia = Nave -> Bool

navesDebiles :: Estrategia
navesDebiles = (<200) . escudo

navesPeligrosas :: Int -> Estrategia
navesPeligrosas num = (<num) . ataque

navesMuertasSiOSi :: Nave -> Estrategia
navesMuertasSiOSi nave1  = (==0) . durabilidad . realizarAtaque nave1 

aplicarEstrategia :: Estrategia -> Nave -> [Nave] -> [Nave]
aplicarEstrategia nave estrategia = map (realizarAtaque nave) . filter estrategia


estrategiaMasEfectiva :: [Nave] -> Nave -> Estrategia -> Estrategia -> Estrategia
comparacionEstrategias flotaEnemiga nave estrategia1 estrategia2 
    | (durabildiadTotalPorEstrategia flotaEnemiga nave estrategia1) > (durabildiadTotalPorEstrategia flotaEnemiga nave estrategia2) = estrategia1
    | otherwise = estrategia2

durabildiadTotalPorEstrategia ::  Nave -> Estrategia -> [Nave] -> Int
ddurabildiadTotalPorEstrategia nave estrategia = durabilidadTotal . aplicarEstrategia estrategia nave 

flotaInfinita :: [Nave]
flotaInfinita = tieFighter:flotaInfinita
--No es posible averiguar la durabilidad total de la flota infinita, ya que estaria haciendo una sumatoria infinita, no se aplica lazy evaluation.
--sin embargo, cuando se aplica una estrategia, se puede acortar la flota infinita y aplicar ataques al rango propuesto, a pesar de que la lista sea infinita