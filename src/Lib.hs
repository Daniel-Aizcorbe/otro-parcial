import Text.Show.Functions

data Heroe = UnHeroe {
    epiteto :: String,
    reconocimiento :: Int,
    artefactos :: [Artefacto],
    tareasRealizadas :: Labor
} deriving (Show)

type Labor = [Tarea]

data Artefacto = UnArtefacto {
    rareza :: Int
} deriving (Show)

data Bestia = UnaBestia {
    nombre :: String,
    debilidad :: Debilidad
} deriving (Show)

type Debilidad = Heroe -> Bool

pasaALaHistoria :: Heroe -> Heroe
pasaALaHistoria heroe
    | 1000 < reconocimiento heroe = cambiarEpiteto "el mitico" heroe   
    | 500 <= reconocimiento heroe = agregarArtefacto lanzaDelOlimpo.cambiarEpiteto "El margnifico" $ heroe
    | 500 > reconocimiento heroe && 100 < reconocimiento heroe = agregarArtefacto xiphos.cambiarEpiteto "Hoplita" $ heroe
    | otherwise = heroe

cambiarEpiteto :: String -> Heroe -> Heroe
cambiarEpiteto nuevoEpiteto heroe = heroe {epiteto = nuevoEpiteto}

agregarArtefacto :: Artefacto -> Heroe -> Heroe
agregarArtefacto artefacto heroe = heroe {artefactos = artefacto:(artefactos heroe)}

lanzaDelOlimpo :: Artefacto
lanzaDelOlimpo = UnArtefacto {
    rareza = 100
}

xiphos :: Artefacto
xiphos = UnArtefacto {
    rareza = 50
}

type Tarea = Heroe -> Heroe

encontrarArtefacto :: Artefacto -> Tarea 
encontrarArtefacto artefacto heroe = agregarArtefacto artefacto.ganarReconocimiento (rareza artefacto) $ heroe

ganarReconocimiento :: Int -> Heroe -> Heroe
ganarReconocimiento cantidad heroe = heroe {reconocimiento = (reconocimiento heroe) + cantidad}

escalarElOlimpo :: Tarea
escalarElOlimpo heroe = agregarArtefacto relampagoDeZeus.quitarArtefactosDeMenosDeXRareza 1000.triplicarRarezaDeArtefactos $ heroe

triplicarRarezaDeArtefactos :: Heroe -> Heroe
triplicarRarezaDeArtefactos heroe = heroe {artefactos = map (multiplicarRarezaPor 3).artefactos $ heroe}

multiplicarRarezaPor :: Int -> Artefacto -> Artefacto
multiplicarRarezaPor cantidad artefacto = artefacto {rareza = (cantidad*).rareza $ artefacto}

quitarArtefactosDeMenosDeXRareza :: Int -> Heroe -> Heroe
quitarArtefactosDeMenosDeXRareza rarezaMinima heroe = heroe {artefactos = filter ((rarezaMinima>=).rareza).artefactos $ heroe}

relampagoDeZeus :: Artefacto
relampagoDeZeus = UnArtefacto {
    rareza = 500
}

ayudarACruzarLaCalle :: Int -> Tarea
ayudarACruzarLaCalle cantidadDeCuadras = cambiarEpiteto ("gros"++(replicate cantidadDeCuadras 'o'))

matarUnaBestia :: Bestia -> Heroe -> Heroe
matarUnaBestia bestia heroe 
    | ($ heroe).debilidad $ bestia = cambiarEpiteto ("El asesino de "++(nombre bestia)) heroe
    | otherwise                    = quitarArtefactos 1.cambiarEpiteto "El cobarde" $ heroe

quitarArtefactos :: Int -> Heroe -> Heroe
quitarArtefactos cantidad heroe = heroe {artefactos = drop cantidad.artefactos $ heroe}

matarAlLeonDeNemea :: Tarea
matarAlLeonDeNemea = matarUnaBestia UnaBestia {
    nombre = "Leon de Nemea",
    debilidad = (>20).length.epiteto
}

hacerTarea :: Tarea -> Heroe -> Heroe
hacerTarea tarea heroe = tarea $ heroe

presumir :: Heroe -> Heroe -> (Heroe,Heroe)
presumir heroe otroHeroe
    | tieneMasReconocimiento heroe otroHeroe = (heroe,otroHeroe)
    | tieneMasReconocimiento otroHeroe heroe = (otroHeroe,heroe)
    | sumatoriaDeRarezasDeArtefactos heroe /= sumatoriaDeRarezasDeArtefactos otroHeroe = ordenarSegunSumatoriaDeRarezas heroe otroHeroe
    | otherwise = presumir (ejecutarTareasDelSegundo heroe otroHeroe) (ejecutarTareasDelSegundo otroHeroe heroe)

tieneMasReconocimiento :: Heroe -> Heroe -> Bool
tieneMasReconocimiento heroe = (reconocimiento heroe >).reconocimiento

sumatoriaDeRarezasDeArtefactos :: Heroe -> Int
sumatoriaDeRarezasDeArtefactos  = sum.map rareza.artefactos 

ordenarSegunSumatoriaDeRarezas :: Heroe -> Heroe -> (Heroe,Heroe)
ordenarSegunSumatoriaDeRarezas heroe otroHeroe
    | sumatoriaDeRarezasDeArtefactos heroe > sumatoriaDeRarezasDeArtefactos otroHeroe = (heroe,otroHeroe)
    | otherwise = (otroHeroe,heroe)

ejecutarTareasDelSegundo :: Heroe -> Heroe -> Heroe
ejecutarTareasDelSegundo heroe otroHeroe = ejecutarLabor (reverse.tareasRealizadas $ otroHeroe) heroe 

ejecutarLabor :: Labor -> Heroe -> Heroe
ejecutarLabor tareas heroe = foldr ($) heroe tareas

{-
PUNTO 8
    nunca terminaria de evaluar, ya que de primera no se cumple ninguna caso base y como no tienen tareas realizadas, en el caso recursivo no cambiarian los atributos de los heroes.
-}

realizarUnLabor :: Labor -> Heroe -> Heroe
realizarUnLabor labor heroe = ejecutarLabor labor heroe

{-
PUNTO 10
    no, primero, como la funcion que ejecuta las tareas del labor esta definida con reverse nunca iniciaria a evaluar al heroe y ademas, aun si no lo hubiese definido asi, al ser una lista infinita de tareas, nunca terminaria de ejecutarse el foldr.
-}

