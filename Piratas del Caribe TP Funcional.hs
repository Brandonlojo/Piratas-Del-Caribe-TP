import Text.Show.Functions

{-
data FormasDeSaqueos = FormaDeSaqueoValioso Tesoro |
                                          FormaDeSaqueoConCorazon Tesoro |
                                          FormaDeSaqueoEspecifico Nombre Tesoro |
                                          FormaDeSaqueoCompleja ListaDeFormas Tesoro
-}

type FormaDeSaqueo = Tesoro -> Bool
formaDeSaqueoValioso :: FormaDeSaqueo

data Pirata = Pirata {
nombre:: String,
tesoros:: [Tesoro]
} deriving (Show, Eq)

data Tesoro = Tesoro {
nombreTesoro :: String,
valorTesoro :: Int
} deriving (Show, Eq)

data Barco = Barco {
  nombreBarco :: String,
  tripulacion :: [Pirata],
  formaDeSaqueoDelBarco :: FormaDeSaqueo
}deriving (Show)

data Isla = Isla {
  nombreIsla :: String,
  botinIsla :: [Tesoro]
} deriving (Show  )

-- Piratas
jackSparrow = Pirata {  nombre = "Jack Sparrow", tesoros = [ Tesoro{ nombreTesoro ="brujula", valorTesoro =10000},
                                                                                                    Tesoro{ nombreTesoro ="frasco de arena", valorTesoro =0}
                                                                                                  ]
                                    }
davidJones = Pirata { nombre = "DavidJones",  tesoros = [Tesoro{ nombreTesoro ="cajita musical", valorTesoro = 1}]
                                  }

anneBonny = Pirata { nombre = "AnneBonny", tesoros = [Tesoro{ nombreTesoro = "doblones", valorTesoro = 100},
                                                                                              Tesoro{ nombreTesoro = "frasco de arena", valorTesoro =1}
                                                                                              ]
                                 }
elizabethSwann = Pirata { nombre = "Elizabeth Swann", tesoros = [Tesoro {nombreTesoro = "moneda cofre muerto", valorTesoro = 100},
                                                                                                            Tesoro {nombreTesoro = "espada de hierro", valorTesoro = 50}
                                                                                                            ]
                                        }

willTurner = Pirata { nombre = " Will Turner", tesoros = [Tesoro {nombreTesoro = "cuchillo de su padre", valorTesoro = 5}]}

--Barcos

perlaNegra = Barco { nombreBarco = "Perla Negra", tripulacion = [jackSparrow, anneBonny], formaDeSaqueoDelBarco = formaDeSaqueoValioso}
holandesErrante = Barco {nombreBarco = " Holandés Errante", tripulacion = [davidJones], formaDeSaqueoDelBarco = formaDeSaqueoValioso}

--Islas

islaTortuga = Isla {nombreIsla = "Isla Tortuga", botinIsla = [Tesoro {nombreTesoro = "Frasco de arena", valorTesoro = 1}]}
islaDelRon = Isla {nombreIsla = " Isla del Ron", botinIsla = [Tesoro {nombreTesoro = "Botella de Ron", valorTesoro = 25}]}



--cantidad de tesoros
cantidadDeTesoros pirata = (length.tesoros) pirata

--afortunado
esAfortunado pirata = 10000 < sum (map valorTesoro (tesoros pirata))

--mismo tesoro, pero distinto valor
mismoTesoro tesoro1 tesoro2 = nombreTesoro tesoro1 == nombreTesoro tesoro2 &&
                                                    valorTesoro tesoro1 /= valorTesoro tesoro2

compararTesoros tesoros tesoro = any (mismoTesoro tesoro) tesoros

tienenMismoTesoro pirata1 pirata2 = any (compararTesoros (tesoros pirata2)) (tesoros pirata1)

-- tesoro mas valioso de un pirata
tesoroMasValioso pirata = maximum ( map valorTesoro (tesoros pirata))

-- adquirir un nuevo tesoro (se puede mejorar para cuando es lista de un tesoro o solo un tesoro (guardas))
adquirirTesoro tesoro pirata = tesoro ++ tesoros pirata

-- verificar si el tesoro es valioso
tesoroValioso tesoro = valorTesoro tesoro > 100


-- devuelvo un nuevo pirata con la lista de tesoros modificada según la condición (no modifica al existente)
perderTesoro condicion pirata = Pirata {nombre = (nombre pirata), tesoros = filter (condicion) (tesoros pirata)}

--condiciones de perder tesoros
condicionPorNombreIgual  nombre  tesoro = (nombre/=(nombreTesoro tesoro))
condicionDeTesorosValiosos tesoro = (not.tesoroValioso) tesoro

--FORMAS DE SAQUEO
agregarTesoro tesoro pirata = Pirata {nombre = (nombre pirata) , tesoros = tesoro:tesoros pirata}

formaDeSaqueoValioso tesoro= tesoroValioso tesoro
formaDeSaqueoEspecifico nombre tesoro = nombreTesoro tesoro == nombre
formaDeSaqueoConCorazon tesoro = False
formaDeSaqueoCompleja listaDeFormas tesoro = any ($ tesoro) listaDeFormas
--(\tesoro formaDeSaqueo -> formaDeSaqueo tesoro) que equivale a $

saquear pirata formaSaqueo tesoro | formaSaqueo  tesoro= agregarTesoro tesoro pirata
                                                         | otherwise = pirata


--Navegando los siete mares
agregarATripulacion barco pirata = Barco {nombreBarco = (nombreBarco barco), tripulacion = pirata : (tripulacion barco), formaDeSaqueoDelBarco = formaDeSaqueoValioso}
sacarDeTripulacion barco pirata = Barco {nombreBarco = (nombreBarco barco), tripulacion = filter (/= pirata) (tripulacion barco), formaDeSaqueoDelBarco = formaDeSaqueoValioso}

anclarEnIsla isla barco = Barco {nombreBarco = (nombreBarco barco), tripulacion =map (generarNuevoPirata (botinIsla isla)) (tripulacion barco), formaDeSaqueoDelBarco = (formaDeSaqueoDelBarco barco)}

generarNuevoPirata tesoro pirata = Pirata {nombre= (nombre pirata), tesoros = (adquirirTesoro tesoro pirata)}

--atacarCiudad ciudad


{-
Tests de Saqueos
saquear anneBonny (formaDeSaqueoEspecifico "oro") Tesoro {nombreTesoro = "oro", valorTesoro = 100}
saquear davidJones formaDeSaqueoConCorazon Tesoro {nombreTesoro = "oro", valorTesoro = 100}
saquear jackSparrow (formaDeSaqueoCompleja [formaDeSaqueoValioso, (formaDeSaqueoEspecifico "sombrero")]) Tesoro {nombreTesoro = "oro", valorTesoro = 100}

 Tests de Barcos
 agregarATripulacion perlaNegra elizabethSwann
((flip sacarDeTripulacion willTurner).agregarATripulacion perlaNegra) willTurner

 -}
