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
--Barcos

perlaNegra = Barco { nombreBarco = "Perla Negra", tripulacion = [jackSparrow, anneBonny], formaDeSaqueoDelBarco = formaDeSaqueoValioso}
holandesErrante = Barco {nombreBarco = " Holandés Errante", tripulacion = [davidJones], formaDeSaqueoDelBarco = formaDeSaqueoValioso}



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

-- adquirir un nuevo tesoro
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

{-
Tests de Saqueos
saquear anneBonny (formaDeSaqueoEspecifico "oro") Tesoro {nombreTesoro = "oro", valorTesoro = 100}
saquear davidJones formaDeSaqueoConCorazon Tesoro {nombreTesoro = "oro", valorTesoro = 100}
saquear jackSparrow (formaDeSaqueoCompleja [formaDeSaqueoValioso, (formaDeSaqueoEspecifico "sombrero")]) Tesoro {nombreTesoro = "oro", valorTesoro = 100}

-}

--Navegando los siete mares
agregarATripulacion barco pirata = Barco {nombreBarco = (nombreBarco barco), tripulacion = pirata : (tripulacion barco), formaDeSaqueoDelBarco = formaDeSaqueoValioso}
sacarDeTripulacion barco pirata = Barco {nombreBarco = (nombreBarco barco), tripulacion = filter (/= pirata) (tripulacion barco), formaDeSaqueoDelBarco = formaDeSaqueoValioso}
