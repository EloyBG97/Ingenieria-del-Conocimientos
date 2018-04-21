(deffacts init
	(Cientifico Newton)
	(Cientifico Faraday)
	(Cientifico Hertz)
	(Cientifico Enstein)
	(Cientifico Marie Curie)
)

(defrule InicioContador
	?f <- (ContarHechos Cientifico)
	=>
	(assert (NumeroHechos Cientifico  0)
         (ComezarRecueno Cientifico)
         (retract ?f)
)

(defrule InicioContador2
        (declare (salience 1))
	(ContarHechos Cientifico)
        ?f<- (NumeroHechos Cientifico  ?)
	=>
(retract ?f)
)
(defrule HacerHechosContables
	(ComezarRecueno Cientifico)
	(Cientifico $?nombre_cientifico)
	=>
	(assert (SumarCientifico))
)


(defrule ContarHechos
        (declare (salience 1)) 
	?x <- (SumarCientifico)
	?y <- (NumeroHechos Cientifico  ?n)
	=>
	(printout t "Hay " (+ ?n 1) " Cientificos" crlf)
	(retract ?x ?y)
	(assert (NumeroHechos Cientifico (+ ?n 1) ) )
)
