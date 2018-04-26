(deffacts init
    (Edad A 20)
    (Edad B 17)
    (Edad C 14)
    (Edad D 21)
    (Edad E 18)
    (Edad F 22)
)

(defrule EncontrarMinimo
    (Minimo)
    (Edad $? ?edad)
    (not (Edad $? ?x &:(< ?x ?edad)))
    =>
    (assert (MinimoEdad ?edad))
)