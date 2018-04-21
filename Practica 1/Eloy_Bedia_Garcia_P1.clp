(deffunction system-string (?arg)
  (bind ?arg (str-cat ?arg " > temp.txt"))
  (system ?arg)
  (open "temp.txt" temp "r")
  (bind ?rv (readline temp))
  (close temp)
  ?rv
)

(deffunction hora ()
  (bind ?rv (integer (string-to-field (sub-string 1 2 (system-string "date +\"%k\"")))))
  ?rv
)

(deffunction minutos ()
  (bind ?rv (integer (string-to-field (sub-string 1 2 (system-string "date +\"%M\"")))))
  ?rv
)

(deffunction mrest (?arg)
  (bind ?rv (+ (* (- (- ?arg 1) (hora)) 60) (- 60 (minutos))))
  ?rv
)

(deffacts init
  (Habitacion Recepcion)
  (Habitacion Pasillo)
  (Habitacion Oficina1)
  (Habitacion Oficina2)
  (Habitacion Oficina3)
  (Habitacion Oficina4)
  (Habitacion Oficina5)
  (Habitacion OficinaX2)
  (Habitacion OficinaGerencia)
  (Habitacion Papeleria)
  (Habitacion OficinaX2)
  (Habitacion BanioF)
  (Habitacion BanioM)
  (Puerta Recepcion Pasillo)
  (Puerta Pasillo Oficina1)
  (Puerta Pasillo Oficina2)
  (Puerta Pasillo Oficina3)
  (Puerta Pasillo Oficina4)
  (Puerta Pasillo Oficina5)
  (Puerta Pasillo OficinaX2)
  (Puerta Pasillo OficinaGerencia)
  (Puerta Pasillo Papeleria)
  (Puerta Pasillo BanioF)
  (Puerta Pasillo BanioM)

  (Tarea G1 TG)
  (Tarea G2 TG)
  (Tarea G3 TG)
  (Tarea G4 TG)
  (Tarea G5 TG)
  (Tarea E1 TE)
  (Tarea E2 TE)

  (Empleado G1 Oficina1)
  (Empleado G2 Oficina2)
  (Empleado G3 Oficina3)
  (Empleado G4 Oficina4)
  (Empleado G5 Oficina5)

  (Empleado G1 Mesa1)
  (Empleado G2 Mesa2)
  (Empleado G3 Mesa3)
  (Empleado G4 Mesa4)
  (Empleado G5 Mesa5)
  (Empleado E1 Mesa6)
  (Empleado E2 Mesa7)

  (Disponible G1)
  (Disponible G2)
  (Disponible G3)
  (Disponible G4)
  (Disponible G5)
  (Disponible E1)
  (Disponible E2)

  (UltimoUser TG 0)
  (UltimoUser TE 0)

  (UltimoAtendido TE 0)
  (UltimoAtendido TG 0)


  ;Tiempo que tarda en atender (15 min)
  (TiempoAtencion TG 15)
  (TiempoAtencion TE 15)

  ;Duracion de una jornada de cada empleado (hasta las 15:00)
  (TiempoTotal TG (/ (mrest 15) 5))
  (TiempoTotal TE (/ (mrest 15) 2))

  ;Duracion restante de la jornada de cada empleado (hasta las 15:00)
  (TiempoRestante G1 (/ (mrest 15) 5))
  (TiempoRestante G2 (/ (mrest 15) 5))
  (TiempoRestante G3 (/ (mrest 15) 5))
  (TiempoRestante G4 (/ (mrest 15) 5))
  (TiempoRestante G5 (/ (mrest 15) 5))
  (TiempoRestante E1 (/ (mrest 15) 2))
  (TiempoRestante E2 (/ (mrest 15) 2))

  (NUser TG 0)
  (NUser TE 0)
)

;Comprueba si existe alguna mesa que pueda atender la Solicitud dentro del horario

;Si existe Verifica la Solicitud
(defrule VerificarSolicitudes
  ?x <- (Solicitud ?TX)
  (Tarea ?GX ?TX)
  (TiempoTotal ?TX ?ttotal)
  (TiempoAtencion ?TX ?tatencion)
  ?y <- (TiempoRestante ?GX ?trestante)
  (test (> ?ttotal (+ ?trestante ?tatencion)))
  =>
  (retract ?x ?y)
  (bind ?a (- ?trestante ?tatencion))
  (assert (TiempoRestante ?GX ?a) )
  (assert (SolicitudVerificada ?TX))
)

;Si no existe, la rechaza
(defrule RechazarSolicitudes
  ?x <- (Solicitud ?TX)
  (Tarea ?GX ?TX)
  (TiempoTotal ?TX ?ttotal)
  (TiempoAtencion ?TX ?tatencion)
  (not
    (and
      (TiempoRestante ?GX ?trestante)
      (test (> ?ttotal (+ ?trestante ?tatencion)))
    )
  )
  =>
  (retract ?x)
  (printout t  "Su solicitud ha sido rechazada, por favor, vuelva otro dia" crlf)
)

;Tramita todas las solicitudes que han sido verificadas
(defrule Solicitudes
  ?x <- (SolicitudVerificada ?TX)
  ?y <- (UltimoUser ?TX ?n)
  =>
  (bind ?a (+ ?n 1))
  (assert(NUser ?TX ?a))
  (assert (UltimoUser ?TX ?a ) )
  (retract ?x ?y)
  (printout t  "Solicitud del usuario " ?a " recibida" crlf)
)

;Cuando existe una mesa disponible, se avisa al siguiente cliente
(defrule AvisarSiguiente
  ?x <- (UltimoAtendido ?TX ?n)
  ?y <- (NUser ?TX ?s)
  (test (eq (+ ?n 1) ?s))
  (Tarea ?GX ?TX)
  ?z <- (Disponible ?GX)
  (Empleado ?GX ?MesaX)
  =>
  (retract ?x ?y ?z)
  (printout t  "El usuario " ?s " para " ?TX " pase a la mesa " ?MesaX crlf)
  (assert (Atendiendo ?GX  ?TX ?s))
)

;Se atiende al cliente
(defrule Atender
  ?x <- (Atendiendo ?GX  ?TX ?n)
  ?y <- (TiempoRestante ?GX ?tiempo)
  =>
  (bind ?a (- ?tiempo 15))
  (printout t "El usuario " ?n " esta siendo atendido" crlf)
  (retract ?x ?y)

  (assert(UltimoAtendido ?TX ?n))
  (assert(Disponible ?GX))
)
