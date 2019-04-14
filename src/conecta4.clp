;* IMPORTANTE:
;utilizaremos el hecho (turno j1) o (turno j2) para cambiar los turnos
;utilizaremos el hecho (comprobar-ganador) para verificar si hay algún ganador.

(defglobal ?*fila* = 3)
(defglobal ?*columna* = 3)
(defglobal ?*fichas* = 2)
(defglobal ?*profundidad* = 0)
(defglobal ?*limiteProfundidad* = 6)
(defglobal ?*identPadre* = 0)
(defglobal ?*posibles_conexiones* = (create$))

(deftemplate tablero
  (slot jugador (allowed-values max min))
  (multislot valor
    (type LEXEME)
    (allowed-lexemes x o .)
    (default .)
  )
)

(deftemplate tableroAux
  (slot jugador (allowed-values max min))
  (multislot valor
    (type LEXEME)
    (allowed-lexemes x o .)
    (default .)
  )
  (slot identificador
    (type INTEGER)
  )
  (slot padre 
    (type INTEGER)
  )
  (slot profundidad
    (type INTEGER)
    (default 0)
  )
  (slot heuristico
    (type INTEGER)
    (default 0)
  )
  (slot heuristicoHijo
    (type INTEGER)
  )
  (slot hacerHijo
    (type INTEGER)
    (default 1)
  )
)

(deffunction heuristico(?valor ?simbolo) ;valor es un array de filas y las filas son srings.
  (bind ?conexiones-x 0)
  (bind ?conexiones-o 0)
  (progn$ (?con ?*posibles_conexiones*)
    (bind ?hayx FALSE)
    (bind ?hayo FALSE)
    (bind ?hayp FALSE)
    (bind ?conexion (explode$ ?con))
    (loop-for-count (?posicion 1 (/ (length$ ?conexion) 2))
      (if (eq (nth$ (nth$ (- (* ?posicion 2) 1) ?conexion) (explode$ (nth$ (nth$ (* ?posicion 2) ?conexion) ?valor))) x) then
        (bind ?hayx TRUE)
      )
      (if (eq (nth$ (nth$ (- (* ?posicion 2) 1) ?conexion) (explode$ (nth$ (nth$ (* ?posicion 2) ?conexion) ?valor))) o) then
        (bind ?hayo TRUE)
      )
      (if (eq (nth$ (nth$ (- (* ?posicion 2) 1) ?conexion) (explode$ (nth$ (nth$ (* ?posicion 2) ?conexion) ?valor))) .) then
        (bind ?hayp TRUE)
      )
    )
    (if ?hayx then
      (if (not ?hayo) then
        (if ?hayp then
          (bind ?conexiones-x (+ ?conexiones-x 1))
        else
          (if (eq ?simbolo x) then
            (return 1000000)
          else
            (return -1000000)
          )
        )
      )
    else
      (if ?hayo then
        (if ?hayp then
          (bind ?conexiones-o (+ ?conexiones-o 1))
        else
          (if (eq ?simbolo x) then
            (return -1000000)
          else
            (return 1000000)
          )
        )
      else
        (bind ?conexiones-x (+ ?conexiones-x 1))
        (bind ?conexiones-o (+ ?conexiones-o 1))
      )
    )
  )
  (if (eq ?simbolo x) then
    (return (- ?conexiones-x ?conexiones-o))
  else
    (return (- ?conexiones-o ?conexiones-x))
  )
)

(defrule Estado-Inicial
  ?inicial <- (initial-fact)
=>
  (printout t "Cuantas columnas quieres?" crlf)
  (bind ?*columna* (read))
  (printout t "Cuantas filas quieres?" crlf)
  (bind ?*fila* (read))
  (printout t "De cuantas fichas conectadas quieres que sea el juego?" crlf)
  (bind ?*fichas* (read))
  
  (bind $?tab (create$))  
  (bind $?filamulti (create$))
  
  (loop-for-count (?j 1 ?*columna*) do
    (bind $?filamulti (insert$ $?filamulti 1 .))
  )
  (bind $?filastrin (implode$ $?filamulti))
    
  (loop-for-count (?i 1 ?*fila*)
    (bind $?tab (insert$ $?tab 1 $?filastrin))
  )
  (assert (tablero (valor (create$ $?tab))))
  
  (loop-for-count (?f 1 ?*fila*) do
    (loop-for-count (?c 1 ?*columna*) do
      (if (<= (- ?f 1) (- ?*fila* ?*fichas*)) then
        (bind ?conexion_vertical_arriba (create$))
        (loop-for-count (?n 0 (- ?*fichas* 1)) do
          (bind ?coordenadas (create$ ?c (+ ?f ?n)))
          (bind $?conexion_vertical_arriba (insert$ $?conexion_vertical_arriba (+ (length$ $?conexion_vertical_arriba) 1) ?coordenadas))
        )
        (bind ?*posibles_conexiones* (insert$ ?*posibles_conexiones* (+ (length$ ?*posibles_conexiones*) 1) (implode$ ?conexion_vertical_arriba)))
      )
      (if (<= (- ?c 1) (- ?*columna* ?*fichas*)) then
        (bind ?conexion_horizontal_derecha (create$))
        (loop-for-count (?n 0 (- ?*fichas* 1)) do
          (bind ?coordenadas (create$ (+ ?c ?n) ?f))
          (bind $?conexion_horizontal_derecha (insert$ $?conexion_horizontal_derecha (+ (length$ $?conexion_horizontal_derecha) 1) ?coordenadas))
        )
        (bind ?*posibles_conexiones* (insert$ ?*posibles_conexiones* (+ (length$ ?*posibles_conexiones*) 1) (implode$ ?conexion_horizontal_derecha)))
      )
      (if (and (<= (- ?f 1) (- ?*fila* ?*fichas*)) (<= (- ?c 1) (- ?*columna* ?*fichas*))) then
        (bind ?conexion_diagonal_arriba_derecha (create$))
        (loop-for-count (?n 0 (- ?*fichas* 1)) do
          (bind ?coordenadas (create$ (+ ?c ?n) (+ ?f ?n)))
          (bind $?conexion_diagonal_arriba_derecha (insert$ $?conexion_diagonal_arriba_derecha (+ (length$ $?conexion_diagonal_arriba_derecha) 1) ?coordenadas))
        )
        (bind ?*posibles_conexiones* (insert$ ?*posibles_conexiones* (+ (length$ ?*posibles_conexiones*) 1) (implode$ ?conexion_diagonal_arriba_derecha)))
      )
      (if (and (<= (- ?f 1) (- ?*fila* ?*fichas*)) (>= ?c ?*fichas*)) then
        (bind ?conexion_diagonal_arriba_izquierda (create$))
        (loop-for-count (?n 0 (- ?*fichas* 1)) do
          (bind ?coordenadas (create$ (- ?c ?n) (+ ?f ?n)))
          (bind $?conexion_diagonal_arriba_izquierda (insert$ $?conexion_diagonal_arriba_izquierda (+ (length$ $?conexion_diagonal_arriba_izquierda) 1) ?coordenadas))
        )
        (bind ?*posibles_conexiones* (insert$ ?*posibles_conexiones* (+ (length$ ?*posibles_conexiones*) 1) (implode$ ?conexion_diagonal_arriba_izquierda)))
      )
    )
  )
)

(defrule dibuja-tablero
  (declare (salience 20))
  ?h<-(mostrar-tablero)
  (tablero (valor $?t))
=>
  (printout t "La situacion del juego es la siguiente:" crlf crlf)
  
  (printout t "         1 ")
  (loop-for-count (?j 2 ?*columna*) do
    (printout t "  "?j" ")
  )
  (printout t crlf)
  (printout t "       |---")
  (loop-for-count (?j 2 ?*columna*) do
    (printout t "+---")
  )
  (printout t "|" crlf)
  (bind ?i 1)
  (bind ?k 1)
  (progn$ (?ald1 $?t)
    (bind $?fila (explode$(nth$ ?i $?t)))
    (printout t "     "?k" |")
    (bind ?k (+ 1 ?k))
    (progn$ (?ald2 $?fila)
      (printout t " "?ald2" |")
    )
    (bind ?i (+ ?i 1))
    (printout t crlf)
    (printout t "       |---")
    (loop-for-count (?j 2 ?*columna*) do
      (printout t "+---")
    )
    (printout t "|" crlf)
  )
  (retract ?h)
  (assert (comprobar-ganador))
  ;(bind ?mostrar (read))
)

(defrule hayGanador
  (declare (salience 15))
  (comprobar-ganador)
  (ficha-jugador ?x j1)
  ?tab <- (tablero (valor $?v))
=>
  (bind ?h (heuristico $?v ?x))
  (bind ?b FALSE)
  (if (or(= ?h 1000000)(= ?h -1000000)) then
    (retract ?tab)
    (printout t "Se ha acabado el juego." crlf)
  else
    (progn$ (?i $?v)
      (if (integerp(str-index "." ?i)) then
        (bind ?b TRUE)
      )
    )
    (if (not ?b) then
      (retract ?tab)
      (printout t "Se ha acabado el juego." crlf)
    )
  )
)

(defrule elige-jugador-para-empezar
  (not (turno ?))
  (tablero (valor $?t))
=>
  (printout t "Elige quien empieza: (j1/j2)" crlf)
  (assert (turno (read)))
)

(defrule incorrecta-eleccion-jugador
  ?eleccion <- (turno ~j1&~j2)
=>
  (retract ?eleccion)
  (printout t "La respuesta no es valida" crlf)
)

(defrule elige-ficha
  (turno j1|j2) ;|)
  (not (ficha-jugador $?))
=>
  (printout t "Elige la ficha que usaras: (x/o)" crlf)
  (assert (ficha-jugador (read) j1))
)

(defrule incorrecta-eleccion-ficha
  ?eleccion <- (ficha-jugador ~x&~o ?x)
=>
  (retract ?eleccion)
  (printout t "El tipo de ficha no es valido" crlf)
)

(defrule correcta-eleccion-j1-x
  ?eleccion <- (ficha-jugador x j1)
=>
  (assert (ficha-jugador o j2))
  (assert (mostrar-tablero))
)

(defrule correcta-eleccion-j1-o
  ?eleccion <- (ficha-jugador o j1)
=>
  (assert (ficha-jugador x j2))
  (assert (mostrar-tablero))
)

(defrule generar-primer-tableroAux
  (tablero (jugador ?j) (valor $?v))
  (turno j1)
  (ficha-jugador ?x j1)
=>
  (bind ?h (heuristico $?v ?x))
  (if (eq ?j max) then
    (assert (tableroAux (jugador ?j) (valor $?v) (identificador ?*identPadre*) (padre -1) (heuristico ?h) (heuristicoHijo -10000000)))
  else
    (assert (tableroAux (jugador ?j) (valor $?v) (identificador ?*identPadre*) (padre -1) (heuristico ?h) (heuristicoHijo 10000000)))
  )
  (bind ?*identPadre* (+ ?*identPadre* 1))
  (bind ?*profundidad* 0)
)

(defrule detener-hijos
  (declare (salience 15))
  ?padre <- (tableroAux (jugador ?ju) (valor $?v) (profundidad ?pro &:(> ?pro ?*limiteProfundidad*)))
  ?turno <- (turno j1)
  (ficha-jugador ?x j1)
=>
  (retract ?padre)
)

(defrule generar-hijos
  (declare (salience 10))
  ?padre <- (tableroAux (jugador ?ju) (valor $?v) (identificador ?ident) (profundidad ?pro) (heuristico ~1000000 & ~-1000000) (hacerHijo 1))
  ?turno <- (turno j1)
  (ficha-jugador ?x j1)
=>
  (bind ?anadidos 0)
  (bind ?i (length$ $?v))
  (bind ?fila (nth$ ?i $?v))
  (bind $?filaMulti (explode$ ?fila))
  (loop-for-count(?j 1 (length$ $?filaMulti))
    (if(eq (nth$ ?j $?filaMulti) .) then
      (if(eq ?ju max) then
        (bind $?filaMulti2 (replace$ $?filaMulti ?j ?j ?x))
        (bind ?fila2 (implode$ $?filaMulti2))
        (bind $?v2 (replace$ $?v ?i ?i ?fila2))
        (bind ?h (heuristico ?v2 ?x))
        (assert (tableroAux (jugador min) (valor $?v2) (profundidad (+ ?pro 1)) (identificador ?*identPadre*) (padre ?ident) (heuristico ?h) (heuristicoHijo 10000000)))
        (bind ?*identPadre* (+ ?*identPadre* 1))
        (bind ?*profundidad* (+ ?*profundidad* 1))
        (bind ?anadidos (+ ?anadidos 1))
        (if (= ?anadidos (length$ $?filaMulti)) then
          (break)
        )
      else
        (if (eq ?x x) then
          (bind $?filaMulti2 (replace$ $?filaMulti ?j ?j o))
        else
          (bind $?filaMulti2 (replace$ $?filaMulti ?j ?j x))
        )
        (bind ?fila2 (implode$ $?filaMulti2))
        (bind $?v2 (replace$ $?v ?i ?i ?fila2))
        (bind ?h (heuristico $?v2 ?x))
        (assert (tableroAux (jugador max) (valor $?v2) (profundidad (+ ?pro 1)) (identificador ?*identPadre*) (padre ?ident) (heuristico ?h) (heuristicoHijo -10000000)))
        (bind ?*identPadre* (+ ?*identPadre* 1))
        (bind ?*profundidad* (+ ?*profundidad* 1))
        (bind ?anadidos (+ ?anadidos 1))
        (if (= ?anadidos (length$ $?filaMulti)) then
          (break)
        )
      )
    else 
      (bind ?iAux (- ?i 1))
      (while (> ?iAux 0)
        (bind ?filaAux (nth$ ?iAux $?v))
        (bind $?filaMultiAux (explode$ ?filaAux))
        (if(eq (nth$ ?j $?filaMultiAux) .) then
          (if(eq ?ju max) then
            (bind $?filaMulti2 (replace$ $?filaMultiAux ?j ?j ?x))
            (bind ?fila2 (implode$ $?filaMulti2))
            (bind $?v2 (replace$ $?v ?iAux ?iAux ?fila2))
            (bind ?h (heuristico $?v2 ?x))
            (assert (tableroAux (jugador min) (valor $?v2) (profundidad (+ ?pro 1)) (identificador ?*identPadre*) (padre ?ident) (heuristico ?h) (heuristicoHijo 10000000)))
            (bind ?*identPadre* (+ ?*identPadre* 1))
            (bind ?*profundidad* (+ ?*profundidad* 1))
            (bind ?anadidos (+ ?anadidos 1))
            (if (= ?anadidos (length$ $?filaMulti)) then
              (break)
            )
            (break)
          else
            (if (eq ?x x) then
              (bind $?filaMulti2 (replace$ $?filaMultiAux ?j ?j o))
            else
              (bind $?filaMulti2 (replace$ $?filaMultiAux ?j ?j x))
            )
            (bind ?fila2 (implode$ $?filaMulti2))
            (bind $?v2 (replace$ $?v ?iAux ?iAux ?fila2))
            (bind ?h (heuristico $?v2 ?x))
            (assert (tableroAux (jugador max) (valor $?v2) (profundidad (+ ?pro 1)) (identificador ?*identPadre*) (padre ?ident) (heuristico ?h) (heuristicoHijo -10000000)))
            (bind ?*identPadre* (+ ?*identPadre* 1))
            (bind ?*profundidad* (+ ?*profundidad* 1))
            (bind ?anadidos (+ ?anadidos 1))
            (if (= ?anadidos (length$ $?filaMulti)) then
              (break)
            )
            (break)
          )
        )
        (bind ?iAux (- ?iAux 1))
      )
    )
  )
)

(defrule algoritmo-min-max
  (declare (salience 8))
  ?hijo <- (tableroAux (jugador ?ju) (padre ?pa) (profundidad ?pro) (heuristico ?h))
  (not(exists(tableroAux(profundidad ?profun &:(> ?profun ?pro)))))
  ?padre <- (tableroAux (jugador ?ju2) (valor $?va2) (identificador ?ident & ?pa) (padre ?pa2) (profundidad ?pro2) (heuristico ?h2) (heuristicoHijo ?hh2))
  (turno j1)
=>  
  (if (eq ?ju2 max) then
    (if (< ?hh2 ?h) then
      (assert (tableroAux (jugador ?ju2) (valor $?va2) (identificador ?ident) (padre ?pa2) (profundidad ?pro2) (heuristico ?h2) (heuristicoHijo ?h) (hacerHijo 0)))
      (retract ?padre)
    )
  else 
    (if (> ?hh2 ?h) then
      (assert (tableroAux (jugador ?ju2) (valor $?va2) (identificador ?ident) (padre ?pa2) (profundidad ?pro2) (heuristico ?h2) (heuristicoHijo ?h) (hacerHijo 0)))
      (retract ?padre)
    )
  )
  (if (> ?pro 1) then
    (retract ?hijo)
  )
)

(defrule crear-nuevo-tablero
  (declare (salience 5))
  (tableroAux (jugador max) (identificador ?ident) (padre -1) (heuristicoHijo ?hh))
  (tableroAux (jugador min) (valor $?va2) (padre ?pa2 &:(= ?ident ?pa2)) (heuristico ?h2 &:(= ?hh ?h2)))
  ?tab <- (tablero (jugador ?ju) (valor $?v))
  ?turno <- (turno j1)
=>
  (retract ?tab)
  (retract ?turno)
  (assert (tablero (jugador min) (valor $?va2)))
  (assert (mostrar-tablero))
  (assert (turno j2))
)

(defrule eliminar-tableroAux
  (declare (salience 25))
  (mostrar-tablero)
  ?tab <- (tableroAux)
=>
  (retract ?tab)
)

(defrule movimiento-jugadorDos
  (declare (salience 10))
  ?turno <- (turno j2)
  ?tab <- (tablero (jugador ?ju) (valor $?v))
  (ficha-jugador ?x j2)
=>
  (bind ?anadido 0)
  (while (= ?anadido 0)
    (printout t "¿En que columna quieres insertar tu ficha?" crlf)
    (bind ?colum (read))
    (if (and(> ?colum 0)(< ?colum (+ ?*columna* 1))) then
      (bind ?i (length$ $?v))
      (bind ?fila (nth$ ?i $?v))
      (bind $?filaMulti (explode$ ?fila))
      (if(eq (nth$ ?colum $?filaMulti) .) then
        (bind $?filaMulti2 (replace$ $?filaMulti ?colum ?colum ?x))
        (bind ?fila2 (implode$ $?filaMulti2))
        (bind $?v2 (replace$ $?v ?i ?i ?fila2))
        (assert (tablero (jugador max) (valor $?v2)))
        (assert (mostrar-tablero))
        (bind ?anadido 1)
      else 
        (bind ?iAux (- ?i 1))
        (while (> ?iAux 0)
          (bind ?filaAux (nth$ ?iAux $?v))
          (bind $?filaMultiAux (explode$ ?filaAux))
          (if(eq (nth$ ?colum $?filaMultiAux) .) then
            (bind $?filaMulti2 (replace$ $?filaMultiAux ?colum ?colum ?x))
            (bind ?fila2 (implode$ $?filaMulti2))
            (bind $?v2 (replace$ $?v ?iAux ?iAux ?fila2))
            (assert (tablero (jugador max) (valor $?v2)))
            (assert (mostrar-tablero))
            (bind ?anadido 1)
            (break)
          )
          (bind ?iAux (- ?iAux 1))
        )
      )
    )
    (if (= ?anadido 0) then
      (printout t "No puedes añadir tu ficha en esa columna." crlf)
    )
  )
  (retract ?tab)
  (retract ?turno)
  (assert (turno j1))
)