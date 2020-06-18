;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname TP4-Apellido1-Apellido2) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
#|
Trabajo Práctico 4: Listas

Integrantes:
- [Apellido, Nombre], comisión [número_comisión].
- [Apellido, Nombre], comisión [número_comisión].
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Datos ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;; Diseño de datos

; Representaremos fechas mediante strings, según el formato aaaa-mm-dd.
; Representaremos los nombres de las localidades y departamentos mediante strings.
; Representaremos la cantidad de casos (sospechosos, descartados y confirmados)
; mediante números.

(define-struct notificacion [fecha loc conf desc sosp notif])
; notificacion es (String, String, Number, Number, Number, Number)
; Interpretación: un elemento en notificacion representa el conjunto de notificaciones
; registradas en una localidad (loc) hasta un día (fecha), en donde:
; - hay conf casos confirmados de COVID-19
; - hay desc casos descartados de COVID-19
; - sosp casos estaban en estudio.
; El último elemento, notif, indica la cantidad total de notificaciones.


;;;;;;;;;;;; Preparación de los Datos

;;;;;; Datos sobre localidades santafesinas

; Datos de entrada sobre localidades
(define INPUT-LOC (read-csv-file "dataset/santa_fe_departamento_localidad.csv"))
(define DATOS-LOC (rest INPUT-LOC))

; tomar-dos : List(X) -> List(X)
; Dada una lista l de dos o más elementos, tomar-dos calcula
; la lista formada por los dos primeros elementos de l, en
; ese orden.
(check-expect (tomar-dos (list "a" "b")) (list "a" "b")) 
(check-expect (tomar-dos (list 0 1 2 3)) (list 0 1))
(define
  (tomar-dos l)
  (list (first l) (second l)))

; Lista de localidades santafecinas
(define LISTA-LOC (map second DATOS-LOC))
; Lista de localidades santafecinas, con su departamento
(define LISTA-DPTO-LOC (map tomar-dos DATOS-LOC))

;;;;;; Datos sobre notificaciones de COVID-19

; Datos de entrada sobre notificaciones
(define INPUT-NOTIF (read-csv-file "dataset/notificaciones_localidad.csv"))
(define DATOS-NOTIF (rest INPUT-NOTIF))

;algunas listas de prueba
(define NOTIF-PRUEBA (list "2020-04-24" "ROSARIO" "20" "10" "10" "40" ))
(define LISTA-PRUEBA (list (list "notificacion" ) (list "2020-04-24" "ROSARIO" "20" "10" "10" "40" )))
(define PRUEBA (list "1" "2" "1" "2" 1 2 1 2))
(define PRUEBA2 (list (make-notificacion "2020-04-24" "ROSARIO" 20 10 10 40) (make-notificacion "2020-04-24" "ARROYO SECO" 0 10 5 15) ))
(define PRUEBA3 (list (make-notificacion "2020-04-24" "ROSARIO" 20 10 10 40) (make-notificacion "2020-04-24" "CAYASTA" 0 5 5 10) ))
(define PRUEBA4 (list "Hola" 1))

;hacer-estructura: List ->  List[Notificacion]
;a partir de una lista de seis strings, devuelve una estructura tipo notificación.

(check-expect (hacer-estructura NOTIF-PRUEBA) (make-notificacion "2020-04-24" "ROSARIO" 20 10 10 40))

(define (hacer-estructura l) (make-notificacion (first l) (second l) (string->number (third l)) (string->number (fourth l)) (string->number (fifth l)) (string->number (sixth l))))

; Lista de notificaciones
(define LISTA-NOTIF (map hacer-estructura DATOS-NOTIF))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Consultas ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ANTES "2020-04-02")
(define HOY "2020-06-02")
(define LIMITE-CASOS 25)

;;;;;;;;;;;; Consulta 1


;define si una notificacion supera el limite de casos
(define
  (supera? x)
  (>= (notificacion-conf x) LIMITE-CASOS))

;dada una notificacion, define si pertenece al dia hoy.
(define (hoy? n) (equal? (notificacion-fecha n) HOY))

;dada una lista de notificaciones, devuelve una lista con aquellas que superan LIMITE DE CASOS hasta el dia HOY.
(define (limite-casos l) (filter supera? (filter hoy? l)))

;dada una lista de notificaciones, devuelve una lista con las localidades que superan LIMITE DE CASOS hasta el dia HOY.
(define (localidades-limite-casos l) (map notificacion-loc (limite-casos l)))


;lista de localidades que superan LIMITE DE CASOS hasta el dia HOY

(define LOCALIDADES-LIMITE-CASOS (localidades-limite-casos LISTA-NOTIF))


;;;;;;;;;;;; Consulta 2

;primeros: List -> List
;A partir de una lista de listas, arma otra lista con los primeros elementos de cada una.

(check-expect (primeros LISTA-PRUEBA) (list "notificacion" "2020-04-24") )
(check-expect (primeros empty) empty)

(define (primeros l) (cond [(empty? l) empty]
                           [else (cons (first (first l)) (primeros (rest l)))]
  ))

;distintos: List -> List
;A partir de una lista, arma otra lista con los elementos de la ingresada, sin repetir.

(check-expect (distintos PRUEBA) (list "1" "2" 1 2))
(check-expect (distintos empty) empty)

(define
  (distintos l)
  (cond [(empty? l) empty]
        [(cons? l) (if (equal? (member? (first l) (rest l)) #f) (cons (first l) (distintos (rest l))) (distintos (rest l)))   
                   ]
        ))

;lista de departamentos.
(define LISTA-DPTO (distintos (primeros LISTA-DPTO-LOC)))

;dpto-loc: String  List -> List[String]
;dado un departamento y una lista de listas de departamentos con sus localidades, devuelve una lista de las localidades de ese departamento.

(check-expect (dpto-loc "Garay" LISTA-DPTO-LOC) (list "CAYASTA" "COLONIA MASCIAS" "HELVECIA" "SALADERO CABAL" "SANTA ROSA DE CALCHINES") ) 

(define (dpto-loc d l) (cond [(empty? l) empty ]
                             [(cons? l) (if (equal? d (first (first l)) ) (cons (second (first l)) (dpto-loc d (rest l))) (dpto-loc d (rest l)))
                                        ]))


;pertenece-dpto?: String String -> Bool
;dada una localidad y un departamento, define si esa localidad pertenece al departamento.

(check-expect (pertenece-dpto? "Rosario" "ARROYO SECO") #t )
(check-expect (pertenece-dpto? "9 de Julio" "CAYASTA") #f )

(define (pertenece-dpto? d loc) (member? loc (dpto-loc d LISTA-DPTO-LOC)))

;notif-fecha-dpto: List[Notificacion] String String -> List[Notificacion]
;dada una lista de notificaciones, una fecha f y un departamento d, devuelve una lista con las notificaciones que tienen fecha f y que pertenecen a d.

(check-expect (notif-fecha-dpto PRUEBA2 "2020-04-24" "Rosario") PRUEBA2)
(check-expect (notif-fecha-dpto PRUEBA3 "2020-04-24" "Rosario") (list (make-notificacion "2020-04-24" "ROSARIO" 20 10 10 40)))

(define (notif-fecha-dpto l f d)
  (cond [(empty? l) empty]
        [(cons? l) (if (and (pertenece-dpto? d (notificacion-loc (first l))) (equal? (notificacion-fecha (first l)) f))
                       (cons (first l) (notif-fecha-dpto (rest l) f d))
                       (notif-fecha-dpto (rest l) f d))]))

;suma-conf: List[Notificacion] -> Number
;suma los casos confirmados de una lista de notificaciones.

(check-expect (suma-conf PRUEBA3) 20)

(define (suma-conf l)
  (cond [(empty? l) 0]
        [else (+ (notificacion-conf (first l)) (suma-conf (rest l)))]))

;confirmados-dpto-fecha: List[Notificacion] String String -> Number
;dada una lista de notificaciones, una fecha y un departamento, devuelve la cantidad de casos confirmados en esa fecha, en ese departamento.

(check-expect (confirmados-dpto-fecha PRUEBA2 "2020-04-24" "Rosario") 20)

(define (confirmados-dpto-fecha l f d) (suma-conf (notif-fecha-dpto l f d)))
  
 
; [Completar, ejercicio 3-3]

(define
  (armar-lista ld f l)
  (cond [(empty? ld) empty]
        [else (cons (list (first ld) (confirmados-dpto-fecha l f (first ld) ) ) (armar-lista (rest ld) f l) )])
  )


; [Completar, ejercicio 3-3]
(define 
  (confirmados-por-dpto l f) 
  (armar-lista LISTA-DPTO f l)
  )


; [Completar, ejercicio 3-4]


(define CONFIRMADOS-DPTO-ANTES (confirmados-por-dpto LISTA-NOTIF ANTES) )


(define CONFIRMADOS-DPTO-HOY (confirmados-por-dpto LISTA-NOTIF HOY) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Salidas ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;; Consulta 1

; [Completar, ejercicio 4 - loc-lim-casos.csv]

;agregar-\n: String -> String
;agrega "\n " al final de un string.

(check-expect (agregar-\n "Hola") "Hola\n ")

(define (agregar-\n x ) (string-append x "\n "))

;archivo .csv de localidades que hoy superan el límite de casos.
(write-file "loc-lim-casos.csv" (string-append "Localidades que hoy superan el limite de casos\n" (foldr string-append "" (map agregar-\n LOCALIDADES-LIMITE-CASOS))))


;;;;;;; Consulta 2

;auxiliar: List -> String
;dada una lista de longitud dos, devuelve un string agregando ", " al primer elemento y "\n " al segundo.

(check-expect (auxiliar PRUEBA4) "Hola, 1\n ")

(define (auxiliar l) (string-append (first l) ", " (number->string (second l)) "\n "))

;archivo .csv de casos confirmados por departamento a la fecha hoy.
(write-file "casos-por-dpto-hoy.csv" (string-append "Casos confirmados por departamento a la fecha hoy\n " (foldr string-append "" (map auxiliar CONFIRMADOS-DPTO-HOY))))

;archivo .csv de casos confirmados por departamento a la fecha antes.
(write-file "casos-por-dpto-antes.csv" (string-append "Casos confirmados por departamento a la fecha antes\n " (foldr string-append "" (map auxiliar CONFIRMADOS-DPTO-ANTES))))

; [Completar, ejercicio 4 - casos-por-dpto-hoy.csv y casos-por-dpto-antes.csv]











