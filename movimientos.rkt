#lang racket

(provide aplicar-movimiento
         rotar-fila-en-cubo
         rotar-columna-en-cubo
         rotar-cara-horaria
         rotar-cara-antihoraria
         es-extremo-fila?
         es-extremo-columna?
         extraer-fila
         reemplazar-fila
         extraer-columna
         reemplazar-columna
         numero)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilitarios Básicos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (numero c) (- (char->integer c) 48))

(define (invertir-lista lst)
  (invertir-aux lst '()))

(define (invertir-aux lst acc)
  (cond
    [(null? lst) acc]
    [else (invertir-aux (cdr lst) (cons (car lst) acc))]))

(define (obtener-elemento fila indice)
  (cond
    [(= indice 0) (car fila)]
    [else (obtener-elemento (cdr fila) (- indice 1))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rotaciones de caras (matriz) 90° horario y antihorario
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (rotar-cara-horaria cara)
  (rotar-cara-horaria-aux cara 0 (length cara)))

(define (rotar-cara-horaria-aux cara col total)
  (cond
    [(= col total) '()]
    [else
     (cons (obtener-columna-invertida cara col)
           (rotar-cara-horaria-aux cara (+ col 1) total))]))

(define (rotar-cara-antihoraria cara)
  (rotar-cara-antihoraria-aux cara (- (length cara) 1) -1))

(define (rotar-cara-antihoraria-aux cara col final)
  (cond
    [(= col final) '()]
    [else
     (cons (obtener-columna cara col)
           (rotar-cara-antihoraria-aux cara (- col 1) final))]))

(define (obtener-columna cara col)
  (cond
    [(null? cara) '()]
    [else
     (cons (obtener-elemento (car cara) col)
           (obtener-columna (cdr cara) col))]))

(define (obtener-columna-invertida cara col)
  (invertir-lista (obtener-columna cara col)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Detección de extremos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (es-extremo-fila? fila n)
  (cond
    [(or (= fila 1) (= fila n)) #t]
    [else #f]))

(define (es-extremo-columna? columna n)
  (cond
    [(or (= columna 1) (= columna n)) #t]
    [else #f]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manipulación de filas y columnas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (extraer-fila matriz i)
  (cond
    [(= i 1) (car matriz)]
    [else (extraer-fila (cdr matriz) (- i 1))]))

(define (reemplazar-fila matriz i nueva-fila)
  (cond
    [(= i 1) (cons nueva-fila (cdr matriz))]
    [else (cons (car matriz)
                (reemplazar-fila (cdr matriz) (- i 1) nueva-fila))]))

(define (extraer-columna matriz j)
  (cond
    [(null? matriz) '()]
    [else (cons (obtener-elemento (car matriz) (- j 1))
                (extraer-columna (cdr matriz) j))]))

(define (reemplazar-columna matriz j nueva-columna)
  (cond
    [(null? matriz) '()]
    [else
     (cons (reemplazar-en-fila (car matriz) j (car nueva-columna))
           (reemplazar-columna (cdr matriz) j (cdr nueva-columna)))]))

(define (reemplazar-en-fila fila j nuevo)
  (cond
    [(= j 1) (cons nuevo (cdr fila))]
    [else (cons (car fila)
                (reemplazar-en-fila (cdr fila) (- j 1) nuevo))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Movimiento principal con casos extremos - funcional puro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (reemplazar-en-lista lista pos nuevo)
  (cond
    [(= pos 0) (cons nuevo (cdr lista))]
    [else (cons (car lista) (reemplazar-en-lista (cdr lista) (- pos 1) nuevo))]))

(define (rotar-fila-en-cubo n cubo fila direccion)
  (cond
    [(es-extremo-fila? fila n)
     (define nuevo-cubo (rotar-fila-extremo cubo fila direccion))
     (cond
       [(and (= fila 1) (equal? direccion 'derecha))
        (reemplazar-en-lista nuevo-cubo 0 (rotar-cara-antihoraria (list-ref nuevo-cubo 0)))]
       [(and (= fila 1) (equal? direccion 'izquierda))
        (reemplazar-en-lista nuevo-cubo 0 (rotar-cara-horaria (list-ref nuevo-cubo 0)))]
       [(and (= fila n) (equal? direccion 'derecha))
        (reemplazar-en-lista nuevo-cubo 5 (rotar-cara-horaria (list-ref nuevo-cubo 5)))]
       [(and (= fila n) (equal? direccion 'izquierda))
        (reemplazar-en-lista nuevo-cubo 5 (rotar-cara-antihoraria (list-ref nuevo-cubo 5)))]
       [else nuevo-cubo])]
    [else (rotar-fila-extremo cubo fila direccion)]))

(define (rotar-fila-extremo cubo fila direccion)
  (define izq (extraer-fila (list-ref cubo 4) fila))
  (define fr (extraer-fila (list-ref cubo 1) fila))
  (define der (extraer-fila (list-ref cubo 2) fila))
  (define post (extraer-fila (list-ref cubo 3) fila))
  (cond
    [(equal? direccion 'derecha)
     (reemplazar-en-lista
      (reemplazar-en-lista
       (reemplazar-en-lista
        (reemplazar-en-lista cubo 4 (reemplazar-fila (list-ref cubo 4) fila post))
        1 (reemplazar-fila (list-ref cubo 1) fila izq))
       2 (reemplazar-fila (list-ref cubo 2) fila fr))
      3 (reemplazar-fila (list-ref cubo 3) fila der))]

    [(equal? direccion 'izquierda)
     (reemplazar-en-lista
      (reemplazar-en-lista
       (reemplazar-en-lista
        (reemplazar-en-lista cubo 4 (reemplazar-fila (list-ref cubo 4) fila fr))
        1 (reemplazar-fila (list-ref cubo 1) fila der))
       2 (reemplazar-fila (list-ref cubo 2) fila post))
      3 (reemplazar-fila (list-ref cubo 3) fila izq))]))


(define (rotar-columna-en-cubo n cubo columna direccion)
  (define sup (extraer-columna (list-ref cubo 0) columna))
  (define fr (extraer-columna (list-ref cubo 1) columna))
  (define inf (extraer-columna (list-ref cubo 5) columna))
  (define post (extraer-columna (list-ref cubo 3) (- n (- columna 1)))) ; invertir columna al entrar/salir
  (define izq? (= columna 1))
  (define der? (= columna n))
  (cond
    [(equal? direccion 'abajo)
     (define nuevo-sup (invertir-lista post))
     (define nuevo-fr sup)
     (define nuevo-inf fr)
     (define nuevo-post (invertir-lista inf))
     (define cubo1 (reemplazar-en-lista cubo 0 (reemplazar-columna (list-ref cubo 0) columna nuevo-sup)))
     (define cubo2 (reemplazar-en-lista cubo1 1 (reemplazar-columna (list-ref cubo1 1) columna nuevo-fr)))
     (define cubo3 (reemplazar-en-lista cubo2 5 (reemplazar-columna (list-ref cubo2 5) columna nuevo-inf)))
     (define nuevo-cubo (reemplazar-en-lista cubo3 3 (reemplazar-columna (list-ref cubo3 3) (- n (- columna 1)) nuevo-post)))
     (cond
       [izq? (reemplazar-en-lista nuevo-cubo 4 (rotar-cara-horaria (list-ref nuevo-cubo 4)))]
       [der? (reemplazar-en-lista nuevo-cubo 2 (rotar-cara-antihoraria (list-ref nuevo-cubo 2)))]
       [else nuevo-cubo])]

    [(equal? direccion 'arriba)
     (define nuevo-sup fr)
     (define nuevo-fr inf)
     (define nuevo-inf (invertir-lista post))
     (define nuevo-post (invertir-lista sup))
     (define cubo1 (reemplazar-en-lista cubo 0 (reemplazar-columna (list-ref cubo 0) columna nuevo-sup)))
     (define cubo2 (reemplazar-en-lista cubo1 1 (reemplazar-columna (list-ref cubo1 1) columna nuevo-fr)))
     (define cubo3 (reemplazar-en-lista cubo2 5 (reemplazar-columna (list-ref cubo2 5) columna nuevo-inf)))
     (define nuevo-cubo (reemplazar-en-lista cubo3 3 (reemplazar-columna (list-ref cubo3 3) (- n (- columna 1)) nuevo-post)))
     (cond
       [izq? (reemplazar-en-lista nuevo-cubo 4 (rotar-cara-antihoraria (list-ref nuevo-cubo 4)))]
       [der? (reemplazar-en-lista nuevo-cubo 2 (rotar-cara-horaria (list-ref nuevo-cubo 2)))]
       [else nuevo-cubo])]))



(define (aplicar-movimiento n cubo mov)
  (cond
    [(and (equal? (string-ref (symbol->string mov) 0) #\F)
          (equal? (string-ref (symbol->string mov) 2) #\D))
     (rotar-fila-en-cubo n cubo (numero (string-ref (symbol->string mov) 1)) 'derecha)]

    [(and (equal? (string-ref (symbol->string mov) 0) #\F)
          (equal? (string-ref (symbol->string mov) 2) #\I))
     (rotar-fila-en-cubo n cubo (numero (string-ref (symbol->string mov) 1)) 'izquierda)]

    [(and (equal? (string-ref (symbol->string mov) 0) #\C)
          (equal? (string-ref (symbol->string mov) 2) #\A))
     (rotar-columna-en-cubo n cubo (numero (string-ref (symbol->string mov) 1)) 'arriba)]

    [(and (equal? (string-ref (symbol->string mov) 0) #\C)
          (equal? (string-ref (symbol->string mov) 2) #\B))
     (rotar-columna-en-cubo n cubo (numero (string-ref (symbol->string mov) 1)) 'abajo)]))

