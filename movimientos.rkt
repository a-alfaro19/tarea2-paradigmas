#lang racket
;; -----------------------------------------------------------------------------
;; Archivo: movimientos.rkt
;; Descripción: Define toda la lógica de manipulación del cubo Rubik, incluyendo
;; rotaciones de filas, columnas, caras y detección de extremos.
;; -----------------------------------------------------------------------------

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

;; -----------------------------------------------------------------------------
;; numero Carácter -> Número
;; Convierte un carácter con valor ASCII numérico (como #\3) al número correspondiente.
;; -----------------------------------------------------------------------------
(define (numero c) (- (char->integer c) 48))

;; -----------------------------------------------------------------------------
;; invertir-lista Lista -> Lista
;; Invierte una lista utilizando acumulador.
;; -----------------------------------------------------------------------------

(define (invertir-lista lst)
  (invertir-aux lst '()))

(define (invertir-aux lst acc)
  (cond
    [(null? lst) acc]
    [else (invertir-aux (cdr lst) (cons (car lst) acc))]))

;; -----------------------------------------------------------------------------
;; obtener-elemento Lista Número -> Elemento
;; Obtiene el elemento en la posición 'indice' de una lista.
;; -----------------------------------------------------------------------------
(define (obtener-elemento fila indice)
  (cond
    [(= indice 0) (car fila)]
    [else (obtener-elemento (cdr fila) (- indice 1))]))

;; -----------------------------------------------------------------------------
;; Rotaciones de caras (90 grados horario y antihorario)
;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
;; rotar-cara-horaria Lista -> Lista
;; Rota una matriz cuadrada (cara del cubo) 90 grados en sentido horario.
;; -----------------------------------------------------------------------------
(define (rotar-cara-horaria cara)
  (rotar-cara-horaria-aux cara 0 (length cara)))

(define (rotar-cara-horaria-aux cara col total)
  (cond
    [(= col total) '()]
    [else
     (cons (obtener-columna-invertida cara col)
           (rotar-cara-horaria-aux cara (+ col 1) total))]))

;; -----------------------------------------------------------------------------
;; rotar-cara-antihoraria Lista -> Lista
;; Rota una matriz cuadrada (cara del cubo) 90 grados en sentido antihorario.
;; -----------------------------------------------------------------------------

(define (rotar-cara-antihoraria cara)
  (rotar-cara-antihoraria-aux cara (- (length cara) 1) -1))

(define (rotar-cara-antihoraria-aux cara col final)
  (cond
    [(= col final) '()]
    [else
     (cons (obtener-columna cara col)
           (rotar-cara-antihoraria-aux cara (- col 1) final))]))

;; -----------------------------------------------------------------------------
;; obtener-columna Lista Número -> Lista
;; Obtiene la columna en la posición 'col' de una matriz.
;; -----------------------------------------------------------------------------

(define (obtener-columna cara col)
  (cond
    [(null? cara) '()]
    [else
     (cons (obtener-elemento (car cara) col)
           (obtener-columna (cdr cara) col))]))

;; -----------------------------------------------------------------------------
;; obtener-columna-invertida Lista Número -> Lista
;; Obtiene una columna invertida desde una matriz, útil para rotaciones horarias.
;; -----------------------------------------------------------------------------

(define (obtener-columna-invertida cara col)
  (invertir-lista (obtener-columna cara col)))

;; -----------------------------------------------------------------------------
;; es-extremo-fila? Número Número -> Booleano
;; Determina si una fila es la primera o la última en una matriz n x n.
;; -----------------------------------------------------------------------------
(define (es-extremo-fila? fila n)
  (cond
    [(or (= fila 1) (= fila n)) #t]
    [else #f]))

;; -----------------------------------------------------------------------------
;; es-extremo-columna? Número Número -> Booleano
;; Determina si una columna es la primera o la última en una matriz n x n.
;; -----------------------------------------------------------------------------
(define (es-extremo-columna? columna n)
  (cond
    [(or (= columna 1) (= columna n)) #t]
    [else #f]))

;; -----------------------------------------------------------------------------
;; extraer-fila Lista Número -> Lista
;; Devuelve la fila 'i' de una matriz.
;; -----------------------------------------------------------------------------
(define (extraer-fila matriz i)
  (cond
    [(= i 1) (car matriz)]
    [else (extraer-fila (cdr matriz) (- i 1))]))

;; -----------------------------------------------------------------------------
;; reemplazar-fila Lista Número Lista -> Lista
;; Reemplaza la fila 'i' en una matriz por una nueva lista.
;; -----------------------------------------------------------------------------
(define (reemplazar-fila matriz i nueva-fila)
  (cond
    [(= i 1) (cons nueva-fila (cdr matriz))]
    [else (cons (car matriz)
                (reemplazar-fila (cdr matriz) (- i 1) nueva-fila))]))
;; -----------------------------------------------------------------------------
;; extraer-columna Lista Número -> Lista
;; Devuelve la columna 'j' de una matriz.
;; -----------------------------------------------------------------------------
(define (extraer-columna matriz j)
  (cond
    [(null? matriz) '()]
    [else (cons (obtener-elemento (car matriz) (- j 1))
                (extraer-columna (cdr matriz) j))]))
;; -----------------------------------------------------------------------------
;; reemplazar-columna Lista Número Lista -> Lista
;; Reemplaza la columna 'j' de una matriz con una nueva columna.
;; ----------------------------------------------------------------------------
(define (reemplazar-columna matriz j nueva-columna)
  (cond
    [(null? matriz) '()]
    [else
     (cons (reemplazar-en-fila (car matriz) j (car nueva-columna))
           (reemplazar-columna (cdr matriz) j (cdr nueva-columna)))]))

;; -----------------------------------------------------------------------------
;; reemplazar-en-fila Lista Número Elemento -> Lista
;; Reemplaza el elemento en la posición 'j' dentro de una fila.
;; -----------------------------------------------------------------------------
(define (reemplazar-en-fila fila j nuevo)
  (cond
    [(= j 1) (cons nuevo (cdr fila))]
    [else (cons (car fila)
                (reemplazar-en-fila (cdr fila) (- j 1) nuevo))]))

;; -----------------------------------------------------------------------------
;; reemplazar-en-lista Lista Número Elemento -> Lista
;; Reemplaza el elemento en la posición 'pos' de una lista cualquiera.
;; -----------------------------------------------------------------------------

(define (reemplazar-en-lista lista pos nuevo)
  (cond
    [(= pos 0) (cons nuevo (cdr lista))]
    [else (cons (car lista) (reemplazar-en-lista (cdr lista) (- pos 1) nuevo))]))

;; -----------------------------------------------------------------------------
;; rotar-fila-en-cubo Número Lista Número Símbolo -> Lista
;; Aplica una rotación a una fila del cubo Rubik. Si es extremo, rota también
;; la cara correspondiente (superior o inferior).
;; -----------------------------------------------------------------------------

(define (rotar-fila-en-cubo n cubo fila direccion)
  (cond
    [(es-extremo-fila? fila n)
     (define nuevo-cubo (rotar-fila-extremo cubo fila direccion))
     (cond
       [(and (= fila 1) (equal? direccion 'derecha))
        (reemplazar-en-lista nuevo-cubo 0 (rotar-cara-antihoraria (list-ref nuevo-cubo 0)))] ; rotación antihoraria de la cara superior
       [(and (= fila 1) (equal? direccion 'izquierda))
        (reemplazar-en-lista nuevo-cubo 0 (rotar-cara-horaria (list-ref nuevo-cubo 0)))] ; rotación horaria de la cara superior
       [(and (= fila n) (equal? direccion 'derecha))
        (reemplazar-en-lista nuevo-cubo 5 (rotar-cara-horaria (list-ref nuevo-cubo 5)))]  ; rotación horaria de la cara inferior
       [(and (= fila n) (equal? direccion 'izquierda))
        (reemplazar-en-lista nuevo-cubo 5 (rotar-cara-antihoraria (list-ref nuevo-cubo 5)))]  ; rotación antihoraria de la cara inferior
       [else nuevo-cubo])]
    [else (rotar-fila-extremo cubo fila direccion)]))

;; -----------------------------------------------------------------------------
;; rotar-fila-extremo: Lista Número Símbolo -> Lista
;; Rota una fila entre las caras izquierda, frontal, derecha y posterior del cubo.
;; No rota caras superior o inferior directamente.
;; -----------------------------------------------------------------------------
(define (rotar-fila-extremo cubo fila direccion)
  ;; Se extraen las filas correspondientes de cada cara lateral involucrada
  (define izq (extraer-fila (list-ref cubo 4) fila))
  (define fr (extraer-fila (list-ref cubo 1) fila))
  (define der (extraer-fila (list-ref cubo 2) fila))
  (define post (extraer-fila (list-ref cubo 3) fila))
  (cond
    ;; Si el movimiento es a la derecha
    [(equal? direccion 'derecha)
     ;; cara izquierda <- cara posterior, cara frontal <- cara izquierda, cara derecha <- cara frontal, cara posterior <-cara derecha
     (reemplazar-en-lista
      (reemplazar-en-lista
       (reemplazar-en-lista
        (reemplazar-en-lista cubo 4 (reemplazar-fila (list-ref cubo 4) fila post))
        1 (reemplazar-fila (list-ref cubo 1) fila izq))
       2 (reemplazar-fila (list-ref cubo 2) fila fr))
      3 (reemplazar-fila (list-ref cubo 3) fila der))]
    ;; Si el movimiento es a la izquierda
    [(equal? direccion 'izquierda)
     ;;cara izquierda <- cara frontal, cara frontal <- cara derecha, cara derecha <- cara posterior, cara posterior <- cara izquierda
     (reemplazar-en-lista
      (reemplazar-en-lista
       (reemplazar-en-lista
        (reemplazar-en-lista cubo 4 (reemplazar-fila (list-ref cubo 4) fila fr))
        1 (reemplazar-fila (list-ref cubo 1) fila der))
       2 (reemplazar-fila (list-ref cubo 2) fila post))
      3 (reemplazar-fila (list-ref cubo 3) fila izq))]))

;; -----------------------------------------------------------------------------
;; rotar-columna-en-cubo: Número Lista Número Símbolo -> Lista
;; Rota una columna entre las caras superior, frontal, inferior y posterior.
;; También rota la cara lateral (izquierda o derecha) si la columna esta en el extremo.
;; -----------------------------------------------------------------------------
(define (rotar-columna-en-cubo n cubo columna direccion)
  ;; Extrae columnas necesarias de las caras implicadas
  (define sup (extraer-columna (list-ref cubo 0) columna))
  (define fr (extraer-columna (list-ref cubo 1) columna))
  (define inf (extraer-columna (list-ref cubo 5) columna))
  ;; Para la cara posterior, se debe invertir el índice de columna
  (define post (extraer-columna (list-ref cubo 3) (- n (- columna 1)))) ; columna inversa en cara posterior
  ;; Identifica si la columna esta al extremo
  (define izq? (= columna 1))
  (define der? (= columna n))
  (cond
    ;; Si la rotacion es hacia abajo
    [(equal? direccion 'abajo)
     ;; cara superior <- cara posterior, cara frontal <- cara superior, cara inferior <- cara frontal, cara posterior <- cara inferior
     (define nuevo-sup (invertir-lista post))
     (define nuevo-fr sup)
     (define nuevo-inf fr)
     (define nuevo-post (invertir-lista inf))
     ;; Aplica cambios en el cubo
     (define cubo1 (reemplazar-en-lista cubo 0 (reemplazar-columna (list-ref cubo 0) columna nuevo-sup)))
     (define cubo2 (reemplazar-en-lista cubo1 1 (reemplazar-columna (list-ref cubo1 1) columna nuevo-fr)))
     (define cubo3 (reemplazar-en-lista cubo2 5 (reemplazar-columna (list-ref cubo2 5) columna nuevo-inf)))
     (define nuevo-cubo (reemplazar-en-lista cubo3 3 (reemplazar-columna (list-ref cubo3 3) (- n (- columna 1)) nuevo-post)))
     ;; Rota cara lateral si la columna esta al extremo 
     (cond
       [izq? (reemplazar-en-lista nuevo-cubo 4 (rotar-cara-horaria (list-ref nuevo-cubo 4)))]
       [der? (reemplazar-en-lista nuevo-cubo 2 (rotar-cara-antihoraria (list-ref nuevo-cubo 2)))]
       [else nuevo-cubo])]
    ;; Si la rotacion es hacia arriba
    [(equal? direccion 'arriba)
     ;; cara superior <- cara frontal, cara frontal <- cara inferior, cara inferior <- cara posterior, cara posterior <- cara superior
     (define nuevo-sup fr)
     (define nuevo-fr inf)
     (define nuevo-inf (invertir-lista post))
     (define nuevo-post (invertir-lista sup))
     ;; Aplica cambios en el cubo
     (define cubo1 (reemplazar-en-lista cubo 0 (reemplazar-columna (list-ref cubo 0) columna nuevo-sup)))
     (define cubo2 (reemplazar-en-lista cubo1 1 (reemplazar-columna (list-ref cubo1 1) columna nuevo-fr)))
     (define cubo3 (reemplazar-en-lista cubo2 5 (reemplazar-columna (list-ref cubo2 5) columna nuevo-inf)))
     (define nuevo-cubo (reemplazar-en-lista cubo3 3 (reemplazar-columna (list-ref cubo3 3) (- n (- columna 1)) nuevo-post)))
     ;; Rota cara lateral si la columna esta al extremo 
     (cond
       [izq? (reemplazar-en-lista nuevo-cubo 4 (rotar-cara-antihoraria (list-ref nuevo-cubo 4)))]
       [der? (reemplazar-en-lista nuevo-cubo 2 (rotar-cara-horaria (list-ref nuevo-cubo 2)))]
       [else nuevo-cubo])]))

;; -----------------------------------------------------------------------------
;; aplicar-movimiento: Número Lista Símbolo -> Lista
;; Aplica un único movimiento al cubo según el símbolo de entrada.
;; Fila: F1D, F2I... / Columna: C3A, C6B, etc.
;; -----------------------------------------------------------------------------

(define (aplicar-movimiento n cubo mov)
  (cond
    ;; Movimiento de fila hacia la derecha
    [(and (equal? (string-ref (symbol->string mov) 0) #\F)
          (equal? (string-ref (symbol->string mov) 2) #\D))
     (rotar-fila-en-cubo n cubo (numero (string-ref (symbol->string mov) 1)) 'derecha)]
    ;; Movimiento de fila hacia la izquierda
    [(and (equal? (string-ref (symbol->string mov) 0) #\F)
          (equal? (string-ref (symbol->string mov) 2) #\I))
     (rotar-fila-en-cubo n cubo (numero (string-ref (symbol->string mov) 1)) 'izquierda)]
    ;; Movimiento de columna hacia arriba
    [(and (equal? (string-ref (symbol->string mov) 0) #\C)
          (equal? (string-ref (symbol->string mov) 2) #\A))
     (rotar-columna-en-cubo n cubo (numero (string-ref (symbol->string mov) 1)) 'arriba)]
    ;; Movimiento de columna hacia abajo
    [(and (equal? (string-ref (symbol->string mov) 0) #\C)
          (equal? (string-ref (symbol->string mov) 2) #\B))
     (rotar-columna-en-cubo n cubo (numero (string-ref (symbol->string mov) 1)) 'abajo)]))

