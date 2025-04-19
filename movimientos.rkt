#lang racket

(provide rotar-columna-en-cubo
         rotar-fila-en-cubo
         columna-a-lista
         reemplazar-columna)

;; =============================================================================
;; FUNCIONES AUXILIARES
;; =============================================================================

(define (columna-a-lista cara i)
  (cond
    [(null? cara) '()]
    [else
     (cons (elemento-en (car cara) (- i 1)) (columna-a-lista (cdr cara) i))]))

(define (reemplazar-columna cara nueva-columna i)
  (reemplazar-columna-aux cara nueva-columna i '()))

(define (reemplazar-columna-aux cara nueva i acc)
  (cond
    [(null? cara) acc]
    [else
     (reemplazar-columna-aux (cdr cara)
                              (cdr nueva)
                              i
                              (append acc
                                      (list (reemplazar-en (car cara) (- i 1) (car nueva)))))]))

(define (elemento-en lista i)
  (cond
    [(zero? i) (car lista)]
    [else (elemento-en (cdr lista) (- i 1))]))

(define (reemplazar-en lista i nuevo)
  (cond
    [(null? lista) '()]
    [(zero? i) (cons nuevo (cdr lista))]
    [else (cons (car lista) (reemplazar-en (cdr lista) (- i 1) nuevo))]))

(define (fila-a-lista cara fila-num)
  (elemento-en cara (- fila-num 1)))

(define (reemplazar-fila cara nueva-fila fila-num)
  (reemplazar-en cara (- fila-num 1) nueva-fila))

;; =============================================================================
;; ROTACIÓN DE COLUMNAS (existente)
;; =============================================================================

(define (rotar-columna-en-cubo n cubo i direccion)
  (define cara-sup (elemento-en cubo 0))
  (define cara-front (elemento-en cubo 1))
  (define cara-post (elemento-en cubo 3))
  (define cara-inf (elemento-en cubo 5))

  (define col-sup (columna-a-lista cara-sup i))
  (define col-front (columna-a-lista cara-front i))
  (define col-post (columna-a-lista cara-post i))
  (define col-inf (columna-a-lista cara-inf i))

  (define col-sup-nuevo (if (equal? direccion 'abajo) col-post col-front))
  (define col-front-nuevo (if (equal? direccion 'abajo) col-sup col-inf))
  (define col-inf-nuevo (if (equal? direccion 'abajo) col-front col-post))
  (define col-post-nuevo (if (equal? direccion 'abajo) col-inf col-sup))

  (define cara-sup-nueva (reemplazar-columna cara-sup col-sup-nuevo i))
  (define cara-front-nueva (reemplazar-columna cara-front col-front-nuevo i))
  (define cara-inf-nueva (reemplazar-columna cara-inf col-inf-nuevo i))
  (define cara-post-nueva (reemplazar-columna cara-post col-post-nuevo i))

  (reemplazar-en
   (reemplazar-en
    (reemplazar-en
     (reemplazar-en cubo 0 cara-sup-nueva)
     1 cara-front-nueva)
    3 cara-post-nueva)
   5 cara-inf-nueva))

;; =============================================================================
;; ROTACIÓN DE FILAS (nuevo)
;; =============================================================================

(define (rotar-fila-en-cubo n cubo fila-num direccion)
  (define cara-front (elemento-en cubo 1))
  (define cara-der (elemento-en cubo 2))
  (define cara-post (elemento-en cubo 3))
  (define cara-izq (elemento-en cubo 4))

  (define fila-front (fila-a-lista cara-front fila-num))
  (define fila-der (fila-a-lista cara-der fila-num))
  (define fila-post (fila-a-lista cara-post fila-num))
  (define fila-izq (fila-a-lista cara-izq fila-num))

  (define nueva-front (if (equal? direccion 'derecha) fila-izq fila-der))
  (define nueva-der (if (equal? direccion 'derecha) fila-front fila-post))
  (define nueva-post (if (equal? direccion 'derecha) fila-der fila-izq))
  (define nueva-izq (if (equal? direccion 'derecha) fila-post fila-front))

  (define cara-front-nuevo (reemplazar-fila cara-front nueva-front fila-num))
  (define cara-der-nuevo (reemplazar-fila cara-der nueva-der fila-num))
  (define cara-post-nuevo (reemplazar-fila cara-post nueva-post fila-num))
  (define cara-izq-nuevo (reemplazar-fila cara-izq nueva-izq fila-num))

  (reemplazar-en
   (reemplazar-en
    (reemplazar-en
     (reemplazar-en cubo 1 cara-front-nuevo)
     2 cara-der-nuevo)
    3 cara-post-nuevo)
   4 cara-izq-nuevo))


