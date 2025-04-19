#lang scheme
(require "cubo.rkt")
(require "movimientos.rkt")

;; -----------------------------------------------------------------------------
;; aplicar-movimientos Número Lista Lista -> Lista
;;
;; Aplica una secuencia de movimientos al cubo, uno por uno.
;;
;; Parámetros:
;; - n: tamaño del cubo (n x n).
;; - cubo: lista de 6 caras (el estado actual del cubo).
;; - movs: lista de movimientos (ej: '(F1D C2B)).
;;
;; Retorna:
;; - Cubo con todos los movimientos aplicados.
;; -----------------------------------------------------------------------------
(define (aplicar-movimientos n cubo movs)
  (cond
    [(null? movs)
     cubo]
    [else
     (aplicar-movimientos n (aplicar-movimiento n cubo (car movs)) (cdr movs))]))
