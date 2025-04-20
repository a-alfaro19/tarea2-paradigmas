#lang racket
(require "cubo.rkt")
(require "movimientos.rkt")

(provide RS)

;; RS Número Lista Lista -> Lista
;; Ejecuta una serie de movimientos sobre un cubo n x n
(define (RS n cubo movimientos)
  (aplicar-movimientos n cubo movimientos))

;; EJEMPLO DE USO:
;; (define cubo (crear-cubo 2))
;; (RS 2 cubo '(F1D C2A F2I C1B))


