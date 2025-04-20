#lang racket

(require "cubo.rkt")
(require "movimientos.rkt")

(provide RS)

;; -----------------------------------------------------------------------------
;; RS: Número Lista Lista -> Lista
;; Función principal del simulador RubikSimulator.
;; Si la lista de cubo está vacía, se genera automáticamente un cubo nxn.
;; Luego se aplican los movimientos indicados en orden.
;;
;; Parámetros:
;; - n: Tamaño del cubo (entero entre 2 y 6).
;; - cubo: Lista de 6 caras (matrices) o lista vacía.
;; - movs: Lista de movimientos como símbolos (e.g., 'F1D, 'C2A).
;;
;; Retorna:
;; - El cubo resultante tras aplicar todos los movimientos.
;; -----------------------------------------------------------------------------
(define (RS n cubo movs)
  (cond
    [(null? movs) cubo]
    [else
     (RS n (aplicar-movimiento n cubo (car movs)) (cdr movs))]))


;(define cubo-ejemplo (crear-cubo 2))

;(RS 2 cubo-ejemplo '(F1I F2D))

