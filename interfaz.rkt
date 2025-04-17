#lang racket
(require racket/gui/base)

;; =============================================================================
;; Función: ventana
;; Especificación:
;;   Crea la ventana principal del simulador de cubo Rubik.
;;   No recibe parámetros.
;;   Retorna un objeto de tipo frame% que representa la ventana.
;; =============================================================================
(define ventana
  (new frame%
       [label "Simulador de Cubo Rubik"]
       [width 1000]
       [height 700]))

;; =============================================================================
;; Función: etiqueta
;; Especificación:
;;   Crea un mensaje de bienvenida dentro de la ventana principal.
;;   No recibe parámetros.
;;   Retorna un objeto de tipo message%.
;; =============================================================================
(define etiqueta
  (new message%
       [parent ventana]
       [label "¡Bienvenido al simulador de Rubik!"]))


;; =============================================================================
;; Función: mostrar-ventana
;; Especificación:
;;   Muestra la ventana principal en pantalla.
;;   No recibe parámetros.
;;   No retorna valor.
;; =============================================================================
(send ventana show #t)