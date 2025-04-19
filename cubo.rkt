#lang racket
(provide crear-cubo crear-cara crear-fila)

; Definición del cubo

;; -----------------------------------------------------------------------------
;; crear-cubo Número -> Lista
;;
;; Crea un cubo tridimensional representado como una lista de 6 caras,
;; donde cada cara es una matriz de tamaño n x n con un color específico.
;;
;; Parámetros:
;; - n: número entero que indica el tamaño de la cara del cubo.
;;
;; Retorna:
;; - Una lista de 6 matrices, cada una de tamaño n x n.
;;
;; Restricciones:
;; - n debe estar en el rango de 2 a 6, inclusive.
;; -----------------------------------------------------------------------------
(define (crear-cubo n)
  (cond
    [(and (>= n 2) (<= n 6)) ;; n está dentro del rango?
     (crear-cubo-aux n '() '(blanco rojo azul naranja verde amarillo))]
    [else
     '()]))


(define (crear-cubo-aux n cubo colores)
  (cond
    [(equal? (length cubo) 6) ;; Cubo tiene 6 caras?
     cubo]
    [else
     (crear-cubo-aux n (append cubo (list(crear-cara n '() (car colores)))) (cdr colores))])) ;; Crear cara y agregarla al cubo


;; -----------------------------------------------------------------------------
;; crear-cara Número Lista Símbolo -> Lista
;;
;; Crea una cara del cubo de tamaño n x n, donde cada fila contiene el color
;; especificado y se repite n veces.
;;
;; Parámetros:
;; - n: número entero que indica el tamaño de la cara (n filas de n columnas).
;; - cara: lista parcial construida recursivamente, inicialmente vacía.
;; - color: símbolo que representa el color de la cara.
;;
;; Retorna:
;; - Una lista de n filas (listas), cada una con n elementos del color indicado.
;; -----------------------------------------------------------------------------
(define (crear-cara n cara color)
  (cond
    [(equal? (length cara) n) ;; La cara tiene 6 filas?
     cara]
    [else
     (crear-cara n (append cara (list (crear-fila n '() color))) color)])) ;; Crear fila tamaño n y agregarla a la cara


;; -----------------------------------------------------------------------------
;; crear-fila Número Lista Símbolo -> Lista
;;
;; Crea una fila de tamaño n, donde cada elemento es el color especificado.
;;
;; Parámetros:
;; - n: número entero que representa la cantidad de columnas de la fila.
;; - fila: lista parcial construida recursivamente, inicialmente vacía.
;; - color: símbolo que representa el color que se repetirá en la fila.
;;
;; Retorna:
;; - Una lista de longitud n con el color repetido n veces.
;; -----------------------------------------------------------------------------
(define (crear-fila n fila color)
  (cond
    [(equal? (length fila) n) ;; La fila tiene 6 columnas?
     fila]
    [else
     (crear-fila n (append fila (list color)) color)])) ;; Agregar una columna de color 'color' a la fila
