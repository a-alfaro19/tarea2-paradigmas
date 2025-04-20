; -----------------------------------------------------------------------------
;; RubikSimulator - Interfaz Gráfica Funcional Pura
;;
;; Este archivo construye la interfaz gráfica del simulador de cubo Rubik,
;; mostrando las caras desplegadas en 2D y permitiendo simular movimientos.
;; Se utiliza el paradigma funcional puro con GUI de Racket.
;; -----------------------------------------------------------------------------

#lang racket/gui
(require "main.rkt") 
(require racket/class) ; Librería GUI de Racket

; -----------------------------------------------------------------------------
;; Constantes de configuración visual (tamaños y diseño del cubo)
; -----------------------------------------------------------------------------

(define min-tam 2)       ; Tamaño mínimo permitido para el cubo (2x2)
(define max-tam 6)       ; Tamaño máximo permitido para el cubo (6x6)
(define cell-size 30)    ; Tamaño de cada celda del cubo en píxeles
(define espaciado 2)     ; Espacio entre celdas para visualización
(define margen-cara 5)   ; Espacio entre caras del cubo en el canvas

; -----------------------------------------------------------------------------
;; Estado del cubo actual y tamaño configurado dinámicamente
; -----------------------------------------------------------------------------

(define cubo-actual (make-parameter '())) ; Guarda el cubo actual como parámetro
(define tam-cubo (make-parameter 3))      ; Guarda el tamaño actual del cubo

; Tabla de colores visuales asociados a símbolos que representan cada color
(define tabla-colores
  '((blanco . "white") (rojo . "red") (azul . "blue")
    (naranja . "orange") (verde . "green") (amarillo . "yellow")))

; Fuentes utilizadas para los distintos textos en la interfaz
(define fuente-titulo (make-object font% 14 'default 'normal 'bold))
(define fuente-normal (make-object font% 11 'default 'normal 'normal))
(define fuente-instruccion (make-object font% 10 'default 'italic 'normal))

; -----------------------------------------------------------------------------
;; draw-cell: Dibuja una sola celda del cubo
;; - dc: contexto de dibujo
;; - color-sym: símbolo del color a dibujar
;; - i, j: coordenadas fila/columna dentro de la cara
;; - origin-x/y: posición superior izquierda de la cara
; -----------------------------------------------------------------------------
(define (draw-cell dc color-sym i j origin-x origin-y)
  (let ((color-str (cdr (assoc color-sym tabla-colores)))
        (x (+ origin-x (* j cell-size) (* j espaciado)))
        (y (+ origin-y (* i cell-size) (* i espaciado))))
    (let ((color (send the-color-database find-color color-str)))
      (send dc set-brush color 'solid)
      (send dc set-pen "black" 1 'solid)
      (send dc draw-rectangle x y cell-size cell-size))))

; -----------------------------------------------------------------------------
;; draw-face-2d: Dibuja una cara del cubo como matriz de colores
;; - face: matriz (lista de listas) con símbolos de color
;; - origin-x/y: punto de inicio en el canvas
; -----------------------------------------------------------------------------
(define (draw-face-2d dc face origin-x origin-y)
  (map (lambda (i)
         (map (lambda (j)
                (draw-cell dc (list-ref (list-ref face i) j) i j origin-x origin-y))
              (build-list (length (car face)) values)))
       (build-list (length face) values)))

; -----------------------------------------------------------------------------
;; draw-cubo-desplegado: Dibuja el cubo completo en formato cruz 2D
;; Muestra las seis caras con separación proporcional
; -----------------------------------------------------------------------------
(define (draw-cubo-desplegado dc ancho alto)
  (let* ((cubo (cubo-actual))
         (n (tam-cubo))
         (cara-size (+ (* cell-size n) (* espaciado (- n 1))))
         (espacio (+ cara-size margen-cara))
         (total-ancho (* espacio 4))
         (total-alto (* espacio 3))
         (x-base (/ (- ancho total-ancho) 2))
         (y-base (+ (/ (- alto total-alto) 2) 170)))
    (send dc set-background (make-color 255 255 255))
    (send dc clear)
    (apply (lambda (cara0 cara1 cara2 cara3 cara4 cara5)
             (draw-face-2d dc cara0 (+ x-base espacio) (- y-base espacio)) ; superior
             (draw-face-2d dc cara4 x-base y-base)                         ; izquierda
             (draw-face-2d dc cara1 (+ x-base espacio) y-base)            ; frontal
             (draw-face-2d dc cara2 (+ x-base (* 2 espacio)) y-base)      ; derecha
             (draw-face-2d dc cara3 (+ x-base (* 3 espacio)) y-base)      ; posterior
             (draw-face-2d dc cara5 (+ x-base espacio) (+ y-base espacio))) ; inferior
           cubo)))

; -----------------------------------------------------------------------------
;; movimiento-valido?: Verifica que un movimiento sea válido sintácticamente
;; Ej: "F1D", "C2A" son válidos; "F9X" no lo es
; -----------------------------------------------------------------------------
(define (movimiento-valido? mov n)
  (cond
    [(not (= (string-length mov) 3)) #f] ; Debe tener exactamente 3 caracteres
    [else
     (let ((c1 (string-ref mov 0)) ; F o C
           (c2 (string-ref mov 1)) ; Número
           (c3 (string-ref mov 2))) ; D/I/A/B
       (and (char-upper-case? c1)
            (char-upper-case? c3)
            (char-numeric? c2)
            (<= 1 (- (char->integer c2) 48) n) ; El número debe estar dentro del tamaño
            (or (and (equal? c1 #\F) (member c3 '(#\D #\I)))
                (and (equal? c1 #\C) (member c3 '(#\A #\B))))))]))

; -----------------------------------------------------------------------------
;; simular-movs: Aplica cada movimiento uno por uno con animación
;; Recorre la lista de movimientos, actualiza el cubo y espera 0.5s
; -----------------------------------------------------------------------------
(define (simular-movs cubo movs)
  (cond
    [(null? movs) (actualizar-cubo! cubo)] ; Caso base: termina
    [else
     (let ((nuevo (RS (tam-cubo) cubo (list (string->symbol (car movs))))))
       (actualizar-cubo! nuevo)
       (sleep/yield 0.5)
       (simular-movs nuevo (cdr movs)))]))

; -----------------------------------------------------------------------------
;; actualizar-cubo!: Actualiza el estado visual y los datos del cubo actual
; -----------------------------------------------------------------------------
(define (actualizar-cubo! nuevo-cubo)
  (cubo-actual nuevo-cubo)
  (send canvas refresh))

; -----------------------------------------------------------------------------
;; Elementos de interfaz gráfica: Controles de la GUI
;; Incluye: ventana, canvas, paneles, botones y campos de texto
; -----------------------------------------------------------------------------

(define ventana (new frame% [label "RubikSimulator - Interfaz Validada"] [width 1150] [height 700]))
(define contenedor-horizontal (new horizontal-panel% [parent ventana] [alignment '(center center)]))

(define canvas
  (new canvas%
       [parent contenedor-horizontal]
       [min-width 800]
       [min-height 600]
       [paint-callback
        (lambda (_canvas dc)
          (define-values (w h) (send canvas get-size))
          (draw-cubo-desplegado dc w h))]))

(define panel-centro (new vertical-panel% [parent contenedor-horizontal] [alignment '(center center)] [stretchable-height #t]))
(define panel-controles (new vertical-panel% [parent panel-centro] [alignment '(center center)] [spacing 0]))

(new message% [parent panel-controles] [label "Seleccione un tamaño"] [font fuente-titulo])

; Botones de selección de tamaño del cubo
(define panel-tamanos (new vertical-panel% [parent panel-controles] [alignment '(center center)] [spacing 1]))
(map (lambda (n)
       (new button%
            [parent panel-tamanos] [label (format "~ax~a" n n)] [font fuente-normal]
            [callback (lambda (_event _control)
                        (tam-cubo n)
                        (actualizar-cubo! (crear-cubo n)))]))
     (build-list (- (+ max-tam 1) min-tam) (lambda (i) (+ i min-tam))))

; Instrucciones de movimiento
(new message% [parent panel-controles] [label "Indique los movimientos"] [font fuente-titulo])

(map (lambda (linea)
       (new message% [parent panel-controles] [label linea] [font fuente-instruccion]))
     '("Movimientos válidos:"
       "FnI: Fila n Izquierda"
       "FnD: Fila n Derecha"
       "CnA: Columna n Arriba"
       "CnB: Columna n Abajo"
       "Ej: F1D C2A"))

; Campo de texto para movimientos
(define input-movs (new text-field% [parent panel-controles] [label ""] [init-value ""] [min-width 200] [font fuente-normal] [stretchable-width #f]))
(define mensaje-error (new message% [parent panel-controles] [label ""] [font fuente-instruccion] [auto-resize #t]))

; Botones de acción: Simular y Reiniciar
(define panel-botones
  (new horizontal-panel% [parent panel-controles] [alignment '(center center)] [spacing 1]))

(new button%
     [parent panel-botones] [label "Simular"] [font fuente-normal]
     [callback (lambda (_event _control)
                 (let* ((texto (send input-movs get-value))
                        (lista (filter (lambda (x) (not (string=? x ""))) (string-split texto)))
                        (n (tam-cubo))
                        (invalidos (filter (lambda (m) (not (movimiento-valido? m n))) lista)))
                   (cond
                     [(null? invalidos)
                      (send mensaje-error set-label "")
                      (simular-movs (cubo-actual) lista)
                      (send input-movs set-value "")]
                     [else
                      (send mensaje-error set-label
                            (format "Error: movimiento inválido → ~a" (string-join invalidos ", ")))])))])

(new button%
     [parent panel-botones] [label "Reiniciar"] [font fuente-normal]
     [callback (lambda (_event _control)
                 (actualizar-cubo! (crear-cubo (tam-cubo)))
                 (send input-movs set-value "")
                 (send mensaje-error set-label ""))])

; Inicialización del cubo y despliegue de la ventana principal
(actualizar-cubo! (crear-cubo (tam-cubo)))
(send ventana show #t)