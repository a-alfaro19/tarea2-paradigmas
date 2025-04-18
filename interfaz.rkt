#lang racket/gui

(require racket/class)  ;; Importa la librería para trabajar con clases en Racket.
(define min-tam 2)  ;; Tamaño mínimo del cubo (2x2).
(define max-tam 6)  ;; Tamaño máximo del cubo (6x6).
(define cell-size 38)  ;; Tamaño de cada celda en píxeles.
(define espaciado 11)  ;; Espaciado entre celdas en la interfaz.
(define cubo-actual (make-parameter '()))  ;; Parámetro que guarda el cubo actual.
(define tam-cubo (make-parameter 3))  ;; Parámetro que guarda el tamaño del cubo, inicialmente 3x3.

;; ===============================================================================================
;; Nombre de la función: generar-cubo-vacio
;; Descripción: Genera un cubo vacío de tamaño n x n x n con colores predeterminados en sus caras.
;; Parámetros de entrada:
;;    - n: Un número entero que representa el tamaño del cubo.
;; Parámetros de salida:
;;    - Una lista tridimensional que representa el cubo, con colores predeterminados en cada cara.
;; ================================================================================================

(define (generar-cubo-vacio n)
  (define colores '(white red blue orange green yellow));; Definir los colores de las caras del cubo
 ;; Crear una lista para cada cara del cubo, con n filas y n columnas
  (for/list ([color colores])  ;; Itera sobre los colores para cada cara
    (build-list n (λ (_)           ;; Crea una lista de n filas
      (build-list n (λ (_) color))))))  ;; Cada fila tiene n veces el color actual

;; =================================================================================
;; Nombre de la función: draw-face
;; Descripción: Dibuja una cara del cubo en un canvas gráfico usando los colores dados.
;; Parámetros de entrada:
;;   - dc: Contexto de dibujo (drawing context) donde se dibujará.
;;   - face: Lista bidimensional con los colores de la cara del cubo.
;;   - x: Posición X donde iniciar el dibujo.
;;   - y: Posición Y donde iniciar el dibujo.
;; Parámetros de salida:
;;   - No retorna nada. Dibuja directamente en el contexto gráfico.
;; =================================================================================

(define (draw-face dc face x y)
  (for ([i (in-naturals)] [row face]);; Recorre cada fila con índice i
    (for ([j (in-naturals)] [color row]);; Recorre cada color en la fila con índice j
      (send dc set-brush (send the-color-database find-color (symbol->string color)) 'solid);; Establece el color del pincel
      ;; Dibuja el rectángulo en la posición correspondiente
      (send dc draw-rectangle (+ x (* j cell-size))    ; Posición X
                              (+ y (* i cell-size))    ; Posición Y
                              cell-size                ; Ancho
                              cell-size))))            ; Alto

;; ==================================================================================
;; Nombre de la funcón: draw-cube
;; Descripción: Dibuja un cubo completo en forma de cruz 2D, centrado en la pantalla.
;; Parámetros de entrada:
;;   - dc: Contexto de dibujo donde se renderiza el cubo.
;;   - ancho: Ancho total de la ventana o canvas.
;;   - alto: Alto total de la ventana o canvas.
;; Parámetros de salida:
;;   - No retorna nada. Dibuja el cubo directamente en el canvas.
;; =================================================================================

(define (draw-cube dc ancho alto)
  (send dc set-background (make-color 0 0 0))     ;; Fondo negro
  (send dc clear)                                 ;; Limpia el canvas
  (define size (tam-cubo))                        ;; Obtiene el tamaño del cubo
  (define cubo (cubo-actual))                     ;; Obtiene el estado actual del cubo

  ;; Calcula el tamaño total del cubo en píxeles
  (define total-ancho (* 4 size cell-size))       ;; Ancho total del cubo (4 caras horizontales)
  (define total-alto (* 3 size cell-size))        ;; Alto total del cubo (3 caras verticales)

  ;; Calcula desplazamientos para centrar el cubo en pantalla
  (define offset-x (/ (- ancho total-ancho) 2))   ;; Offset horizontal
  (define offset-y (/ (- alto total-alto) 2))     ;; Offset vertical

  ;; Dibuja el cubo solo si tiene 6 caras
  (when (= (length cubo) 6)
    (define cara-xy
      `((0 ,(+ offset-x (* size cell-size)) ,offset-y)                         ;; cara superior
        (1 ,(+ offset-x (* size cell-size)) ,(+ offset-y (* size cell-size))) ;; cara frontal
        (2 ,(+ offset-x (* 2 size cell-size)) ,(+ offset-y (* size cell-size))) ;; cara derecha
        (3 ,offset-x ,(+ offset-y (* size cell-size)))                          ;; cara izquierda
        (4 ,(+ offset-x (* 3 size cell-size)) ,(+ offset-y (* size cell-size))) ;; cara trasera
        (5 ,(+ offset-x (* size cell-size)) ,(+ offset-y (* 2 size cell-size))))) ;; cara inferior

    ;; Dibuja cada una de las 6 caras
    (for ([i (in-range 6)])
      (define face (list-ref cubo i))              ;; Selecciona la cara i del cubo
      (define-values (ix x y) (apply values (list-ref cara-xy i))) ;; Posición de la cara
      (draw-face dc face x y))))                   ;; Dibuja la cara en su posición

;; =============================
;; Nombre de la funciónn: Interfaz gráfica del simulador del Cubo de Rubik
;; Descripción: Crea una ventana con un canvas para dibujar el cubo y botones para cambiar su tamaño.
;; Parámetros de entrada:
;;   - Ninguno directamente. Usa valores globales como `min-tam`, `max-tam`, etc.
;; Parámetros de salida:
;;   - No retorna valores. Crea y muestra la interfaz gráfica del cubo.
;; =============================

(define ventana (new frame%
                     [label "Cubo de Rubik"]   ;; Título de la ventana
                     [width 1000]              ;; Ancho de la ventana
                     [height 600]))            ;; Alto de la ventana

(define main-panel (new vertical-panel% [parent ventana])) ;; Panel principal vertical

(define canvas
  (new canvas%
       [parent main-panel]       ;; El canvas está dentro del panel principal
       [min-width 00]            ;; Ancho mínimo (parece un error, debería ser >0)
       [min-height 700]          ;; Alto mínimo
       [paint-callback           ;; Función que dibuja el cubo cuando se necesita
        (λ (_ dc)
          (define-values (ancho alto) (send canvas get-size)) ;; Obtiene el tamaño del canvas
          (draw-cube dc ancho alto))]))                       ;; Dibuja el cubo centrado

(define botonera (new horizontal-panel% [parent main-panel])) ;; Panel horizontal para los botones

;; Crea botones para cada tamaño permitido del cubo
(for ([n (in-range min-tam (+ max-tam 1))])
  (new button%
       [parent botonera]                             ;; El botón va en la botonera
       [label (format "~ax~a" n n)]                  ;; Texto del botón, por ejemplo: 3x3
       [callback (λ (_event _control)
                   (tam-cubo n)                      ;; Actualiza el tamaño del cubo
                   (actualizar-cubo! (generar-cubo-vacio n)))])) ;; Genera un nuevo cubo vacío

;; =============================
;; Nombre de la función: actualizar-cubo!
;; Descripción: Actualiza el estado actual del cubo y redibuja el canvas.
;; Parámetros de entrada:
;;   - nuevo-cubo: Una lista tridimensional que representa el nuevo estado del cubo.
;; Parámetros de salida:
;;   - No retorna nada. Actualiza el cubo y fuerza su redibujado en pantalla.
;; =============================
(define (actualizar-cubo! nuevo-cubo)
  (cubo-actual nuevo-cubo)   ;; Establece el nuevo estado del cubo
  (send canvas refresh))     ;; Redibuja el canvas para mostrar el nuevo cubo

;; Inicializar
(actualizar-cubo! (generar-cubo-vacio (tam-cubo)))
(send ventana show #t)
