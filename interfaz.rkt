#lang racket/gui

(require "cubo.rkt") `

(require racket/class)  ;; Importa la librería para trabajar con clases en Racket.
(define min-tam 2)  ;; Tamaño mínimo del cubo (2x2).
(define max-tam 6)  ;; Tamaño máximo del cubo (6x6).
(define cell-size 35)  ;; Tamaño de cada celda en píxeles.
(define espaciado 2)  ;; Espaciado entre celdas en la interfaz.
(define cubo-actual (make-parameter '()))  ;; Parámetro que guarda el cubo actual.
(define tam-cubo (make-parameter 3))  ;; Parámetro que guarda el tamaño del cubo, inicialmente 3x3.
(define vista-frontal? (make-parameter #t)) ;; true: frontal/derecha/inferior, false: trasera/izquierda/superior
(define cara-actual (make-parameter 0)) ;; valores de 0 a 5


;; ================================================================================
;; Función: draw-face-2d
;; Dibuja una cara del cubo en el canvas usando los colores definidos.
;;
;; Parámetros:
;;   - dc: contexto gráfico donde se va a dibujar.
;;   - face: una lista de listas que representa una cara del cubo (matriz de colores).
;;   - origin-x, origin-y: coordenadas donde empieza a dibujar.
;;
;; Recorre cada celda de la cara, obtiene su color desde una tabla,
;; y dibuja un rectángulo con ese color.
;; ================================================================================

(define tabla-colores
  '((blanco . "white")
    (rojo . "red")
    (azul . "blue")
    (naranja . "orange")
    (verde . "green")
    (amarillo . "yellow"))) ; Mapea símbolos usados en la lógica a strings reconocidos por Racket.

(define (draw-face-2d dc face origin-x origin-y)
  (for ([i (in-range (length face))])        ; Recorre filas
    (for ([j (in-range (length (list-ref face i)))]  ; Recorre columnas
          #:when (assoc (list-ref (list-ref face i) j) tabla-colores)) ; Solo si el color existe en la tabla
      (let* ((color-sym (list-ref (list-ref face i) j))               ; Color como símbolo (ej. 'rojo)
             (color-str (cdr (assoc color-sym tabla-colores)))       ; Color como string (ej. "red")
             (actual-color (send the-color-database find-color color-str)) ; Obtiene el color real para dibujar
             (x (+ origin-x (* j cell-size) (* j espaciado)))         ; Coordenada X del cuadro
             (y (+ origin-y (* i cell-size) (* i espaciado))))        ; Coordenada Y del cuadro
        (send dc set-brush actual-color 'solid)                       ; Color de relleno
        (send dc set-pen "black" 1 'solid)                            ; Borde negro
        (send dc draw-rectangle x y cell-size cell-size)))))          ; Dibuja el cuadro


;; ==================================================================================
;; Función: draw-cara-unica
;; Dibuja solo una cara del cubo (la seleccionada) centrada en el canvas.
;;
;; Parámetros:
;;   - dc: contexto gráfico donde se va a dibujar.
;;   - ancho: ancho del área de dibujo.
;;   - alto: alto del área de dibujo.
;;
;; Limpia el canvas, calcula el centro y dibuja la cara actual del cubo.
;; ==================================================================================

(define (draw-cara-unica dc ancho alto)
  (send dc set-background (make-color 0 0 0)) ; Establece fondo negro
  (send dc clear)                             ; Limpia el canvas
  ;; Obtiene el cubo y su configuración actual
  (define cubo (cubo-actual))                ; Cubo actual (lista de 6 caras)
  (define size (tam-cubo))                   ; Tamaño del cubo (n x n)
  (define cara (list-ref cubo (cara-actual))) ; Toma la cara seleccionada (0 a 5)
  (define total-size (+ (* cell-size size) (* espaciado (- size 1))))  ;; Calcula el tamaño total de la cara (en píxeles) incluyendo los espacios
  (define start-x (/ (- ancho total-size) 2));; Calcula la posición de inicio para centrar la cara
  (define start-y (/ (- alto total-size) 2))
  (draw-face-2d dc cara start-x start-y));; Dibuja la cara en el centro del canvas


;; ===================================================================================
;; Función: Interfaz gráfica del simulador del Cubo de Rubik
;; Descripción: Crea una ventana con un canvas para dibujar el cubo y botones para cambiar su tamaño.
;; 
;; Parámetros de entrada:
;;   - Ninguno directamente. Usa valores globales como `min-tam`, `max-tam`, etc.
;;
;; Parámetros de salida:
;;   - No retorna valores. Crea y muestra la interfaz gráfica del cubo.
;; ===================================================================================

;; Crear la ventana principal donde se dibujará el cubo
(define ventana (new frame%
                     [label "Cubo de Rubik"]  ;; Título de la ventana
                     [width 1000]             ;; Ancho de la ventana
                     [height 600]))           ;; Alto de la ventana

;; Crear el panel principal que contiene el canvas (para el cubo) y la botonera
(define main-panel (new vertical-panel% [parent ventana])) 

;; Crear el canvas donde se dibujará el cubo, con una función de dibujo personalizada
(define canvas
  (new canvas%
       [parent main-panel]       ;; El canvas está dentro del panel principal
       [min-width 0]             ;; Ancho mínimo (parece un error, debería ser > 0)
       [min-height 700]          ;; Alto mínimo
       [paint-callback           ;; Función que dibuja el cubo cuando se necesita
        (λ (_ dc)
          (define-values (ancho alto) (send canvas get-size)) ;; Obtiene el tamaño del canvas
          (draw-cara-unica dc ancho alto))])) ;; Dibuja el cubo centrado

;; Crear la botonera (panel horizontal) con los botones de tamaño del cubo
(define botonera (new horizontal-panel% [parent main-panel]))

;; Crear los botones para seleccionar el tamaño del cubo (2x2, 3x3, etc.)
(for ([n (in-range min-tam (+ max-tam 1))])
  (new button%
       [parent botonera]                             ;; El botón va en la botonera
       [label (format "~ax~a" n n)]                  ;; Texto del botón, por ejemplo: 3x3
       [callback (λ (_event _control)
                  (tam-cubo n)                        ;; Actualiza el tamaño del cubo
                  (actualizar-cubo! (crear-cubo n)))])) ;; Genera un cubo de tamaño n

;; Crear los botones para cambiar entre las caras del cubo (0 a 5)
(for ([i (in-range 6)])
  (new button%
       [parent botonera]
       [label (format "Cara ~a" i)]                 ;; Texto del botón, por ejemplo: "Cara 0"
       [callback (λ (_event _control)
                  (cara-actual i)                    ;; Cambia a la cara seleccionada
                  (send canvas refresh))]))         ;; Refresca el canvas para mostrar la nueva cara



;; =================================================================================
;; Nombre de la función: actualizar-cubo!
;; Descripción: Actualiza el estado actual del cubo y redibuja el canvas.
;; Parámetros de entrada:
;;   - nuevo-cubo: Una lista tridimensional que representa el nuevo estado del cubo.
;; Parámetros de salida:
;;   - No retorna nada. Actualiza el cubo y fuerza su redibujado en pantalla.
;; =================================================================================

(define (actualizar-cubo! nuevo-cubo)
  (cubo-actual nuevo-cubo)   ;; Establece el nuevo estado del cubo, guardándolo en la variable global
  (send canvas refresh))     ;; Redibuja el canvas para mostrar el nuevo cubo con el estado actualizado

;; Inicializar
(actualizar-cubo! (crear-cubo (tam-cubo)))   ;; Crea el cubo inicial con el tamaño actual y lo actualiza
(send ventana show #t)                      ;; Muestra la ventana con la interfaz gráfica
