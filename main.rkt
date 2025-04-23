#lang racket
;; -----------------------------------------------------------------------------
;; Archivo: main.rkt
;; Descripción: Define toda la lógica de manipulación del cubo Rubik, incluyendo su creación,
;; rotaciones de filas, columnas, caras y detección de extremos.
;; -----------------------------------------------------------------------------

(provide crear-cubo
         RS)

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
    [(= (length cubo) 6) (invertir-lista cubo)]
    [else (crear-cubo-aux n (cons (crear-cara n '() (car colores)) cubo) (cdr colores))]))


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
    [(= (length cara) n) (invertir-lista cara)]
    [else (crear-cara n (cons (crear-fila n '() color) cara) color)])) ;; Crear fila tamaño n y agregarla a la cara


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
    [(= (length fila) n) (invertir-lista fila)]
    [else (crear-fila n (cons color fila) color)])) ;; Agregar una columna de color 'color' a la fila


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
;; Reemplaza el elemento en la posición 'posterior' de una lista cualquiera.
;; -----------------------------------------------------------------------------

(define (reemplazar-en-lista lista pos nuevo)
  (cond
    [(= pos 0) (cons nuevo (cdr lista))]
    [else (cons (car lista) (reemplazar-en-lista (cdr lista) (- pos 1) nuevo))]))

;; -----------------------------------------------------------------------------
;; rotar-fila-en-cubo: Número Lista Número Símbolo -> Lista
;;
;; Rota una fila en el cubo. Si es extremo, también rota la cara correspondiente.
;; -----------------------------------------------------------------------------

(define (rotar-fila-en-cubo n cubo fila direccion)
  (cond
    ;; Si es extremo, aplicar rotación especial
    [(es-extremo-fila? fila n)
     (rotar-fila-en-cubo-extremo n (rotar-fila-extremo cubo fila direccion) fila direccion)]
    ;; Si no es extremo, solo se rota la parte lateral
    [else (rotar-fila-extremo cubo fila direccion)]))

;; -----------------------------------------------------------------------------
;; rotar-fila-en-cubo-extremo: Número Lista Número Símbolo -> Lista
;;
;; Rota la cara superior (0) o inferior (5) si la fila es 1 o n respectivamente.
;; -----------------------------------------------------------------------------

(define (rotar-fila-en-cubo-extremo n cubo fila direccion)
  (cond
    ;; Fila superior (1) hacia la derecha → rotar cara superior antihoraria
    [(and (= fila 1) (equal? direccion 'derecha))
     (reemplazar-en-lista cubo 0 (rotar-cara-antihoraria (obtener-elemento cubo 0)))]

    ;; Fila superior (1) hacia la izquierda → rotar cara superior horaria
    [(and (= fila 1) (equal? direccion 'izquierda))
     (reemplazar-en-lista cubo 0 (rotar-cara-horaria (obtener-elemento cubo 0)))]

    ;; Fila inferior (n) hacia la derecha → rotar cara inferior horaria
    [(and (= fila n) (equal? direccion 'derecha))
     (reemplazar-en-lista cubo 5 (rotar-cara-horaria (obtener-elemento cubo 5)))]

    ;; Fila inferior (n) hacia la izquierda → rotar cara inferior antihoraria
    [(and (= fila n) (equal? direccion 'izquierda))
     (reemplazar-en-lista cubo 5 (rotar-cara-antihoraria (obtener-elemento cubo 5)))]

    ;; Ninguna rotación necesaria
    [else cubo]))


;; -----------------------------------------------------------------------------
;; rotar-fila-extremo: Lista Número Símbolo -> Lista
;;
;; Extrae las filas necesarias y llama a la auxiliar que rota entre
;; las caras 4 (izquierda), 1 (frontal), 2 (derecha), 3 (posterior).
;; -----------------------------------------------------------------------------

(define (rotar-fila-extremo cubo fila direccion)
  (rotar-fila-extremo-aux
   cubo
   fila
   direccion
   (extraer-fila (obtener-elemento cubo 4) fila) ; izq
   (extraer-fila (obtener-elemento cubo 1) fila) ; fr
   (extraer-fila (obtener-elemento cubo 2) fila) ; der
   (extraer-fila (obtener-elemento cubo 3) fila))) ; post

;; -----------------------------------------------------------------------------
;; rotar-fila-extremo-aux: Lista Número Símbolo Lista Lista Lista Lista -> Lista
;;
;; Rota las filas entre las caras 4, 1, 2 y 3 del cubo según la dirección dada.
;; -----------------------------------------------------------------------------

(define (rotar-fila-extremo-aux cubo fila direccion izq fr der post)
  (cond
    ;; Movimiento hacia la derecha:
    ;; izq <- post, fr <- izq, der <- fr, post <- der
    [(equal? direccion 'derecha)
     (reemplazar-en-lista
      (reemplazar-en-lista
       (reemplazar-en-lista
        (reemplazar-en-lista cubo 4 (reemplazar-fila (obtener-elemento cubo 4) fila post))
        1 (reemplazar-fila (obtener-elemento cubo 1) fila izq))
       2 (reemplazar-fila (obtener-elemento cubo 2) fila fr))
      3 (reemplazar-fila (obtener-elemento cubo 3) fila der))]

    ;; Movimiento hacia la izquierda:
    ;; izq <- fr, fr <- der, der <- post, post <- izq
    [(equal? direccion 'izquierda)
     (reemplazar-en-lista
      (reemplazar-en-lista
       (reemplazar-en-lista
        (reemplazar-en-lista cubo 4 (reemplazar-fila (obtener-elemento cubo 4) fila fr))
        1 (reemplazar-fila (obtener-elemento cubo 1) fila der))
       2 (reemplazar-fila (obtener-elemento cubo 2) fila post))
      3 (reemplazar-fila (obtener-elemento cubo 3) fila izq))]))

;; -----------------------------------------------------------------------------
;; rotar-columna-en-cubo: Número Lista Número Símbolo -> Lista
;;
;; Rota una columna del cubo entre las caras superior (0), frontal (1),
;; inferior (5) y posterior (3), en dirección 'arriba o 'abajo.
;; También rota la cara lateral si la columna esta al extremo.
;; -----------------------------------------------------------------------------

(define (rotar-columna-en-cubo n cubo columna direccion)
  (rotar-columna-en-cubo-aux
   n
   cubo
   columna
   direccion
   (extraer-columna (obtener-elemento cubo 0) columna) ; sup
   (extraer-columna (obtener-elemento cubo 1) columna) ; fr
   (extraer-columna (obtener-elemento cubo 5) columna) ; inf
   (extraer-columna (obtener-elemento cubo 3) (- n (- columna 1))) ; post inversa
   (= columna 1) ; izq?
   (= columna n))) ; der?

;; -----------------------------------------------------------------------------
;; rotar-columna-en-cubo-aux: Número Lista Número Símbolo Lista Lista Lista Lista Bool Bool -> Lista
;; Prepara las columnas y su orientación según la dirección de rotación y 
;; llama a la función que actualiza el cubo con los nuevos valores.
;; -----------------------------------------------------------------------------

(define (rotar-columna-en-cubo-aux n cubo columna direccion sup fr inf post izq? der?)
  (cond
    [(equal? direccion 'abajo)
     (rotar-columna-con-cambios
      n cubo columna
      (invertir-lista post) sup fr (invertir-lista inf)
      direccion izq? der?)] 

    [(equal? direccion 'arriba)
     (rotar-columna-con-cambios
      n cubo columna
      fr inf (invertir-lista post) (invertir-lista sup)
      direccion izq? der?)])) ; 

;; -----------------------------------------------------------------------------
;; rotar-columna-con-cambios: Número Lista Número Lista Lista Lista Lista Bool Bool -> Lista
;;
;; Aplica los reemplazos en orden, y rota la cara lateral si corresponde.
;; -----------------------------------------------------------------------------

(define (rotar-columna-con-cambios n cubo columna nuevo-sup nuevo-fr nuevo-inf nuevo-post direccion izq? der?)
  (rotar-columna-con-cambios-lateral
   n
   (reemplazar-en-lista
    (reemplazar-en-lista
     (reemplazar-en-lista
      (reemplazar-en-lista
       cubo
       0 (reemplazar-columna (obtener-elemento cubo 0) columna nuevo-sup))
      1 (reemplazar-columna (obtener-elemento cubo 1) columna nuevo-fr))
     5 (reemplazar-columna (obtener-elemento cubo 5) columna nuevo-inf))
    3 (reemplazar-columna (obtener-elemento cubo 3) (- n (- columna 1)) nuevo-post))
   direccion ;
   izq?
   der?))


;; -----------------------------------------------------------------------------
;; rotar-columna-con-cambios-lateral: Número Lista Bool Bool -> Lista
;;
;; Aplica la rotación de cara lateral si columna es izquierda o derecha.
;; -----------------------------------------------------------------------------

(define (rotar-columna-con-cambios-lateral n cubo direccion izq? der?)
  (cond
    ;; Columna izquierda rotando abajo → cara 4 debe girar horaria
    [(and izq? (equal? direccion 'abajo))
     (reemplazar-en-lista cubo 4 (rotar-cara-horaria (obtener-elemento cubo 4)))]
    
    ;; Columna izquierda rotando arriba → cara 4 debe girar antihoraria
    [(and izq? (equal? direccion 'arriba))
     (reemplazar-en-lista cubo 4 (rotar-cara-antihoraria (obtener-elemento cubo 4)))]

    ;; Columna derecha rotando abajo → cara 2 debe girar antihoraria
    [(and der? (equal? direccion 'abajo))
     (reemplazar-en-lista cubo 2 (rotar-cara-antihoraria (obtener-elemento cubo 2)))]

    ;; Columna derecha rotando arriba → cara 2 debe girar horaria
    [(and der? (equal? direccion 'arriba))
     (reemplazar-en-lista cubo 2 (rotar-cara-horaria (obtener-elemento cubo 2)))]

    [else cubo]))



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


;; -----------------------------------------------------------------------------
;; RS: Número Lista Lista -> Lista
;; Función principal de RubikSimulator.
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


;Funcionamiento
;(RS 2 (crear-cubo 2) '(F1I F2D))

