#lang racket

;Prototipo Resaltador v2
;Antonio Rafael Cedillo Rodríguez 


;Checando que tipo de lenguaje es (toma el string del archivo y lo escanea por elementos clave dependiendo de cada lenguaje (no necesario pero checar)

(define (identificar-codigo code)
  (define python 0)
  (define racket 0)
  (define c-plus 0)

  (if (or (string-contains? code ";") (string-contains? code "}") (string-contains? code "{")) 
      (set! c-plus (+ c-plus 1))
      (if (or (string-contains? code ":") (string-contains? code "#") (string-contains? code "def") (string-contains? code "print")) 
          (set! python (+ python 1))
          (if (string-contains? code "define")
              (set! racket (+ racket 1))
              "el codigo ingresado no es valido")))

  (display "****Resultados de Identificación de lenguaje****\n")

  (if (and (< python racket) (< python c-plus))
      (display "El codigo es en su mayoria python\n")
      (if (and (< racket python) (< racket c-plus))
          (display "El codigo es en su mayoria racket\n")
          (if (and (< c-plus racket) (< c-plus python))
              (display "El codigo es en su mayoria c++\n")
              (display "El codigo tiene rasgos de 2 o mas lenguajes o no es valido.\n")))))



          

;Checando cuantas categorias lexicas para resaltar (basado en elementos en comun y lenguaje del documento)---

(define (analisis code)
  (define operadores 0)
  (define condicional 0)
  (define ciclos 0)

  (if (string-contains? code "for") (set! ciclos (+ ciclos 1)) "no existen condicionales en el codigo" )
  (if (or (string-contains? code "else") (string-contains? code "if")) (set! condicional (+ condicional 1)) "no existen ciclos en el codigo")
  (if (or (string-contains? code "and") (string-contains? code "else")) (set! operadores (+ operadores 1)) "no existen operadores booleanos en el codigo")
  (if (or (string-contains? code "+") (string-contains? code "-") (string-contains? code "*") (string-contains? code "/")) (set! operadores (+ operadores 1)) "No exitsten operadores aritmeticos en el codigo")
  (if (or (string-contains? code ">") (string-contains? code "<")) (set! operadores (+ operadores 1)) "No existen operadores aritmeticos/comparación en el codigo")

  (display "****Resultados de Analisis****\n")
  (display "operadores:")
  (displayln operadores)
  (display "condicionales:")
  (displayln condicional)
  (display "ciclos:")
  (displayln ciclos)

  )




;Tomar el input del documento, guardarlo en variable ---
(display "Ingresa el nombre del documento (incluye '.txt'): ")
(define doc (read-line))
(display "Leyendo el documento: ")
(displayln doc)


;Transformando .txt a string para usar los metodos de busqueda
(define codigo (file->string doc))

;Leyendo el documento linea por linea ---

; Abre el archivo para leer
(define input-port (open-input-file doc))

; Lee y muestra cada línea del archivo
(let loop ()
  (define line (read-line input-port))
  (unless (eof-object? line)
    (displayln line)
    (loop)))

; Cierra el archivo
(close-input-port input-port)



;Usando los procedimientos


(identificar-codigo codigo)
(analisis codigo)
