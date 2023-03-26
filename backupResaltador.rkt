#lang racket

;Prototipo Resaltador v2
;Antonio Rafael Cedillo Rodríguez
;Lo unico que importa es que se use en el lenguaje


;Checando que tipo de lenguaje es (toma el string del archivo y lo escanea por elementos clave dependiendo de cada lenguaje (no necesario pero checar)

(define (identificar-codigo code) ;Complejidad O(1)
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

(define (analisis code) ;Complejidad O(1)
  (define operadores 0)
  (define condicional 0)
  (define ciclos 0)
  (define simbolos 0)
  (define palabras 0)
 
  (if (or (string-contains? code "for") (string-contains? code "while")) (set! ciclos (+ ciclos 1)) "no existen condicionales en el codigo" )
  (if (or (string-contains? code "else") (string-contains? code "if")  (string-contains? code "case")) (set! condicional (+ condicional 1)) "no existen ciclos en el codigo")
  (if (or (string-contains? code "and") (string-contains? code "or")) (set! operadores (+ operadores 1)) "no existen operadores booleanos en el codigo")
  (if (or (string-contains? code "+") (string-contains? code "-") (string-contains? code "*") (string-contains? code "/")) (set! operadores (+ operadores 1)) "No exitsten operadores aritmeticos en el codigo")
  (if (or (string-contains? code ">") (string-contains? code "<")) (set! operadores (+ operadores 1)) "No existen operadores aritmeticos/comparación en el codigo")
  (if (or (string-contains? code ">=") (string-contains? code "<=")) (set! operadores (+ operadores 1)) "No existen operadores aritmeticos/comparación en el codigo")
  (if (or (string-contains? code "=") (string-contains? code "(") (string-contains? code ")")) (set! simbolos (+ simbolos 1)) "No existen simbolos en el codigo")
  (if (or (string-contains? code "'") (string-contains? code "\"") (string-contains? code "[")) (set! simbolos (+ simbolos 1)) "No existen simbolos en el codigo")
  (if (or (string-contains? code "]") (string-contains? code "{") (string-contains? code "}")) (set! simbolos (+ simbolos 1)) "No existen simbolos en el codigo")
  (if (or (string-contains? code ";") (string-contains? code ",") (string-contains? code "|" )) (set! simbolos (+ simbolos 1)) "No existen simbolos en el codigo")
  (if (or (string-contains? code "break") (string-contains? code "double") (string-contains? code "char")) (set! palabras (+ palabras 1)) "No existen palabras en el codigo")
  (if (or (string-contains? code "string") (string-contains? code "list")) (set! palabras (+ palabras 1)) "No existen palabras en el codigo")
  (if (string-contains? code "let") (set! palabras (+ palabras 1)) "No existen palabras en el codigo")
  (if (or (string-contains? code "lambda") (string-contains? code "def") (string-contains? code "define")) (set! palabras (+ palabras 1)) "No existen palabras en el codigo")


  
  

  
  (display "****Resultados de Analisis****\n")
  (display "operadores:")
  (displayln operadores)
  (display "condicionales:")
  (displayln condicional)
  (display "ciclos:")
  (displayln ciclos)
  (display "simbolos:")
  (displayln simbolos)
  (display "palabras reservadas:")
  (displayln palabras)

  )




;Tomar el input del documento, guardarlo en variable ---
(display "Ingresa el nombre del documento (incluye '.txt'): ") ;O(1)
(define doc (read-line)) ;O(1)
(display "Leyendo el documento: ") ;O(1)
(displayln doc) ;O(1)


;Metodos de Racket ------

;Transformando .txt a string para usar los metodos de busqueda
(define codigo (file->string doc)) ;O(1)

;Leyendo el documento linea por linea ---

; Abre el archivo para leer
(define input-port (open-input-file doc)) ;O(1)

; Lee y muestra cada línea del archivo
(let loop () ;O(n) -> n cambia dependiendo del # de lineas del documento .txt
  (define line (read-line input-port))
  (unless (eof-object? line)
    (displayln line)
    (loop)))

; Cierra el archivo
(close-input-port input-port) ;O(n)



;Usando los procedimientos para imprimir en consola de Racket

(identificar-codigo codigo) ;O(1)
(analisis codigo) ; O(1)

;Metodos de HTML -----
(define (leer-lineas archivo) ;O(n) -> Depende del tamaño del archivo
  (with-input-from-file archivo
    (lambda ()
      (let loop ((lineas '())
                 (line (read-line)))
        (cond ((eof-object? line) (reverse lineas))
              (else (loop (cons line lineas) (read-line))))))))


(define (escribir-html archivo lineas) ;O(n*m) -> n es numero de lineas, m es numero de palabras por letra
  (with-output-to-file archivo
    (lambda ()
      (display "<html>\n<head>\n")
      (display "<link rel=\"stylesheet\" type=\"text/css\" href=\"estilo.css\">\n")
      (display "</head>\n<body>\n")
      (display "<h2> Resultados de Analisis </h2>\n")
      (display "<h4> Simbologia </h4>\n")
      (display "<h4 class=\"operador\" > Rojo: Operadores </h4>")
      (display "<h4 class=\"ciclo\"> Azul: Ciclos </h4>")
      (display "<h4 class=\"condicional\"> Morado: Condicionales </h4>")
      (display "<h4 class=\"simbolo\"> Naranja: Simbolos </h4>")
      (display "<h4 class=\"palabra\"> Verde: Palabras reservadas </h4>")
      (display "<h2> *****************</h2>")
      

      (for-each (lambda (linea)
                  (let ((linea-resaltada
                         (string-join
                          (map (lambda (palabra)

                                 ;Loops--
                                 (cond ((string=? palabra "for") (string-append "<span class=\"ciclo\">" palabra "</span>"))
                                 ((string=? palabra "while") (string-append "<span class=\"ciclo\">" palabra "</span>"))

                                 ;Condicionales--
                                       ((string=? palabra "if") (string-append "<span class=\"condicional\">" palabra "</span>")) ;condicional
                                       ((string=? palabra "else") (string-append "<span class=\"condicional\">" palabra "</span>")) ;condicional
                                       ((string=? palabra "case") (string-append "<span class=\"condicional\">" palabra "</span>")) ;condicional

                                 ;Operadores (Aritmetico y booleano)----
                                       ((string=? palabra "and") (string-append "<span class=\"operador\">" palabra "</span>")) ;operador
                                       ((string=? palabra "or") (string-append "<span class=\"operador\">" palabra "</span>")) ;operador
                                       ((string=? palabra "+") (string-append "<span class=\"operador\">" palabra "</span>")) ;operador
                                       ((string=? palabra "-") (string-append "<span class=\"operador\">" palabra "</span>")) ;operador
                                       ((string=? palabra "*") (string-append "<span class=\"operador\">" palabra "</span>")) ;operador
                                       ((string=? palabra "/") (string-append "<span class=\"operador\">" palabra "</span>")) ;operador
                                       ((string=? palabra ">") (string-append "<span class=\"operador\">" palabra "</span>")) ;operador
                                       ((string=? palabra "<") (string-append "<span class=\"operador\">" palabra "</span>")) ;operador
                                       ((string=? palabra ">=") (string-append "<span class=\"operador\">" palabra "</span>")) ;operador
                                       ((string=? palabra "<=") (string-append "<span class=\"operador\">" palabra "</span>")) ;operador
                                 ;Simbolos---
                                       ((string=? palabra "=") (string-append "<span class=\"simbolo\">" palabra "</span>")) ;simbolo
                                       ((string=? palabra "(") (string-append "<span class=\"simbolo\">" palabra "</span>")) ;simbolo
                                       ((string=? palabra ")") (string-append "<span class=\"simbolo\">" palabra "</span>")) ;simbolo
                                       ((string=? palabra "'") (string-append "<span class=\"simbolo\">" palabra "</span>")) ;simbolo
                                       ((string=? palabra "\"") (string-append "<span class=\"simbolo\">" palabra "</span>")) ;simbolo
                                       ((string=? palabra "[") (string-append "<span class=\"simbolo\">" palabra "</span>")) ;simbolo
                                       ((string=? palabra "]") (string-append "<span class=\"simbolo\">" palabra "</span>")) ;simbolo
                                       ((string=? palabra "{") (string-append "<span class=\"simbolo\">" palabra "</span>")) ;simbolo
                                       ((string=? palabra "}") (string-append "<span class=\"simbolo\">" palabra "</span>")) ;simbolo
                                       ((string=? palabra ";") (string-append "<span class=\"simbolo\">" palabra "</span>")) ;simbolo
                                       ((string=? palabra ",") (string-append "<span class=\"simbolo\">" palabra "</span>")) ;simbolo
                                       ((string=? palabra "|") (string-append "<span class=\"simbolo\">" palabra "</span>")) ;simbolo





                                 ;Palabras reservadas---
                                       ((string=? palabra "list") (string-append "<span class=\"palabra\">" palabra "</span>")) ;palabra
                                       ((string=? palabra "string") (string-append "<span class=\"palabra\">" palabra "</span>")) ;palabra
                                       ((string=? palabra "break") (string-append "<span class=\"palabra\">" palabra "</span>")) ;palabra
                                       ((string=? palabra "double") (string-append "<span class=\"palabra\">" palabra "</span>")) ;palabra
                                       ((string=? palabra "char") (string-append "<span class=\"palabra\">" palabra "</span>")) ;palabra
                                       ((string=? palabra "lambda") (string-append "<span class=\"palabra\">" palabra "</span>")) ;palabra


                                 ;Palabras que no deben ser remarcadas---

                                       (else palabra)))
                               (string-split linea " "))
                          " ")))
                    (display (string-append "<p>" linea-resaltada "</p>\n"))))
                lineas)
      (display "</body>\n</html>\n"))))

(with-output-to-file "estilo.css" ;cambiar el nombre en cada uso, O(1)
  (lambda ()
    (display ".ciclo { color: blue; }\n")
    (display ".condicional { color: purple; }\n")
    (display ".operador { color: red; }\n")
    (display ".simbolo { color: orange; }\n")
    (display ".palabra { color: green; }\n")))




(define (txt-a-html archivo-txt archivo-html)
  (escribir-html archivo-html (leer-lineas archivo-txt)))

(txt-a-html doc "result.html") ;Cambiar el nombre cada vez que se usa



;Calculos de tiempo usando la función time

(time(identificar-codigo codigo))
(time(analisis codigo))

;Metodo HTML
(time (leer-lineas doc))
(time(txt-a-html doc "tiempo.html"))
