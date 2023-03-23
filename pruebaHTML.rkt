#lang racket
;Tomar el input del documento, guardarlo en variable ---
(display "Ingresa el nombre del documento (incluye '.txt'): ")
(define doc (read-line))
(display "Leyendo el documento: ")
(displayln doc)

(define (leer-lineas archivo)
  (with-input-from-file archivo
    (lambda ()
      (let loop ((lineas '())
                 (line (read-line)))
        (cond ((eof-object? line) (reverse lineas))
              (else (loop (cons line lineas) (read-line))))))))


(define (escribir-html archivo lineas)
  (with-output-to-file archivo
    (lambda ()
      (display "<html>\n<body>\n")
      (for-each (lambda (linea)
                  (display (string-append "<p>" linea "</p>\n")))
                lineas)
      (display "</body>\n</html>\n"))))


(define (txt-a-html archivo-txt archivo-html)
  (escribir-html archivo-html (leer-lineas archivo-txt)))


(txt-a-html doc "resultado.html")



