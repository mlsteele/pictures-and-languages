;;; SVG Render Backend
;;;
;;; Tools for rendering a Uniform Represention to SVG.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SVG Generator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define svg:xml-header "<?xml version=\"1.0\"?>")
(define svg:doctype "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">")
(define (svg:tag entries)
  ;; entries is a list of strings
  (apply string-append
    `("<svg height=\"210\" width=\"500\">\n"
      ,@entries
      "</svg>")))

(define (svg:document entries)
  ;; entries is a list of strings
  (ensure (list? entries) "entries must be a list")
  (string-append
    svg:xml-header "\n"
    svg:doctype "\n"
    (svg:tag entries)))

(define (svg:line x1 y1 x2 y2)
  (let ((x1 (number->string x1))
        (y1 (number->string y1))
        (x2 (number->string x2))
        (y2 (number->string y2)))
    (string-append
        "<line x1=\"" x1 "\" y1=\"" y1
        "\" x2=\"" x2 "\" y2=\"" y2
        "\" style=\"stroke:rgb(0,0,0);stroke-width:1\" />\n")))

#| Test Cases
(define s
  (svg:document (list
    (svg:line 0 0 200 200)
    (svg:line 0 0 100 200))))
(display s) ; svg string
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Uniform Representation Converter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Convert a uniform rep to an SVG string.
(define (ur->svg ur)
  (svg:document
    (map (lambda (ele)
      (case (car ele)
        ((line) (apply svg:line (cdr ele)))
        ;; color not implemented
        ((color) "")
        ;; point not implemented
        ((point) "")
        (else (error "ur->svg does not recognize ur element" ele))))
      (ur-fit-for-svg ur 500 210))))

(define (ur->svg-file u filename)
  (let ((s (ur->svg u)))
    (ensure (string? s) "s had better be a string to write")
    (call-with-output-file "out.svg"
      (lambda (f)
        (write-string s f)))))
