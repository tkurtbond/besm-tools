(module besm-totals ()
  
(import (scheme))

(import (chicken base))
(import (chicken io))
(import (chicken port))
(import (chicken process-context))
(import (chicken string))

(import (args))
(import (bindings))
(import (fmt))
(import (loop))
(import (srfi 13))

(define infile "FV2021-Coleopteran.dat")

(define (bold s)
  (cond
   ((= 0 (string-length s)) s)
   (*bolding* (fmt #f "**" s "**"))
   (else s)))

(define (sep c)
  (fmt #t (with-width *table-width*
                      (pad-char c
                                (columnar "+" *num-width* (pad *num-width* (dsp ""))
                                          "+" *num-width* (pad *num-width* (dsp ""))
                                          "+" *num-width* (pad *num-width* (dsp ""))
                                          "+" (pad (- *table-width* 1
                                                      *num-width* 1
                                                      *num-width* 1
                                                      *num-width* 1
                                                      1 1)
                                                   (dsp ""))
                                          "+")))))

(define (sep c)
  (fmt #t (pad-char c
                    "+" (pad *num-width* "")
                    "+" (pad *num-width* "")
                    "+" (pad *num-width* "")
                    "+" (pad (- *table-width* 1
                                *num-width* 1
                                *num-width* 1
                                *num-width* 1
                                1)
                             (dsp ""))
                    "+"
                    nl)))

(define (header lvl eff cst attribute)
  (fmt #t (with-width *table-width*
                      (columnar "|" *num-width* (dsp lvl)
                                "|" *num-width* (dsp eff)
                                "|" *num-width* (dsp cst)
                                "|" (wrap-lines attribute)
                                "|"))))

(define (row lvl eff cst attribute)
  (fmt #t (with-width *table-width*
                      (columnar "|" *num-width* (if lvl (num lvl) (dsp ""))
                                "|" *num-width* (if eff (num eff) (dsp ""))
                                "|" *num-width* (num cst)
                                "|" (wrap-lines attribute)
                                "|"))))

(define (empty)
  (fmt #t (dsp "|") "  " (pad (- *table-width* 4) (dsp "")) (dsp "|") nl))

(define (rl)
  (let ((line (read-line)))
    (if (eof-object? line)
        line
        (string-trim-both line))))

(define (process-section)
  ;; Accumulate lines until we come to eof or a blank line.
  (let loop ((line (rl))
             (lines '()))
    (fmt #f "line: " line)
    (cond
     ((or (eof-object? line) (= (string-length line) 0))
      (reverse lines))
     (else
      (loop (rl) (cons line lines))))))

(define (split-lines lines)
  (loop for line in lines
        collect (string-split line "|" #t)))

(define (num-or-false s)
  (string->number (string-trim-both s)))

(define-syntax dbg
  (syntax-rules ()
    ((_ e1 e2 ...)
     (when *debugging*
       e1 e2 ...
       (flush-output (current-error-port))))))

(define (translate-lines lines)
  (loop for line in lines
        collect (bind (lvl eff cst text) line
                  (dbg (fmt (current-error-port) "translate-lines: line: "
                       line nl))
                  (list (num-or-false lvl)
                        (num-or-false eff)
                        (num-or-false cst)
                        (string-trim-both text)))))

(define (sum-costs attributes)
  (loop for att in attributes sum (caddr att)))
        
(define (process-file)
  (let* ((title (rl))
         (_ (rl))
         (attributes (translate-lines (split-lines (process-section))))
         (attributes-cost (sum-costs attributes))
         (defects (translate-lines (split-lines (process-section))))
         (defects-cost (sum-costs defects))
         (total-cost (+ attributes-cost defects-cost)))
    (dbg (fmt (current-error-port) "process-file: title: " title nl))
    (fmt #t title nl)
    (fmt #t (make-string (string-length title) *underline*) nl)
    (fmt #t nl)
    (unless (null? attributes)
      (sep #\-)
      (header (bold "Lvl") (bold "Eff") (bold "Cst") (bold "Attribute"))
      (sep #\=)
      (loop for att in attributes
            do (begin
                 (apply row att)
                 (sep #\-)))
      (row #f #f attributes-cost (bold "Attribute Total"))
      (sep #\-))
    (fmt #t nl)
    (unless (null? defects)
      (sep #\-)
      (header (bold "Rnk") "" (bold "Cst") (bold "Defect"))
      (sep #\=)
      (loop for def in defects
            do (begin
                 (apply row def)
                 (sep #\-)))
      (row #f #f defects-cost (bold "Defect Total"))
      (sep #\-))
    (unless (and (null? attributes) (null? defects))
      (fmt #t nl)
      (sep #\-)
      (row #f #f total-cost (bold "Total"))
      (sep #\-))))

(define (process-file-only-one)
  (let* ((title (rl))
         (_ (rl))
         (attributes (translate-lines (split-lines (process-section))))
         (attributes-cost (sum-costs attributes))
         (defects (translate-lines (split-lines (process-section))))
         (defects-cost (sum-costs defects))
         (total-cost (+ attributes-cost defects-cost)))
    (dbg (fmt (current-error-port) "process-file-only-one: title: " title nl))
    (fmt #t title nl)
    (fmt #t (make-string (string-length title) *underline*) nl)
    (fmt #t nl)
    (unless (null? attributes)
      (sep #\-)
      (header (bold "Lvl") (bold "Eff") (bold "Cst") (bold "Attribute"))
      (sep #\=)
      (loop for att in attributes
            do (begin
                 (apply row att)
                 (sep #\-)))
      (row #f #f attributes-cost (bold "Attribute Total"))
      (sep #\-))
    (unless (null? defects)
      (empty)
      (sep #\-)
      (header (bold "Rnk") "" (bold "Cst") (bold "Defect"))
      (sep #\=)
      (loop for def in defects
            do (begin
                 (apply row def)
                 (sep #\-)))
      (row #f #f defects-cost (bold "Defect Total"))
      (sep #\-))
    (unless (and (null? attributes) (null? defects))
      (empty)
      (sep #\-)
      (row #f #f total-cost (bold "Total"))
      (sep #\-))
    ))

(define (process-filename filename)
  (with-input-from-file filename (if *only-one*
                                     process-file-only-one
                                     process-file)))

(define (usage)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print "Usage: " (program-name) " [options...] [files...]")
      (newline)
      (print (args:usage opts))
      (fmt #t "Current argv: " (argv) nl)))
  (exit 1))

(define *bolding* #f)
(define *num-width* 3)
(define *debugging* #f)
(define *table-width* 60)
(define *only-one* #f)
(define *underline* #\-)

(define opts
  (list (args:make-option
         (b bold) #:none "Turn on bolding of headers."
         (set! *bolding* #t)
         (set! *num-width* 7))
        (args:make-option
         (d debug) #:none "Turn on debugging."
         (set! *debugging* #t))
        (args:make-option
         (O one) #:none "Use only one table."
         (set! *only-one* #t))
        (args:make-option
         (u underline) #:required "Set character to use for underlining the header."
         (set! *underline* (string-ref arg 0)))        (args:make-option
         (w width)
         (required: "NUMBER") "Width of table in characters"
         (set! *table-width* (string->number arg)))))

(receive (options operands) (args:parse (command-line-arguments) opts)
  (if  (= 0 (length operands))
       (with-input-from-port (current-input-port) (if *only-one*
                                                      process-file-only-one
                                                      process-file))
       (loop for filename in operands do (process-filename filename))))
)
