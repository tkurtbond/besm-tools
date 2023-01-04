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

(define-syntax dbg
  (syntax-rules ()
    ((_ e1 e2 ...)
     (when *debugging*
       e1 e2 ...
       (flush-output (current-error-port))))))

(define (dfmt . args)
  (apply fmt (cons (current-error-port) args)))

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

(define (row i lvl eff cst attribute . details)
  (dbg (dfmt "details: " (pretty details)))
  (fmt #t (with-width *table-width*
                      (columnar "|" *num-width* (if lvl (num lvl) (dsp ""))
                                "|" *num-width* (if eff (num eff) (dsp ""))
                                "|" *num-width* (num cst)
                                "|" (wrap-lines
                                     attribute
                                     (if (and (not (null? details))
                                              (not (zero? (string-length (car details)))))
                                         (string-append ".  " (car details) ".  ")
                                         ""))
                                "|"))))

(define (row-terse i lvl eff cst attribute . details)
  (fmt #t (if (> i 1) ", " "")
       attribute " – " (num lvl) (if eff (fmt #f "(" (num eff) ")") "")
       " (" (if (and (not (null? details))
                     (not (zero? (string-length (car details)))))
                (string-append (car details) ".  ")
                "")
       (num cst) " CP)"))

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
     ((or (eof-object? line) (zero? (string-length line)))
      (reverse lines))
     (else
      (loop (rl) (cons line lines))))))

(define (split-lines lines)
  (dbg (dfmt "split-lines: lines: " (pretty lines) nl))
  (loop for line in lines
        for i from 1
        do (dbg (dfmt "i: " (num i) " line: " line nl))
        collect (string-split line "|" #t)))

(define (num-or-false s)
  (string->number (string-trim-both s)))

(define (translate-lines lines)
  (dbg (dfmt "translate-lines: lines: " (pretty lines) nl))
  (loop for line in lines
        collect (bind (lvl eff cst att . details) line
                  (dbg (dfmt "translate-lines: line: "
                       line nl))
                  (list (num-or-false lvl)
                        (num-or-false eff)
                        (num-or-false cst)
                        (string-trim-both att)
                        (if (null? details)
                            ""
                            (string-trim-both (car details)))))))

(define (sum-costs attributes)
  (loop for att in attributes sum (caddr att)))
        
(define (process-file)
  (dbg (dfmt "process-file entered" nl))
  (let* ((title (rl))
         (_ (rl))
         (attributes (translate-lines (split-lines (process-section))))
         (attributes-cost (sum-costs attributes))
         (defects (translate-lines (split-lines (process-section))))
         (defects-cost (sum-costs defects))
         (total-cost (+ attributes-cost defects-cost)))
    (dbg (dfmt "process-file: title: " title nl))
    (fmt #t title nl)
    (fmt #t (make-string (string-length title) *underline*) nl)
    (fmt #t nl)
    (unless (null? attributes)
      (dbg (dfmt "process-file: attributes" nl))
      (sep #\-)
      (header (bold "Lvl") (bold "Eff") (bold "Cst") (bold "Attribute"))
      (sep #\=)
      (loop for att in attributes
            for i from 1
            do (begin
                 (apply row (cons i att))
                 (sep #\-)))
      (row 1 #f #f attributes-cost (bold "Attribute Total"))
      (sep #\-))
    (fmt #t nl)
    (unless (null? defects)
      (dbg (dfmt "process-file: defects" nl))
      (sep #\-)
      (header (bold "Rnk") "" (bold "Cst") (bold "Defect"))
      (sep #\=)
      (loop for def in defects
            for i from 1 
            do (begin
                 (apply row (cons i def))
                 (sep #\-)))
      (row 1 #f #f defects-cost (bold "Defect Total"))
      (sep #\-))
    (unless (and (null? attributes) (null? defects))
      (fmt #t nl)
      (sep #\-)
      (row 1 #f #f total-cost (bold "Total"))
      (sep #\-))))

(define (process-file-terse)
  (dbg (dfmt "process-file-terse entered" nl))
  (let* ((title (rl))
         (_ (rl))
         (attributes (translate-lines (split-lines (process-section))))
         (attributes-cost (sum-costs attributes))
         (defects (translate-lines (split-lines (process-section))))
         (defects-cost (sum-costs defects))
         (total-cost (+ attributes-cost defects-cost)))
    (dbg (dfmt "process-file-terse: title: " title nl))
    (fmt #t title nl)
    (fmt #t (make-string (string-length title) *underline*) nl)
    (fmt #t nl)
    (unless (null? attributes)
      (dbg (dfmt "process-file: attributes" nl))
      (fmt #t (bold "Attributes") " (" (num attributes-cost) ") – ")
      (loop for att in attributes
            for i from 1
            do (apply row-terse (cons i att)))
      (fmt #t nl))
    (unless (null? defects)
      (dbg (dfmt "process-file: defects" nl))
      (fmt #t nl)
      (fmt #t (bold "Defects") " (" (num defects-cost) ") – ")
      (loop for def in defects
            for i from 1 
            do (apply row-terse (cons i def)))
      (fmt #t nl))
    (unless (and (null? attributes) (null? defects))
      (fmt #t nl)
      (fmt #t (bold "Total Cost:") " " (num total-cost) nl))))

(define (process-file-only-one)
  (dbg (dfmt "process-file-terse-only-one entered" nl))
  (let* ((title (rl))
         (_ (rl))
         (attributes (translate-lines (split-lines (process-section))))
         (attributes-cost (sum-costs attributes))
         (defects (translate-lines (split-lines (process-section))))
         (defects-cost (sum-costs defects))
         (total-cost (+ attributes-cost defects-cost)))
    (dbg (dfmt "process-file-only-one: title: " title nl))
    (fmt #t title nl)
    (fmt #t (make-string (string-length title) *underline*) nl)
    (fmt #t nl)
    (unless (null? attributes)
      (sep #\-)
      (header (bold "Lvl") (bold "Eff") (bold "Cst") (bold "Attribute"))
      (sep #\=)
      (loop for att in attributes
            for i from 1
            do (begin
                 (apply row (cons i att))
                 (sep #\-)))
      (row 1 #f #f attributes-cost (bold "Attribute Total"))
      (sep #\-))
    (unless (null? defects)
      (empty)
      (sep #\-)
      (header (bold "Rnk") "" (bold "Cst") (bold "Defect"))
      (sep #\=)
      (loop for def in defects
            for i from 1
            do (begin
                 (apply row (cons i def))
                 (sep #\-)))
      (row 1 #f #f defects-cost (bold "Defect Total"))
      (sep #\-))
    (unless (and (null? attributes) (null? defects))
      (empty)
      (sep #\-)
      (row 1 #f #f total-cost (bold "Total"))
      (sep #\-))
    ))

(define (process-filename filename)
  (with-input-from-file filename *output-formatter*))

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
(define *output-formatter* process-file)
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
         (h help) #:none "Display this text."
         (usage))
        (args:make-option
         (|1| one) #:none "Use only one table."
         (dbg (dfmt "one only" nl))
         (set! *output-formatter* process-file-only-one))
        (args:make-option
         (t terse) #:none "Use terse output."
         (set! *output-formatter* process-file-terse))
        (args:make-option
         (u underline) #:required "Set character to use for underlining the header."
         (set! *underline* (string-ref arg 0)))        (args:make-option
         (w width)
         (required: "NUMBER") "Width of table in characters"
         (set! *table-width* (string->number arg)))))

(receive (options operands) (args:parse (command-line-arguments) opts)
  (dbg (dfmt "main *output-formatter*: " (pretty *output-formatter*) nl))
  (if  (= 0 (length operands))
       (with-input-from-port (current-input-port) *output-formatter*)
       (loop for filename in operands do (process-filename filename))))
)
