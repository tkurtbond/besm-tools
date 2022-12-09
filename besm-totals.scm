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

(define total-width 40)
(define num-width 4)

(define infile "FV2021-Coleopteran.dat")

(define (sep c)
  (fmt #t (with-width total-width
                      (pad-char c
                                (columnar "+" num-width (pad num-width (dsp ""))
                                          "+" num-width (pad num-width (dsp ""))
                                          "+" num-width (pad num-width (dsp ""))
                                          "+" (pad (- total-width 1
                                                      num-width 1
                                                      num-width 1
                                                      num-width 1
                                                      1 1)
                                                   (dsp ""))
                                          "+")))))

(define (header lvl eff cst attribute)
  (fmt #t (with-width total-width
                      (columnar "|" num-width (dsp lvl)
                                "|" num-width (dsp eff)
                                "|" num-width (dsp cst)
                                "|" (wrap-lines attribute)
                                "|"))))

(define (row lvl eff cst attribute)
  (fmt #t (with-width total-width
                      (columnar "|" num-width (if lvl (num lvl) (dsp ""))
                                "|" num-width (if eff (num eff) (dsp ""))
                                "|" num-width (num cst)
                                "|" (wrap-lines attribute)
                                "|"))))

(define (empty)
  (fmt #t (dsp "|") (pad (- total-width 2) (dsp "")) (dsp "|")))

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

(define (translate-lines lines)
  (loop for line in lines
        collect (bind (lvl eff cst text) line
                  (list (num-or-false lvl)
                        (num-or-false eff)
                        (num-or-false cst)
                        (string-trim-both text)))))

(define (sum-costs attributes)
  (loop for att in attributes sum (caddr att)))
        

(define (process-file)
  (let* ((attributes (translate-lines (split-lines (process-section))))
         (attribute-cost (sum-costs attributes))
         (defects (translate-lines (split-lines (process-section))))
         (defects-cost (sum-costs defects)))
    ;; (fmt #t "attributes: " attributes nl "defects: " defects nl)
    (unless (null? attributes)
      (sep #\-)
      (header "Lvl" "Eff" "Cst" "Attribute")
      (sep #\=)
      (loop for att in attributes
            do (begin
                 (apply row att)
                 (sep #\-)))
      (row #f #f attribute-cost "**Attribute Total**")
      (sep #\-))
    (fmt #t nl)
    (unless (null? defects)
      (sep #\-)
      (header "Rnk" "" "Cst" "Defect")
      (sep #\=)
      (loop for def in defects
            do (begin
                 (apply row def)
                 (sep #\-)))
      (row #f #f defects-cost "**Defect Total**")
      (sep #\-))))

(define (process-filename filename)
  (with-input-from-file filename process-file))

(define opts
  (list (args:make-option
         (d debug)
         #:none "Turn on debugging."
         (set! debugging #f))))

(receive (options operands) (args:parse (command-line-arguments) opts)
  (if  (= 0 (length operands))
       (with-input-from-port (current-input-port) process-file)
       (loop for filename in operands do (process-filename filename))))
)
