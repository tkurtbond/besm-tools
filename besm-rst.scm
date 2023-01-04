;;;!> besm-rst.scm -- Convert a YAML BESM 4E character or template into reST.

;;; Design Decisions:
;;;
;;; - All mapping keys are lowercase.
;;; - Details is a string, not a list of strings.
;;; - enhancers and limiters are lists, generally of strings, but of
;;;   lists of [name, counts-as] for those that count as more than one
;;;   assigment.
;;; - At this time I don't plan to calculate the points values of
;;;   anything.  The person entering the YAML version of the template
;;;   or character has to add up and enter the points themselves.
;;;   This way we don't have to do any calculations.  Enhancer and
;;;   limiters that count as more than one get entered (if the person
;;;   is interested) so the program can display the "Enhancer -X" or
;;;   "Limiter +X" if desired.  Still no calculation of points.
;;; - Haven't decided yet if weapons or armour should be entered as
;;;   templates, but I'm leaning towards NOT.
;;; - Items (like the Hazmat Suit, p. 212) probably should definitely
;;;   be enterable as templates.
;;; - Derived Values are in a section with the mapping key "derived".

(module besm-rst ()

(import (scheme))

(import (chicken base))
(import (chicken io))
(import (chicken port))
(import (chicken process-context))
(import (chicken sort))
(import (chicken string))
(import (chicken pretty-print))

(import args)
(import bindings)
(import fmt)
(import loop)
(import matchable)
(import (srfi 1))
(import (srfi 152))
(import yaml)

(define-syntax dbg
  (syntax-rules ()
    ((_ e1 e2 ...)
     (when *debugging*
       e1 e2 ...
       (flush-output (current-error-port))))))

(define (dfmt . args)
  (apply fmt (cons (current-error-port) args)))

;; (put 'when-in-alist 'scheme-indent-function 1)
(define-syntax when-in-alist
  (syntax-rules ()
    ((_ (var key alist) b1 ...)
     (let ((val (assoc key alist)))
       (when val
	 (let ((var (cdr val)))
	   b1 ...))))))

(define (bold s)
  (cond
   ((string-null? s) s)
   (else (fmt #f "**" s "**"))))

(define (italicize s)
  (cond
   ((string-null? s) s)
   (else (fmt #f "*" s "*"))))

(define (table-bold s)
  (if *bolding*
      (bold s)
      s))

(define (separator-line num-columns sep c)
  ;; The last column is the description, and absorbs any unused space.
  (loop for i from 1 to (- num-columns 1)
        do (fmt #t (pad-char c sep (pad *num-width*))))
  (fmt #t (pad-char c sep
                    (pad (- *table-width* 1 1 (* (- num-columns 1)
                                                 (+ *num-width* 1)))) sep)
       nl))

(define (sep1)
  (separator-line 1 #\+ #\-))

(define (empty)
  (fmt #t (with-width *table-width* (columnar "|" (dsp " ") "|"))))

(define (row2 col1 col2)
  (dbg (dfmt "row2: col1: " (wrt col1) " col2: " (wrt col2) nl))
  (fmt #t (with-width *table-width*
                      (columnar "|" *num-width* (dsp col1)
                                "|" (wrap-lines col2)
                                "|"))))

(define (headsep2)
  (separator-line 2 #\+ *head-sep*))
(define (sep2)
  (separator-line 2 #\+ #\-))

(define (row3 col1 col2 col3)
  ;; (dbg (dfmt "row3: col1: " (wrt col1) " col2: " (wrt col2) " col3: "
  ;;            (wrt col3) nl))
  (fmt #t (with-width *table-width*
                      (columnar "|" *num-width* (dsp col1)
                                "|" *num-width* (dsp col2)
                                "|" (wrap-lines (dsp col3))
                                "|"))))
(define (headsep3)
  (separator-line 3 #\+ *head-sep*))
(define (sep3)
  (separator-line 3 #\+ #\-))


(define (must-exist item alist)
  (cdr (assoc item alist)))

(define (may-exist item alist)
  (let ((result (assoc item alist)))
    (if result
        (cdr result)
        result)))

(define (process-stat stat)
  (dbg (dfmt "process-stat: " (pretty stat)))
  (let ((name   (must-exist "name" stat))
        (value  (must-exist "value" stat))
        (points (must-exist "points" stat)))
    (row3 value points name)
    points))

(define (process-derived derived)
  (dbg (dfmt "process-derived: " (pretty derived)))
  ;; There are no points.
  (let* ((name          (must-exist "name" derived))
         (value         (must-exist "value" derived))
         (alternatives  (may-exist "alternatives" derived))
         (description   (if alternatives
                           (string-append name " ("
                                          (string-join alternatives ", ")
                                          ")")
                           name)))
    (row2 value description)))

(define (format-customizers items type)
  (loop for item in items
        collect (match item
                  [(? string? s)
                   (fmt #f (dsp s) ": " (if (eq? type 'enhancer) "-1" "+1"))]
                  [((? string? name) (? number? counts-as))
                   (fmt #f (dsp name) ": " (if (eq? type 'enhancer) "-" "+")
                        (dsp counts-as))]
                  [_ (error 'format-customizers
                            "do not understand customizer" item)])))

(define (make-attribute-details details enhancers limiters)
  (dbg (dfmt "make-attribute-details: details: " details
             " enhancers: " enhancers " limiters: " limiters))
  (let* ((details   (if details   (list (string-trim-both details)) '()))
         (enhancers (if enhancers (format-customizers enhancers 'enhancer) '()))
         (limiters  (if limiters  (format-customizers limiters 'limiter) '()))
         (partial   (append '() enhancers limiters))
         (sorted    (sort partial string-ci<?))
         (joined    (if (null? sorted)
                        '()
                        (list (string-join sorted ", "))))
         (result    (append joined details)))
    (dbg (dfmt "enhancers: " (pretty enhancers))
         (dfmt "limiters: " (pretty limiters))
         (dfmt "partial: " (pretty partial))
         (dfmt "sorted: " (pretty sorted))
         (dfmt "result: " (pretty result)))
    (if (every null? result)
        #f
        (string-join result "; "))))

  
(define (process-attribute attribute)
  (dbg (dfmt "process-attribute: " (pretty attribute)))
  ;; returns the cost of the attribute
  (let* ((name        (must-exist "name" attribute))
         (level       (must-exist "level" attribute))
         (points      (must-exist "points" attribute))
         (details     (may-exist  "details" attribute))
         (effective   (may-exist  "effective" attribute))
         (level       (if effective (fmt #f level "(" effective ")") level))
         ;; TODO: enhancers and limiters
         (enhancers   (may-exist  "enhancers" attribute))
         (limiters    (may-exist  "limiters" attribute))
         (details     (make-attribute-details details enhancers limiters))
         (description (if details (fmt #f name " (" details ")") name)))
    (row3 level points description)
    points))

(define (process-defect defect)
  (dbg (dfmt "process-defect: " (pretty defect)))
  ;; returns the cost of the defect
  (let* ((name        (must-exist "name" defect))
         (rank        (must-exist "rank" defect))
         (points      (must-exist "points" defect))
         (details     (may-exist  "details" defect))
         (description (if details (fmt #f name " (" details ")") name))
         )
    (dbg (dfmt "process-defect: before row3" nl))
    (row3 rank points description)
    (dbg (dfmt "process-defect: after row3" nl))
    points))

(define (process-entity entity)
  (dbg (dfmt "process-entity: " (pretty entity)))
  ;; It might be a template, an item, or a full character.
  (let ((stats-total 0)
        (attributes-total 0)
        (defects-total 0)
        (entity-total 0))

    (when-in-alist (entity-name "name" entity)
      (let ((underline (make-string (string-length entity-name) *underliner*)))
        (fmt #t entity-name nl underline nl nl)))

    (when-in-alist (tagline "tagline" entity)
      (fmt #t (italicize (string-trim-both tagline)) nl nl))

    (when-in-alist (description "description" entity)
      (fmt #t description nl nl))
  
    (when-in-alist (size "size" entity)
      (fmt #t (bold "Size:") " " size nl nl))

    (when-in-alist (stats "stats" entity)
      (sep3)
      (row3 (table-bold "VALUE") (table-bold "POINTS") (table-bold "STAT"))
      (headsep3)
      (set! stats-total
        (loop for stat in stats sum (process-stat stat) do (sep3)))
      (when *show-subtotals*
        (row3 "" (table-bold (number->string stats-total))
              (table-bold "STATS TOTAL"))
        (sep3))
      (cond (*one-table* (empty))
            (else (fmt #t nl))))

    (when-in-alist (derived "derived" entity)
      (sep2)
      (row2 (table-bold "VALUE") (table-bold "DERIVED VALUE"))
      (headsep2)
      (loop for d in derived do (process-derived d) do (sep2))
      (cond (*one-table* (empty))
            (else (fmt #t nl))))

    (when-in-alist (attributes "attributes" entity)
      (sep3)
      (row3 (table-bold "LEVEL") (table-bold "POINTS") (table-bold "ATTRIBUTE"))
      (headsep3)
      (set! attributes-total
        (loop for attribute in attributes sum (process-attribute attribute)
              do (sep3)))
      (when *show-subtotals*
        (row3 "" (table-bold (number->string attributes-total))
              (table-bold "ATTRIBUTES TOTAL"))
        (sep3))
      (cond (*one-table* (empty))
            (else (fmt #t nl))))

    (when-in-alist (defects "defects" entity)
      (sep3)
      (row3 (table-bold "RANK") (table-bold "POINTS") (table-bold "DEFECT"))
      (headsep3)
      (set! defects-total 
        (loop for defect in defects sum (process-defect defect) do (sep3)))
      (when *show-subtotals*
        (row3 "" (table-bold (number->string defects-total))
              (table-bold "DEFECTS TOTAL"))
        (sep3))
      (cond (*one-table* (empty))
            (else (fmt #t nl))))

    ;; Output total.
    (sep3)
    (set! entity-total (+ stats-total attributes-total defects-total))
    (row3 "" (table-bold (number->string entity-total)) (table-bold "TOTAL"))
    (sep3)
    ))


(define (process-file)
  ;;; It is a file of possibly multiple entities.
  (let ((entities (yaml-load (current-input-port))))
    (loop for entity in entities do (process-entity entity))))

(define (process-file-terse)
  (let ((y (yaml-load (current-input-port))))
    (pp y)
    (newline)))

(define (process-filename filename)
  (with-input-from-file filename *output-formatter*))



(define (usage)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print "Usage: " (program-name) " [options...] [files...]")
      (newline)
      (print (args:usage *command-line-options*))
      (newline)
      (print
       "Note: use -1 (or --one) if you are generating this for HTML output,
as that looks better.")
      (newline)
      (fmt #t "Current argv: " (argv) nl)))
  (exit 1))

(define *bolding* #t)
(define *num-width* (max
                     (string-length "LEVEL")
                     (string-length "VALUE")
                     (string-length "POINTS")
                     ))
(define *debugging* #f)
(define *head-sep* #\=)
(define *one-table* #f)
(define *output-formatter* process-file)
(define *show-subtotals* #f)
(define *table-width* 60)
(define *underliner* #\-)

(define *command-line-options*
  (list (args:make-option
         (b bold) #:none "Turn OFF bolding of headers."
         (set! *bolding* #f))
        (args:make-option
         (d debug) #:none "Turn on debugging."
         (set! *debugging* #t))
        (args:make-option
         (h help) #:none "Display this text."
         (usage))
        (args:make-option
         (|1| one) #:none "Use only one table."
         (dbg (dfmt "one only" nl))
         (set! *one-table* #t)
         ;; Having multiple header separator lines doesn't cause pandoc to
         ;; complain, but it does make the first line a header which looks
         ;; different in HTML.
         (set! *head-sep* #\-))
        (args:make-option
         (s subtotals) #:none
         "Show subtotals for stats, attributes, and defects."
         (set! *show-subtotals* #t))
        (args:make-option
         (t terse) #:none "Use terse output."
         (set! *output-formatter* process-file-terse))
        (args:make-option
         (u underliner) #:required "Character to use for underlining the header."
         (set! *underliner* (string-ref arg 0)))
        (args:make-option
         (w width)
         (required: "NUMBER") "Width of table in characters"
         (set! *table-width* (string->number arg)))))

(receive (options operands) (args:parse (command-line-arguments)
                                        *command-line-options*)
  ;; This outputs reST, so bolding is two asterisks on each side.
  (set! *num-width* (+ *num-width* 4))

  (if  (zero? (length operands))
       (with-input-from-port (current-input-port) process-file)
       (loop for filename in operands do (process-filename filename))))
)
