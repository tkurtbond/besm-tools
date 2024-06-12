;;;!> besm-rst.scm -- Convert a YAML BESM 4E character or template into reST.

;;; Design Decisions:
;;;
;;; - All mapping keys are lowercase.
;;;   
;;; - Defect points are NEGATIVE.
;;;
;;; - Things that this program has to interpret, like defect points,
;;;   use ASCII charater 45, Unicode hyphen-minus.  Things it doesn't,
;;;   like the contents of details, should use whichever is
;;;   appropriate in typeset text, like em and en dashes, Unicode
;;;   MINUS SIGN, Unicode MULTIPLICATION SIGN, etc.
;;;   
;;; - Details is a string, not a list of strings, and it is not a
;;;   complete sentence (and is used as part of something else), so do
;;;   NOT end it with a period.
;;;   
;;; - enhancements and limiters are lists, generally of strings, but
;;;   of lists of [name, counts-as] for those that count as more than
;;;   one assigment.
;;;   
;;; - Note that the Attack Helicopter, BESM 4E p. 217, shows
;;;   enhancements as negative and limiters as positive.
;;;   
;;; - At this time I don't plan to calculate the points values of
;;;   individual attributes, defects, or skills. The person entering
;;;   the YAML version of the template or character has to add up and
;;;   enter the points for those themselves. This way we don't have to
;;;   do any calculations on the individual items.  Enhancement and
;;;   limiters that count as more than one get entered (if the person
;;;   is interested) so the program can display the "Enhancement -X"
;;;   or "Limiter +X" if desired. Still no calculation of points.
;;;
;;; - Haven't decided yet if weapons or armour should be entered as
;;;   templates, but I'm leaning towards NOT.
;;;   
;;; - Items (like the Hazmat Suit, BESM 4E p. 212) probably should
;;;   definitely be enterable as templates.
;;;   
;;; - Derived Values are in a section with the mapping key "derived".
;;;   
;;; - TODO: Undecided as to whether the attributes, defects, and
;;;   skills should be sorted by the program.
;;;   
;;; - Specialisations are a list.
;;;   
;;; - TODO: Items have not been considered much.  Currently, Items are
;;;   recorded with the character entity as single (not nested) entry,
;;;   and then another entity is created, often in the same file for
;;;   something simple, like a hand-held weapon.

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
(import (schemepunk show))
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
  (apply show (cons (current-error-port) args)))

(define (die status . args)
  (show (current-error-port) (program-name) ": ")
  (apply show (cons (current-error-port) args))
  (show (current-error-port) "\n")
  (exit status))

;; (put 'when-in-alist 'scheme-indent-function 1)
(define-syntax when-in-alist
  (syntax-rules ()
    ((_ (var key alist) b1 ...)
     (let ((val (assoc key alist)))
       (when val
	 (let ((var (cdr val)))
	   b1 ...))))))


(define mecha? (make-parameter #f))

(define (label-points points)
  (show #f (abs points) (if (mecha?)
                            (if (< points 0) " MBP" " MP")
                            (if (< points 0) " BP" " CP"))))

(define (space-to-newline s)
  (string-map (lambda (c) (if (char=? c #\newline) #\space c)) s)) 

;; Always bold. A formatter that works with SRFI 166: Monadic
;; Formatting, which means it has to be used within the arguments to a
;; call to show.
(define (bold . args)
  (each "**" (each-in-list args) "**"))

(define (italics . args)
  (each "*" (each-in-list args) "*"))

;; Only bold if the command line option for bolding has been set. This
;; generally happens in headers in reST table output and in attribute
;; and defect names and levels in terse mode. This is a formatter that
;; works with SRFI 166: Monadic Formatting, which means it has to be
;; used within the arguments to a call to show.
(define (bolding . args)
  (if *bolding*
      (bold (each-in-list args))
      (each-in-list args)))

(define (italicizing . args)
  (if *italicizing*
      (italics (each-in-list args))
      (each-in-list args)))

(define (text . args)
  (show #f (each-in-list args)))

;;(define (italics s)
;;  (cond
;;   ((string-null? s) s)
;;   (else (show #f "*" s "*"))))

(define (separator-line num-columns sep c)
  ;; The last column is the description, and absorbs any unused space.
  (loop for i from 1 to (- num-columns 1)
        do (show #t  sep (with ((pad-char c)) (padded *num-width*))))
  (show #t (with ((pad-char c))
             sep (padded (- *table-width* 1 1 (* (- num-columns 1)
                                                 (+ *num-width* 1)))) sep)
       nl))

(define (sep1)
  (separator-line 1 #\+ #\-))

(define (empty)
  (show #t (with ((width *table-width*))
             (columnar "|" (displayed " ") "|"))))

(define (row2 col1 col2)
  (dbg (dfmt "row2: col1: " (written col1) " col2: " (written col2) nl))
  (show #t (with ((width *table-width*))
             (columnar "|" *num-width* (displayed col1)
                       "|" (wrapped col2)
                       "|"))))

(define (headsep2)
  (separator-line 2 #\+ *head-sep*))

(define (sep2)
  (separator-line 2 #\+ #\-))

(define (row3 col1 col2 col3)
  ;; (dbg (dfmt "row3: col1: " (wrt col1) " col2: " (wrt col2) " col3: "
  ;;            (wrt col3) nl))
  (show #t (with ((width *table-width*))
             (columnar "|" *num-width* (displayed col1)
                       "|" *num-width* (displayed col2)
                       "|" (wrapped (displayed col3))
                       "|"))))
(define (headsep3)
  (separator-line 3 #\+ *head-sep*))

(define (sep3)
  (separator-line 3 #\+ #\-))


(define (must-exist item alist)
  (let ((found (assoc item alist)))
    (if found
        (cdr found)
        (die 2 "Unable to find " (written item) " in " (written alist)))))

(define (may-exist item alist)
  (let ((result (assoc item alist)))
    (if result
        (cdr result)
        result)))

(define (process-stat stat)
  (dbg (dfmt "process-stat: " (pretty stat) nl))
  (let ((name   (must-exist "name" stat))
        (value  (must-exist "value" stat))
        (points (must-exist "points" stat)))
    (row3 value points name)
    points))

(define derived-abbreviations
  '(("ACV" . "Attack Combat Value")
    ("DCV" . "Defence Combat Value")
    ("DM"  . "Damage Multiplier")
    ("HP"  . "Health Points")
    ("EP"  . "Energy Points")
    ("SV"  . "Shock Value")
    ("AR"  . "Armour Rating")))

(define (process-derived derived)
  (dbg (dfmt "process-derived: " (pretty derived) nl))
  ;; There are no points.
  (let* ((name          (must-exist "name" derived))
         (expansion     (may-exist name derived-abbreviations))
         (name          (if expansion expansion name))
         (value         (must-exist "value" derived))
         (alternatives  (may-exist "alternatives" derived))
         (description   (if alternatives
                           (string-append name " ("
                                          (string-join alternatives ", ")
                                          ")")
                           name)))
    (row2 value description)))

(define (format-customizers items type)
  ;; Note that the Attack Helicopter, BESM 4E p. 217, shows enhancements
  ;; as negative and limiters as positive.
  ;;
  ;; When I display or print the PDFs on my Unix boxes, the spacing
  ;; between Unicode MINUS SIGN and the following number is off,
  ;; probably due to bugs in the rendering software.  The spacing is
  ;; fine on macOS.  So use Unicode HYPEN-MINUS, ASCII -, code 45, despite
  ;; it looking bad typographically.
  (loop for item in items
        collect (match item
                  [(? string? s)
                   (show #f (displayed s) " "
                         (if (eq? type 'enhancement) "-1" "+1"))]
                  [((? string? name) (? number? counts-as))
                   (show #f (displayed name) " "
                         (if (eq? type 'enhancement) "-" "+")
                         (displayed counts-as))]
                  [((? string? name) (? number? counts-as) (? string? applies-to))
                   (show #f (displayed name) ": " (displayed applies-to) " "
                         (if (eq? type 'enhancement) "-" "+")
                         (displayed counts-as))]
                  [((? string? name) (? number? counts-as) . applies-to)
                   (show #f (displayed name) ": "
                         (joined displayed applies-to ", ") " "
                         (if (eq? type 'enhancement) "-" "+")
                         (displayed counts-as))]
                  [_ (error 'format-customizers
                            "do not understand customizer" item)])))

(define (make-attribute-details details enhancements limiters elements)
  (dbg (dfmt "make-attribute-details: details: " details
             " enhancements: " enhancements " limiters: " limiters nl))
  (let* ((details      (if details (list (string-trim-both details)) '()))
         (elements     (if elements
                           (list (string-join (sort elements string-ci<?) ", "))
                           '()))
         (enhancements (if (and enhancements (pair? enhancements))
                           (format-customizers enhancements 'enhancement)
                           '()))
         (limiters     (if (and limiters (pair? limiters))
                           (format-customizers limiters 'limiter) '()))
         (partial      (append '() enhancements limiters))
         (sorted       (sort partial string-ci<?))
         (joined       (if (null? sorted)
                           '()
                           (list (string-join sorted ", "))))
         (result       (append elements joined details)))
    (dbg (dfmt "enhancements: " (pretty enhancements) nl)
         (dfmt "limiters: " (pretty limiters) nl)
         (dfmt "partial: " (pretty partial) nl)
         (dfmt "sorted: " (pretty sorted)nl )
         (dfmt "result: " (pretty result) nl))
    (if (every null? result)
        #f
        (string-join result "; "))))

  
(define (process-attribute attribute)
  (dbg (dfmt "process-attribute: " (pretty attribute) nl))
  ;; returns the cost of the attribute
  (let* ((name         (must-exist "name" attribute))
         (level        (may-exist "level" attribute))
         (level        (if level level ""))
         (points       (must-exist "points" attribute))
         (details      (may-exist  "details" attribute))
         (details      (if details (string-trim-both details) details))
         (effective    (may-exist  "effective" attribute))
         (level        (if effective (show #f level " (" effective ")") level))
         (enhancements (may-exist  "enhancements" attribute))
         (limiters     (may-exist  "limiters" attribute))
         (elements     (may-exist  "elements" attribute))
         (details      (make-attribute-details details enhancements limiters
                                               elements))
         (description (if details (show #f name " (" details ")") name)))
    (row3 level points description)
    points))

(define (process-defect defect)
  (dbg (dfmt "process-defect: " (pretty defect) nl))
  ;; Returns the cost of the defect
  (let* ((name        (must-exist "name" defect))
         (points      (must-exist "points" defect))
         (details     (may-exist  "details" defect))
         (details     (if details (string-trim-both details) details))
         (description (if details (show #f name " (" details ")") name))
         )
    (dbg (dfmt "process-defect: before row3" nl))
    (row2 points description)
    (dbg (dfmt "process-defect: after row3" nl))
    points))

(define (process-skill skill)
  (dbg (dfmt "process-skill: " (pretty skill) nl))
  ;; Returns the cost of the skill.
  (let* ((name            (must-exist "name" skill))
         (level           (must-exist "level" skill))
         (points          (must-exist "points" skill))
         (specialisations (may-exist "specialisations" skill))
         (description     (show #f name (if specialisations
                                            (string-append
                                             " ("
                                             (string-join specialisations ", ")
                                             ")")
                                            ""))))
    (row3 level points description)
    points))

(define (name-ci<? a b)
  (let ((a-name (must-exist "name" a))
        (b-name (must-exist "name" b)))
    (string-ci<? a-name b-name)))

(define (process-entity entity entity-no)
  (dbg (dfmt "process-entity: " (pretty entity) nl))
  ;; It might be a template, an item, or a full character.
  (let ((stats-total 0)
        (attributes-total 0)
        (defects-total 0)
        (skills-total 0)                ; Not added to entity total!
        (entity-total 0))

    (when-in-alist (entity-name "name" entity)
      (let ((underline (make-string (string-length entity-name)
                                    (if (and (> entity-no 1)
                                             *subunderliner*)
                                        *subunderliner*
                                        *underliner*))))
        (show #t entity-name nl underline nl nl)))

    (when-in-alist (tagline "tagline" entity)
      (show #t (italics (string-trim-both tagline)) nl nl))

    (unless *omit-entity-description*
      (when-in-alist (description "description" entity)
        (show #t description nl nl)))
  
    (when-in-alist (size "size" entity)
      (show #t (bold "Size:") " " size nl nl))

    (when-in-alist (stats "stats" entity)
      (sep3)
      (row3 (text (bolding "VALUE"))
            (text (bolding "POINTS"))
            (text (bolding "STAT")))
      (headsep3)
      (set! stats-total
        (loop for stat in stats sum (process-stat stat) do (sep3)))
      (when *show-subtotals*
        (row3 "" (text (bolding (number->string stats-total)))
              (text (bolding "STATS TOTAL")))
        (sep3))
      (cond (*one-table* (empty))
            (else (show #t nl))))

    (when-in-alist (derived "derived" entity)
      (sep2)
      (row2 (text (bolding "VALUE")) (text (bolding "DERIVED VALUE")))
      (headsep2)
      (loop for d in derived do (process-derived d) do (sep2))
      (cond (*one-table* (empty))
            (else (show #t nl))))

    (when-in-alist (attributes "attributes" entity)
      (sep3)
      (row3 (text (bolding "LEVEL"))
            (text (bolding "POINTS"))
            (text (bolding "ATTRIBUTE")))
      (headsep3)
      (set! attributes-total
        (loop for attribute in (sort attributes name-ci<?)
              sum (process-attribute attribute)
              do (sep3)))
      (when *show-subtotals*
        (row3 "" (text (bolding (number->string attributes-total)))
              (text (bolding "ATTRIBUTES TOTAL")))
        (sep3))
      (cond (*one-table* (empty))
            (else (show #t nl))))

    (when-in-alist (defects "defects" entity)
      (sep3)
      (row2 (text (bolding "POINTS")) (text (bolding "DEFECT")))
      (headsep3)
      (set! defects-total 
        (loop for defect in (sort defects name-ci<?)
              sum (process-defect defect)
              do (sep2)))
      (when *show-subtotals*
        (row2 (text (bolding (number->string defects-total)))
              (text (bolding "DEFECTS TOTAL")))
        (sep2))
      (cond (*one-table* (empty))
            (else (show #t nl))))

    (when-in-alist (skills "skills" entity)
      (sep3)
      (row3 (text (bolding "LEVEL"))
            (text (bolding "POINTS"))
            (text (bolding "SKILL")))
      (headsep3)
      (set! skills-total
        (loop for skill in (sort skills name-ci<?)
              sum (process-skill skill)
              do (sep3)))
      (row3 "" (text (bolding (number->string skills-total)))
            (text (bolding "SKILL POINTS TOTAL")))
      (sep3)
      (cond (*one-table* (empty))
            (else (show #t nl))))

    ;; Output total.
    (sep3)
    (set! entity-total (+ stats-total attributes-total defects-total))
    (row3 "" (text (bolding (number->string entity-total)))
          (text (bolding "TOTAL")))
    (sep3)
    (show #t nl)
    ))


(define (total-points items)
  (loop for item in items sum (must-exist "points" item)))

(define (process-stat-terse stat)
  (show #t (must-exist "name" stat) " " (must-exist "value" stat)
        " (" (label-points (must-exist "points" stat))  ")"))

(define (process-derived-terse derived)
  (dbg (dfmt "process-derived-terse: " (pretty derived) nl))
  ;; There are no points.
  (let* ((name          (must-exist "name" derived))
         (value         (must-exist "value" derived))
         (alternatives  (may-exist "alternatives" derived))
         (alternatives  (if alternatives
                            (string-append " ("
                                           (string-join alternatives ", ")
                                           ")")
                            #f)))
    (show #t name " " (displayed value) (if alternatives alternatives ""))))

(define (process-attribute-terse attribute)
  (dbg (dfmt "process-attribute-terse: " (pretty attribute) nl))
  ;; returns the cost of the attribute
  (let* ((name         (must-exist "name" attribute))
         (level        (may-exist "level" attribute))
         (level        (if level level ""))
         (points       (must-exist "points" attribute))
         (details      (may-exist  "details" attribute))
         (details      (if details (string-trim-both details) details))
         (effective    (may-exist  "effective" attribute))
         (level        (if effective (show #f level " (" effective ")") level))
         (enhancements (may-exist  "enhancements" attribute))
         (limiters     (may-exist  "limiters" attribute))
         (elements     (may-exist  "elements" attribute))
         (details      (make-attribute-details details enhancements limiters
                                               elements)))
    (show #t (italicizing name " " level) " ("
          (if details (string-append details ". ") "")
          (label-points points) ")")))

(define (process-defect-terse defect)
  (dbg (dfmt "process-defect-terse: " (pretty defect) nl))
  ;; Returns the cost of the defect
  (let* ((name        (must-exist "name" defect))
         (points      (must-exist "points" defect))
         (details     (may-exist  "details" defect))
         (details     (if details (string-trim-both details) details)))
    (show #t (italicizing name) " ("
          (if details (string-append details ".  ") "")
          (label-points points) ")")))

(define (process-skill-terse skill)
  (dbg (dfmt "process-skill-terse: " (pretty skill) nl))
  ;; Returns the cost of the skill.
  (let* ((name            (must-exist "name" skill))
         (level           (must-exist "level" skill))
         (points          (must-exist "points" skill))
         (specialisations (may-exist "specialisations" skill)))
    (show #t (italicizing name " " level) " ("
          (if specialisations
              (string-append (string-join specialisations ", ") ".  ")
              "")
         (displayed points) " SP)")))

(define (process-entity-terse entity entity-no)
  (dbg (dfmt "process-entity-terse: " (pretty entity) nl))
  ;; It might be a template, an item, or a full character.
  (let* ((entity-name (may-exist "name" entity))
         (tagline     (may-exist "tagline" entity))
         (description (may-exist "description" entity))
         (size        (may-exist "size" entity))
         (stats       (may-exist "stats" entity))
         (derived     (may-exist "derived" entity))
         (attributes  (may-exist "attributes" entity))
         (defects     (may-exist "defects" entity))
         (skills      (may-exist "skills" entity))

         (stats-total      (if stats (total-points stats) 0))
         (attributes-total (if attributes (total-points attributes) 0))
         (defects-total    (if defects (total-points defects) 0))
         ;; Skills are not added to entity total!
         (skills-total     (if skills (total-points skills) 0))
         (entity-total     (+ stats-total attributes-total defects-total))
         )

    (cond (entity-name
           (let* ((entity-header (show #f entity-name " ("
                                       (label-points entity-total) ")"))
                  (underline (make-string (string-length entity-header)
                                          (if (and (> entity-no 1)
                                                   *subunderliner*)
                                              *subunderliner*
                                              *underliner*))))
             (show #t entity-header nl underline nl nl)))
          (else
           (show #t (label-points entity-total) nl nl)))

    (when tagline
      (show #t (italics (string-trim-both tagline)) nl nl))

    (when (and description (not *omit-entity-description*))
      (show #t description nl nl))
    
    (when size
      (show #t (bold "Size:") " " size nl nl))

    (when stats
      (show #t (bold "Statistics"))
      (when *show-subtotals*
        (show #t " (" (label-points stats-total) ") "))
      (show #t " — " nl)
      (loop for stat in stats
            for i from 1
            when (> i 1) do (show #t ", ")
            do (process-stat-terse stat))
      (show  #t nl nl))

    (when derived
      (show #t (bold "Derived Values") " — ")
      (loop for d in derived
            for i from 1
            when (> i 1) do (show #t ", ")
            do (process-derived-terse d))
      (show #t nl nl))

    (when attributes
      (if (mecha?)
          (show #t (bold "Mecha Sub-Attributes"))
          (show  #t (bold "Attributes")))
      (when *show-subtotals*
        (show #t " (" (label-points attributes-total) " )"))
      (show #t " — " nl)
      (loop for attribute in (sort attributes name-ci<?)
            for i from 1
            when (> i 1) do (show #t ", ")
            do (process-attribute-terse attribute))
      (show #t nl nl))

    (when defects
      (if (mecha?)
          (show #t (bold "Mecha Defects"))
          (show #t (bold "Defects")))
      (when *show-subtotals*
        (show #t " (" (label-points defects-total) ")"))
      (show #t " — " nl)
      (loop for defect in (sort defects name-ci<?)
            for i from 1
            when (> i 1) do (show #t ", ")
            do (process-defect-terse defect))
      (show #t nl nl))

    (when skills
      (show #t (bold "Skills"))
      (when *show-subtotals*
        (show #t " (" (displayed skills-total) " SP)"))
      (show #t " — " nl)
      (loop for skill in (sort skills name-ci<?)
            for i from 1
            when (> i 1) do (show #t ", ")
            do (process-skill-terse skill)))
    ))


(define (tbold s)                       ; Troff bold.
  (cond
   ((string-null? s) s)
   (else (string-append "\\fB" s "\\fP"))))

(define (titalics s)                     ; Troff italics.
  (cond
   ((string-null? s) s)
   (else (string-append "\\fI" s "\\fP*"))))

(define *raw-prefix* "   ")             ;

(define (process-stat-raw-ms stat)
  (dbg (dfmt "process-stat-raw-ms: " (pretty stat) nl))
  (let ((name   (must-exist "name" stat))
        (value  (must-exist "value" stat))
        (points (must-exist "points" stat)))
    (show #t *raw-prefix* value "#" points "#" name nl)
    points))

(define (process-derived-raw-ms derived)
  (dbg (dfmt "process-derived-raw-ms: " (pretty derived) nl))
  ;; There are no points.
  (let* ((name          (must-exist "name" derived))
         (expansion     (may-exist name derived-abbreviations))
         (name          (if expansion expansion name))
         (value         (must-exist "value" derived))
         (alternatives  (may-exist "alternatives" derived))
         (description   (if alternatives
                            (string-append name " ("
                                           (string-join alternatives ", ")
                                           ")")
                            name)))
    (show #t *raw-prefix* value "#T{" nl
          *raw-prefix* description nl
          *raw-prefix* "T}" nl)))

(define (process-attribute-raw-ms attribute)
  (dbg (dfmt "process-attribute-raw-ms: " (pretty attribute) nl))
  ;; returns the cost of the attribute
  (let* ((name         (must-exist "name" attribute))
         (level        (may-exist "level" attribute))
         (level        (if level level ""))
         (points       (must-exist "points" attribute))
         (details      (may-exist  "details" attribute))
         (details      (if details (string-trim-both details) details))
         (effective    (may-exist  "effective" attribute))
         (level        (if effective (show #f level " (" effective ")") level))
         (enhancements (may-exist  "enhancements" attribute))
         (limiters     (may-exist  "limiters" attribute))
         (elements     (may-exist  "elements" attribute))
         (details      (make-attribute-details details enhancements limiters
                                               elements))
         (description (if details (show #f name " ("
                                        (space-to-newline details) ")") name)))
    (show #t *raw-prefix* level "#" points "#T{" nl
          *raw-prefix* description nl
          *raw-prefix* "T}" nl)
    points))

(define (process-defect-raw-ms defect)
  (dbg (dfmt "process-defect-raw-ms: " (pretty defect) nl))
  ;; Returns the cost of the defect
  (let* ((name        (must-exist "name" defect))
         (points      (must-exist "points" defect))
         (details     (may-exist  "details" defect))
         (details     (if details (string-trim-both details) details))
         (description (if details (show #f name " ("
                                        (space-to-newline details) ")") name))
         )
    (show #t *raw-prefix* "#" points "#T{" nl
          *raw-prefix* description nl
          *raw-prefix* "T}" nl)
    points))

(define (process-skill-raw-ms skill)
  (dbg (dfmt "process-skill-raw-ms: " (pretty skill) nl))
  ;; Returns the cost of the skill.
  (let* ((name            (must-exist "name" skill))
         (level           (must-exist "level" skill))
         (points          (must-exist "points" skill))
         (specialisations (may-exist "specialisations" skill))
         (description     (show #f name (if specialisations
                                            (string-append
                                             " ("
                                             (string-join specialisations ", ")
                                             ")")
                                            ""))))
    (show #t *raw-prefix* level "#"  points "#T{" nl
          *raw-prefix* description nl
          *raw-prefix* "T}" nl)
    points))

(define (process-entity-raw-ms entity entity-no)
  (dbg (dfmt "process-entity-raw-ms: " (pretty entity) nl))
  ;; It might be a template, an item, or a full character.
  (let ((paragraph-seen #f)
        (first-section-seen #f)
        (stats-total 0)
        (attributes-total 0)
        (defects-total 0)
        (skills-total 0)                ; Not added to entity total!
        (entity-total 0))

    (when-in-alist (entity-name "name" entity)
      (let ((underline (make-string (string-length entity-name)
                                    (if (and (> entity-no 1)
                                             *subunderliner*)
                                        *subunderliner*
                                        *underliner*))))
        (show #t entity-name nl underline nl nl)))

    (when-in-alist (tagline "tagline" entity)
      (set! paragraph-seen #t)
      (show #t (italics (string-trim-both tagline)) nl nl))

    (unless *omit-entity-description*
      (when-in-alist (description "description" entity)
        (set! paragraph-seen #t)
        (show #t description nl nl)))
  
    (when-in-alist (size "size" entity)
      (set! paragraph-seen #t)
      (show #t (bold "Size:") " " size nl nl))

    (show #t ".. raw:: ms" nl nl)
    ;; groff output from here to the end of this function.

    (unless paragraph-seen
      (show #t *raw-prefix* ".LP" nl))

    (show #t *raw-prefix* ".TS" nl)
    (show #t *raw-prefix* "tab(#) ;" nl)

    (when-in-alist (stats "stats" entity)
      (set! first-section-seen #t)
      (show #t *raw-prefix* "c c lx ." nl)
      (show #t *raw-prefix* "=" nl)
      (show #t *raw-prefix* (tbold "VALUE") "#" (tbold "POINTS") "#"
            (tbold "STAT") nl)
      (set! stats-total
        (loop for stat in stats sum (process-stat-raw-ms stat)))
      (when *show-subtotals*
        (show #t *raw-prefix* "#" (tbold (number->string stats-total)) "#"
              (tbold "STATS TOTAL") nl))
      (show #t *raw-prefix* nl))

    (when-in-alist (derived "derived" entity)
      (when first-section-seen
        (show #t *raw-prefix* ".T&" nl))
      (show #t *raw-prefix* "c l sx ." nl)
      (unless first-section-seen
        (set! first-section-seen #t)
        (show #t *raw-prefix* "=" nl))
      (show #t *raw-prefix* (tbold "VALUE") "#" (tbold "DERIVED VALUE") nl)
      (loop for d in derived do (process-derived-raw-ms d))
      (show #t *raw-prefix* nl))

    (when-in-alist (attributes "attributes" entity)
      (when first-section-seen
        (show #t *raw-prefix* ".T&" nl))
      (show #t *raw-prefix* "c c lx ." nl)
      (unless first-section-seen
        (set! first-section-seen #t)
        (show #t *raw-prefix* "=" nl))
      (show #t *raw-prefix* (tbold "LEVEL") "#" (tbold "POINTS") "#"
            (tbold "ATTRIBUTE") nl)
      (set! attributes-total
        (loop for attribute in (sort attributes name-ci<?)
              sum (process-attribute-raw-ms attribute)))
      (when *show-subtotals*
        (show #t *raw-prefix* "#" (tbold (number->string attributes-total)) "#" 
              (tbold "ATTRIBUTES TOTAL") nl))
      (show #t *raw-prefix* nl))

    (when-in-alist (defects "defects" entity)
      (when first-section-seen
        (show #t *raw-prefix* ".T&" nl))
      (show #t *raw-prefix* "c c lx ." nl)
      (unless first-section-seen
        (set! first-section-seen #t)
        (show #t *raw-prefix* "=" nl))
      (show #t *raw-prefix* "#"
            (tbold "POINTS") "#"
            (tbold "DEFECT") nl)
      (set! defects-total 
        (loop for defect in (sort defects name-ci<?)
              sum (process-defect-raw-ms defect)))
      (when *show-subtotals*
        (show #t *raw-prefix* "#" (tbold (number->string defects-total)) "#"
              (tbold "DEFECTS TOTAL") nl))
      (show #t *raw-prefix* nl))

    (when-in-alist (skills "skills" entity)
      (when first-section-seen
        (show #t *raw-prefix* ".T&" nl))
      (show #t *raw-prefix* "c c lx ." nl)
      (unless first-section-seen
        (set! first-section-seen #t)
        (show #t *raw-prefix* "=" nl))
      (show #t *raw-prefix* (tbold "LEVEL") "#" (tbold "POINTS") "#"
            (tbold "SKILL") nl)
      (set! skills-total
        (loop for skill in (sort skills name-ci<?)
              sum (process-skill-raw-ms skill)))
      (show #t *raw-prefix* "#" (tbold (number->string skills-total)) "#"
            (tbold "SKILL POINTS TOTAL") nl)
      (show #t *raw-prefix* nl))

    ;; Output total.
    (set! entity-total (+ stats-total attributes-total defects-total))
    (when (> entity-total 0)
      (show #t *raw-prefix* "#" (tbold (number->string entity-total)) "#"
            (tbold "TOTAL") nl))
    (show #t *raw-prefix* "=" nl)
    (show #t *raw-prefix* ".TE" nl)
    ))


(define (process-file)
  ;; It is a file of possibly multiple entities.
  (let ((entities (yaml-load (current-input-port))))
    (loop for entity in entities
          for entity-no from 1
          do (parameterize ((mecha? (assoc "mecha" entity)))
               (*output-formatter* entity entity-no)))))


(define (process-filename filename)
  (with-input-from-file filename process-file))



(define (usage)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print "Usage: " (program-name) " [options...] [files...]")
      (newline)
      (print (args:usage +command-line-options+))
      (newline)
      (print
       "Note: use -1 (or --one) if you are generating this for HTML output,
as that looks better.")
      (newline)
      (show #t "Current argv: " (written (argv)) nl)))
  (exit 1))

(define *bolding* #t)
(define *italicizing* #t)
(define *debugging* #f)
(define *head-sep* #\=)
(define *num-width* (max
                     (string-length "LEVEL")
                     (string-length "VALUE")
                     (string-length "POINTS")
                     ))
(define *one-table* #f)
(define *output-file* #f)
(define *output-formatter* process-entity)
(define *omit-entity-description* #f)
(define *show-subtotals* #f)
(define *table-width* 60)
(define *underliner* #\-)
(define *subunderliner* #f)

(define +command-line-options+
  (list (args:make-option
            (b bold) #:none (show #f
                                  "Turn OFF bolding of headers in plain
                          reST headers and names and levels of
                          attributes and defects in terse mode.")
         (set! *bolding* #f))
        ;; c is reserved for raw ConTeXt output.
        (args:make-option
         (d debug) #:none "Turn on debugging."
         (set! *debugging* #t))
        (args:make-option
         (D omit-description) #:none "Omit the entiy description."
         (set! *omit-entity-description* #t))
        (args:make-option
            (i italics) #:none (show #f
                                  "Turn OFF italicizing of names and levels of
                          attributes and defects in terse mode.")
          (set! *italicizing* #f))
        (args:make-option
         (m raw-ms-tables) #:none "Use groff tbl output in a raw ms block."
         (set! *output-formatter* process-entity-raw-ms))
        (args:make-option
         (o output) #:required "Output file."
         (set! *output-file* arg))
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
         (set! *output-formatter* process-entity-terse))
        (args:make-option
         (u underliner) #:required
         "Character to use for underlining the header."
         (set! *underliner* (string-ref arg 0)))
        (args:make-option
         (U subunderliner) #:required
         "Entities after the first are subentities,
                          and use a different character for
                          underlining the subheader."
         (set! *subunderliner* (string-ref arg 0)))
        (args:make-option
         (w width)
         (required: "NUMBER") "Width of table in characters"
         (set! *table-width* (string->number arg)))))

(define (main)
  (receive (options operands) (args:parse (command-line-arguments)
                                          +command-line-options+)
    (define (process-operands)
      (if  (zero? (length operands))
           (with-input-from-port (current-input-port) process-file)
           (loop for filename in operands do (process-filename filename))))

    ;; When normally outputing reST bolding is two asterisks on each side.
    (set! *num-width* (+ *num-width* 4))

    (if *output-file*
        (with-output-to-file *output-file* process-operands)
        (process-operands))))

;; Only invoke main if this has been compiled.  That way we can load the
;; module into csi and debug it. 
(cond-expand
  ((and chicken-5 compiling)
   (main))
  ((and chicken-5 csi)))
)
