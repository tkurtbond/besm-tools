;;;!> besm2-rst-f.scm -- Convert a YAML BESM 2E character or template into
;;;   reST, walking the input with slibfyaml's handle/tree-based document
;;;   API -- (slibfyaml documents) + (slibfyaml nodes) -- instead of
;;;   materializing the whole document into alists/lists up front the way
;;;   besm2-rst.scm (this file's starting point) does via the yaml egg or
;;;   slibfyaml's own value-materializing (slibfyaml scheme) convenience
;;;   layer. Entities, and their stats/attributes/defects/skills, stay node
;;;   handles until a leaf scalar is actually needed for output or
;;;   arithmetic, at which point it's decoded with a typed accessor
;;;   (node-string-value/node-integer-value/node-boolean-value). See
;;;   PLAN.md (not checked in) for the field-by-field design.
;;;
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
;;;   MINUS SIGN, Unicode MULTIPLICATION SIGN, etc.  -n/--unicode-minus
;;;   overrides this default and uses Unicode MINUS SIGN for negative
;;;   numbers instead.
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
(import (chicken condition))
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
(import (slibfyaml documents))
(import (slibfyaml documents streams))
(import (slibfyaml nodes))

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

;; may-exist is kept only for the plain-alist derived-abbreviations lookup
;; below -- every YAML-sourced field is read via a typed slibfyaml node
;; accessor instead (node-string-value/node-integer-value/
;; node-boolean-value, or one of the node-optional-* helpers just below).
(define (may-exist item alist)
  (let ((result (assoc item alist)))
    (if result
        (cdr result)
        result)))

;; The node-handle analogues of may-exist: slibfyaml's own optional typed
;; accessors take a *default value* for an absent key (map key default),
;; not a "return #f" convention, so these exist to cover the "return #f, or
;; the child node/decoded value/list, if present" shape used throughout
;; this file's entity/attribute/defect/skill processing.

;; mapping, not map, as the parameter name throughout this section: node-
;; optional-customizers below needs to call the real (list-processing) map
;; procedure in its own body, so a mapping-node parameter actually named
;; map would shadow it there -- confirmed live (a "call of non-procedure"
;; failure) before this rename, so every sibling here uses the same
;; non-shadowing name for consistency even where it isn't itself at risk.

(define (node-optional mapping key)
  (let ((v (node-value mapping key)))
    (if (node-valid? v) v #f)))

(define (node-optional-string mapping key)
  (let ((v (node-optional mapping key)))
    (and v (node-scalar-value v))))

(define (node-items seq)
  ;; seq's items, as a plain Scheme list of node handles, in order.
  (let ((acc '()))
    (node-iterate-items seq (lambda (item) (set! acc (cons item acc))))
    (reverse acc)))

(define (node-strings seq)
  (map node-scalar-value (node-items seq)))

(define (node-optional-items mapping key)
  (let ((v (node-optional mapping key)))
    (and v (node-items v))))

(define (node-optional-strings mapping key)
  (let ((v (node-optional mapping key)))
    (and v (node-strings v))))

(define (node-decode-customizer item)
  ;; Decode one enhancements/limiters sequence item into the exact shape
  ;; format-customizers already pattern-matches on below: a bare string, or
  ;; a list of (name counts-as [applies-to ...]) -- so format-customizers
  ;; itself needs no changes, only its caller's input does.
  (if (node-scalar? item)
      (node-scalar-value item)
      (let* ((n (node-length item))
             (name (node-scalar-value (node-item item 1)))
             (counts-as (node-integer-value (node-item item 2))))
        (cond ((= n 2) (list name counts-as))
              ((= n 3) (list name counts-as (node-scalar-value (node-item item 3))))
              (else (append (list name counts-as)
                            (map (lambda (i) (node-scalar-value (node-item item i)))
                                 (iota (- n 2) 3))))))))

(define (node-optional-customizers mapping key)
  (let ((v (node-optional mapping key)))
    (and v (map node-decode-customizer (node-items v)))))

;; The node-handle analogues of when-in-alist: (var key map) binds var to
;; the decoded string / list-of-node-handles for key in the mapping node
;; map, running the body only if key is present -- otherwise identical in
;; use to when-in-alist, so bodies below are unchanged.
;; (put 'when-string-in-node 'scheme-indent-function 1)
;; (put 'when-items-in-node 'scheme-indent-function 1)
(define-syntax when-string-in-node
  (syntax-rules ()
    ((_ (var key map) b1 ...)
     (let ((var (node-optional-string map key)))
       (when var b1 ...)))))

(define-syntax when-items-in-node
  (syntax-rules ()
    ((_ (var key map) b1 ...)
     (let ((var (node-optional-items map key)))
       (when var b1 ...)))))


(define mecha? (make-parameter #f))

(define (label-points points)
  (show #f (abs points) (if (mecha?)
                            (if (< points 0) " MBP" " MP")
                            (if (< points 0) " BP" " CP"))))

;; The glyph used for a negative sign wherever this program builds one
;; itself (as opposed to relying on number->string's own "-"): ASCII
;; hyphen-minus by default (see the design-decision note at the top of
;; the file), or Unicode MINUS SIGN (U+2212) under -n/--unicode-minus.
(define (minus-glyph)
  (if *unicode-minus* "−" "-"))

;; points is known negative (defect points always are); render its
;; magnitude with the configured minus glyph in front, instead of
;; relying on number->string's own always-ASCII "-".
(define (negative-number->string points)
  (string-append (minus-glyph) (number->string (abs points))))

;; points might be positive, negative, or zero -- e.g. defects-total
;; (always <= 0, since it's a sum of always-negative defect points) or
;; entity-total (a sum of stats/attributes/defects totals, which can
;; go negative if defects outweigh the rest). Render it with the
;; configured minus glyph when negative, otherwise plain.
(define (points->string points)
  (if (< points 0) (negative-number->string points) (number->string points)))

(define (space-to-newline s)
  (string-map (lambda (c) (if (char=? c #\newline) #\space c)) s))

;; Always bold. A formatter that works with SRFI 166: Monadic
;; Formatting, which means it has to be used within the arguments to a
;; call to show.
(define (bold . args)
  (each "**" (each-in-list args) "**"))

(define (italics . args)
  (each "*" (each-in-list args) "*"))

;; Only bold if the -B/--bold-head command line option has been set.
;; This is for bolding in headers in reST table output.
(define (hbolding . args)
  (if *bold-head*
      (each "**" (each-in-list args) "**")
      (each-in-list args)))

;; Only bold if the command line option for bolding has been set. This
;; generally happens in headers in reST table output and in attribute
;; and defect names and levels in terse mode. This is a formatter that
;; works with SRFI 166: Monadic Formatting, which means it has to be
;; used within the arguments to a call to the show procedure.
(define (bolding . args)
  (if *bolding*
      (bold (each-in-list args))
      (each-in-list args)))

(define (italicizing . args)
  (if *italicizing*
      (italics (each-in-list args))
      (each-in-list args)))

(define (emphasizing . args)
  (cond
   (*bolding*     (bold (each-in-list args)))
   (*italicizing* (italics (each-in-list args)))
   (else          (each-in-list args))))

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


(define (process-stat stat)
  (dbg (dfmt "process-stat: " (node-path stat) nl))
  (let ((name   (node-string-value stat "name"))
        (value  (node-string-value stat "value"))
        (points (node-integer-value stat "points")))
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
  (dbg (dfmt "process-derived: " (node-path derived) nl))
  ;; There are no points.
  (let* ((name          (node-string-value derived "name"))
         (expansion     (may-exist name derived-abbreviations))
         (name          (if expansion expansion name))
         (value         (node-string-value derived "value"))
         (alternatives  (node-optional-strings derived "alternatives"))
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
  ;; it looking bad typographically.  -n/--unicode-minus overrides this
  ;; and uses Unicode MINUS SIGN instead.
  (loop for item in items
        collect (match item
                  [(? string? s)
                   (show #f (displayed s) " "
                         (if (eq? type 'enhancement) (string-append (minus-glyph) "1") "+1"))]
                  [((? string? name) (? number? counts-as))
                   (show #f (displayed name) " "
                         (if (eq? type 'enhancement) (minus-glyph) "+")
                         (displayed counts-as))]
                  [((? string? name) (? number? counts-as) (? string? applies-to))
                   (show #f (displayed name) ": " (displayed applies-to) " "
                         (if (eq? type 'enhancement) (minus-glyph) "+")
                         (displayed counts-as))]
                  [((? string? name) (? number? counts-as) . applies-to)
                   (show #f (displayed name) ": "
                         (joined displayed applies-to ", ") " "
                         (if (eq? type 'enhancement) (minus-glyph) "+")
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
  (dbg (dfmt "process-attribute: " (node-path attribute) nl))
  ;; returns the cost of the attribute
  (let* ((name         (node-string-value attribute "name"))
         (level        (node-string-value attribute "level"))
         (points       (node-integer-value attribute "points"))
         (details      (node-optional-string attribute "details"))
         (details      (if details (string-trim-both details) details))
         (effective    (node-optional-string attribute "effective"))
         (level        (if effective (show #f level " (" effective ")") level))
         (enhancements (node-optional-customizers attribute "enhancements"))
         (limiters     (node-optional-customizers attribute "limiters"))
         (elements     (node-optional-strings attribute "elements"))
         (details      (make-attribute-details details enhancements limiters
                                               elements))
         (description (if details (show #f name " (" details ")") name)))
    (row3 level points description)
    points))

(define (process-defect defect)
  (dbg (dfmt "process-defect: " (node-path defect) nl))
  ;; Returns the cost of the defect
  (let* ((name        (node-string-value defect "name"))
         (points      (node-integer-value defect "points"))
         (details     (node-optional-string defect "details"))
         (details     (if details (string-trim-both details) details))
         (description (if details (show #f name " (" details ")") name))
         )
    (dbg (dfmt "process-defect: before row3" nl))
    ;; Defect points are negative, but in rst that starts an itemized list,
    ;; so we put a backslash before them to quote them.
    (row3 "" (string-append "\\" (negative-number->string points)) description)
    (dbg (dfmt "process-defect: after row3" nl))
    points))

(define (process-skill skill)
  (dbg (dfmt "process-skill: " (node-path skill) nl))
  ;; Returns the cost of the skill.
  (let* ((name            (node-string-value skill "name"))
         (level           (node-string-value skill "level"))
         (points          (node-integer-value skill "points"))
         (specialisations (node-optional-strings skill "specialisations"))
         (description     (show #f name (if specialisations
                                            (string-append
                                             " ("
                                             (string-join specialisations ", ")
                                             ")")
                                            ""))))
    (row3 level points description)
    points))

(define (name-ci<? a b)
  (let ((a-name (node-string-value a "name"))
        (b-name (node-string-value b "name")))
    (string-ci<? a-name b-name)))

(define (process-entity entity entity-no)
  (dbg (dfmt "process-entity: " (node-path entity) nl))
  ;; It might be a template, an item, or a full character.
  (let ((stats-total 0)
        (attributes-total 0)
        (defects-total 0)
        (skills-total 0)                ; Not added to entity total!
        (entity-total 0))

    (when-string-in-node (entity-name "name" entity)
      (let ((underline (make-string (string-length entity-name)
                                    (if (and (> entity-no 1)
                                             *subunderliner*)
                                        *subunderliner*
                                        *underliner*))))
        (show #t entity-name nl underline nl nl)))

    (when-string-in-node (tagline "tagline" entity)
      (show #t (italics (string-trim-both tagline)) nl nl))

    (unless *omit-entity-description*
      (when-string-in-node (description "description" entity)
        (show #t description nl nl)))

    (when-string-in-node (size "size" entity)
      (show #t (bold "Size:") " " size nl nl))

    (when-items-in-node (stats "stats" entity)
      (sep3)
      (row3 (text (hbolding "VALUE"))
            (text (hbolding "POINTS"))
            (text (hbolding "STAT")))
      (headsep3)
      (set! stats-total
        (loop for stat in stats sum (process-stat stat) do (sep3)))
      (when *show-subtotals*
        (row3 "" (text (hbolding (number->string stats-total)))
              (text (hbolding "STATS TOTAL")))
        (sep3))
      (cond (*one-table* (empty))
            (else (show #t nl))))

    (when-items-in-node (derived "derived" entity)
      (sep2)
      (row2 (text (hbolding "VALUE")) (text (hbolding "DERIVED VALUE")))
      (headsep2)
      (loop for d in derived do (process-derived d) do (sep2))
      (cond (*one-table* (empty))
            (else (show #t nl))))

    (when-items-in-node (attributes "attributes" entity)
      (sep3)
      (row3 (text (hbolding "LEVEL"))
            (text (hbolding "POINTS"))
            (text (hbolding "ATTRIBUTE")))
      (headsep3)
      (set! attributes-total
        (loop for attribute in (sort attributes name-ci<?)
              sum (process-attribute attribute)
              do (sep3)))
      (when *show-subtotals*
        (row3 "" (text (hbolding (number->string attributes-total)))
              (text (hbolding "ATTRIBUTES TOTAL")))
        (sep3))
      (cond (*one-table* (empty))
            (else (show #t nl))))

    (when-items-in-node (defects "defects" entity)
      (sep3)
      (row3 "" (text (hbolding "POINTS")) (text (hbolding "DEFECT")))
      (headsep3)
      (set! defects-total
        (loop for defect in (sort defects name-ci<?)
              sum (process-defect defect)
              do (sep3)))
      (when *show-subtotals*
        (row3 "" (text (hbolding (points->string defects-total)))
              (text (hbolding "DEFECTS TOTAL")))
        (sep3))
      (cond (*one-table* (empty))
            (else (show #t nl))))

    (when-items-in-node (skills "skills" entity)
      (sep3)
      (row3 (text (hbolding "LEVEL"))
            (text (hbolding "POINTS"))
            (text (hbolding "SKILL")))
      (headsep3)
      (set! skills-total
        (loop for skill in (sort skills name-ci<?)
              sum (process-skill skill)
              do (sep3)))
      (row3 "" (text (hbolding (number->string skills-total)))
            (text (hbolding "SKILL POINTS TOTAL")))
      (sep3)
      (cond (*one-table* (empty))
            (else (show #t nl))))

    ;; Output total.
    (sep3)
    (set! entity-total (+ stats-total attributes-total defects-total))
    (row3 "" (text (hbolding (points->string entity-total)))
          (text (hbolding "TOTAL")))
    (sep3)
    (show #t nl)
    ))


(define (total-points items)
  (loop for item in items sum (node-integer-value item "points")))

(define (process-stat-terse stat)
  (show #t (node-string-value stat "name") " " (node-string-value stat "value")
        " (" (label-points (node-integer-value stat "points"))  ")"))

(define (process-derived-terse derived)
  (dbg (dfmt "process-derived-terse: " (node-path derived) nl))
  ;; There are no points.
  (let* ((name          (node-string-value derived "name"))
         (value         (node-string-value derived "value"))
         (alternatives  (node-optional-strings derived "alternatives"))
         (alternatives  (if alternatives
                            (string-append " ("
                                           (string-join alternatives ", ")
                                           ")")
                            #f)))
    (show #t name " " (displayed value) (if alternatives alternatives ""))))

(define (process-attribute-terse attribute)
  (dbg (dfmt "process-attribute-terse: " (node-path attribute) nl))
  ;; returns the cost of the attribute
  (let* ((name         (node-string-value attribute "name"))
         (level        (node-string-value attribute "level"))
         (points       (node-integer-value attribute "points"))
         (details      (node-optional-string attribute "details"))
         (details      (if details (string-trim-both details) details))
         (effective    (node-optional-string attribute "effective"))
         (level        (if effective (show #f level " (" effective ")") level))
         (enhancements (node-optional-customizers attribute "enhancements"))
         (limiters     (node-optional-customizers attribute "limiters"))
         (elements     (node-optional-strings attribute "elements"))
         (details      (make-attribute-details details enhancements limiters
                                               elements)))
    (show #t (emphasizing name
                          (if *em-dash* " — " " ")
                          (if *level* "Level " "") level) " ("
          (if details (string-append details ". ") "")
          (label-points points) ")")))

(define (process-defect-terse defect)
  (dbg (dfmt "process-defect-terse: " (node-path defect) nl))
  ;; Returns the cost of the defect
  (let* ((name        (node-string-value defect "name"))
         (points      (node-integer-value defect "points"))
         (details     (node-optional-string defect "details"))
         (details     (if details (string-trim-both details) details)))
    (show #t (emphasizing name) " ("
          (if details (string-append details ".  ") "")
          (label-points points) ")")))

(define (process-skill-terse skill)
  (dbg (dfmt "process-skill-terse: " (node-path skill) nl))
  ;; Returns the cost of the skill.
  (let* ((name            (node-string-value skill "name"))
         (level           (node-string-value skill "level"))
         (points          (node-integer-value skill "points"))
         (specialisations (node-optional-strings skill "specialisations")))
    (show #t (emphasizing name
                          (if *em-dash* " — " " ")
                          (if *level* "Level " "")level) " ("
          (if specialisations
              (string-append (string-join specialisations ", ") ".  ")
              "")
         (displayed points) " SP)")))

(define (process-entity-terse entity entity-no)
  (dbg (dfmt "process-entity-terse: " (node-path entity) nl))
  ;; It might be a template, an item, or a full character.
  (let* ((entity-name (node-optional-string entity "name"))
         (tagline     (node-optional-string entity "tagline"))
         (description (node-optional-string entity "description"))
         (size        (node-optional-string entity "size"))
         (stats       (node-optional-items entity "stats"))
         (derived     (node-optional-items entity "derived"))
         (attributes  (node-optional-items entity "attributes"))
         (defects     (node-optional-items entity "defects"))
         (skills      (node-optional-items entity "skills"))

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
      (show #t description nl nl)
      (when *page-after-description*
        (show #t ".. raw:: ms" nl nl "   .bp" nl nl)))

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
        (show #t " (" (label-points attributes-total) ")"))
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
            do (process-skill-terse skill))
      (show #t nl nl))
    ))

(define-syntax depth+
  (syntax-rules ()
    ((_ e1 e2 ...) (parameterize ((*hmm-depth* (+ (*hmm-depth*) 1))) e1 e2 ...))))

(define (make-tabs n)
  (make-string n #\tab))

(define (indent)
  (make-tabs (*hmm-depth*)))

(define (all-one-line s)
  (string-join (string-split s "\n") " "))

(define (process-stat-hmm stat)
  (show #t (node-string-value stat "name") " " (node-string-value stat "value")
        " (" (label-points (node-integer-value stat "points"))  ")"))

(define (process-derived-hmm derived)
  (dbg (dfmt "process-derived-hmm: " (node-path derived) nl))
  ;; There are no points.
  (let* ((name          (node-string-value derived "name"))
         (value         (node-string-value derived "value"))
         (alternatives  (node-optional-strings derived "alternatives"))
         (alternatives  (if alternatives
                            (string-append " ("
                                           (string-join alternatives ", ")
                                           ")")
                            #f)))
    (show #t name " " (displayed value) (if alternatives alternatives ""))))

(define (process-attribute-hmm attribute)
  (dbg (dfmt "process-attribute-hmm: " (node-path attribute) nl))
  ;; returns the cost of the attribute
  (let* ((name         (node-string-value attribute "name"))
         (level        (node-string-value attribute "level"))
         (points       (node-integer-value attribute "points"))
         (details      (node-optional-string attribute "details"))
         (details      (if details (string-trim-both details) details))
         (effective    (node-optional-string attribute "effective"))
         (level        (if effective (show #f level " (" effective ")") level))
         (enhancements (node-optional-customizers attribute "enhancements"))
         (limiters     (node-optional-customizers attribute "limiters"))
         (elements     (node-optional-strings attribute "elements"))
         (details      (make-attribute-details details enhancements limiters
                                               elements)))
    (show #t (emphasizing name
                          (if *em-dash* " — " " ")
                          (if *level* "Level " "") level) " ("
                          (if details (all-one-line (string-append details ". ")) "")
                          (label-points points) ")")))

(define (process-defect-hmm defect)
  (dbg (dfmt "process-defect-hmm: " (node-path defect) nl))
  ;; Returns the cost of the defect
  (let* ((name        (node-string-value defect "name"))
         (points      (node-integer-value defect "points"))
         (details     (node-optional-string defect "details"))
         (details     (if details (string-trim-both details) details)))
    (show #t (emphasizing name) " ("
          (if details (all-one-line (string-append details ".  ")) "")
          (label-points points) ")")))

(define (process-skill-hmm skill)
  (dbg (dfmt "process-skill-hmm: " (node-path skill) nl))
  ;; Returns the cost of the skill.
  (let* ((name            (node-string-value skill "name"))
         (level           (node-string-value skill "level"))
         (points          (node-integer-value skill "points"))
         (specialisations (node-optional-strings skill "specialisations")))
    (show #t (emphasizing name
                          (if *em-dash* " — " " ")
                          (if *level* "Level " "")
                          level) " ("
                          (if specialisations
                              (string-append (string-join specialisations ", ") ".  ")
                              "")
                          (displayed points) " SP)")))

(define (process-entity-hmm entity entity-no)
  (dbg (dfmt "process-entity-hmm: " (node-path entity) nl))
  ;; It might be a template, an item, or a full character.
  (let* ((entity-name (node-optional-string entity "name"))
         (tagline     (node-optional-string entity "tagline"))
         (description (node-optional-string entity "description"))
         (size        (node-optional-string entity "size"))
         (stats       (node-optional-items entity "stats"))
         (derived     (node-optional-items entity "derived"))
         (attributes  (node-optional-items entity "attributes"))
         (defects     (node-optional-items entity "defects"))
         (skills      (node-optional-items entity "skills"))

         (stats-total      (if stats (total-points stats) 0))
         (attributes-total (if attributes (total-points attributes) 0))
         (defects-total    (if defects (total-points defects) 0))
         ;; Skills are not added to entity total!
         (skills-total     (if skills (total-points skills) 0))
         (entity-total     (+ stats-total attributes-total defects-total))
         )

    (depth+
      (show #t (indent))
      (cond (entity-name
             (let* ((entity-header (show #f entity-name " ("
                                         (label-points entity-total) ")"))
                    )
               (show #t entity-header nl)))
            (else
             (show #t (label-points entity-total) nl)))

      (when tagline
        (depth+
          (show #t (indent) (italics (all-one-line (string-trim-both tagline))) nl)))

      (when (and description (not *omit-entity-description*))
        (depth+
          (show #t (indent) (all-one-line description) " " nl)))

      (when size
        (depth+
          (show #t (indent) (bold "Size:") " " size nl)))

      (when stats
        (depth+
          (show #t (indent) (bold "Statistics"))
          (when *show-subtotals*
            (show #t " (" (label-points stats-total) ") "))
          (show #t nl)
          (depth+
            (show #t (indent))
            (loop for stat in stats
                  for i from 1
                  when (> i 1) do (show #t (if *hmm-separate* (each nl (indent)) ", "))
                  do (process-stat-hmm stat))
            (show  #t nl))))

      (when derived
        (depth+
          (show #t (indent) (bold "Derived Values") nl)
          (depth+
            (show #t (indent))
            (loop for d in derived
                  for i from 1
                  when (> i 1) do (show #t (if *hmm-separate* (each nl (indent)) ", "))
                  do (process-derived-hmm d))
            (show #t nl))))

      (when attributes
        (depth+
          (show #t (indent))
          (if (mecha?)
              (show #t (bold "Mecha Sub-Attributes"))
              (show #t (bold "Attributes")))
          (when *show-subtotals*
            (show #t " (" (label-points attributes-total) ")"))
          (show #t nl)
          (depth+
            (show #t (indent))
            (loop for attribute in (sort attributes name-ci<?)
                  for i from 1
                  when (> i 1) do (show #t (if *hmm-separate* (each nl (indent)) ", "))
                  do (process-attribute-hmm attribute))
            (show #t nl))))

      (when defects
        (depth+
          (show #t (indent))
          (if (mecha?)
              (show #t (bold "Mecha Defects"))
              (show #t (bold "Defects")))
          (when *show-subtotals*
            (show #t " (" (label-points defects-total) ")"))
          (show #t nl)
          (depth+
            (show #t (indent))
            (loop for defect in (sort defects name-ci<?)
                  for i from 1
                  when (> i 1) do (show #t (if *hmm-separate* (each nl (indent)) ", "))
                  do (process-defect-hmm defect))
            (show #t nl))))

      (when skills
        (depth+
          (show #t (indent) (bold "Skills"))
          (when *show-subtotals*
            (show #t " (" (displayed skills-total) " SP)"))
          (show #t nl)
          (depth+
            (show #t (indent))
            (loop for skill in (sort skills name-ci<?)
                  for i from 1
                  when (> i 1) do (show #t (if *hmm-separate* (each nl (indent)) ", "))
                  do (process-skill-hmm skill))
            (show #t nl)))))))


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
  (dbg (dfmt "process-stat-raw-ms: " (node-path stat) nl))
  (let ((name   (node-string-value stat "name"))
        (value  (node-string-value stat "value"))
        (points (node-integer-value stat "points")))
    (show #t *raw-prefix* value "#" points "#" name nl)
    points))

(define (process-derived-raw-ms derived)
  (dbg (dfmt "process-derived-raw-ms: " (node-path derived) nl))
  ;; There are no points.
  (let* ((name          (node-string-value derived "name"))
         (expansion     (may-exist name derived-abbreviations))
         (name          (if expansion expansion name))
         (value         (node-string-value derived "value"))
         (alternatives  (node-optional-strings derived "alternatives"))
         (description   (if alternatives
                            (string-append name " ("
                                           (string-join alternatives ", ")
                                           ")")
                            name)))
    (show #t *raw-prefix* value "#T{" nl
          *raw-prefix* description nl
          *raw-prefix* "T}" nl)))

(define (process-attribute-raw-ms attribute)
  (dbg (dfmt "process-attribute-raw-ms: " (node-path attribute) nl))
  ;; returns the cost of the attribute
  (let* ((name         (node-string-value attribute "name"))
         (level        (node-string-value attribute "level"))
         (points       (node-integer-value attribute "points"))
         (details      (node-optional-string attribute "details"))
         (details      (if details (string-trim-both details) details))
         (effective    (node-optional-string attribute "effective"))
         (level        (if effective (show #f level " (" effective ")") level))
         (enhancements (node-optional-customizers attribute "enhancements"))
         (limiters     (node-optional-customizers attribute "limiters"))
         (elements     (node-optional-strings attribute "elements"))
         (details      (make-attribute-details details enhancements limiters
                                               elements))
         (description (if details (show #f name " ("
                                        (space-to-newline details) ")") name)))
    (show #t *raw-prefix* level "#" points "#T{" nl
          *raw-prefix* description nl
          *raw-prefix* "T}" nl)
    points))

(define (process-defect-raw-ms defect)
  (dbg (dfmt "process-defect-raw-ms: " (node-path defect) nl))
  ;; Returns the cost of the defect
  (let* ((name        (node-string-value defect "name"))
         (points      (node-integer-value defect "points"))
         (details     (node-optional-string defect "details"))
         (details     (if details (string-trim-both details) details))
         (description (if details (show #f name " ("
                                        (space-to-newline details) ")") name))
         )
    (show #t *raw-prefix* "#" (negative-number->string points) "#T{" nl
          *raw-prefix* description nl
          *raw-prefix* "T}" nl)
    points))

(define (process-skill-raw-ms skill)
  (dbg (dfmt "process-skill-raw-ms: " (node-path skill) nl))
  ;; Returns the cost of the skill.
  (let* ((name            (node-string-value skill "name"))
         (level           (node-string-value skill "level"))
         (points          (node-integer-value skill "points"))
         (specialisations (node-optional-strings skill "specialisations"))
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
  (dbg (dfmt "process-entity-raw-ms: " (node-path entity) nl))
  ;; It might be a template, an item, or a full character.
  (let ((paragraph-seen #f)
        (first-section-seen #f)
        (stats-total 0)
        (attributes-total 0)
        (defects-total 0)
        (skills-total 0)                ; Not added to entity total!
        (entity-total 0))

    (when-string-in-node (entity-name "name" entity)
      (let ((underline (make-string (string-length entity-name)
                                    (if (and (> entity-no 1)
                                             *subunderliner*)
                                        *subunderliner*
                                        *underliner*))))
        (show #t entity-name nl underline nl nl)))

    (when-string-in-node (tagline "tagline" entity)
      (set! paragraph-seen #t)
      (show #t (italics (string-trim-both tagline)) nl nl))

    (unless *omit-entity-description*
      (when-string-in-node (description "description" entity)
        (set! paragraph-seen #t)
        (show #t description nl nl)))

    (when-string-in-node (size "size" entity)
      (set! paragraph-seen #t)
      (show #t (bold "Size:") " " size nl nl))

    (show #t ".. raw:: ms" nl nl)
    ;; groff output from here to the end of this function.

    (unless paragraph-seen
      (show #t *raw-prefix* ".LP" nl))

    (show #t *raw-prefix* ".TS" nl)
    (show #t *raw-prefix* "tab(#) ;" nl)

    (when-items-in-node (stats "stats" entity)
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

    (when-items-in-node (derived "derived" entity)
      (when first-section-seen
        (show #t *raw-prefix* ".T&" nl))
      (show #t *raw-prefix* "c l sx ." nl)
      (unless first-section-seen
        (set! first-section-seen #t)
        (show #t *raw-prefix* "=" nl))
      (show #t *raw-prefix* (tbold "VALUE") "#" (tbold "DERIVED VALUE") nl)
      (loop for d in derived do (process-derived-raw-ms d))
      (show #t *raw-prefix* nl))

    (when-items-in-node (attributes "attributes" entity)
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

    (when-items-in-node (defects "defects" entity)
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
        (show #t *raw-prefix* "#" (tbold (points->string defects-total)) "#"
              (tbold "DEFECTS TOTAL") nl))
      (show #t *raw-prefix* nl))

    (when-items-in-node (skills "skills" entity)
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
      (show #t *raw-prefix* "#" (tbold (points->string entity-total)) "#"
            (tbold "TOTAL") nl))
    (show #t *raw-prefix* "=" nl)
    (show #t *raw-prefix* ".TE" nl)
    ))


;; Processes every "---"-separated document that stream yields, in
;; order, threading entity-no across all of them so entity numbering is
;; per *file* (or per stdin stream), not restarted at 1 for each
;; document -- matches besm2_fmt's own Process_One/Process_Entities
;; split (Ada, ~/Repos/RPG/Tools/besm2_fmt). Reading only the first
;; document via document-parse-port, as this file used to, silently
;; dropped every document after the first on multi-document input --
;; see besm2_fmt's PERFORMANCE-COMPARISON.md, "The multi-document file
;; exposes a real split within the besm2-rst family" for how that was
;; found and confirmed (this file's own process-file, not a slibfyaml
;; limitation: (slibfyaml documents streams) already provided this).
(define (process-document-stream stream)
  (let ((entity-no 0))
    (let loop-docs ()
      (when (document-stream-has-next? stream)
        (with-document (doc (document-stream-next! stream))
          (let ((entities (node-items (document-root doc))))
            (loop for entity in entities
                  do (set! entity-no (+ entity-no 1))
                  ;; node-boolean-value's own optional (map, key, default)
                  ;; form already returns the actual #t/#f/absent value --
                  ;; "mecha: false" turns mecha mode OFF, same distinction
                  ;; may-exist used to preserve against a bare assoc/
                  ;; node-has-key?.
                  do (parameterize ((mecha? (node-boolean-value entity "mecha" #f)))
                       (*output-formatter* entity entity-no)))))
        (loop-docs)))))

;; Shared by process-file/process-filename: reports a load/parse error
;; the same way for both (instead of aborting the whole run) rather
;; than duplicating the handler, then streams every document
;; open-stream's thunk yields.
(define (process-stream open-stream)
  (handle-exceptions exn
      (begin
        (show (current-error-port) "Error while trying to load YAML input from " (yaml-input-filename) nl)
        (print-error-message exn (current-error-port)))
    (with-document-stream (stream (open-stream))
      (process-document-stream stream))))

;; slibfyaml has no port-based streaming constructor (only
;; document-stream-open-string/-open-file), so stdin's whole content is
;; read into memory first -- the same tradeoff besm2_fmt's own Ada port
;; makes for its stdin case (Read_All_Standard_Input + Open_String).
(define (process-file)
  (process-stream
   (lambda () (document-stream-open-string (read-string #f (current-input-port))))))

(define yaml-input-filename (make-parameter "(stdin)"))

;; Streams directly from the named file -- no need to read it into
;; memory first the way process-file's stdin case must.
(define (process-filename filename)
  (parameterize ((yaml-input-filename filename))
    (process-stream (lambda () (document-stream-open-file filename)))))

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

(define *bold-head* #t)
(define *bolding* #f)
(define *italicizing* #f)
(define *debugging* #f)
;; Use an em-dash to separate the attribute name and level in terse mode.
(define *em-dash* #f)
(define *head-sep* #\=)
(define *hmm-output* #f)
(define *hmm-depth* (make-parameter 0))
(define *hmm-root* #f)                  ; Don't output root if #f.
(define *hmm-separate* #f)              ; Default to not outputing subitems as separate h-m-m nodes.
(define *level* #f)
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
;; Use Unicode MINUS SIGN (U+2212) instead of ASCII hyphen-minus for
;; negative numbers this program builds itself (defect points, and
;; enhancement/limiter signs in format-customizers).
(define *unicode-minus* #f)
(define *underliner* #\-)
(define *subunderliner* #f)
(define *page-after-description* #f)

(define +command-line-options+
  (list (args:make-option
         (|1| one) #:none "Use only one table."
         (dbg (dfmt "one only" nl))
         (set! *one-table* #t)
         ;; Having multiple header separator lines doesn't cause pandoc to
         ;; complain, but it does make the first line a header which looks
         ;; different in HTML.
         (set! *head-sep* #\-))
        (args:make-option
          (B no-bold-head) #:none (show #f "Turns OFF bolding of headers in plain reST output.")
          (set! *bold-head* #f))
        (args:make-option
         (b bold) #:none (show #f
                                  "Turn on bolding of names and levels of
                          attributes, defects, and skills in terse mode.
                          Overrides italicizing (-i/--italics).")
         (set! *bolding* #t))
        ;; c is reserved for raw ConTeXt output.
        (args:make-option
         (D omit-description) #:none "Omit the entiy description."
         (set! *omit-entity-description* #t))
        (args:make-option
         (d debug) #:none "Turn on debugging."
         (set! *debugging* #t))
        (args:make-option
         (H hmm) #:none "Output in h-m-m format."
         (set! *output-formatter* process-entity-hmm)
         (set! *hmm-output* #t))
        (args:make-option
         (L hmm-depth) #:required "Depth (Level) of h-m-m output."
         (*hmm-depth* (string->number arg)))
        (args:make-option
         (R hmm-root) #:required "Text for root node of h-m-m output."
         (set! *hmm-root* arg))
        (args:make-option
         (S hmm-separate) #:none "Output subitems as separate h-m-m nodes."
         (set! *hmm-separate* #t))
        (args:make-option
         (h help) #:none "Display this text."
         (usage))
        (args:make-option
         (i italics) #:none (show #f
                                  "Turn on italicizing of names and levels of
                          attributes and defects in terse mode.")
         (set! *italicizing* #t))
        (args:make-option
         (l level) #:none "Output the word \"Level\" before the level number
                          in terse mode."
         (set! *level* #t))
        (args:make-option
         (M em-dash) #:none "Separate the attribute name and the level with
                          an em dash in terse mode."
         (set! *em-dash* #t))
        (args:make-option
         (m raw-ms-tables) #:none "Use groff tbl output in a raw ms block."
         (set! *output-formatter* process-entity-raw-ms))
        (args:make-option
         (n unicode-minus) #:none "Use Unicode MINUS SIGN (U+2212) instead
                          of ASCII hyphen-minus for negative numbers."
         (set! *unicode-minus* #t))
        (args:make-option
         (o output) #:required "Output file."
         (set! *output-file* arg))
        (args:make-option
         (p page) #:none "Page after description.  (Only for ms output!)"
         (set! *page-after-description* #t))
        (args:make-option
         (s subtotals) #:none
         "Show subtotals for stats, attributes, and defects."
         (set! *show-subtotals* #t))
        (args:make-option
         (t terse) #:none "Use terse output."
         (set! *output-formatter* process-entity-terse))
        (args:make-option
         (U subunderliner) #:required
         "Entities after the first are subentities,
                          and use a different character for
                          underlining the subheader."
         (set! *subunderliner* (string-ref arg 0)))
        (args:make-option
         (u underliner) #:required
         "Character to use for underlining the header."
         (set! *underliner* (string-ref arg 0)))
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

    (when (and *hmm-output* *hmm-root*)
      (show #t (indent) *hmm-root* nl))

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
