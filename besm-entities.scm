;;;!> besm-entities.scm -- Shared entity data model and reST/terse/h-m-m/
;;;   raw-ms formatters for besm2-rst-e.scm and besm2-rst-f-e.scm.
;;;
;;; Each entity (character/template/item) is decoded exactly once into the
;;; <entity> record below by a `load-entity` that knows how to walk its own
;;; program's YAML representation -- besm2-rst-e.scm's load-entity walks a
;;; plain alist (from the yaml egg or (slibfyaml scheme)); besm2-rst-f-e
;;; .scm's load-entity walks a slibfyaml node/tree handle. Everything
;;; downstream of that -- the four process-entity-* formatters below, and
;;; the output-configuration parameters they read -- has no idea which
;;; loader built the record, and so is identical between the two programs.
;;;
;;; This mirrors BESM2_Fmt.Entities / BESM2_Fmt.Format_* in the author's
;;; Ada besm2_fmt port of this program, which introduced this same "decode
;;; once into a record, format many times" split against a single
;;; (libfyaml) loader -- see besm2_fmt-entities.ads's header comment. Here
;;; there are two loaders rather than one, so `load-entity` itself can't be
;;; shared the way Ada's Load_Entity is, but everything past it can be, and
;;; even each program's own two loading paths collapse: besm2-rst-e.scm's
;;; -f/--fyaml flag still exists, but now only selects which egg builds the
;;; alist `load-entity` walks, since (slibfyaml scheme) and the yaml egg
;;; both hand back the same alist/list/scalar shape.
;;;
;;; What moved here, relative to besm2-rst.scm/besm2-rst-f.scm:
;;; - The record types themselves (stat/derived/attribute/defect/skill/
;;;   entity), replacing per-formatter re-derivation of the same fields
;;;   from a raw alist or node on every access.
;;; - format-customizers and make-attribute-details, unchanged: both
;;;   already operated on plain decoded Scheme values (strings, numbers,
;;;   lists), not on alists or nodes directly, so they need no changes,
;;;   only relocation.
;;; - total-points-of / sort-by-name-ci: small generic helpers factoring
;;;   out the "sum a list of records' points field" / "sort a list of
;;;   records case-insensitively by name field" patterns each loader needs
;;;   once per category (stats/attributes/defects/skills).
;;; - dbg/dfmt/die and *debugging*: needed both by the formatters here and
;;;   by each program's own CLI option handling (e.g. the -1/--one handler
;;;   logs via dbg), so defined once and exported rather than duplicated.
;;; - Every other command-line-configurable behavior flag (*bolding*,
;;;   *table-width*, *unicode-minus*, etc.) and `mecha?`: these are read
;;;   only by the formatters below, but *set* by each program's CLI option
;;;   handlers in a different module, so they're make-parameter objects
;;;   (like `mecha?` and `*hmm-depth*` already were in besm2-rst.scm)
;;;   rather than plain mutable globals -- calling a parameter object as a
;;;   procedure works cleanly across a module boundary; `set!`-ing an
;;;   imported plain variable does too, but is flagged with a compiler
;;;   warning at every call site, confirmed while prototyping this file.
;;; - The four process-entity-* formatters and their per-item
;;;   (process-stat/-derived/-attribute/-defect/-skill, and their -terse/
;;;   -hmm/-raw-ms siblings) helpers, rewritten to read already-decoded
;;;   record fields instead of calling must-exist/may-exist or a node-*
;;;   accessor at every field access.
;;;
;;; Two behavioral notes from this refactor, both a byproduct of moving to
;;; "decode once, up front" rather than "decode while formatting":
;;;
;;; 1. Debug (`dbg`/`dfmt`) calls that used to pretty-print a whole raw
;;;    alist/node now print just the item's name (or "(unnamed)" for an
;;;    entity without a "name" field) -- cheap and meaningful for a record,
;;;    and avoids relying on `pretty` having sensible behavior on a
;;;    record, or a node-path call needing a live node handle that may no
;;;    longer exist by formatting time (see note 2).
;;; 2. Each program's process-file now loads *all* of a file's entities
;;;    into plain records before formatting any of them (`map load-entity
;;;    ...` up front, rather than looping load-and-immediately-format one
;;;    at a time as besm2-rst.scm/besm2-rst-f.scm do). A malformed later
;;;    entity is therefore caught before any output for *any* entity in
;;;    that file has been printed, rather than aborting mid-stream after
;;;    earlier entities' output has already gone out -- a small, deliberate
;;;    improvement, not something to change silently without mention.
;;;
;;; Field-level design notes (why some things are pre-computed once and
;;; others are deliberately left for the formatters to finish):
;;;
;;; - attribute-level already has any "effective" value folded in
;;;   (" (effective)"), and attribute-details is already the fully-combined
;;;   result of make-attribute-details -- identical in all four backends
;;;   today, so computed once in each load-entity's load-attribute.
;;; - entity-tagline is already string-trim-both'd -- identical in all
;;;   four backends (matches Ada's Load_Entity, which pre-trims the same
;;;   way). entity-description and entity-size are stored as-is: no
;;;   backend trims them.
;;; - defect-details is already string-trim-both'd, but each backend still
;;;   applies its own final touch at format time (all-one-line for -hmm,
;;;   space-to-newline for -raw-ms) -- that finishing step is genuinely
;;;   output-format-specific, not a loading concern.
;;; - derived-name is stored RAW (e.g. "ACV"), *not* expanded via
;;;   derived-abbreviations: only process-derived (plain) and
;;;   process-derived-raw-ms expand abbreviations in the original code --
;;;   process-derived-terse/-hmm show the raw name. This is a real, existing
;;;   asymmetry in besm2-rst.scm's behavior, preserved here by keeping
;;;   derived-abbreviations and its lookup at format time, in exactly the
;;;   two backends that used it, rather than promoting it into load-derived
;;;   where it would incorrectly apply to all four.
;;; - derived-alternatives/skill-specialisations are stored as raw string
;;;   lists: each backend joins/wraps them into its own final shape (grid
;;;   columns vs. an inline parenthetical), so joining once at load time
;;;   would just have to be undone.
;;; - stats-total/attributes-total/defects-total/skills-total/entity-total
;;;   are computed once in load-entity (via total-points-of) instead of
;;;   separately by process-entity's inline `loop ... sum` and by
;;;   process-entity-terse/-hmm's now-removed `total-points` calls on the
;;;   same lists -- one computation, used by all four backends.
;;; - attributes/defects/skills are sorted case-insensitively by name once
;;;   in load-entity (via sort-by-name-ci), replacing each backend's own
;;;   `(sort xs name-ci<?)` at format time.

(declare (unit besm-entities))

(module besm-entities
  (
   ;; Records.
   make-stat stat? stat-name stat-value stat-points
   make-derived derived? derived-name derived-value derived-alternatives
   make-attribute attribute? attribute-name attribute-level
   attribute-points attribute-details
   make-defect defect? defect-name defect-points defect-details
   make-skill skill? skill-name skill-level skill-points
   skill-specialisations
   make-entity entity? entity-name entity-tagline entity-description
   entity-size entity-mecha? entity-stats entity-derived
   entity-attributes entity-defects entity-skills
   entity-stats-total entity-attributes-total entity-defects-total
   entity-skills-total entity-entity-total

   ;; Loader helpers shared by both programs' load-entity.
   make-attribute-details total-points-of sort-by-name-ci

   ;; Debug/error helpers.
   dbg dfmt die *debugging*

   ;; Output configuration -- set by each program's CLI option handlers,
   ;; read only here.
   mecha? *bold-head* *bolding* *italicizing* *em-dash* *head-sep*
   *hmm-depth* *hmm-separate* *level* *num-width* *one-table*
   *omit-entity-description* *show-subtotals* *table-width*
   *unicode-minus* *underliner* *subunderliner* *page-after-description*

   ;; Formatters.
   process-entity process-entity-terse process-entity-hmm
   process-entity-raw-ms

   ;; Small utilities each program's main()/CLI handling also needs.
   indent make-tabs
   )

(import (scheme))
(import (chicken base))
(import (chicken port))
(import (chicken process-context))
(import (chicken sort))
(import (chicken string))
(import (chicken pretty-print))

(import (schemepunk show))
(import loop)
(import matchable)
(import (srfi 1))
(import (srfi 152))

;;; ------------------------------------------------------------------
;;; Debug/error helpers.
;;; ------------------------------------------------------------------

(define *debugging* (make-parameter #f))

(define-syntax dbg
  (syntax-rules ()
    ((_ e1 e2 ...)
     (when (*debugging*)
       e1 e2 ...
       (flush-output (current-error-port))))))

(define (dfmt . args)
  (apply show (cons (current-error-port) args)))

(define (die status . args)
  (show (current-error-port) (program-name) ": ")
  (apply show (cons (current-error-port) args))
  (show (current-error-port) "\n")
  (exit status))

;;; ------------------------------------------------------------------
;;; Output configuration. All make-parameter, not plain mutable globals,
;;; so each program's CLI option handlers can set them from a different
;;; module without a cross-module `set!` warning at every call site.
;;; ------------------------------------------------------------------

(define mecha? (make-parameter #f))

(define *bold-head* (make-parameter #t))
(define *bolding* (make-parameter #f))
(define *italicizing* (make-parameter #f))
;; Use an em-dash to separate the attribute name and level in terse mode.
(define *em-dash* (make-parameter #f))
(define *head-sep* (make-parameter #\=))
(define *hmm-depth* (make-parameter 0))
;; Default to not outputting subitems as separate h-m-m nodes.
(define *hmm-separate* (make-parameter #f))
(define *level* (make-parameter #f))
(define *num-width* (make-parameter (max
                                      (string-length "LEVEL")
                                      (string-length "VALUE")
                                      (string-length "POINTS"))))
(define *one-table* (make-parameter #f))
(define *omit-entity-description* (make-parameter #f))
(define *show-subtotals* (make-parameter #f))
(define *table-width* (make-parameter 60))
;; Use Unicode MINUS SIGN (U+2212) instead of ASCII hyphen-minus for
;; negative numbers this program builds itself (defect points, and
;; enhancement/limiter signs in format-customizers).
(define *unicode-minus* (make-parameter #f))
(define *underliner* (make-parameter #\-))
(define *subunderliner* (make-parameter #f))
(define *page-after-description* (make-parameter #f))

;;; ------------------------------------------------------------------
;;; Records.
;;; ------------------------------------------------------------------

(define-record-type <stat>
  (make-stat name value points)
  stat?
  (name stat-name)
  (value stat-value)
  (points stat-points))

;; alternatives: #f or a list of strings. name is the RAW YAML name (e.g.
;; "ACV") -- see the header comment on derived-abbreviations below for why
;; the abbreviation expansion is not done here.
(define-record-type <derived>
  (make-derived name value alternatives)
  derived?
  (name derived-name)
  (value derived-value)
  (alternatives derived-alternatives))

;; level already has any "effective" value folded in; details is already
;; the finished result of make-attribute-details (#f or a string).
(define-record-type <attribute>
  (make-attribute name level points details)
  attribute?
  (name attribute-name)
  (level attribute-level)
  (points attribute-points)
  (details attribute-details))

;; details is already string-trim-both'd; #f if absent.
(define-record-type <defect>
  (make-defect name points details)
  defect?
  (name defect-name)
  (points defect-points)
  (details defect-details))

;; specialisations: #f or a list of strings.
(define-record-type <skill>
  (make-skill name level points specialisations)
  skill?
  (name skill-name)
  (level skill-level)
  (points skill-points)
  (specialisations skill-specialisations))

;; name/tagline/description/size: #f if the corresponding YAML key is
;; absent. tagline is already string-trim-both'd. stats/derived are #f or
;; a list, in document order; attributes/defects/skills are #f or a list,
;; sorted case-insensitively by name (sort-by-name-ci, in each program's
;; load-entity). *-total fields are pre-summed (total-points-of).
(define-record-type <entity>
  (make-entity name tagline description size mecha?
               stats derived attributes defects skills
               stats-total attributes-total defects-total skills-total
               entity-total)
  entity?
  (name entity-name)
  (tagline entity-tagline)
  (description entity-description)
  (size entity-size)
  (mecha? entity-mecha?)
  (stats entity-stats)
  (derived entity-derived)
  (attributes entity-attributes)
  (defects entity-defects)
  (skills entity-skills)
  (stats-total entity-stats-total)
  (attributes-total entity-attributes-total)
  (defects-total entity-defects-total)
  (skills-total entity-skills-total)
  (entity-total entity-entity-total))

;;; ------------------------------------------------------------------
;;; Loader helpers shared by both programs' load-entity.
;;; ------------------------------------------------------------------

;; #f if items is #f (category absent from the entity); otherwise the sum
;; of (accessor item) over items -- e.g. (total-points-of stats stat-points).
(define (total-points-of items accessor)
  (if items (loop for item in items sum (accessor item)) 0))

;; #f if items is #f; otherwise items sorted case-insensitively by
;; (name-accessor item) -- e.g. (sort-by-name-ci attrs attribute-name).
(define (sort-by-name-ci items name-accessor)
  (and items
       (sort items (lambda (a b)
                      (string-ci<? (name-accessor a) (name-accessor b))))))

;;; ------------------------------------------------------------------
;;; Display helpers (unchanged from besm2-rst.scm/besm2-rst-f.scm, apart
;;; from *-parameters now being called rather than referenced bare).
;;; ------------------------------------------------------------------

(define (label-points points)
  (show #f (abs points) (if (mecha?)
                            (if (< points 0) " MBP" " MP")
                            (if (< points 0) " BP" " CP"))))

;; The glyph used for a negative sign wherever this program builds one
;; itself (as opposed to relying on number->string's own "-"): ASCII
;; hyphen-minus by default, or Unicode MINUS SIGN (U+2212) under
;; -n/--unicode-minus.
(define (minus-glyph)
  (if (*unicode-minus*) "−" "-"))

;; points is known negative (defect points always are); render its
;; magnitude with the configured minus glyph in front, instead of relying
;; on number->string's own always-ASCII "-".
(define (negative-number->string points)
  (string-append (minus-glyph) (number->string (abs points))))

;; points might be positive, negative, or zero -- e.g. defects-total
;; (always <= 0) or entity-total (can go negative if defects outweigh the
;; rest). Render it with the configured minus glyph when negative,
;; otherwise plain.
(define (points->string points)
  (if (< points 0) (negative-number->string points) (number->string points)))

(define (space-to-newline s)
  (string-map (lambda (c) (if (char=? c #\newline) #\space c)) s))

;; Always bold. A formatter that works with SRFI 166: Monadic Formatting,
;; which means it has to be used within the arguments to a call to show.
(define (bold . args)
  (each "**" (each-in-list args) "**"))

(define (italics . args)
  (each "*" (each-in-list args) "*"))

;; Only bold if the -B/--bold-head command line option has been set. This
;; is for bolding in headers in reST table output.
(define (hbolding . args)
  (if (*bold-head*)
      (each "**" (each-in-list args) "**")
      (each-in-list args)))

;; Only bold if the command line option for bolding has been set. This
;; generally happens in headers in reST table output and in attribute and
;; defect names and levels in terse mode. This is a formatter that works
;; with SRFI 166: Monadic Formatting, which means it has to be used within
;; the arguments to a call to the show procedure.
(define (bolding . args)
  (if (*bolding*)
      (bold (each-in-list args))
      (each-in-list args)))

(define (italicizing . args)
  (if (*italicizing*)
      (italics (each-in-list args))
      (each-in-list args)))

(define (emphasizing . args)
  (cond
   ((*bolding*)     (bold (each-in-list args)))
   ((*italicizing*) (italics (each-in-list args)))
   (else            (each-in-list args))))

(define (text . args)
  (show #f (each-in-list args)))

(define (separator-line num-columns sep c)
  ;; The last column is the description, and absorbs any unused space.
  (loop for i from 1 to (- num-columns 1)
        do (show #t  sep (with ((pad-char c)) (padded (*num-width*)))))
  (show #t (with ((pad-char c))
             sep (padded (- (*table-width*) 1 1 (* (- num-columns 1)
                                                    (+ (*num-width*) 1)))) sep)
       nl))

(define (sep1)
  (separator-line 1 #\+ #\-))

(define (empty)
  (show #t (with ((width (*table-width*)))
             (columnar "|" (displayed " ") "|"))))

(define (row2 col1 col2)
  (dbg (dfmt "row2: col1: " (written col1) " col2: " (written col2) nl))
  (show #t (with ((width (*table-width*)))
             (columnar "|" (*num-width*) (displayed col1)
                       "|" (wrapped col2)
                       "|"))))

(define (headsep2)
  (separator-line 2 #\+ (*head-sep*)))

(define (sep2)
  (separator-line 2 #\+ #\-))

(define (row3 col1 col2 col3)
  (show #t (with ((width (*table-width*)))
             (columnar "|" (*num-width*) (displayed col1)
                       "|" (*num-width*) (displayed col2)
                       "|" (wrapped (displayed col3))
                       "|"))))
(define (headsep3)
  (separator-line 3 #\+ (*head-sep*)))

(define (sep3)
  (separator-line 3 #\+ #\-))

(define-syntax depth+
  (syntax-rules ()
    ((_ e1 e2 ...) (parameterize ((*hmm-depth* (+ (*hmm-depth*) 1))) e1 e2 ...))))

(define (make-tabs n)
  (make-string n #\tab))

(define (indent)
  (make-tabs (*hmm-depth*)))

(define (all-one-line s)
  (string-join (string-split s "\n") " "))

(define (tbold s)                       ; Troff bold.
  (cond
   ((string-null? s) s)
   (else (string-append "\\fB" s "\\fP"))))

(define (titalics s)                     ; Troff italics.
  (cond
   ((string-null? s) s)
   (else (string-append "\\fI" s "\\fP*"))))

(define *raw-prefix* "   ")

;;; ------------------------------------------------------------------
;;; format-customizers / make-attribute-details -- unchanged from
;;; besm2-rst.scm/besm2-rst-f.scm: both already operate on plain decoded
;;; Scheme values (strings, numbers, lists), never on an alist or node
;;; directly, so no changes are needed here, only relocation.
;;; ------------------------------------------------------------------

(define (format-customizers items type)
  ;; Note that the Attack Helicopter, BESM 4E p. 217, shows enhancements
  ;; as negative and limiters as positive.
  ;;
  ;; When I display or print the PDFs on my Unix boxes, the spacing
  ;; between Unicode MINUS SIGN and the following number is off, probably
  ;; due to bugs in the rendering software. The spacing is fine on macOS.
  ;; So use Unicode HYPEN-MINUS, ASCII -, code 45, despite it looking bad
  ;; typographically. -n/--unicode-minus overrides this and uses Unicode
  ;; MINUS SIGN instead.
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

;;; ------------------------------------------------------------------
;;; derived-abbreviations. Deliberately consulted only by process-derived
;;; (plain) and process-derived-raw-ms below, matching besm2-rst.scm's
;;; existing (asymmetric) behavior: process-derived-terse/-hmm show the
;;; raw derived-name, uunexpanded. See this file's header comment.
;;; ------------------------------------------------------------------

(define derived-abbreviations
  '(("ACV" . "Attack Combat Value")
    ("DCV" . "Defence Combat Value")
    ("DM"  . "Damage Multiplier")
    ("HP"  . "Health Points")
    ("EP"  . "Energy Points")
    ("SV"  . "Shock Value")
    ("AR"  . "Armour Rating")))

(define (expand-derived-abbreviation name)
  (let ((expansion (assoc name derived-abbreviations)))
    (if expansion (cdr expansion) name)))

;;; ------------------------------------------------------------------
;;; Plain (grid-table) output.
;;; ------------------------------------------------------------------

(define (process-stat stat)
  (dbg (dfmt "process-stat: " (stat-name stat) nl))
  (row3 (stat-value stat) (stat-points stat) (stat-name stat))
  (stat-points stat))

(define (process-derived derived)
  (dbg (dfmt "process-derived: " (derived-name derived) nl))
  ;; There are no points.
  (let* ((name         (expand-derived-abbreviation (derived-name derived)))
         (alternatives (derived-alternatives derived))
         (description  (if alternatives
                           (string-append name " ("
                                          (string-join alternatives ", ")
                                          ")")
                           name)))
    (row2 (derived-value derived) description)))

(define (process-attribute attribute)
  (dbg (dfmt "process-attribute: " (attribute-name attribute) nl))
  ;; returns the cost of the attribute
  (let* ((details     (attribute-details attribute))
         (description (if details
                          (show #f (attribute-name attribute) " (" details ")")
                          (attribute-name attribute))))
    (row3 (attribute-level attribute) (attribute-points attribute) description)
    (attribute-points attribute)))

(define (process-defect defect)
  (dbg (dfmt "process-defect: " (defect-name defect) nl))
  ;; Returns the cost of the defect
  (let* ((details     (defect-details defect))
         (description (if details
                          (show #f (defect-name defect) " (" details ")")
                          (defect-name defect))))
    ;; Defect points are negative, but in rst that starts an itemized list,
    ;; so we put a backslash before them to quote them.
    (row3 "" (string-append "\\" (negative-number->string (defect-points defect)))
          description)
    (defect-points defect)))

(define (process-skill skill)
  (dbg (dfmt "process-skill: " (skill-name skill) nl))
  ;; Returns the cost of the skill.
  (let* ((specialisations (skill-specialisations skill))
         (description     (show #f (skill-name skill)
                                (if specialisations
                                    (string-append
                                     " (" (string-join specialisations ", ") ")")
                                    ""))))
    (row3 (skill-level skill) (skill-points skill) description)
    (skill-points skill)))

(define (process-entity entity entity-no)
  (dbg (dfmt "process-entity: " (or (entity-name entity) "(unnamed)") nl))
  ;; It might be a template, an item, or a full character.

  (when (entity-name entity)
    (let ((underline (make-string (string-length (entity-name entity))
                                  (if (and (> entity-no 1)
                                           (*subunderliner*))
                                      (*subunderliner*)
                                      (*underliner*)))))
      (show #t (entity-name entity) nl underline nl nl)))

  (when (entity-tagline entity)
    (show #t (italics (entity-tagline entity)) nl nl))

  (unless (*omit-entity-description*)
    (when (entity-description entity)
      (show #t (entity-description entity) nl nl)))

  (when (entity-size entity)
    (show #t (bold "Size:") " " (entity-size entity) nl nl))

  (when (entity-stats entity)
    (sep3)
    (row3 (text (hbolding "VALUE"))
          (text (hbolding "POINTS"))
          (text (hbolding "STAT")))
    (headsep3)
    (loop for stat in (entity-stats entity) do (process-stat stat) do (sep3))
    (when (*show-subtotals*)
      (row3 "" (text (hbolding (number->string (entity-stats-total entity))))
            (text (hbolding "STATS TOTAL")))
      (sep3))
    (cond ((*one-table*) (empty))
          (else (show #t nl))))

  (when (entity-derived entity)
    (sep2)
    (row2 (text (hbolding "VALUE")) (text (hbolding "DERIVED VALUE")))
    (headsep2)
    (loop for d in (entity-derived entity) do (process-derived d) do (sep2))
    (cond ((*one-table*) (empty))
          (else (show #t nl))))

  (when (entity-attributes entity)
    (sep3)
    (row3 (text (hbolding "LEVEL"))
          (text (hbolding "POINTS"))
          (text (hbolding "ATTRIBUTE")))
    (headsep3)
    (loop for attribute in (entity-attributes entity)
          do (process-attribute attribute) do (sep3))
    (when (*show-subtotals*)
      (row3 "" (text (hbolding (number->string (entity-attributes-total entity))))
            (text (hbolding "ATTRIBUTES TOTAL")))
      (sep3))
    (cond ((*one-table*) (empty))
          (else (show #t nl))))

  (when (entity-defects entity)
    (sep3)
    (row3 "" (text (hbolding "POINTS")) (text (hbolding "DEFECT")))
    (headsep3)
    (loop for defect in (entity-defects entity)
          do (process-defect defect) do (sep3))
    (when (*show-subtotals*)
      (row3 "" (text (hbolding (points->string (entity-defects-total entity))))
            (text (hbolding "DEFECTS TOTAL")))
      (sep3))
    (cond ((*one-table*) (empty))
          (else (show #t nl))))

  (when (entity-skills entity)
    (sep3)
    (row3 (text (hbolding "LEVEL"))
          (text (hbolding "POINTS"))
          (text (hbolding "SKILL")))
    (headsep3)
    (loop for skill in (entity-skills entity) do (process-skill skill) do (sep3))
    (row3 "" (text (hbolding (number->string (entity-skills-total entity))))
          (text (hbolding "SKILL POINTS TOTAL")))
    (sep3)
    (cond ((*one-table*) (empty))
          (else (show #t nl))))

  ;; Output total.
  (sep3)
  (row3 "" (text (hbolding (points->string (entity-entity-total entity))))
        (text (hbolding "TOTAL")))
  (sep3)
  (show #t nl))

;;; ------------------------------------------------------------------
;;; Terse output.
;;; ------------------------------------------------------------------

(define (process-stat-terse stat)
  (show #t (stat-name stat) " " (stat-value stat)
        " (" (label-points (stat-points stat)) ")"))

(define (process-derived-terse derived)
  (dbg (dfmt "process-derived-terse: " (derived-name derived) nl))
  ;; There are no points. Note: unlike process-derived/-raw-ms, this does
  ;; NOT expand derived-abbreviations -- see this file's header comment.
  (let* ((alternatives (derived-alternatives derived))
         (alternatives (if alternatives
                           (string-append " ("
                                          (string-join alternatives ", ")
                                          ")")
                           #f)))
    (show #t (derived-name derived) " " (displayed (derived-value derived))
          (if alternatives alternatives ""))))

(define (process-attribute-terse attribute)
  (dbg (dfmt "process-attribute-terse: " (attribute-name attribute) nl))
  (let ((details (attribute-details attribute)))
    (show #t (emphasizing (attribute-name attribute)
                          (if (*em-dash*) " — " " ")
                          (if (*level*) "Level " "") (attribute-level attribute)) " ("
          (if details (string-append details ". ") "")
          (label-points (attribute-points attribute)) ")")))

(define (process-defect-terse defect)
  (dbg (dfmt "process-defect-terse: " (defect-name defect) nl))
  (let ((details (defect-details defect)))
    (show #t (emphasizing (defect-name defect)) " ("
          (if details (string-append details ".  ") "")
          (label-points (defect-points defect)) ")")))

(define (process-skill-terse skill)
  (dbg (dfmt "process-skill-terse: " (skill-name skill) nl))
  (let ((specialisations (skill-specialisations skill)))
    (show #t (emphasizing (skill-name skill)
                          (if (*em-dash*) " — " " ")
                          (if (*level*) "Level " "") (skill-level skill)) " ("
          (if specialisations
              (string-append (string-join specialisations ", ") ".  ")
              "")
         (displayed (skill-points skill)) " SP)")))

(define (process-entity-terse entity entity-no)
  (dbg (dfmt "process-entity-terse: " (or (entity-name entity) "(unnamed)") nl))
  ;; It might be a template, an item, or a full character.

  (cond ((entity-name entity)
         (let* ((entity-header (show #f (entity-name entity) " ("
                                     (label-points (entity-entity-total entity)) ")"))
                (underline (make-string (string-length entity-header)
                                        (if (and (> entity-no 1)
                                                 (*subunderliner*))
                                            (*subunderliner*)
                                            (*underliner*)))))
           (show #t entity-header nl underline nl nl)))
        (else
         (show #t (label-points (entity-entity-total entity)) nl nl)))

  (when (entity-tagline entity)
    (show #t (italics (entity-tagline entity)) nl nl))

  (when (and (entity-description entity) (not (*omit-entity-description*)))
    (show #t (entity-description entity) nl nl)
    (when (*page-after-description*)
      (show #t ".. raw:: ms" nl nl "   .bp" nl nl)))

  (when (entity-size entity)
    (show #t (bold "Size:") " " (entity-size entity) nl nl))

  (when (entity-stats entity)
    (show #t (bold "Statistics"))
    (when (*show-subtotals*)
      (show #t " (" (label-points (entity-stats-total entity)) ") "))
    (show #t " — " nl)
    (loop for stat in (entity-stats entity)
          for i from 1
          when (> i 1) do (show #t ", ")
          do (process-stat-terse stat))
    (show  #t nl nl))

  (when (entity-derived entity)
    (show #t (bold "Derived Values") " — ")
    (loop for d in (entity-derived entity)
          for i from 1
          when (> i 1) do (show #t ", ")
          do (process-derived-terse d))
    (show #t nl nl))

  (when (entity-attributes entity)
    (if (mecha?)
        (show #t (bold "Mecha Sub-Attributes"))
        (show  #t (bold "Attributes")))
    (when (*show-subtotals*)
      (show #t " (" (label-points (entity-attributes-total entity)) ")"))
    (show #t " — " nl)
    (loop for attribute in (entity-attributes entity)
          for i from 1
          when (> i 1) do (show #t ", ")
          do (process-attribute-terse attribute))
    (show #t nl nl))

  (when (entity-defects entity)
    (if (mecha?)
        (show #t (bold "Mecha Defects"))
        (show #t (bold "Defects")))
    (when (*show-subtotals*)
      (show #t " (" (label-points (entity-defects-total entity)) ")"))
    (show #t " — " nl)
    (loop for defect in (entity-defects entity)
          for i from 1
          when (> i 1) do (show #t ", ")
          do (process-defect-terse defect))
    (show #t nl nl))

  (when (entity-skills entity)
    (show #t (bold "Skills"))
    (when (*show-subtotals*)
      (show #t " (" (displayed (entity-skills-total entity)) " SP)"))
    (show #t " — " nl)
    (loop for skill in (entity-skills entity)
          for i from 1
          when (> i 1) do (show #t ", ")
          do (process-skill-terse skill))
    (show #t nl nl)))

;;; ------------------------------------------------------------------
;;; h-m-m output.
;;; ------------------------------------------------------------------

(define (process-stat-hmm stat)
  (show #t (stat-name stat) " " (stat-value stat)
        " (" (label-points (stat-points stat)) ")"))

(define (process-derived-hmm derived)
  (dbg (dfmt "process-derived-hmm: " (derived-name derived) nl))
  ;; There are no points. Not abbreviation-expanded -- see header comment.
  (let* ((alternatives (derived-alternatives derived))
         (alternatives (if alternatives
                           (string-append " ("
                                          (string-join alternatives ", ")
                                          ")")
                           #f)))
    (show #t (derived-name derived) " " (displayed (derived-value derived))
          (if alternatives alternatives ""))))

(define (process-attribute-hmm attribute)
  (dbg (dfmt "process-attribute-hmm: " (attribute-name attribute) nl))
  (let ((details (attribute-details attribute)))
    (show #t (emphasizing (attribute-name attribute)
                          (if (*em-dash*) " — " " ")
                          (if (*level*) "Level " "") (attribute-level attribute)) " ("
                          (if details (all-one-line (string-append details ". ")) "")
                          (label-points (attribute-points attribute)) ")")))

(define (process-defect-hmm defect)
  (dbg (dfmt "process-defect-hmm: " (defect-name defect) nl))
  (let ((details (defect-details defect)))
    (show #t (emphasizing (defect-name defect)) " ("
          (if details (all-one-line (string-append details ".  ")) "")
          (label-points (defect-points defect)) ")")))

(define (process-skill-hmm skill)
  (dbg (dfmt "process-skill-hmm: " (skill-name skill) nl))
  (let ((specialisations (skill-specialisations skill)))
    (show #t (emphasizing (skill-name skill)
                          (if (*em-dash*) " — " " ")
                          (if (*level*) "Level " "")
                          (skill-level skill)) " ("
                          (if specialisations
                              (string-append (string-join specialisations ", ") ".  ")
                              "")
                          (displayed (skill-points skill)) " SP)")))

(define (process-entity-hmm entity entity-no)
  (dbg (dfmt "process-entity-hmm: " (or (entity-name entity) "(unnamed)") nl))
  ;; It might be a template, an item, or a full character.

  (depth+
    (show #t (indent))
    (cond ((entity-name entity)
           (let ((entity-header (show #f (entity-name entity) " ("
                                      (label-points (entity-entity-total entity)) ")")))
             (show #t entity-header nl)))
          (else
           (show #t (label-points (entity-entity-total entity)) nl)))

    (when (entity-tagline entity)
      (depth+
        (show #t (indent) (italics (all-one-line (entity-tagline entity))) nl)))

    (when (and (entity-description entity) (not (*omit-entity-description*)))
      (depth+
        (show #t (indent) (all-one-line (entity-description entity)) " " nl)))

    (when (entity-size entity)
      (depth+
        (show #t (indent) (bold "Size:") " " (entity-size entity) nl)))

    (when (entity-stats entity)
      (depth+
        (show #t (indent) (bold "Statistics"))
        (when (*show-subtotals*)
          (show #t " (" (label-points (entity-stats-total entity)) ") "))
        (show #t nl)
        (depth+
          (show #t (indent))
          (loop for stat in (entity-stats entity)
                for i from 1
                when (> i 1) do (show #t (if (*hmm-separate*) (each nl (indent)) ", "))
                do (process-stat-hmm stat))
          (show  #t nl))))

    (when (entity-derived entity)
      (depth+
        (show #t (indent) (bold "Derived Values") nl)
        (depth+
          (show #t (indent))
          (loop for d in (entity-derived entity)
                for i from 1
                when (> i 1) do (show #t (if (*hmm-separate*) (each nl (indent)) ", "))
                do (process-derived-hmm d))
          (show #t nl))))

    (when (entity-attributes entity)
      (depth+
        (show #t (indent))
        (if (mecha?)
            (show #t (bold "Mecha Sub-Attributes"))
            (show #t (bold "Attributes")))
        (when (*show-subtotals*)
          (show #t " (" (label-points (entity-attributes-total entity)) ")"))
        (show #t nl)
        (depth+
          (show #t (indent))
          (loop for attribute in (entity-attributes entity)
                for i from 1
                when (> i 1) do (show #t (if (*hmm-separate*) (each nl (indent)) ", "))
                do (process-attribute-hmm attribute))
          (show #t nl))))

    (when (entity-defects entity)
      (depth+
        (show #t (indent))
        (if (mecha?)
            (show #t (bold "Mecha Defects"))
            (show #t (bold "Defects")))
        (when (*show-subtotals*)
          (show #t " (" (label-points (entity-defects-total entity)) ")"))
        (show #t nl)
        (depth+
          (show #t (indent))
          (loop for defect in (entity-defects entity)
                for i from 1
                when (> i 1) do (show #t (if (*hmm-separate*) (each nl (indent)) ", "))
                do (process-defect-hmm defect))
          (show #t nl))))

    (when (entity-skills entity)
      (depth+
        (show #t (indent) (bold "Skills"))
        (when (*show-subtotals*)
          (show #t " (" (displayed (entity-skills-total entity)) " SP)"))
        (show #t nl)
        (depth+
          (show #t (indent))
          (loop for skill in (entity-skills entity)
                for i from 1
                when (> i 1) do (show #t (if (*hmm-separate*) (each nl (indent)) ", "))
                do (process-skill-hmm skill))
          (show #t nl))))))

;;; ------------------------------------------------------------------
;;; raw-ms (groff tbl) output.
;;; ------------------------------------------------------------------

(define (process-stat-raw-ms stat)
  (dbg (dfmt "process-stat-raw-ms: " (stat-name stat) nl))
  (show #t *raw-prefix* (stat-value stat) "#" (stat-points stat) "#"
        (stat-name stat) nl)
  (stat-points stat))

(define (process-derived-raw-ms derived)
  (dbg (dfmt "process-derived-raw-ms: " (derived-name derived) nl))
  ;; There are no points.
  (let* ((name         (expand-derived-abbreviation (derived-name derived)))
         (alternatives (derived-alternatives derived))
         (description  (if alternatives
                           (string-append name " ("
                                          (string-join alternatives ", ")
                                          ")")
                           name)))
    (show #t *raw-prefix* (derived-value derived) "#T{" nl
          *raw-prefix* description nl
          *raw-prefix* "T}" nl)))

(define (process-attribute-raw-ms attribute)
  (dbg (dfmt "process-attribute-raw-ms: " (attribute-name attribute) nl))
  (let* ((details     (attribute-details attribute))
         (description (if details
                          (show #f (attribute-name attribute) " ("
                                (space-to-newline details) ")")
                          (attribute-name attribute))))
    (show #t *raw-prefix* (attribute-level attribute) "#" (attribute-points attribute)
          "#T{" nl
          *raw-prefix* description nl
          *raw-prefix* "T}" nl)
    (attribute-points attribute)))

(define (process-defect-raw-ms defect)
  (dbg (dfmt "process-defect-raw-ms: " (defect-name defect) nl))
  (let* ((details     (defect-details defect))
         (description (if details
                          (show #f (defect-name defect) " ("
                                (space-to-newline details) ")")
                          (defect-name defect))))
    (show #t *raw-prefix* "#" (negative-number->string (defect-points defect)) "#T{" nl
          *raw-prefix* description nl
          *raw-prefix* "T}" nl)
    (defect-points defect)))

(define (process-skill-raw-ms skill)
  (dbg (dfmt "process-skill-raw-ms: " (skill-name skill) nl))
  (let* ((specialisations (skill-specialisations skill))
         (description     (show #f (skill-name skill)
                                (if specialisations
                                    (string-append
                                     " (" (string-join specialisations ", ") ")")
                                    ""))))
    (show #t *raw-prefix* (skill-level skill) "#" (skill-points skill) "#T{" nl
          *raw-prefix* description nl
          *raw-prefix* "T}" nl)
    (skill-points skill)))

(define (process-entity-raw-ms entity entity-no)
  (dbg (dfmt "process-entity-raw-ms: " (or (entity-name entity) "(unnamed)") nl))
  ;; It might be a template, an item, or a full character.
  (let ((paragraph-seen #f)
        (first-section-seen #f))

    (when (entity-name entity)
      (let ((underline (make-string (string-length (entity-name entity))
                                    (if (and (> entity-no 1)
                                             (*subunderliner*))
                                        (*subunderliner*)
                                        (*underliner*)))))
        (show #t (entity-name entity) nl underline nl nl)))

    (when (entity-tagline entity)
      (set! paragraph-seen #t)
      (show #t (italics (entity-tagline entity)) nl nl))

    (unless (*omit-entity-description*)
      (when (entity-description entity)
        (set! paragraph-seen #t)
        (show #t (entity-description entity) nl nl)))

    (when (entity-size entity)
      (set! paragraph-seen #t)
      (show #t (bold "Size:") " " (entity-size entity) nl nl))

    (show #t ".. raw:: ms" nl nl)
    ;; groff output from here to the end of this function.

    (unless paragraph-seen
      (show #t *raw-prefix* ".LP" nl))

    (show #t *raw-prefix* ".TS" nl)
    (show #t *raw-prefix* "tab(#) ;" nl)

    (when (entity-stats entity)
      (set! first-section-seen #t)
      (show #t *raw-prefix* "c c lx ." nl)
      (show #t *raw-prefix* "=" nl)
      (show #t *raw-prefix* (tbold "VALUE") "#" (tbold "POINTS") "#"
            (tbold "STAT") nl)
      (loop for stat in (entity-stats entity) do (process-stat-raw-ms stat))
      (when (*show-subtotals*)
        (show #t *raw-prefix* "#" (tbold (number->string (entity-stats-total entity))) "#"
              (tbold "STATS TOTAL") nl))
      (show #t *raw-prefix* nl))

    (when (entity-derived entity)
      (when first-section-seen
        (show #t *raw-prefix* ".T&" nl))
      (show #t *raw-prefix* "c l sx ." nl)
      (unless first-section-seen
        (set! first-section-seen #t)
        (show #t *raw-prefix* "=" nl))
      (show #t *raw-prefix* (tbold "VALUE") "#" (tbold "DERIVED VALUE") nl)
      (loop for d in (entity-derived entity) do (process-derived-raw-ms d))
      (show #t *raw-prefix* nl))

    (when (entity-attributes entity)
      (when first-section-seen
        (show #t *raw-prefix* ".T&" nl))
      (show #t *raw-prefix* "c c lx ." nl)
      (unless first-section-seen
        (set! first-section-seen #t)
        (show #t *raw-prefix* "=" nl))
      (show #t *raw-prefix* (tbold "LEVEL") "#" (tbold "POINTS") "#"
            (tbold "ATTRIBUTE") nl)
      (loop for attribute in (entity-attributes entity)
            do (process-attribute-raw-ms attribute))
      (when (*show-subtotals*)
        (show #t *raw-prefix* "#" (tbold (number->string (entity-attributes-total entity))) "#"
              (tbold "ATTRIBUTES TOTAL") nl))
      (show #t *raw-prefix* nl))

    (when (entity-defects entity)
      (when first-section-seen
        (show #t *raw-prefix* ".T&" nl))
      (show #t *raw-prefix* "c c lx ." nl)
      (unless first-section-seen
        (set! first-section-seen #t)
        (show #t *raw-prefix* "=" nl))
      (show #t *raw-prefix* "#"
            (tbold "POINTS") "#"
            (tbold "DEFECT") nl)
      (loop for defect in (entity-defects entity)
            do (process-defect-raw-ms defect))
      (when (*show-subtotals*)
        (show #t *raw-prefix* "#" (tbold (points->string (entity-defects-total entity))) "#"
              (tbold "DEFECTS TOTAL") nl))
      (show #t *raw-prefix* nl))

    (when (entity-skills entity)
      (when first-section-seen
        (show #t *raw-prefix* ".T&" nl))
      (show #t *raw-prefix* "c c lx ." nl)
      (unless first-section-seen
        (set! first-section-seen #t)
        (show #t *raw-prefix* "=" nl))
      (show #t *raw-prefix* (tbold "LEVEL") "#" (tbold "POINTS") "#"
            (tbold "SKILL") nl)
      (loop for skill in (entity-skills entity) do (process-skill-raw-ms skill))
      (show #t *raw-prefix* "#" (tbold (number->string (entity-skills-total entity))) "#"
            (tbold "SKILL POINTS TOTAL") nl)
      (show #t *raw-prefix* nl))

    ;; Output total.
    (when (> (entity-entity-total entity) 0)
      (show #t *raw-prefix* "#" (tbold (points->string (entity-entity-total entity))) "#"
            (tbold "TOTAL") nl))
    (show #t *raw-prefix* "=" nl)
    (show #t *raw-prefix* ".TE" nl)))

)
