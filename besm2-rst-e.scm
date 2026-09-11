;;;!> besm2-rst-e.scm -- Convert a YAML BESM 2E character or template into
;;;   reST. A copy-and-refactor of besm2-rst.scm that decodes each entity
;;;   exactly once into a shared <entity> record (besm-entities.scm)
;;;   instead of re-deriving the same name/level/points/details fields
;;;   from a raw alist separately in each of four process-entity-*
;;;   backends. See PLAN.md (not checked in) for the design, and
;;;   besm-entities.scm's header comment for what moved there and why.
;;;
;;;   Loading itself is unchanged from besm2-rst.scm: entities still come
;;;   from a plain alist, built by either the yaml egg (default) or
;;;   (slibfyaml scheme) (-f/--fyaml) -- both hand back the same
;;;   alist/list/scalar shape, so this program's own load-entity (below)
;;;   doesn't need to know or care which one produced its input.
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

(declare (uses besm-entities))

(module besm-rst ()

(import (scheme))

(import (chicken base))
(import (chicken condition))
(import (chicken io))
(import (chicken port))
(import (chicken process-context))
(import (chicken string))

(import args)
(import bindings)
(import (schemepunk show))
(import loop)
(import (srfi 152))
(import yaml)
(import (rename (only (slibfyaml scheme) load-port) (load-port fyaml-load)))

(import besm-entities)

(define load-from-yaml yaml-load)

;; fyaml-load (slibfyaml's load-port) always returns a list of decoded
;; documents, one per YAML document in the stream -- even for
;; single-document input, unlike yaml-load, which hands back the
;; document's decoded value directly. All of this program's input files
;; are a single YAML document (one leading "---", no second "---"), so
;; unwrap to that one document's value here to give -f/--fyaml the same
;; contract as the default yaml-load.
(define (fyaml-load-entities port)
  (car (fyaml-load port)))

;; must-exist/may-exist are this program's own load-entity's business, not
;; besm-entities.scm's: they're specific to walking a plain alist, one of
;; the two ways of getting to that point (the other being
;; besm2-rst-f-e.scm's node-based walk), not something both loaders share.
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

;;; ------------------------------------------------------------------
;;; load-entity: build besm-entities' record types from a raw alist. All
;;; of the actual formatting is in besm-entities.scm; this only decodes.
;;; ------------------------------------------------------------------

(define (load-stat stat)
  (make-stat (must-exist "name" stat)
             (must-exist "value" stat)
             (must-exist "points" stat)))

(define (load-derived derived)
  (make-derived (must-exist "name" derived)
                (must-exist "value" derived)
                (may-exist "alternatives" derived)))

(define (load-attribute attribute)
  (let* ((name         (must-exist "name" attribute))
         (level        (must-exist "level" attribute))
         (points       (must-exist "points" attribute))
         (details      (may-exist "details" attribute))
         (details      (if details (string-trim-both details) details))
         (effective    (may-exist "effective" attribute))
         (level        (if effective (show #f level " (" effective ")") level))
         (enhancements (may-exist "enhancements" attribute))
         (limiters     (may-exist "limiters" attribute))
         (elements     (may-exist "elements" attribute))
         (details      (make-attribute-details details enhancements limiters
                                               elements)))
    (make-attribute name level points details)))

(define (load-defect defect)
  (let* ((name    (must-exist "name" defect))
         (points  (must-exist "points" defect))
         (details (may-exist "details" defect))
         (details (if details (string-trim-both details) details)))
    (make-defect name points details)))

(define (load-skill skill)
  (make-skill (must-exist "name" skill)
              (must-exist "level" skill)
              (must-exist "points" skill)
              (may-exist "specialisations" skill)))

(define (load-entity entity)
  (let* ((stats       (may-exist "stats" entity))
         (stats       (and stats (map load-stat stats)))
         (derived     (may-exist "derived" entity))
         (derived     (and derived (map load-derived derived)))
         (attributes  (may-exist "attributes" entity))
         (attributes  (and attributes
                           (sort-by-name-ci (map load-attribute attributes)
                                            attribute-name)))
         (defects     (may-exist "defects" entity))
         (defects     (and defects
                           (sort-by-name-ci (map load-defect defects) defect-name)))
         (skills      (may-exist "skills" entity))
         (skills      (and skills
                           (sort-by-name-ci (map load-skill skills) skill-name)))
         (stats-total      (total-points-of stats stat-points))
         (attributes-total (total-points-of attributes attribute-points))
         (defects-total    (total-points-of defects defect-points))
         (skills-total     (total-points-of skills skill-points))
         (tagline          (may-exist "tagline" entity)))
    (make-entity (may-exist "name" entity)
                 (and tagline (string-trim-both tagline))
                 (may-exist "description" entity)
                 (may-exist "size" entity)
                 ;; may-exist, not (assoc "mecha" entity) directly: assoc
                 ;; returns the found pair (truthy) whenever the key
                 ;; exists, regardless of its value, so "mecha: false"
                 ;; would turn mecha mode ON same as "mecha: true" --
                 ;; may-exist unwraps to the actual #t/#f/absent value.
                 (may-exist "mecha" entity)
                 stats derived attributes defects skills
                 stats-total attributes-total defects-total skills-total
                 (+ stats-total attributes-total defects-total))))

(define (process-file)
  ;; It is a file of possibly multiple entities. Every entity is decoded
  ;; up front (map load-entity ...) before any of them are formatted --
  ;; see besm-entities.scm's header comment (note 2) for why that's a
  ;; small, deliberate behavior change from besm2-rst.scm.
  (handle-exceptions exn
      (begin
        (show (current-error-port) "Error while trying to load YAML input from " (yaml-input-filename) nl)
        (print-error-message exn (current-error-port)))
    (let ((entities (map load-entity (load-from-yaml (current-input-port)))))
      (loop for entity in entities
            for entity-no from 1
            do (parameterize ((mecha? (entity-mecha? entity)))
                 (*output-formatter* entity entity-no))))))

(define yaml-input-filename (make-parameter "(stdin)"))

(define (process-filename filename)
  (parameterize ((yaml-input-filename filename))
    (with-input-from-file filename process-file)))

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

(define *hmm-output* #f)
(define *hmm-root* #f)                  ; Don't output root if #f.
(define *output-file* #f)
(define *output-formatter* process-entity)

(define +command-line-options+
  (list (args:make-option
         (|1| one) #:none "Use only one table."
         (dbg (dfmt "one only" nl))
         (*one-table* #t)
         ;; Having multiple header separator lines doesn't cause pandoc to
         ;; complain, but it does make the first line a header which looks
         ;; different in HTML.
         (*head-sep* #\-))
        (args:make-option
          (B no-bold-head) #:none (show #f "Turns OFF bolding of headers in plain reST output.")
          (*bold-head* #f))
        (args:make-option
         (b bold) #:none (show #f
                                  "Turn on bolding of names and levels of
                          attributes, defects, and skills in terse mode.
                          Overrides italicizing (-i/--italics).")
         (*bolding* #t))
        ;; c is reserved for raw ConTeXt output.
        (args:make-option
         (D omit-description) #:none "Omit the entiy description."
         (*omit-entity-description* #t))
        (args:make-option
         (d debug) #:none "Turn on debugging."
         (*debugging* #t))
        (args:make-option
         (f fyaml) #:none "Use fyaml via slibfyaml egg instead of using the yaml egg."
         (set! load-from-yaml fyaml-load-entities))
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
         (*hmm-separate* #t))
        (args:make-option
         (h help) #:none "Display this text."
         (usage))
        (args:make-option
         (i italics) #:none (show #f
                                  "Turn on italicizing of names and levels of
                          attributes and defects in terse mode.")
         (*italicizing* #t))
        (args:make-option
         (l level) #:none "Output the word \"Level\" before the level number
                          in terse mode."
         (*level* #t))
        (args:make-option
         (M em-dash) #:none "Separate the attribute name and the level with
                          an em dash in terse mode."
         (*em-dash* #t))
        (args:make-option
         (m raw-ms-tables) #:none "Use groff tbl output in a raw ms block."
         (set! *output-formatter* process-entity-raw-ms))
        (args:make-option
         (n unicode-minus) #:none "Use Unicode MINUS SIGN (U+2212) instead
                          of ASCII hyphen-minus for negative numbers."
         (*unicode-minus* #t))
        (args:make-option
         (o output) #:required "Output file."
         (set! *output-file* arg))
        (args:make-option
         (p page) #:none "Page after description.  (Only for ms output!)"
         (*page-after-description* #t))
        (args:make-option
         (s subtotals) #:none
         "Show subtotals for stats, attributes, and defects."
         (*show-subtotals* #t))
        (args:make-option
         (t terse) #:none "Use terse output."
         (set! *output-formatter* process-entity-terse))
        (args:make-option
         (U subunderliner) #:required
         "Entities after the first are subentities,
                          and use a different character for
                          underlining the subheader."
         (*subunderliner* (string-ref arg 0)))
        (args:make-option
         (u underliner) #:required
         "Character to use for underlining the header."
         (*underliner* (string-ref arg 0)))
        (args:make-option
         (w width)
         (required: "NUMBER") "Width of table in characters"
         (*table-width* (string->number arg)))))

(define (main)
  (receive (options operands) (args:parse (command-line-arguments)
                                          +command-line-options+)
    (define (process-operands)
      (if  (zero? (length operands))
           (with-input-from-port (current-input-port) process-file)
           (loop for filename in operands do (process-filename filename))))

    ;; When normally outputing reST bolding is two asterisks on each side.
    (*num-width* (+ (*num-width*) 4))

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
