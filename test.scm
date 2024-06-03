(import yaml)

(define fv2021 (car (call-with-input-file
                        "test-data/FV2021-Coleopteran-2e.yaml" yaml-load)))
(pp fv2021)

(define fv2021-attributes (must-exist "attributes" fv2021))
(pp fv2021-attributes)

(define fv2021-defects (must-exist "defects" fv2021))
(pp fv2021-defects)

(define (name-ci<? a b)
  (let ((a-name (must-exist "name" a))
        (b-name (must-exist "name" b)))
    (string-ci<? a-name b-name)))
    
(define sa (sort fv2021-attributes name-ci<?))
(pp sa)

(define sd (sort fv2021-defects name-ci<?))
(pp sd)
