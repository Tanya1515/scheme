#lang scheme/base
(require scheme/mpair)


(define (rot-left! mlst)
    (let loop ((first (mcar mlst)) (curr mlst))
        (cond ((null? (mcdr curr))
            (set-mcar! curr first))
            (else
                (set-mcar! curr (mcar(mcdr curr)))
                (loop first (mcdr curr))
            )
        )
    )
)


(define-syntax rot-left_macr!
    (syntax-rules ()
        ((rot-left_macr! mlst)
            (let loop ((first (mcar mlst)) (curr mlst))
                (cond ((null? (mcdr curr))
                    (set-mcar! curr first))
                    (else
                        (set-mcar! curr (mcar(mcdr curr)))
                        (loop first (mcdr curr))
                    )
                )
            )
        )
    )
)

(define l (mlist 1 2 3 4 5 6))
l
(rot-left_macr! l)
l
(rot-left! l)
l
