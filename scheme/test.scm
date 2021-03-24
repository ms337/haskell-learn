(define-syntax (bind-vars bindings)
    `(map car bindings))

(define-syntax (bind-vals bindings)
    `(map cadr bindings))

(define-syntax (let bindings body)
    `(apply (lambda ,(bind-vars bindings) ,body) ',(bind-vals bindings)))

(define-syntax (while condition . body)
    `(let loop ()
        (cond (,condition
            (begin . ,body)
            (loop)))))

(define counter 10)

(while (> counter 0) 
    (display counter)
    (set! counter (- counter 1)))

; ; (if () (if () ()))
; (define-syntax (switch body)
;     (if (null? body)
;         '()
;         `(if (caar body)


; (switch (((cond) (proc)) (cond) (proc))

)