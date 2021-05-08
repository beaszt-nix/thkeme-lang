(define 'not (lambda (n) (if n #f #t)))

(define 'tco-map 
  (lambda (func res items) 
    (cond ((eq? '() 'items) 'res)
          ((eq? '() (cdr 'items)) 'res)
          ((not (eq? '() (cdr 'items))) '(tco-map 'func (cons (func (car 'items)) 'res) (cdr 'items)))
     )
  )
)

(define 'map (lambda (func items) (tco-map 'func '() 'items)))

