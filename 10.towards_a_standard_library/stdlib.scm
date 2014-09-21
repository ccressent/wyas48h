(define (not x)     (if x #f #t))
(define (null? obj) (if (eqv? obj '()) #t #f))

(define (id obj)      obj)
(define (list . objs) objs)

(define (flip func) (lambda (arg1 arg2) (func arg2 arg1)))

(define (curry func arg1) (lambda (arg) (apply func (cons arg1 (list arg)))))
(define (compose f g)     (lambda (arg) (f (apply g arg))))

(define zero?     (curry = 0))
(define positive? (curry < 0))
(define negative? (curry > 0))
(define (odd? x)  (= (mod x 2) 1))
(define (even? x) (= (mod x 2) 0))

(define (foldr func acc lst)
  (if (null? lst)
      acc
      (func (car lst) (foldr func acc (cdr lst)))))

(define (foldl func acc lst)
  (if (null? lst)
      acc
      (foldl func (func acc (car lst)) (cdr lst))))

(define fold   foldl)
(define reduce foldr)

(define (unfold func init pred)
  (if (pred init)
      (cons init '())
      (cons init (unfold func (func init) pred))))

(define (sum . lst)     (fold + 0 lst))
(define (product . lst) (fold * 1 lst))
(define (and . lst)     (fold && #t lst))
(define (or . lst)      (fold || #f lst))

(define (max first . rest) (fold (lambda (old new) (if (> old new) old new)) first rest))
(define (min first . rest) (fold (lambda (old new) (if (< old new) old new)) first rest))

(define (length lst) (fold (lambda (x y) (+ x 1)) 0 lst))

(define (reverse lst) (fold (flip cons) '() lst))

(define (mem-helper pred op) (lambda (acc next) (if (and (not acc) (pred (op next))) next acc)))
(define (memq   obj lst)     (fold (mem-helper (curry eq? obj)     id) #f lst))
(define (memv   obj lst)     (fold (mem-helper (curry eqv? obj)    id) #f lst))
(define (member obj lst)     (fold (mem-helper (curry equal? obj)  id) #f lst))
(define (assq  obj alist)    (fold (mem-helper (curry eq? obj)    car) #f alist))
(define (assv  obj alist)    (fold (mem-helper (curry eqv? obj)   car) #f alist))
(define (assoc obj alist)    (fold (mem-helper (curry equal? obj) car) #f alist))

(define (map func lst)    (foldr (lambda (val acc) (cons (func val) acc)) '() lst))
(define (filter pred lst) (foldr (lambda (val acc) (if (pred val) (cons val acc) acc)) '() lst))

(define (list-tail lst k) (foldr (lambda (val acc)
                                    (if (< (length acc) (- (length lst) k)) (cons val acc) acc))
                                    '()
                                    lst))

(define (list-ref list k) (car (list-tail list k)))
