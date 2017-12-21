;CSC335 Project
;Shirong Zheng  & Tongzheng Zeng

;--type Numbered a = (Int,a)
(define (make-numbered-value tag val) (cons tag val))
(define (nvalue-tag tv) (car tv))
(define (nvalue-val tv) (cdr tv))

;-- return:: a -> NumberedM a
(define (return val)
  (lambda (curr_counter)
    (make-numbered-value curr_counter val)))

(define incr 
  (lambda (n)
    (make-numbered-value (+ 1 n) n)))

;for bind, -- (>>=):: NumberedM a -> (a -> NumberedM b) -> NumberedM b
(define (>>= m f)
  (lambda (curr_counter)
    (let* ((m_result (m curr_counter))
      (n1 (nvalue-tag m_result))        ; result of the delayed computation
      (v  (nvalue-val m_result))        ; represented by m
      (m1 (f v)))                       ; feed the result to f, get another m1
   (m1 n1))))                           ; The result of the bigger monad

(define (make-node val kids)
  (>>= incr
       (lambda (counter)
         (return (cons (make-numbered-value counter val) kids)))))

;try to produces a tagged binary tree
(define (build-btree-r depth)
  (if (zero? depth) (make-node depth '())
    (>>=
      (build-btree-r (- depth 1))
      (lambda (left-branch)
        (>>= (build-btree-r (- depth 1))
             (lambda (right-branch)
               (make-node depth (list left-branch right-branch))))))))

(define (runM m init-counter) (m init-counter))

; (letM ((name initializer)) expression) ==> >>= initializer (lambda (name) expression))
;regular let:
;  (let ((name initializer)) body) ==>(apply (lambda (name) body) (list initializer))
(define-macro letM
       (lambda (binding expr)
         (apply
          (lambda (name-val)
            (apply (lambda (name initializer)
                     `(>>= ,initializer (lambda (,name) ,expr)))
                   name-val))
          binding)))

(define-macro letM*
       (lambda (bindings expr)
         (if (and (pair? bindings) (pair? (cdr bindings)))
             `(letM ,(list (car bindings))
                    (letM* ,(cdr bindings) ,expr))
             `(letM ,bindings ,expr))))

;Each node of the tree is uniquely tagged 
(define (build-btree depth)
       (if (zero? depth) (make-node depth '())
           (let*  ((left-branch (build-btree (- depth 1)))
                   (right-branch (build-btree (- depth 1))))
                  (make-node depth (list left-branch right-branch)))))

 ;constructs a regular, non-tagged full binary tree:
 (define (build-btree-c depth)
       (if (zero? depth) (cons depth '())
           (let ((left-branch (build-btree-c (- depth 1)))
                 (right-branch (build-btree-c (- depth 1))))
                  (cons depth (list left-branch right-branch)))))


;=========
;for test:
; (runM (build-btree 3) 100)
; (build-btree-c 3))