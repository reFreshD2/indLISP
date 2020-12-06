(setq numbers '(3 5 9 1 7 12 4 12 5 3 1 24))

(defun diff (data iter complexity) 
(cond
    ((or (and (eq (MOD iter 2) 1) (NULL (CADDR data))) (and (eq (MOD iter 2) 0) (NULL (CADDDR data)))) (list (list (+ complexity 1))))
    (T (cond
           ((eq (MOD iter 2) 1) (cons (- (CAR data) (CADDR data)) (diff (CDDR data) (+ iter 2) (+ complexity 1))))
           (T (cons (- (CADR data) (CADDDR data)) (diff (CDDDR data) (+ iter 3) (+ complexity 1))))
        ))
)
)

(defun sameElem (data container complexity)
    (defun findElem (data container)
    (cond
        ((NULL container) NIL)
        ((eq data (CAR container)) T)
        (T (findElem data (CDR container)))
    )
    )
(cond
    ((NULL data) (list (list (+ complexity 1))))
    ((eq T (findElem (CAR data) container)) (cons (CAR data) (sameElem (CDR data) container (+ complexity 1))))
    (T (sameElem (CDR data) container (+ complexity 1)))
)
)

(defun len (data count complexity)
(cond
    ((NULL (CDR data)) (cons (+ count 1) (list (list (+ complexity 1)))))
    (T (len (CDR data) (+ count 1) (+ complexity 1)))
)
)

(defun minDiff (data complexity)
(cond
    ((NULL (CDR data)) (cons (CAR data) (list (list (+ complexity 1)))))
    ((< (CAR data) (CADR data)) (minDiff (cons (CAR data) (CDDR data)) (+ complexity 1)))
    (T (minDiff (CDR data) (+ complexity 1)))
)
)

(defun mult (data complexity)
(cond
    ((NULL (CDR data)) (list (list (+ complexity 1))))
    (T (cons (* (CAR data) (CADR data)) (mult (CDR data) (+ complexity 1))))
)
)

(defun delElem (elem data)
(cond
    ((NULL data) NIL)
    ((eq elem (CAR data)) (delElem elem (CDR data)))
    (T (cons (CAR data) (delElem elem (CDR data))))
)
)

(defun sortMult (data complexity)
    (defun maxMult (data)
        (cond
            ((NULL (CDR data)) (CAR data))
            ((> (CAR data) (CADR data)) (maxMult (cons (CAR data) (CDDR data))))
            (T (maxMult (CDR data)))
        )
    )
(cond
    ((NULL data) (list (list (+ complexity 1))))
    (T (cons (maxMult data) (sortMult (delElem (maxMult data) data) (+ complexity 1))))
)
)

(defun sortDiff (data complexity)
(cond
    ((NULL data) (list (list (+ complexity 1))))
    (T (cons (CAR (minDiff data 0)) (sortDiff (delElem (CAR (minDiff data 0)) data) (+ complexity 1))))
)
)

(print(diff numbers 0 0))
(print(sameElem (butlast (diff numbers 0 0)) numbers 0))
(print(len (butlast (diff numbers 0 0)) 0 0))
(print(minDiff (butlast (diff numbers 0 0)) 0))
(print(mult numbers 0))
(print(sortMult (butlast (mult numbers 0)) 0))
(print(sortDiff (butlast (diff numbers 0 0)) 0))