(defun xcount (L) 
    (if (null L) 
        0 
        (+ 1 (xcount (rest L)))))

(defun myappend (L1 L2) 
     (if (null L1)
         L2
        (cons (car L1) (myappend (cdr L1) L2))
     )
)

(defun mylast (L)
    (if (null (rest L))
        (first L)
        (mylast (rest L))))

(defun myremovelast (L)
    (if (null (rest L))
        nil
        (cons (first L) (myremovelast (rest L)))
    )
)

(defun append2 (L1 L2) 
     (if (null L1)
         L2
         (append2 (myremovelast L1)
                (cons (mylast L1) L2))
    )
)

(defun myreverse (L)
      (if (null L)
          L
          (append (myreverse (cdr L))
                  (cons (car L) nil))
      )
)

(defun pairElements (X Y)
    (cons X (cons Y nil))
)

(defun pair (X L)
      (if (null L)
        nil
        (cons (pairElements X (first L))
            (pair X (rest L))
        )
    )
)

(defun cart (L1 L2)
      (if (null L1)
        nil
        (append (pair (first L1) L2)
            (cart (rest L1) L2)
        )
    )
)

