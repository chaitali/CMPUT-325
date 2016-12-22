;Chaitali Patel
;1404270
;cmput 325 LEC B1
;assignment 1

#| Question 1.

The function xmember takes in the argument X and list Y, and returns T if X is a member of list Y and NIL otherwise. 

test cases:
(xmember '1 '(1)) => T
(xmember '1 '( (1) 2 3)) => NIL
(xmember nil '(nil)) => T


|#

(defun xmember (X Y)
   (cond
         ((null Y) nil)
         ((equal X (first Y)) t)
         (t (xmember X (cdr Y))))
)

#| Question 2.

The function flatten takes in a list x, which contains sublists nested to any depth. The result of flatten is the complete list of atoms with no nesting, in the same order as given in x. 

test cases:
(flatten '(a (b c) (d ((e)) f))) => (a b c d e f)
(flatten '((((a))))) => (a)

|#

(defun flatten (x)
   (cond
         ((null x) nil)
         ((atom (car x)) (append (list (car X)) (flatten (cdr X))))
         (t (append (flatten (car X)) (flatten (cdr X)))))
)


#| Question 3.

The function mix takes in two lists L1 and L2, and mixes the elements into a single list, where the function returns a list containing elements from L1 and L2 that have been chosen alternatingly. If one list is shorter than the other, the extra elements from the longer list will be appended at the end. 

test cases:
(mix '(a b c) '(d e f)) => (a d b e c f)
(mix '((a) (b c)) '(d e f g h)) => ((a) d (b c) e f g h)

|#

(defun mix (L1 L2)
   (cond
         ((null L1) L2)
         ((null L2) L1)
         (t (append (list (car L1) (car L2)) (mix (cdr L1) (cdr L2)))))
)


#| Question 4.

The function split takes in a list L and returns a list of two sublists L1 and L2, where elements of L are put into L1 and L2 alternatingly. 

test cases:

(split '(1 2 3 4 5 6)) => ((1 3 5) (2 4 6))
(split '((a) (b c) (d e f) g h)) => (((a) (d e f) h) ((b c) g))

|#

(defun split (L)
   (if 
       (null (car L))
       (list nil nil)
       (cons (left L) (list (right (cdr L)))))
)

; helper functions, left is used to get the create the sublist L1, and right is used to create the sublist L2 by taking alternating elements from L. 

(defun left (L)
   (if 
       (null (car L))
        nil
       (cons (car L) (left (cddr L))))
)


(defun right (L)
   (if 
       (null (car L))
        nil
       (cons (car L) (right (cddr L))))
)

#| Question 5.


Question 5.1

No, because if one of the lists is larger than the other, mix will append the end of the larger list to the end, so split will not return a list (L1 L2)
   eg. (split (mix '(1 2 3 4 5 6 7) '(a b c))) => ((1 2 3 4 6) (A B C 5 7))

Question 5.2

Yes, because both lists are equal so split would return two equal lists in both cases, with two internal sublists. Then car would take the first sublist and cadr would take the second sublist from the two equal lists returned in split. Then since the first list given to mix is always larger (because with even elements, same size and odd elements, L1 is bigger) the lists will be put togther in the correct order and the odd element appended at the end.  



eg. (mix (car (split '(1 2 3 4 5))) (cadr (split '(1 2 3 4 5)))) => (1 2 3 4 5)


|#



#| Question 6.

The function subsetsum takes in a list of numbers L and a sum S, and finds a subset of numbers in L that sums to S. If no subset of numbers is found, the function returns nil. Each number in list L can only be used once. 

test cases:

(subsetsum '(1 2 3) 5) => (2 3)
(subsetsum '(1 2 3 4) 2) => (2)
(subsetsum '(1 5 3) 2) => nil

|# 

(defun subsetsum (L S)
   (fixorder L (removeExtra (sort (copy-list L) #'>) S)))


#| removeExtra removes any numbers larger than S in L, and returns (S) if there is a number equal to S. 

eg. (removeExtra (10 7 5) 6) => (5)

|#

(defun removeExtra (L S)
   (cond
         ((> (car L) S) (removeExtra (cdr L) S))
         ((= (car L) S) (list S))
         (t (recurse L S))))

#| recurse goes through L, and finds a subset of numbers that sums to S. recurse does this by going through the list and subtracting the first element of L from S. If the result is negative, nil is returned and the next element in L is tried. Else the first element of L is returned and recurse is called again. When the function finds a subset or L is empty, the list containing a subset of numbers that sum to S is returned, or NIL.
|#

(defun recurse (L S)
   (cond
         ((null L) nil)
	 ((<= S 0) nil)
	 ((= (car L) S) (list (car L)))
         (t 
	    (let ((check (recurse (cdr L) (- S (car L)))))
	         (if (null check)
		     (recurse (cdr L) S)
		     (append (list (car L)) check))))))

#| fixorder and findorder are used so the subset found is returned in the correct order, which is the same order they appear in the original list L. If the first element of the original list L matches the subset LS, it's returned in the correct order. 
|#

(defun fixorder (L LS)
   (cond 
         ((null L) nil)
	 ((finditem LS (car L)) (cons (car L) (fixorder (cdr L) LS)))
	 (t (fixorder (cdr L) LS))))
	   
(defun finditem (L X)
   (cond
         ((null L) nil)
         ((equal X (car L)) T)
         (t (finditem (cdr L) X))))

	



									
