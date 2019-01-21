(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
  "A grammar for a trivial subset of english")
(defvar *grammar* *simple-grammar*
  "The grammar used by generate. Initially, this is *simple grammar*, but we can switch to others.")

(defun one-of (set)
  "Pick one element of a set at random. Return as list."
  (list (random-elt set)))
(defun random-elt (choices)
  "Choose an element of a list at random."
  (elt choices (random (length choices))))

(defun generate (phrase)
  "Generate a random sentence or phrase"
  (cond ((listp phrase)
         (mappend #'generate phrase))
        ((rewrites phrase)
         (generate (random-elt (rewrites phrase))))
        (t (list phrase))))

(defun rule-lhs (rule)
  "The left-hand side of a rule."
  (first rule))
(defun rule-rhs (rule)
  "The right-hand side of a rule."
  (rest (rest rule)))
(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))

(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (apply #'append (mapcar fn the-list)))
;;everything above is given in book

;;2.1 write a version of generate that uses cond but avoids calling rewrites twice.
;;accidentally rewrote one from the book.
(defun generate-opt (phrase)
  "Optimized version of generate."
  (if (listp phrase)
      (mappend #'generate phrase)
      (let ((options (rewrites phrase)))
        (if (null options)
            (list phrase)
            (generate (random-elt options))))))
;;2.1 says use cond and don't call rewrite twice. Seems rather pointless to force cond usage. Will skip
;;2.2 Write a version of generate that explicitly differentiates between terminal
;; symbols and nonterminal symbols

(defun terminalp (word)
  "Returns whether a symbol is terminal or nonterminal."
  (if (assoc word *grammar*)
      nil
      T))
(defun generate-clarity (phrase)
  "Version of generate that explicitly deals with terminal or nonterminals."
  (if (listp phrase)
      (mappend #'generate phrase)
      (if (terminalp phrase)
          (list phrase)
          (generate (random-elt (rewrites phrase))))))

;;2.3 trivial grammar for a subset of bash
(defparameter *bash-grammar-simple*
  '((command -> (program options arguments output))
    (program -> ls mv cp)
    (options -> -a -l -x -f)
    (arguments -> one-arg two-args)
    (one-arg -> ./ ./dev ./Downloads ./dev/ ./Downloads/)
    (two-args -> (one-arg one-arg))
    (output -> &>log.txt ()))) ; either redirect stdout and stderr or dont redirect output

;; 2.4 Make a high-order function cross-product and define combine-all in terms of cross-product

;; here is the original combine-all
(defun combine-all (xlist ylist)
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (append x y)) xlist))
           ylist))

;;start with obvious solution
(defun cross-product (fn xlist ylist)
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (funcall fn x y)) xlist))
           ylist))

(defun new-combine-all (xlist ylist)
  (cross-product #'append xlist ylist))

;;test cross-product with a different function than append
(defun repeat-time (xlist ylist)
  (let ((reps (first ylist)))
    (cond ((= reps 0) '())
          ((= reps 1) xlist)
          ((> reps 1) (repeat-time (append xlist xlist) (list (- reps 1)))))))
