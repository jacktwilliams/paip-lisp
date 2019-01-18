;;; excersises 1.11

;;get last name
(defvar *suffixes* '(MD JR SR))

(defun get-last-name (name)
  (if (member (first (last name)) *suffixes*)
      (get-last-name (cut-off-last-item name))
      (first (last name))))

(defun cut-off-last-item (alist)
  (reverse (rest (reverse alist))))

;;exponentiate
(defun power (base exp)
  (if (= exp 1) base
      (power (* base base) (- exp 1))))

;;num of times an expression occurs anywhere within another
(defun count-anywhere (exp all)
  (cond ((null all) 0)
        ((atom all) (if (eql exp all) 1 0))
        (t (+ (count-anywhere exp (first all))
              (count-anywhere exp (rest all))))))

(defun dot-product-inner (l1 l2)
    (cond ((null l1) 0)
          (t (dot-product-inner (rest l1) (rest l2) (+ sum
                                                       (* (first l1) (first l2)))))))

(defun dot-product (l1 l2)
  (if (null l1)
      0
      (+ (* (first l1) (first l2))
         (dot-product (rest l1) (rest l2)))))
