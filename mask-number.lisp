;;;mask-number.lisp
;; This is a joke. This program is not qualified to give medical (or any) advice.

(defun mask-number ()
  (format t "The CDC says you should wear ~A mask~:P."
	  (- (nth 5 (multiple-value-list (get-decoded-time)))
             2019)))

;; (mask-number)
