;;;; cl-vaultless.lisp
;;; 
;;; A password manager which requires no storage.
;;; Each password is generated deterministically when you need it.
;;; 
;;; How to Use this:
;;;   (1) When you need a password for a website, run (derive-password ...)
;;;     (a) MASTER-PASSWORD needs to be as long and random as you can make it
;;;     (b) URL can be a website (e.g. gmail.com) or a computer name (e.g. 
;;;     (c) USERNAME is obvious.
;;;     (d) Make URL and USERNAME as memorable as possible.
;;;   (2) Paste that password into the website.
;;;   (3) When you return to log into that website, repeat (1) and (2).
;;;
;;; 
;;; 
;;; 
;;; Warning:
;;;   Do not use this unless you understand how it works.
;;;   This isn't complicated.
;;;   But, don't get mad at me if you lose access to something.

(defpackage :cl-vaultless)
(in-package :cl-vaultless)

(ql:quickload :ironclad)
(ql:quickload :flexi-streams)

(defun shake128 (string)
  "Given a string, return a hash of that string"
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :shake128 (flexi-streams:string-to-octets string))))

(defun derive-password (master-password url username)
  "Master-password is something you need to come up with. URL could be a website (e.g. gmail.com), but keep it trivial to remember. USERNAME is self-explanatory."
  (print (shake128 (concatenate 'string master-password url username))))

(derive-password "staple horse remove battery network"
                 "gmail.com"
                 "alfranken123")
