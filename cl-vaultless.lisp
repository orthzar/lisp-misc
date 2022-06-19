;;;; cl-vaultless.lisp
;;; 
;;; Buzzword Summary:
;;; Vaultless is a serverless, stateless, mobile-optimized, quantum-resistant password manager,
;;; which fights password fatigue by being storage-efficient and by upcycling an uncommon hash function.
;;; This innovative open-source technology is guaranteed to disrupt the legacy cloud-based password management market.
;;; 
;;; Actual (non-joke) Summary:
;;; Vaultless is a password "manager" which requires no storage.
;;; Instead, you give it a master password, URL, and username, and Vaultless spits out a hash, which is your password.
;;; Generating a new password is the same operation as generating an old password.
;;;
;;; How to Use this:
;;;   (1) When you need a password for a website, eval the function (derive-password ...) in a Common Lisp implementation (e.g. SBCL):
;;;     (a) MASTER-PASSWORD needs to be as long and random as you can make it (I recommend Diceware)
;;;     (b) URL can be a website (e.g. gmail.com) or a computer hostname (e.g. atlas-012)
;;;     (c) Make URL and USERNAME as memorable as possible. (e.g. "gmail.com" is easier to remember than "google.com/mail")
;;;   (2) Paste that password into the website.
;;;   (3) When you return to log into that website, repeat (1) and (2).
;;;
;;; You could keep a list of usernames and URLs in a text file, since that information is borderline public information anyways.
;;; 
;;; Warnings:
;;;   (1) Do not use this unless you understand how it works. 
;;;   (2) In the event of a password leak, the most you can do with Vaultless is to use a new master password.
;;;       But then it isn't really a master password anymore. (This is a lesson for all those who want everything to be simple.)
;;;   (3) Vaultless, presently, cannot make short passwords (for websites that have password length limits),
;;;       nor can it make passwords with special characters in it (it only outputs numbers and lowercare letters).
;;;       All I can say is that those are badly-run websites and you shouldn't trust them.

(defpackage :cl-vaultless
  (:use #:cl)
  (:export #:derive-password))
(in-package :cl-vaultless)

(ql:quickload :ironclad)
(ql:quickload :flexi-streams)

;; I don't remember why I picked this hash function. You'll have to ask God next time you talk to Him.
;; This function could be folded into DERIVE-PASSWORD, but "password-manager in one function" sounds like an annoying code-golf challenge.
(defun shake128 (string)
  "Given a string, return a hash of that string"
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :shake128 (flexi-streams:string-to-octets string))))

(defun derive-password (master-password url username)
  "Prints a hash of three strings. That's it. That's the password manager."
  (print (shake128 (concatenate 'string master-password url username))))

;; Example usage:
;; (derive-password "example master password" "example.com" "example_username")
;; => "b9c50f5a670a4da91785aa672e504a81"
