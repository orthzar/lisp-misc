;;;; cl-vaultless.lisp
;;; 
;;; Buzzword Summary:
;;; Vaultless is a serverless, stateless, mobile-optimized, quantum-resistant, AI-safe password manager,
;;; which fights password fatigue by being storage-efficient and by upcycling an uncommon hash function.
;;; This innovative open-source technology is guaranteed to disrupt the legacy cloud-based password management market.
;;; 
;;; Actual (non-joke) Summary:
;;; Vaultless is a password "manager" which requires no storage.
;;; Instead, you give it a master-password, URL, username, and optional counter, and Vaultless spits out a hash, which is your password.
;;; Generating a new password is the same operation as generating an old password.
;;;
;;; How to Use this:
;;;   (1) When you need a password for a website, eval the function (derive-password ...) in a Common Lisp implementation (e.g. SBCL):
;;;     (a) MASTER-PASSWORD needs to be as long and random as you can make it (I recommend Diceware)
;;;     (b) URL can be a website (e.g. gmail.com) or a computer hostname (e.g. atlas-012)
;;;     (c) Make URL and USERNAME as memorable as possible. (e.g. "gmail.com" is easier to remember than "google.com/mail")
;;;     (d) COUNTER is optional and intended to be used when you need another password for a website.
;;;         In that case, you will need to either remember, store, or guess the counter.
;;;         (Warning: guessing the counter could get you locked out of your account.)
;;;   (2) Paste that password into the website.
;;;   (3) When you return to log into that website, repeat (1) and (2).
;;;
;;; You could keep a list of usernames and URLs in a text file, since that information is borderline public information anyways.
;;; 
;;; Warnings:
;;;   (1) Do not use this unless you understand how it works.
;;;   (2) Vaultless cannot make short passwords for websites that have password length limits, or character
;;;       requirements (e.g. special characters, upper case cahracters). Vaultless only outputs numbers and lowercare letters.
;;;       Such websites are either run by incompetent people, or they are coerced to use asinine password rules by some government.
;;;       In either case, such websites are unfit to exist. Don't use them.

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

(defun sha512 (string)
  "Given a string, return a SHA512 hash of that string"
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha512 (flexi-streams:string-to-octets string))))

(defun blake2 (string)
  "Given a string, return a SHA3 hash of that string"
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :blake2 (flexi-streams:string-to-octets string))))

;; Three different hash functions are used just in case a serious flaw is found in one or two of the hash functions.
;; If two of these hash functions fail, then the security of this password manager is no lower than the third hash function.
(defun derive-password (master-password url username &optional counter)
  "Prints a hash of three strings. That's it. That's the password manager."
  (print
   ;; SHAKE128 was shosen because it returns 128 bits, which is short enough for most password prompts, and because it is derived from SHA3.
   ;; While SipHash could have been chosen because it returns 64 bits, it requires a fixed length key, and I don't yet want to think about how to handle that.
   (shake128
    ;; SHA512 was chosen for it's output length and for it's continued resistance to cracking.
    (sha512
     ;; BLAKE2 was chosen for using ARX.
     (blake2
      (concatenate 'string master-password url username counter))))))

;; Example usage:
;; (derive-password "example master password" "example.com" "example_username")
;; => "c3e6afe8d660d99477f689cab791fdb0"
