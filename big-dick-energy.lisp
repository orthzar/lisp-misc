;;;; big-dick-energy.lisp
;;; Generates a random phrase using synonyms for big, dick, and energy
;;; big, dick, and energy are included in the *-synonyms* variables to pad out this dumb program.

;;
;; Variables
;;

(defparameter *big-synonyms* '("big" "colossal" "considerable" "enormous" "fat" "full" "gigantic" "hefty"
                               "huge" "immense" "massive" "sizable" "substantial" "tremendous"
                               "vast" "a-whale-of-a" "ample" "awash" "brimming" "bulky" "bull"
                               "burly" "capacious" "chock-full" "commodious" "copious" "crowded"
                               "extensive" "heavy-duty" "heavyweight" "hulking" "humongous"
                               "husky" "jumbo" "mammoth" "mondo" "monster" "oversize" "packed"
                               "ponderous" "prodigious" "roomy" "spacious" "strapping"
                               "stuffed" "super-colossal" "thundering" "voluminous" "walloping"
                               "whopper" "whopping"))

(defparameter *dick-synonyms* '("dick" "member" "penis" "prick" "tool" "schlong"
                                "phallus" "pecker" "putz" "johnson" "willy"
                                "cock" "peter" "shaft" "manhood" "wood"))

(defparameter *energy-synonyms* '("energy" "efficiency" "intensity" "power" "spirit" "stamina" "strength"
                                  "toughness" "vitality" "activity" "animation" "application" "ardor"
                                  "birr" "dash" "drive" "effectiveness" "efficacy" "endurance"
                                  "enterprise" "exertion" "fire" "force" "forcefulness" "fortitude"
                                  "get-up-and-go" "go" "hardihood" "initiative" "juice" "life"
                                  "liveliness" "might" "moxie" "muscle" "pep" "pizzazz" "pluck" "potency"
                                  "puissance" "punch" "spontaneity" "steam" "tuck" "vehemence" "verve" "vim"
                                  "virility" "vivacity" "zeal" "zest" "zing" "zip" "operativeness" "élan"))

;;
;; Functions
;;

(defun random-pick (var)
  (let ((len (length var)))
    (nth (random len) var)))

(defun generate-big-dick-energy ()
  (concatenate 'string
               (random-pick *big-synonyms*)
               " "
               (random-pick *dick-synonyms*)
               " "
               (random-pick *energy-synonyms*)))
  
;; (princ (generate-big-dick-energy))
