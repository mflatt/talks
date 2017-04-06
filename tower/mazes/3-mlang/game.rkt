#lang s-exp "maze.rkt"

;; Verbs ----------------------------------------

;; This declaration must be first:
(define-verbs all-verbs
  [north (n) "go north"]
  [south (s) "go south"]
  [east (e) "go east"]
  [west (w) "go west"]
  [up () "go up"]
  [down () "go down"]
  [in (enter) "enter"]
  [out (leave) "leave"]  
  [get _ (grab take) "take"]
  [put _ (drop leave) "drop"]
  [open _ (unlock) "open"]
  [close _ (lock) "close"]
  [knock _]
  [quit (exit) "quit"]
  [look (show) "look"]
  [inventory () "check inventory"]
  [help]
  [save]
  [load])

;; Global actions ----------------------------------------

;; This declaration must be second:
(define-actions everywhere-actions
  ([quit (printf "Bye!\n") (exit)]
   [look (show-current-place)]
   [inventory (show-inventory)]
   [save (save-game)]
   [load (load-game)]
   [help (show-help)]))

;; Objects ----------------------------------------

(define-thing cactus
  [get "Ouch!"])

(define-thing door
  [open (if (have-item? key)
            (begin
              (set-thing-state! door 'open)
              "The door is now unlocked and open.")
            "The door is locked.")]
  [close (set-thing-state! door #f)
         "The door is now closed."]
  [knock "No one is home."])

(define-thing key
  [get (if (have-item? key)
           "You already have the key."
           (begin
             (take-item! key)
             "You now have the key."))]
  [put (if (have-item? key)
           (begin
             (drop-item! key)
             "You have dropped the key.")
           "You don't have the key.")])

(define-thing trophy
  [get (begin
         (take-item! trophy)
         "You win!")])

;; Places ----------------------------------------

(define-place start
  "You're standing in a field. There is a house to the north."
  ()
  ([north house-front]
   [south desert]))

(define-place house-front
  "You are standing in front of a house."
  (door)
  ([in (if (eq? (thing-state door) 'open)
           room
           "The door is not open.")]
   [south start]))

(define-place desert
  "You're in a desert. There is nothing for miles around."
  (cactus key)
  ([north start]
   [south desert]
   [east desert]
   [west desert]))

(define-place room
  "You're in the house."
  (trophy)
  ([out house-front]))

;; Starting place ----------------------------------

;; The module must end with the starting place name:
start
