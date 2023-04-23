;;; blackjack.el --- The game of Blackjack
;; -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Greg Donald

;; Author: Greg Donald <gdonald@gmail.com>
;; Version: 1.0
;; Package-Requires: ((emacs "26.2"))
;; Keywords: games
;; URL: https://https://github.com/gdonald/blackjack-el

;;; Commentary:
;;; This package lets you play Blackjack in Emacs.

;;; Code:

(require 'cl-lib)
(require 'eieio)

(defclass blackjack-card ()
  ((id :initarg :id :initform 0 :type integer)
   (value :initarg :value :initform 0 :type integer)
   (suit :initarg :suit :initform 0 :type integer)))

(cl-defmethod cl-print-object ((obj blackjack-card) stream)
  "Print OBJ to STREAM."
  (princ
   (format "#<%s id: %d value: %s suit: %s>"
           (eieio-class-name (eieio-object-class obj))
	   (slot-value obj 'id)
           (slot-value obj 'value)
	   (slot-value obj 'suit))
   stream))

(defclass blackjack-hand ()
  ((cards :initarg :cards :initform '() :type list)
   (played :initarg :played :initform nil :type boolean)))

(defclass blackjack-player-hand (blackjack-hand)
  ((id :initarg :id :initform 0 :type integer)
   (bet :initarg :bet :initform 0 :type integer)
   (status :initarg :status :initform 'unknown :type symbol)
   (payed :initarg :payed :initform nil :type boolean)
   (stood :intiarg :stood :initform nil :type boolean)))

(cl-defmethod cl-print-object ((obj blackjack-player-hand) stream)
  "Print OBJ to STREAM."
  (princ
   (format "#<%s id: %d cards: %s played: %s status: %s payed: %s stood: %s bet: %s>"
           (eieio-class-name (eieio-object-class obj))
           (slot-value obj 'id)
           (slot-value obj 'cards)
	   (slot-value obj 'played)
	   (slot-value obj 'status)
	   (slot-value obj 'payed)
	   (slot-value obj 'stood)
	   (slot-value obj 'bet))
   stream))

(defclass blackjack-dealer-hand (blackjack-hand)
  ((hide-down-card :initarg :hide-down-card :initform t :type boolean)))

(cl-defmethod cl-print-object ((obj blackjack-dealer-hand) stream)
  "Print OBJ to STREAM."
  (princ
   (format "#<%s cards: %s played: %s hide-down-card: %s>"
           (eieio-class-name (eieio-object-class obj))
           (slot-value obj 'cards)
	   (slot-value obj 'played)
	   (slot-value obj 'hide-down-card))
   stream))

(defclass blackjack-game ()
  ((id :initarg :id :initform 0 :type integer)
   (shoe :initarg :shoe :initform '() :type list)
   (dealer-hand :initarg :dealer-hand :initform nil :type atom)
   (player-hands :initarg :player-hands :initform '() :type list)
   (num-decks :initarg :num-decks :initform 1 :type integer)
   (deck-type :initarg :deck-type :initform 'regular :type symbol)
   (face-type :initarg :face-type :initform 'ascii :type symbol)
   (money :initarg :money :initform 10000 :type integer)
   (current-bet :initarg :current-bet :initform 500 :type integer)
   (current-player-hand :initarg :current-player-hand :initform 0 :type integer)
   (quitting :initarg :quitting :initform nil :type boolean)
   (faces-ascii :initarg :faces :initform '[["A♠" "A♥" "A♣" "A♦"]
					    ["2♠" "2♥" "2♣" "2♦"]
					    ["3♠" "3♥" "3♣" "3♦"]
					    ["4♠" "4♥" "4♣" "4♦"]
					    ["5♠" "5♥" "5♣" "5♦"]
					    ["6♠" "6♥" "6♣" "6♦"]
					    ["7♠" "7♥" "7♣" "7♦"]
					    ["8♠" "8♥" "8♣" "8♦"]
					    ["9♠" "9♥" "9♣" "9♦"]
					    ["T♠" "T♥" "T♣" "T♦"]
					    ["J♠" "J♥" "J♣" "J♦"]
					    ["Q♠" "Q♥" "Q♣" "Q♦"]
					    ["K♠" "K♥" "K♣" "K♦"]
					    ["??"]] :type array)
   (faces-unicode :initarg :faces2 :initform '[["🂡" "🂱" "🃁" "🃑"]
					       ["🂢" "🂲" "🃂" "🃒"]
					       ["🂣" "🂳" "🃃" "🃓"]
					       ["🂤" "🂴" "🃄" "🃔"]
					       ["🂥" "🂵" "🃅" "🃕"]
					       ["🂦" "🂶" "🃆" "🃖"]
					       ["🂧" "🂷" "🃇" "🃗"]
					       ["🂨" "🂸" "🃈" "🃘"]
					       ["🂩" "🂹" "🃉" "🃙"]
					       ["🂪" "🂺" "🃊" "🃚"]
					       ["🂫" "🂻" "🃋" "🃛"]
					       ["🂭" "🂽" "🃍" "🃝"]
					       ["🂮" "🂾" "🃎" "🃞"]
					       ["🂠"]] :type array)
   (shuffle-specs :initarg :shuffle-specs :initform '[80 81 82 84 86 89 92 95] :type array)
   (cards-per-deck :initarg :cards-per-deck :initform 52 :type integer)
   (min-bet :initarg :min-bet :initform 500 :type integer)
   (max-bet :initarg :min-bet :initform 100000000 :type integer)
   (max-player-hands :initarg :min-bet :initform 7 :type integer)))

(defun blackjack-deal-new-hand (game)
  "Deal new GAME hands."
  (if (blackjack-need-to-shuffle game)
      (blackjack-shuffle game))
  (let* ((shoe (slot-value game 'shoe))
	 (player-hand nil)
	 (dealer-hand nil))
    (setf (slot-value game 'player-hands) '())
    (setf player-hand (blackjack-player-hand :id (blackjack-next-id game) :bet (slot-value game 'current-bet)))
    (setf dealer-hand (blackjack-dealer-hand))
    (dotimes (x 2)
      (blackjack-deal-card game player-hand)
      (blackjack-deal-card game dealer-hand))
    (push player-hand (slot-value game 'player-hands))
    (setf (slot-value game 'current-player-hand) 0)
    (setf (slot-value game 'dealer-hand) dealer-hand)
    (if (and
	 (blackjack-dealer-upcard-is-ace dealer-hand)
	 (not (blackjack-hand-is-blackjack (slot-value player-hand 'cards))))
        (progn
          (blackjack-draw-hands game)
	  (blackjack-ask-insurance-action game))
      (if (blackjack-player-hand-done game player-hand)
          (progn
	    (setf (slot-value dealer-hand 'hide-down-card) nil)
            (blackjack-pay-hands game)
            (blackjack-draw-hands game)
            (blackjack-ask-bet-action game))
        (progn
          (blackjack-draw-hands game)
          (blackjack-ask-hand-action game)
          (blackjack-save game))))))

(defun blackjack-deal-card (game hand)
  "Deal a card into HAND from GAME shoe."
  (let* ((shoe (slot-value game 'shoe))
	 (cards (slot-value hand 'cards))
	 (card nil))
    (setf card (car shoe))
    (add-to-list 'cards card :append)
    (setf shoe (cl-remove card shoe :count 1))
    (setf (slot-value hand 'cards) cards)
    (setf (slot-value game 'shoe) shoe)))

(defun blackjack-next-id (game)
  "Return next GAME object id."
  (let* ((id (slot-value game 'id)))
    (setf (slot-value game 'id) (1+ id))))

(defun blackjack-pay-hands (game)
  "Pay GAME player hands."
  (let* ((dealer-hand (slot-value game 'dealer-hand))
	 (dealer-hand-value (blackjack-dealer-hand-value dealer-hand 'soft))
	 (dealer-busted (blackjack-dealer-hand-is-busted dealer-hand))
	 (player-hands (slot-value game 'player-hands)))
    (dotimes (x (length player-hands))
      (blackjack-pay-player-hand game (nth x player-hands) dealer-hand-value dealer-busted))
    (blackjack-normalize-current-bet game)
    (blackjack-save game)))

(defun blackjack-pay-player-hand (game player-hand dealer-hand-value dealer-hand-busted)
  "Pay GAME PLAYER-HAND based on DEALER-HAND-VALUE and DEALER-HAND-BUSTED."
  (if (not (slot-value player-hand 'payed))
      (progn
        (setf (slot-value player-hand 'payed) t)
        (let* ((player-hand-value nil))
          (setf player-hand-value (blackjack-player-hand-value (slot-value player-hand 'cards) 'soft))
          (if (blackjack-player-hand-won player-hand-value dealer-hand-value dealer-hand-busted)
	      (blackjack-pay-won-hand game player-hand)
	    (if (blackjack-player-hand-lost player-hand-value dealer-hand-value)
                (blackjack-collect-lost-hand game player-hand)
	      (setf (slot-value player-hand 'status) 'push)))))))

(defun blackjack-collect-lost-hand (game player-hand)
  "Collect bet into GAME money from losing PLAYER-HAND."
  (setf (slot-value game 'money) (- (slot-value game 'money) (slot-value player-hand 'bet))
	(slot-value player-hand 'status) 'lost))

(defun blackjack-pay-won-hand (game player-hand)
  "Pay winning PLAYER-HAND bet into GAME money."
  (let* ((bet (slot-value player-hand 'bet)))
    (if (blackjack-hand-is-blackjack (slot-value player-hand 'cards))
	(setf bet (truncate (* 1.5 bet))))
    (setf (slot-value game 'money) (+ (slot-value game 'money) bet)
	  (slot-value player-hand 'status) 'won
	  (slot-value player-hand 'bet) bet)))

(defun blackjack-player-hand-lost (player-hand-value dealer-hand-value)
  "Return non-nil if PLAYER-HAND-VALUE < DEALER-HAND-VALUE."
  (if (< player-hand-value dealer-hand-value)
      t))

(defun blackjack-player-hand-won (player-hand-value dealer-hand-value dealer-hand-busted)
  "Return non-nil if PLAYER-HAND-VALUE > DEALER-HAND-VALUE && !DEALER-HAND-BUSTED."
  (if (or
       dealer-hand-busted
       (> player-hand-value dealer-hand-value))
      t))

(defun blackjack-player-hand-done (game player-hand)
  "Return non-nil when GAME PLAYER-HAND is done."
  (if (not (blackjack-no-more-actions player-hand))
      nil
    (progn
      (setf (slot-value player-hand 'played) t)
      (if (and
           (not (slot-value player-hand 'payed))
           (blackjack-player-hand-is-busted (slot-value player-hand 'cards)))
          (blackjack-collect-busted-hand game player-hand))
      t)))

(defun blackjack-collect-busted-hand (game player-hand)
  "Collect bet from GAME PLAYER-HAND."
  (setf (slot-value player-hand 'payed) t
	(slot-value player-hand 'status) 'lost
	(slot-value game 'money) (- (slot-value game 'money)
				    (slot-value player-hand 'bet))))

(defun blackjack-no-more-actions (player-hand)
  "Return non-nil when PLAYER-HAND has no more actions."
  (let* ((cards (slot-value player-hand 'cards)))
    (or
     (slot-value player-hand 'played)
     (slot-value player-hand 'stood)
     (blackjack-hand-is-blackjack cards)
     (blackjack-player-hand-is-busted cards)
     (= 21 (blackjack-player-hand-value cards 'soft))
     (= 21 (blackjack-player-hand-value cards 'hard)))))

(defun blackjack-need-to-shuffle (game)
  "Is GAME shoe nearly exhausted?"
  (let* ((shoe (slot-value game 'shoe))
	 (cards-count (length shoe))
	 (num-decks (slot-value game 'num-decks)))
    (if (> cards-count 0)
	(progn
	  (let* ((used (- (blackjack-total-cards game) cards-count))
		 (spec (aref (slot-value game 'shuffle-specs) (1- (slot-value game 'num-decks)))))
	    (> (* 100 (/ (float used) cards-count)) spec)))
      t)))

(defun blackjack-total-cards (game)
  "Return total number of GAME cards per SHOE."
  (* (slot-value game 'cards-per-deck) (slot-value game 'num-decks)))

(defun blackjack-shuffle (game)
  "Fill a new GAME shoe with card VALUES."
  (let* ((total-cards (blackjack-total-cards game))
	 (shoe '())
	 (values '()))
    (setq values
	  (pcase (slot-value game 'deck-type)
	    ('regular (number-sequence 0 12))
	    ('aces '(0))
	    ('jacks '(10))
	    ('aces-jacks '(0 10))
	    ('sevens '(6))
	    ('eights '(7))))
    (while (< (length shoe) total-cards)
      (dotimes (suit 4)
	(dolist (value values)
	  (if (< (length shoe) total-cards)
	      (setf shoe (cons (blackjack-card :id (blackjack-next-id game) :value value :suit suit) shoe))))))
    (setf (slot-value game 'shoe) (blackjack-shuffle-loop shoe))))

(defun blackjack-shuffle-loop (shoe)
  "Shuffle SHOE."
  (dotimes (x (* 7 (length shoe)))
    (setf shoe (blackjack-move-rand-card shoe)))
  shoe)

(defun blackjack-move-rand-card (shoe)
  "Move a random card to the top of the SHOE."
  (let* ((rand (random (length shoe)))
         (card (nth rand shoe)))
    (setf shoe (cl-remove card shoe :count 1))
    (setf shoe (cons card shoe))
    shoe))

(defun blackjack-draw-hands (game)
  "Draw GAME dealer and player hands."
  (erase-buffer)
  (insert "\n  Dealer:\n")
  (blackjack-draw-dealer-hand game)
  (insert "\n\n  Player $")
  (insert (blackjack-format-money (/ (slot-value game 'money) 100)))
  (insert ":\n")
  (blackjack-draw-player-hands game))

(defun blackjack-format-money (money)
  "Format MONEY."
  (format "%.2f" money))

(defun blackjack-more-hands-to-play (game)
  "Are there more GAME hands to play?"
  (let* ((current-player-hand (slot-value game 'current-player-hand))
	 (player-hands (slot-value game 'player-hands)))
    (< current-player-hand (1- (length player-hands)))))

(defun blackjack-play-more-hands (game)
  "Advance to next GAME player hand."
  (let* ((player-hand nil))
    (setf (slot-value game 'current-player-hand) (1+ (slot-value game 'current-player-hand)))
    (setf player-hand (blackjack-current-player-hand game))
    (blackjack-deal-card game player-hand)
    (if (blackjack-player-hand-done game player-hand)
	(blackjack-process game)
      (progn
	(blackjack-draw-hands game)
	(blackjack-ask-hand-action game)))))

(defun blackjack-need-to-play-dealer-hand (game)
  "Do player hands require playing the GAME dealer hand?"
  (let* ((player-hands (slot-value game 'player-hands)))
    (cl-dolist (player-hand player-hands)
      (when (not (or
		  (blackjack-player-hand-is-busted (slot-value player-hand 'cards))
		  (blackjack-hand-is-blackjack (slot-value player-hand 'cards))))
	(cl-return t)))))

(defun blackjack-dealer-hand-counts (dealer-hand)
  "Calculates soft and hard counts for DEALER-HAND."
  (let* ((soft-count (blackjack-dealer-hand-value dealer-hand 'soft))
	 (hard-count (blackjack-dealer-hand-value dealer-hand 'hard))
	 (counts '()))
    (setf counts (cons hard-count counts))
    (setf counts (cons soft-count counts))
    counts))

(defun blackjack-deal-required-cards (game)
  "Dealer required cards for GAME dealer hand."
  (let* ((dealer-hand (slot-value game 'dealer-hand))
	 (counts (blackjack-dealer-hand-counts dealer-hand)))
    (while
	(and
	 (< (nth 0 counts) 18)
	 (< (nth 1 counts) 17))
      (blackjack-deal-card game dealer-hand)
      (setf counts (blackjack-dealer-hand-counts dealer-hand)))))

(defun blackjack-play-dealer-hand (game)
  "Player GAME dealer hand."
  (let* ((playing (blackjack-need-to-play-dealer-hand game))
	 (dealer-hand (slot-value game 'dealer-hand))
	 (cards (slot-value dealer-hand 'cards)))
    (if
	(or
	 playing
	 (blackjack-hand-is-blackjack cards))
	(setf (slot-value dealer-hand 'hide-down-card) nil))
    (if playing
	(blackjack-deal-required-cards game))
    (setf (slot-value dealer-hand 'played) t)
    (blackjack-pay-hands game)
    (blackjack-draw-hands game)
    (blackjack-ask-bet-action game)))

(defun blackjack-process (game)
  "Handle more GAME hands to play."
  (if (blackjack-more-hands-to-play game)
      (blackjack-play-more-hands game)
    (blackjack-play-dealer-hand game)))

(defun blackjack-hit (game)
  "Deal a new card to the current GAME player hand."
  (let* ((player-hand (blackjack-current-player-hand game))
	 (cards (slot-value player-hand 'cards)))
    (blackjack-deal-card game player-hand)
    (if (blackjack-player-hand-done game player-hand)
	(blackjack-process game)
      (progn
	(blackjack-draw-hands game)
	(blackjack-ask-hand-action game)))))

(defun blackjack-double (game)
  "Double the current GAME player hand."
  (let* ((player-hand (blackjack-current-player-hand game)))
    (blackjack-deal-card game player-hand)
    (setf (slot-value player-hand 'played) t
	  (slot-value player-hand 'bet) (* 2 (slot-value player-hand 'bet)))
    (if (blackjack-player-hand-done game player-hand)
	(blackjack-process game))))

(defun blackjack-stand (game)
  "End the current GAME player hand."
  (let* ((player-hand (blackjack-current-player-hand game)))
    (setf
     (slot-value player-hand 'stood) t
     (slot-value player-hand 'played) t)
    (blackjack-process game)))

(defun blackjack-split (game)
  "Split the current GAME player hand."
  (let* ((player-hands (slot-value game 'player-hands))
	 (player-hand nil)
	 (card nil)
	 (hand nil)
	 (x 0))
    (setf hand (blackjack-player-hand :id (blackjack-next-id game) :bet (slot-value game 'current-bet)))
    (add-to-list 'player-hands hand :append)
    (setf (slot-value game 'player-hands) player-hands)
    (setf x (1- (length player-hands)))
    (while (> x (slot-value game 'current-player-hand))
      (setf player-hand (nth (1- x) player-hands))
      (setf hand (nth x player-hands))
      (setf (slot-value hand 'cards) (slot-value player-hand 'cards))
      (setf x (1- x)))
    (setf player-hand (nth (slot-value game 'current-player-hand) player-hands))
    (setf hand (nth (1+ (slot-value game 'current-player-hand)) player-hands))
    (setf (slot-value hand 'cards) '())
    (setf card (nth 1 (slot-value player-hand 'cards)))
    (push card (slot-value hand 'cards))
    (setf (slot-value player-hand 'cards) (cl-remove card (slot-value player-hand 'cards) :count 1))
    (blackjack-deal-card game player-hand)
    (if (blackjack-player-hand-done game player-hand)
	(blackjack-process game)
      (progn
	(blackjack-draw-hands game)
	(blackjack-ask-hand-action game)))))

(defun blackjack-can-hit (game)
  "Return non-nil if the current GAME player hand can hit."
  (let* ((player-hand (blackjack-current-player-hand game))
	 (cards (slot-value player-hand 'cards)))
    (not (or
	  (slot-value player-hand 'played)
	  (slot-value player-hand 'stood)
	  (= 21 (blackjack-player-hand-value cards 'hard))
	  (blackjack-hand-is-blackjack cards)
	  (blackjack-player-hand-is-busted cards)))))

(defun blackjack-can-stand (game)
  "Return non-nil if the current GAME player hand can stand."
  (let* ((player-hand (blackjack-current-player-hand game))
	 (cards (slot-value player-hand 'cards)))
    (not (or
	  (slot-value player-hand 'stood)
	  (blackjack-player-hand-is-busted cards)
	  (blackjack-hand-is-blackjack cards)))))

(defun blackjack-can-split (game)
  "Return non-nil if the current GAME player hand can split."
  (let* ((player-hand (blackjack-current-player-hand game))
	 (cards (slot-value player-hand 'cards)))
    (if (and
         (not (slot-value player-hand 'stood))
         (< (length (slot-value game 'player-hands)) 7)
	 (>= (slot-value game 'money) (+ (blackjack-all-bets game) (slot-value player-hand 'bet)))
	 (eq (length cards) 2))
        (let* ((card-0 (nth 0 cards))
	       (card-1 (nth 1 cards)))
	  (if (eq (slot-value card-0 'value) (slot-value card-1 'value))
	      t)))))

(defun blackjack-can-double (game)
  "Return non-nil if the current GAME player hand can double."
  (let* ((player-hand (blackjack-current-player-hand game))
	 (cards (slot-value player-hand 'cards)))
    (if (and
         (>= (slot-value game 'money) (+ (blackjack-all-bets game) (slot-value player-hand 'bet)))
         (not (or (slot-value player-hand 'stood) (not (eq 2 (length cards)))
                  (blackjack-hand-is-blackjack cards))))
        t)))

(defun blackjack-current-player-hand (game)
  "Return current GAME player hand."
  (nth (slot-value game 'current-player-hand) (slot-value game 'player-hands)))

(defun blackjack-all-bets (game)
  "Sum of all GAME player hand bets."
  (let* ((player-hands (slot-value game 'player-hands))
	 (total 0))
    (dotimes (x (length player-hands))
      (setf total (+ total (slot-value (nth x player-hands) 'bet))))
    total))

(defun blackjack-ask-hand-action (game)
  "Ask hand action for GAME."
  (let* ((answer (blackjack-hand-actions-menu game)))
    (pcase answer
      ("stand" (if (blackjack-can-stand game)
		   (blackjack-stand game)
		 (blackjack-ask-hand-action game)))
      ("hit" (if (blackjack-can-hit game)
		 (blackjack-hit game)
	       (blackjack-ask-hand-action game)))
      ("split" (if (blackjack-can-split game)
		   (blackjack-split game)
		 (blackjack-ask-hand-action game)))
      ("double" (if (blackjack-can-double game)
		    (blackjack-double game)
		  (blackjack-ask-hand-action game))))))

(defun blackjack-hand-actions-menu (game)
  "Hand actions menu for GAME."
  (let* ((read-answer-short t)
	 (actions '(("help" ?? "show help"))))
    (if (blackjack-can-hit game)
        (setf actions (cons '("hit" ?h "deal a new card") actions)))
    (if (blackjack-can-stand game)
        (setf actions (cons '("stand" ?s "end current hand with no further actions") actions)))
    (if (blackjack-can-split game)
        (setf actions (cons '("split" ?p "split hand into two hands") actions)))
    (if (blackjack-can-double game)
        (setf actions (cons '("double" ?d "double bet, deal a new card, and end hand") actions)))
    (read-answer "Hand Action " actions)))

(defun blackjack-ask-insurance-action (game)
  "Ask about insuring GAME hand."
  (let* ((answer (blackjack-ask-insurance-menu game)))
    (pcase answer
      ("yes" (blackjack-insure-hand game))
      ("no" (blackjack-no-insurance game))
      ("help" ?? "show help"))))

(defun blackjack-insure-hand (game)
  "Insure GAME hand."
  (let* ((player-hand (blackjack-current-player-hand game))
	 (bet (slot-value player-hand 'bet))
	 (new-bet (/ bet 2))
	 (money (slot-value game 'money)))
    (setf (slot-value player-hand 'bet) new-bet
	  (slot-value player-hand 'played) t
	  (slot-value player-hand 'payed) t
	  (slot-value player-hand 'status) 'lost
	  (slot-value game 'money) (- money new-bet))
    (blackjack-draw-hands game)
    (blackjack-ask-bet-action game)))

(defun blackjack-no-insurance (game)
  "Decline GAME hand insurance."
  (let* ((dealer-hand (slot-value game 'dealer-hand))
	 (dealer-hand-cards (slot-value dealer-hand 'cards)))
    (if (blackjack-hand-is-blackjack dealer-hand-cards)
	(progn
	  (setf (slot-value dealer-hand 'hide-down-card) nil)
	  (blackjack-pay-hands game)
	  (blackjack-draw-hands game)
	  (blackjack-ask-bet-action game))
      (let* ((player-hand (blackjack-current-player-hand game)))
	(if (blackjack-player-hand-done game player-hand)
	    (blackjack-play-dealer-hand game)
	  (progn
	    (blackjack-draw-hands game)
	    (blackjack-ask-hand-action game)))))))

(defun blackjack-ask-insurance-menu (game)
  "Ask about insuring GAME hand."
  (let* ((read-answer-short t))
    (read-answer "Hand Insurance: "
                 '(("yes" ?y "insure hand")
                   ("no" ?n "no insurance")
                   ("help" ?? "show help")))))

(defun blackjack-ask-bet-action (game)
  "Ask about next GAME bet action."
  (let* ((answer (blackjack-bet-actions-menu game)))
    (pcase answer
      ("deal" nil)
      ("bet" (blackjack-ask-new-bet game))
      ("options" (blackjack-ask-game-options game))
      ("quit" (setf (slot-value game 'quitting) t)))))

(defun blackjack-bet-actions-menu (game)
  "Bet actions menu for GAME."
  (let* ((read-answer-short t))
    (read-answer "Game Actions: "
                 '(("deal" ?d "deal new hand")
                   ("bet" ?b "change current bet")
                   ("options" ?o "change game options")
                   ("quit" ?q "quit blackjack")
                   ("help" ?? "show help")))))

(defun blackjack-ask-new-bet (game)
  "Update the current GAME bet."
  (let* ((answer (blackjack-new-bet-menu game))
	 (bet 0))
    (setf bet (* 100 (string-to-number answer)))
    (setf (slot-value game 'current-bet) bet)
    (blackjack-normalize-current-bet game)))

(defun blackjack-new-bet-menu (game)
  "New GAME bet menu."
  (read-string "Bet Amount: "))

(defun blackjack-ask-new-number-decks (game)
  "Ask for new number of GAME decks."
  (let* ((answer (blackjack-new-number-decks-menu game))
	 (num-decks 1))
    (setf num-decks (string-to-number answer))
    (if (< num-decks 1)
	(setf num-decks 1))
    (if (> num-decks 8)
	(setf num-decks 8))
    (setf (slot-value game 'num-decks) num-decks)))

(defun blackjack-new-number-decks-menu (game)
  "New GAME number of decks menu."
  (read-string "Number of Decks: "))

(defun blackjack-ask-game-options (game)
  "Ask about which GAME option to update."
  (let* ((answer (blackjack-game-options-menu game)))
    (pcase answer
      ("number-decks" (blackjack-ask-new-number-decks game))
      ("deck-type" (blackjack-ask-new-deck-type game))
      ("face-type" (blackjack-ask-new-face-type game))
      ("back" (blackjack-ask-bet-action game)))))

(defun blackjack-game-options-menu (game)
  "GAME options menu."
  (let* ((read-answer-short t))
    (read-answer "Options: "
                 '(("number-decks" ?n "change number of decks")
                   ("deck-type" ?t "change the deck type")
                   ("face-type" ?f "change the card face type")
                   ("back" ?b "go back to previous menu")
                   ("help" ?? "show help")))))

(defun blackjack-ask-new-deck-type (game)
  "Ask for new GAME deck type."
  (let* ((answer (blackjack-deck-type-menu game))
	 (deck-type (intern answer)))
     (setf (slot-value game 'deck-type) deck-type)
     (blackjack-shuffle game)
     (blackjack-save game)))

(defun blackjack-deck-type-menu (game)
  "New GAME deck type menu."
  (let* ((read-answer-short t))
    (read-answer "Deck Type: "
                 '(("regular" ?1 "regular deck")
		   ("aces" ?2 "deck of aces")
		   ("jacks" ?3 "deck of jacks")
		   ("aces-jacks" ?4 "deck of aces and jacks")
		   ("sevens" ?5 "deck of sevens")
		   ("eights" ?6 "deck of eights")
                   ("help" ?? "show help")))))

(defun blackjack-ask-new-face-type (game)
  "Ask for new GAME face type."
  (let* ((answer (blackjack-face-type-menu game))
	 (face-type (intern answer)))
    (setf (slot-value game 'face-type) face-type)
    (blackjack-save game)))

(defun blackjack-face-type-menu (game)
  "New GAME face type menu."
  (let* ((read-answer-short t))
    (read-answer "Card Face Type: "
                 '(("ascii" ?a "use ascii face type")
		   ("unicode" ?u "use unicode face type")
                   ("help" ?? "show help")))))

(defun blackjack-player-hand-is-busted (cards)
  "Return non-nil if CARDS value is more than 21."
  (> (blackjack-player-hand-value cards 'soft) 21))

(defun blackjack-dealer-hand-is-busted (dealer-hand)
  "Return non-nil if DEALER-HAND cards value is more than 21."
  (let* ((cards (slot-value dealer-hand 'cards)))
    (> (blackjack-dealer-hand-value dealer-hand 'soft) 21)))

(defun blackjack-hand-is-blackjack (cards)
  "Return non-nil if hand CARDS is blackjack."
  (if (eq 2 (length cards))
      (let* ((card-0 (nth 0 cards))
	     (card-1 (nth 1 cards)))
        (or
	 (and
	  (blackjack-is-ace card-0)
	  (blackjack-is-ten card-1))
	 (and
	  (blackjack-is-ace card-1)
	  (blackjack-is-ten card-0))))))

(defun blackjack-dealer-upcard-is-ace (dealer-hand)
  "Return non-nil if DEALER-HAND upcard is an ace."
  (blackjack-is-ace (nth 1 (slot-value dealer-hand 'cards))))

(defun blackjack-draw-dealer-hand (game)
  "Draw the GAME dealer-hand."
  (let* ((dealer-hand (slot-value game 'dealer-hand))
	 (cards (slot-value dealer-hand 'cards))
         (hide-down-card (slot-value dealer-hand 'hide-down-card))
         (card nil)
         (suit nil)
         (value nil))
    (insert "  ")
    (dotimes (x (length cards))
      (setf card (nth x cards))
      (if (and hide-down-card (= x 0))
          (progn
	    (setf value 13)
	    (setf suit 0))
        (progn
          (setf value (slot-value card 'value))
          (setf suit (slot-value card 'suit))))
      (insert (blackjack-card-face game value suit))
      (insert " "))
    (insert " ⇒  ")
    (insert (number-to-string (blackjack-dealer-hand-value dealer-hand 'soft)))))

(defun blackjack-dealer-hand-value (dealer-hand count-method)
  "Calculates DEALER-HAND cards total value based on COUNT-METHOD."
  (let* ((cards (slot-value dealer-hand 'cards))
         (hide-down-card (slot-value dealer-hand 'hide-down-card))
	 (total 0)
         (card nil))
    (dotimes (x (length cards))
      (if (not (and hide-down-card (= x 0)))
          (progn
	    (setf card (nth x cards))
	    (setf total (+ total (blackjack-card-val card count-method total))))))
    (if (and (eq count-method 'soft) (> total 21))
        (setf total (blackjack-dealer-hand-value dealer-hand 'hard)))
    total))

(defun blackjack-draw-player-hands (game)
  "Draw GAME players hands."
  (let* ((player-hands (slot-value game 'player-hands))
	 (player-hand nil))
    (dotimes (x (length player-hands))
      (setf player-hand (nth x player-hands))
      (blackjack-draw-player-hand game player-hand x))))

(defun blackjack-draw-player-hand (game player-hand index)
  "Draw the GAME PLAYER-HAND by INDEX."
  (insert (blackjack-player-hand-cards game player-hand))
  (insert (blackjack-player-hand-money game player-hand index))
  (insert (blackjack-player-hand-status player-hand))
  (insert "\n\n"))

(defun blackjack-player-hand-cards (game player-hand)
  "Draw GAME PLAYER-HAND cards."
  (let* ((cards (slot-value player-hand 'cards))
	 (card nil)
	 (suit nil)
	 (value nil)
	 (out "  "))
    (dotimes (x (length cards))
      (setf card (nth x cards))
      (setf value (slot-value card 'value))
      (setf suit (slot-value card 'suit))
      (setf out (concat out (blackjack-card-face game value suit)))
      (setf out (concat out " ")))
    (setf out (concat out " ⇒  "))
    (setf out (concat out (number-to-string (blackjack-player-hand-value cards 'soft)) "  "))
    out))

(defun blackjack-player-hand-status (player-hand)
  "Return PLAYER-HAND status."
  (let* ((cards (slot-value player-hand 'cards))
	 (status (slot-value player-hand 'status)))
    (pcase status
      ('lost (if (blackjack-player-hand-is-busted cards)
		 "Busted!"
	       "Lost!"))
      ('won (if (blackjack-hand-is-blackjack cards)
		"Blackjack!"
	      "Won!"))
      ('push "Push")
      ('unknown ""))))

(defun blackjack-player-hand-money (game player-hand index)
  "Return GAME PLAYER-HAND money by INDEX."
  (let* ((current-hand (slot-value game 'current-player-hand))
	 (played (slot-value player-hand 'played))
	 (status (slot-value player-hand 'status))
	 (bet (slot-value player-hand 'bet))
	 (out ""))
    (if (equal status 'lost)
	(setf out (concat out "-")))
    (if (equal status 'won)
	(setf out (concat out "+")))
    (setf out (concat out "$" (blackjack-format-money (/ bet 100.0))))
    (if (and
	 (not played)
	 (= index current-hand))
	(setf out (concat out " ⇐")))
    (setf out (concat out "  "))
    out))

(defun blackjack-player-hand-value (cards count-method)
  "Calculates CARDS total value based on COUNT-METHOD."
  (let* ((total 0)
	 (card nil))
    (dotimes (x (length cards))
      (setf card (nth x cards))
      (setf total (+ total (blackjack-card-val card count-method total))))
    (if (and (eq count-method 'soft) (> total 21))
        (setf total (blackjack-player-hand-value cards 'hard)))
    total))

(defun blackjack-card-val (card count-method total)
  "Calculates CARD value based on COUNT-METHOD and running hand TOTAL."
  (let* ((value (1+ (slot-value card 'value))))
    (if (> value 9)
        (setf value 10))
    (if (and (eq count-method 'soft) (eq value 1) (< total 11))
        (setf value 11))
    value))

(defun blackjack-card-face (game value suit)
  "Return GAME card face based on VALUE and SUIT."
  (let* ((face nil))
    (if (eq (slot-value game 'face-type) 'unicode)
	(setq face (slot-value game 'faces-unicode))
      (setq face (slot-value game 'faces-ascii)))
    (aref (aref face value) suit)))

(defun blackjack-is-ace (card)
  "Is the CARD an ace?"
  (= 0 (slot-value card 'value)))

(defun blackjack-is-ten (card)
  "Is the CARD a 10 value?"
  (> (slot-value card 'value) 8))

(defun blackjack-normalize-current-bet (game)
  "Normalize current GAME bet."
  (let* ((min-bet (slot-value game 'min-bet))
	 (max-bet (slot-value game 'max-bet))
	 (current-bet (slot-value game 'current-bet))
	 (money (slot-value game 'money)))
    (if (< current-bet min-bet)
	(setf current-bet min-bet))
    (if (> current-bet max-bet)
	(setf current-bet max-bet))
    (if (> current-bet money)
	(setf current-bet money))
    (setf (slot-value game 'current-bet) current-bet)))

(defun blackjack-load-saved-game (game)
  "Load persisted GAME state."
  (let* ((content nil)
	 (parts '()))
    (ignore-errors
      (with-temp-buffer
	(insert-file-contents "blackjack.txt")
	(setf content (buffer-string))))
    (if (not (eq content nil))
	(setf parts (split-string content "|")))
    (if (= (length parts) 5)
	(progn
	  (setf (slot-value game 'num-decks) (string-to-number (nth 0 parts))
		(slot-value game 'deck-type) (intern (nth 1 parts))
		(slot-value game 'face-type) (intern (nth 2 parts))
		(slot-value game 'money) (string-to-number (nth 3 parts))
		(slot-value game 'current-bet) (string-to-number (nth 4 parts)))))))

(defun blackjack-save (game)
  "Persist GAME state."
  (ignore-errors
    (with-temp-file "blackjack.txt"
      (insert (format "%s|%s|%s|%s|%s"
		      (slot-value game 'num-decks)
		      (slot-value game 'deck-type)
		      (slot-value game 'face-type)
		      (slot-value game 'money)
		      (slot-value game 'current-bet))))))

(defun blackjack ()
  "Run Blackjack."
  (interactive)
  (let* ((debug-on-error t))
    (let* ((buffer-name "blackjack")
	   (game (blackjack-game)))
      (blackjack-load-saved-game game)
      (get-buffer-create buffer-name)
      (switch-to-buffer buffer-name)
      (with-current-buffer buffer-name
	(while (not (slot-value game 'quitting))
	  (blackjack-deal-new-hand game))))
    (quit-window)))

(provide 'blackjack)
;;; blackjack.el ends here
