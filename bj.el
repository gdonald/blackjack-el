;;; bj.el --- The game of Blackjack

;; Copyright (C) 2022 Greg Donald

;; Author: Greg Donald <gdonald@gmail.com>
;; Version: 1.0
;; Package-Requires: ((emacs "26.2"))
;; Keywords: games
;; URL: https://https://github.com/gdonald/bj-el

;;; Commentary:
;;; This package lets you play Blackjack in Emacs.

;;; Code:

(require 'cl-lib)
(require 'eieio)

(defclass bj-card ()
  ((id :initarg :id :initform 0 :type integer)
   (value :initarg :value :initform 0 :type integer)
   (suit :initarg :suit :initform 0 :type integer)))

(cl-defmethod cl-print-object ((obj bj-card) stream)
  "Print OBJ to STREAM."
  (princ
   (format "#<%s id: %d value: %s suit: %s>"
           (eieio-class-name (eieio-object-class obj))
	   (slot-value obj 'id)
           (slot-value obj 'value)
	   (slot-value obj 'suit))
   stream))

(defclass bj-hand ()
  ((cards :initarg :cards :initform '() :type list)
   (played :initarg :played :initform nil :type boolean)))

(defclass bj-player-hand (bj-hand)
  ((id :initarg :id :initform 0 :type integer)
   (bet :initarg :bet :initform 0 :type integer)
   (status :initarg :status :initform 'unknown :type symbol)
   (payed :initarg :payed :initform nil :type boolean)
   (stood :intiarg :stood :initform nil :type boolean)))

(cl-defmethod cl-print-object ((obj bj-player-hand) stream)
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

(defclass bj-dealer-hand (bj-hand)
  ((hide-down-card :initarg :hide-down-card :initform t :type boolean)))

(cl-defmethod cl-print-object ((obj bj-dealer-hand) stream)
  "Print OBJ to STREAM."
  (princ
   (format "#<%s cards: %s played: %s hide-down-card: %s>"
           (eieio-class-name (eieio-object-class obj))
           (slot-value obj 'cards)
	   (slot-value obj 'played)
	   (slot-value obj 'hide-down-card))
   stream))

(defclass bj-game ()
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

(defun bj-deal-new-hand (game)
  "Deal new GAME hands."
  (if (bj-need-to-shuffle game)
      (bj-shuffle game))
  (let* ((shoe (slot-value game 'shoe))
	 (player-hand nil)
	 (dealer-hand nil))
    (setf (slot-value game 'player-hands) '())
    (setf player-hand (bj-player-hand :id (bj-next-id game) :bet (slot-value game 'current-bet)))
    (setf dealer-hand (bj-dealer-hand))
    (dotimes (x 2)
      (bj-deal-card game player-hand)
      (bj-deal-card game dealer-hand))
    (push player-hand (slot-value game 'player-hands))
    (setf (slot-value game 'current-player-hand) 0)
    (setf (slot-value game 'dealer-hand) dealer-hand)
    (if (and
	 (bj-dealer-upcard-is-ace dealer-hand)
	 (not (bj-hand-is-blackjack (slot-value player-hand 'cards))))
        (progn
          (bj-draw-hands game)
	  (bj-ask-insurance-action game))
      (if (bj-player-hand-done game player-hand)
          (progn
	    (setf (slot-value dealer-hand 'hide-down-card) nil)
            (bj-pay-hands game)
            (bj-draw-hands game)
            (bj-ask-bet-action game))
        (progn
          (bj-draw-hands game)
          (bj-ask-hand-action game)
          (bj-save game))))))

(defun bj-deal-card (game hand)
  "Deal a card into HAND from GAME shoe."
  (let* ((shoe (slot-value game 'shoe))
	 (cards (slot-value hand 'cards))
	 (card nil))
    (setf card (car shoe))
    (add-to-list 'cards card :append)
    (setf shoe (cl-remove card shoe :count 1))
    (setf (slot-value hand 'cards) cards)
    (setf (slot-value game 'shoe) shoe)))

(defun bj-next-id (game)
  "Return next GAME object id."
  (let* ((id (slot-value game 'id)))
    (setf (slot-value game 'id) (1+ id))))

(defun bj-pay-hands (game)
  "Pay GAME player hands."
  (let* ((dealer-hand (slot-value game 'dealer-hand))
	 (dealer-hand-value (bj-dealer-hand-value dealer-hand 'soft))
	 (dealer-busted (bj-dealer-hand-is-busted dealer-hand))
	 (player-hands (slot-value game 'player-hands)))
    (dotimes (x (length player-hands))
      (bj-pay-player-hand game (nth x player-hands) dealer-hand-value dealer-busted))
    (bj-normalize-current-bet game)
    (bj-save game)))

(defun bj-pay-player-hand (game player-hand dealer-hand-value dealer-hand-busted)
  "Pay GAME PLAYER-HAND based on DEALER-HAND-VALUE and DEALER-HAND-BUSTED."
  (if (not (slot-value player-hand 'payed))
      (progn
        (setf (slot-value player-hand 'payed) t)
        (let* ((player-hand-value nil))
          (setf player-hand-value (bj-player-hand-value (slot-value player-hand 'cards) 'soft))
          (if (bj-player-hand-won player-hand-value dealer-hand-value dealer-hand-busted)
	      (bj-pay-won-hand game player-hand)
	    (if (bj-player-hand-lost player-hand-value dealer-hand-value)
                (bj-collect-lost-hand game player-hand)
	      (setf (slot-value player-hand 'status) 'push)))))))

(defun bj-collect-lost-hand (game player-hand)
  "Collect bet into GAME money from losing PLAYER-HAND."
  (setf (slot-value game 'money) (- (slot-value game 'money) (slot-value player-hand 'bet))
	(slot-value player-hand 'status) 'lost))

(defun bj-pay-won-hand (game player-hand)
  "Pay winning PLAYER-HAND bet into GAME money."
  (let* ((bet (slot-value player-hand 'bet)))
    (if (bj-hand-is-blackjack (slot-value player-hand 'cards))
	(setf bet (truncate (* 1.5 bet))))
    (setf (slot-value game 'money) (+ (slot-value game 'money) bet)
	  (slot-value player-hand 'status) 'won
	  (slot-value player-hand 'bet) bet)))

(defun bj-player-hand-lost (player-hand-value dealer-hand-value)
  "Return non-nil if PLAYER-HAND-VALUE < DEALER-HAND-VALUE."
  (if (< player-hand-value dealer-hand-value)
      t))

(defun bj-player-hand-won (player-hand-value dealer-hand-value dealer-hand-busted)
  "Return non-nil if PLAYER-HAND-VALUE > DEALER-HAND-VALUE && !DEALER-HAND-BUSTED."
  (if (or
       dealer-hand-busted
       (> player-hand-value dealer-hand-value))
      t))

(defun bj-player-hand-done (game player-hand)
  "Return non-nil when GAME PLAYER-HAND is done."
  (if (not (bj-no-more-actions player-hand))
      nil
    (progn
      (setf (slot-value player-hand 'played) t)
      (if (and
           (not (slot-value player-hand 'payed))
           (bj-player-hand-is-busted (slot-value player-hand 'cards)))
          (bj-collect-busted-hand game player-hand))
      t)))

(defun bj-collect-busted-hand (game player-hand)
  "Collect bet from GAME PLAYER-HAND."
  (setf (slot-value player-hand 'payed) t
	(slot-value player-hand 'status) 'lost
	(slot-value game 'money) (- (slot-value game 'money)
				    (slot-value player-hand 'bet))))

(defun bj-no-more-actions (player-hand)
  "Return non-nil when PLAYER-HAND has no more actions."
  (let* ((cards (slot-value player-hand 'cards)))
    (or
     (slot-value player-hand 'played)
     (slot-value player-hand 'stood)
     (bj-hand-is-blackjack cards)
     (bj-player-hand-is-busted cards)
     (= 21 (bj-player-hand-value cards 'soft))
     (= 21 (bj-player-hand-value cards 'hard)))))

(defun bj-need-to-shuffle (game)
  "Is GAME shoe nearly exhausted?"
  (let* ((shoe (slot-value game 'shoe))
	 (cards-count (length shoe))
	 (num-decks (slot-value game 'num-decks)))
    (if (> cards-count 0)
	(progn
	  (let* ((used (- (bj-total-cards game) cards-count))
		 (spec (aref (slot-value game 'shuffle-specs) (1- (slot-value game 'num-decks)))))
	    (> (* 100 (/ (float used) cards-count)) spec)))
      t)))

(defun bj-total-cards (game)
  "Return total number of GAME cards per SHOE."
  (* (slot-value game 'cards-per-deck) (slot-value game 'num-decks)))

(defun bj-shuffle (game)
  "Fill a new GAME shoe with card VALUES."
  (let* ((total-cards (bj-total-cards game))
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
	      (setf shoe (cons (bj-card :id (bj-next-id game) :value value :suit suit) shoe))))))
    (setf (slot-value game 'shoe) (bj-shuffle-loop shoe))))

(defun bj-shuffle-loop (shoe)
  "Shuffle SHOE."
  (dotimes (x (* 7 (length shoe)))
    (setf shoe (bj-move-rand-card shoe)))
  shoe)

(defun bj-move-rand-card (shoe)
  "Move a random card to the top of the SHOE."
  (let* ((rand (random (length shoe)))
         (card (nth rand shoe)))
    (setf shoe (cl-remove card shoe :count 1))
    (setf shoe (cons card shoe))
    shoe))

(defun bj-draw-hands (game)
  "Draw GAME dealer and player hands."
  (erase-buffer)
  (insert "\n  Dealer:\n")
  (bj-draw-dealer-hand game)
  (insert "\n\n  Player $")
  (insert (bj-format-money (/ (slot-value game 'money) 100)))
  (insert ":\n")
  (bj-draw-player-hands game))

(defun bj-format-money (money)
  "Format MONEY."
  (format "%.2f" money))

(defun bj-more-hands-to-play (game)
  "Are there more GAME hands to play?"
  (let* ((current-player-hand (slot-value game 'current-player-hand))
	 (player-hands (slot-value game 'player-hands)))
    (< current-player-hand (1- (length player-hands)))))

(defun bj-play-more-hands (game)
  "Advance to next GAME player hand."
  (let* ((player-hand nil))
    (setf (slot-value game 'current-player-hand) (1+ (slot-value game 'current-player-hand)))
    (setf player-hand (bj-current-player-hand game))
    (bj-deal-card game player-hand)
    (if (bj-player-hand-done game player-hand)
	(bj-process game)
      (progn
	(bj-draw-hands game)
	(bj-ask-hand-action game)))))

(defun bj-need-to-play-dealer-hand (game)
  "Do player hands require playing the GAME dealer hand?"
  (let* ((player-hands (slot-value game 'player-hands)))
    (cl-dolist (player-hand player-hands)
      (when (not (or
		  (bj-player-hand-is-busted (slot-value player-hand 'cards))
		  (bj-hand-is-blackjack (slot-value player-hand 'cards))))
	(cl-return t)))))

(defun bj-dealer-hand-counts (dealer-hand)
  "Calculates soft and hard counts for DEALER-HAND."
  (let* ((soft-count (bj-dealer-hand-value dealer-hand 'soft))
	 (hard-count (bj-dealer-hand-value dealer-hand 'hard))
	 (counts '()))
    (setf counts (cons hard-count counts))
    (setf counts (cons soft-count counts))
    counts))

(defun bj-deal-required-cards (game)
  "Dealer required cards for GAME dealer hand."
  (let* ((dealer-hand (slot-value game 'dealer-hand))
	 (counts (bj-dealer-hand-counts dealer-hand)))
    (while
	(and
	 (< (nth 0 counts) 18)
	 (< (nth 1 counts) 17))
      (bj-deal-card game dealer-hand)
      (setf counts (bj-dealer-hand-counts dealer-hand)))))

(defun bj-play-dealer-hand (game)
  "Player GAME dealer hand."
  (let* ((playing (bj-need-to-play-dealer-hand game))
	 (dealer-hand (slot-value game 'dealer-hand))
	 (cards (slot-value dealer-hand 'cards)))
    (if
	(or
	 playing
	 (bj-hand-is-blackjack cards))
	(setf (slot-value dealer-hand 'hide-down-card) nil))
    (if playing
	(bj-deal-required-cards game))
    (setf (slot-value dealer-hand 'played) t)
    (bj-pay-hands game)
    (bj-draw-hands game)
    (bj-ask-bet-action game)))

(defun bj-process (game)
  "Handle more GAME hands to play."
  (if (bj-more-hands-to-play game)
      (bj-play-more-hands game)
    (bj-play-dealer-hand game)))

(defun bj-hit (game)
  "Deal a new card to the current GAME player hand."
  (let* ((player-hand (bj-current-player-hand game))
	 (cards (slot-value player-hand 'cards)))
    (bj-deal-card game player-hand)
    (if (bj-player-hand-done game player-hand)
	(bj-process game)
      (progn
	(bj-draw-hands game)
	(bj-ask-hand-action game)))))

(defun bj-double (game)
  "Double the current GAME player hand."
  (let* ((player-hand (bj-current-player-hand game)))
    (bj-deal-card game player-hand)
    (setf (slot-value player-hand 'played) t
	  (slot-value player-hand 'bet) (* 2 (slot-value player-hand 'bet)))
    (if (bj-player-hand-done game player-hand)
	(bj-process game))))

(defun bj-stand (game)
  "End the current GAME player hand."
  (let* ((player-hand (bj-current-player-hand game)))
    (setf
     (slot-value player-hand 'stood) t
     (slot-value player-hand 'played) t)
    (bj-process game)))

(defun bj-split (game)
  "Split the current GAME player hand."
  (let* ((player-hands (slot-value game 'player-hands))
	 (player-hand nil)
	 (card nil)
	 (hand nil)
	 (x 0))
    (setf hand (bj-player-hand :id (bj-next-id game) :bet (slot-value game 'current-bet)))
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
    (bj-deal-card game player-hand)
    (if (bj-player-hand-done game player-hand)
	(bj-process game)
      (progn
	(bj-draw-hands game)
	(bj-ask-hand-action game)))))

(defun bj-can-hit (game)
  "Return non-nil if the current GAME player hand can hit."
  (let* ((player-hand (bj-current-player-hand game))
	 (cards (slot-value player-hand 'cards)))
    (not (or
	  (slot-value player-hand 'played)
	  (slot-value player-hand 'stood)
	  (= 21 (bj-player-hand-value cards 'hard))
	  (bj-hand-is-blackjack cards)
	  (bj-player-hand-is-busted cards)))))

(defun bj-can-stand (game)
  "Return non-nil if the current GAME player hand can stand."
  (let* ((player-hand (bj-current-player-hand game))
	 (cards (slot-value player-hand 'cards)))
    (not (or
	  (slot-value player-hand 'stood)
	  (bj-player-hand-is-busted cards)
	  (bj-hand-is-blackjack cards)))))

(defun bj-can-split (game)
  "Return non-nil if the current GAME player hand can split."
  (let* ((player-hand (bj-current-player-hand game))
	 (cards (slot-value player-hand 'cards)))
    (if (and
         (not (slot-value player-hand 'stood))
         (< (length (slot-value game 'player-hands)) 7)
	 (>= (slot-value game 'money) (+ (bj-all-bets game) (slot-value player-hand 'bet)))
	 (eq (length cards) 2))
        (let* ((card-0 (nth 0 cards))
	       (card-1 (nth 1 cards)))
	  (if (eq (slot-value card-0 'value) (slot-value card-1 'value))
	      t)))))

(defun bj-can-double (game)
  "Return non-nil if the current GAME player hand can double."
  (let* ((player-hand (bj-current-player-hand game))
	 (cards (slot-value player-hand 'cards)))
    (if (and
         (>= (slot-value game 'money) (+ (bj-all-bets game) (slot-value player-hand 'bet)))
         (not (or (slot-value player-hand 'stood) (not (eq 2 (length cards)))
                  (bj-hand-is-blackjack cards))))
        t)))

(defun bj-current-player-hand (game)
  "Return current GAME player hand."
  (nth (slot-value game 'current-player-hand) (slot-value game 'player-hands)))

(defun bj-all-bets (game)
  "Sum of all GAME player hand bets."
  (let* ((player-hands (slot-value game 'player-hands))
	 (total 0))
    (dotimes (x (length player-hands))
      (setf total (+ total (slot-value (nth x player-hands) 'bet))))
    total))

(defun bj-ask-hand-action (game)
  "Ask hand action for GAME."
  (let* ((answer (bj-hand-actions-menu game)))
    (pcase answer
      ("stand" (if (bj-can-stand game)
		   (bj-stand game)
		 (bj-ask-hand-action game)))
      ("hit" (if (bj-can-hit game)
		 (bj-hit game)
	       (bj-ask-hand-action game)))
      ("split" (if (bj-can-split game)
		   (bj-split game)
		 (bj-ask-hand-action game)))
      ("double" (if (bj-can-double game)
		    (bj-double game)
		  (bj-ask-hand-action game))))))

(defun bj-hand-actions-menu (game)
  "Hand actions menu for GAME."
  (let* ((read-answer-short t)
	 (actions '(("help" ?? "show help"))))
    (if (bj-can-hit game)
        (setf actions (cons '("hit" ?h "deal a new card") actions)))
    (if (bj-can-stand game)
        (setf actions (cons '("stand" ?s "end current hand with no further actions") actions)))
    (if (bj-can-split game)
        (setf actions (cons '("split" ?p "split hand into two hands") actions)))
    (if (bj-can-double game)
        (setf actions (cons '("double" ?d "double bet, deal a new card, and end hand") actions)))
    (read-answer "Hand Action " actions)))

(defun bj-ask-insurance-action (game)
  "Ask about insuring GAME hand."
  (let* ((answer (bj-ask-insurance-menu game)))
    (pcase answer
      ("yes" (bj-insure-hand game))
      ("no" (bj-no-insurance game))
      ("help" ?? "show help"))))

(defun bj-insure-hand (game)
  "Insure GAME hand."
  (let* ((player-hand (bj-current-player-hand game))
	 (bet (slot-value player-hand 'bet))
	 (new-bet (/ bet 2))
	 (money (slot-value game 'money)))
    (setf (slot-value player-hand 'bet) new-bet
	  (slot-value player-hand 'played) t
	  (slot-value player-hand 'payed) t
	  (slot-value player-hand 'status) 'lost
	  (slot-value game 'money) (- money new-bet))
    (bj-draw-hands game)
    (bj-ask-bet-action game)))

(defun bj-no-insurance (game)
  "Decline GAME hand insurance."
  (let* ((dealer-hand (slot-value game 'dealer-hand))
	 (dealer-hand-cards (slot-value dealer-hand 'cards)))
    (if (bj-hand-is-blackjack dealer-hand-cards)
	(progn
	  (setf (slot-value dealer-hand 'hide-down-card) nil)
	  (bj-pay-hands game)
	  (bj-draw-hands game)
	  (bj-ask-bet-action game))
      (let* ((player-hand (bj-current-player-hand game)))
	(if (bj-player-hand-done game player-hand)
	    (bj-play-dealer-hand game)
	  (progn
	    (bj-draw-hands game)
	    (bj-ask-hand-action game)))))))

(defun bj-ask-insurance-menu (game)
  "Ask about insuring GAME hand."
  (let* ((read-answer-short t))
    (read-answer "Hand Insurance: "
                 '(("yes" ?y "insure hand")
                   ("no" ?n "no insurance")
                   ("help" ?? "show help")))))

(defun bj-ask-bet-action (game)
  "Ask about next GAME bet action."
  (let* ((answer (bj-bet-actions-menu game)))
    (pcase answer
      ("deal" nil)
      ("bet" (bj-ask-new-bet game))
      ("options" (bj-ask-game-options game))
      ("quit" (setf (slot-value game 'quitting) t)))))

(defun bj-bet-actions-menu (game)
  "Bet actions menu for GAME."
  (let* ((read-answer-short t))
    (read-answer "Game Actions: "
                 '(("deal" ?d "deal new hand")
                   ("bet" ?b "change current bet")
                   ("options" ?o "change game options")
                   ("quit" ?q "quit blackjack")
                   ("help" ?? "show help")))))

(defun bj-ask-new-bet (game)
  "Update the current GAME bet."
  (let* ((answer (bj-new-bet-menu game))
	 (bet 0))
    (setf bet (* 100 (string-to-number answer)))
    (setf (slot-value game 'current-bet) bet)
    (bj-normalize-current-bet game)))

(defun bj-new-bet-menu (game)
  "New GAME bet menu."
  (read-string "Bet Amount: "))

(defun bj-ask-new-number-decks (game)
  "Ask for new number of GAME decks."
  (let* ((answer (bj-new-number-decks-menu game))
	 (num-decks 1))
    (setf num-decks (string-to-number answer))
    (if (< num-decks 1)
	(setf num-decks 1))
    (if (> num-decks 8)
	(setf num-decks 8))
    (setf (slot-value game 'num-decks) num-decks)))

(defun bj-new-number-decks-menu (game)
  "New GAME number of decks menu."
  (read-string "Number of Decks: "))

(defun bj-ask-game-options (game)
  "Ask about which GAME option to update."
  (let* ((answer (bj-game-options-menu game)))
    (pcase answer
      ("number-decks" (bj-ask-new-number-decks game))
      ("deck-type" (bj-ask-new-deck-type game))
      ("face-type" (bj-ask-new-face-type game))
      ("back" (bj-ask-bet-action game)))))

(defun bj-game-options-menu (game)
  "GAME options menu."
  (let* ((read-answer-short t))
    (read-answer "Options: "
                 '(("number-decks" ?n "change number of decks")
                   ("deck-type" ?t "change the deck type")
                   ("face-type" ?f "change the card face type")
                   ("back" ?b "go back to previous menu")
                   ("help" ?? "show help")))))

(defun bj-ask-new-deck-type (game)
  "Ask for new GAME deck type."
  (let* ((answer (bj-deck-type-menu game))
	 (deck-type (intern answer)))
     (setf (slot-value game 'deck-type) deck-type)
     (bj-shuffle game)
     (bj-save game)))

(defun bj-deck-type-menu (game)
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

(defun bj-ask-new-face-type (game)
  "Ask for new GAME face type."
  (let* ((answer (bj-face-type-menu game))
	 (face-type (intern answer)))
    (setf (slot-value game 'face-type) face-type)
    (bj-save game)))

(defun bj-face-type-menu (game)
  "New GAME face type menu."
  (let* ((read-answer-short t))
    (read-answer "Card Face Type: "
                 '(("ascii" ?a "use ascii face type")
		   ("unicode" ?u "use unicode face type")
                   ("help" ?? "show help")))))

(defun bj-player-hand-is-busted (cards)
  "Return non-nil if CARDS value is more than 21."
  (> (bj-player-hand-value cards 'soft) 21))

(defun bj-dealer-hand-is-busted (dealer-hand)
  "Return non-nil if DEALER-HAND cards value is more than 21."
  (let* ((cards (slot-value dealer-hand 'cards)))
    (> (bj-dealer-hand-value dealer-hand 'soft) 21)))

(defun bj-hand-is-blackjack (cards)
  "Return non-nil if hand CARDS is blackjack."
  (if (eq 2 (length cards))
      (let* ((card-0 (nth 0 cards))
	     (card-1 (nth 1 cards)))
        (or
	 (and
	  (bj-is-ace card-0)
	  (bj-is-ten card-1))
	 (and
	  (bj-is-ace card-1)
	  (bj-is-ten card-0))))))

(defun bj-dealer-upcard-is-ace (dealer-hand)
  "Return non-nil if DEALER-HAND upcard is an ace."
  (bj-is-ace (nth 1 (slot-value dealer-hand 'cards))))

(defun bj-draw-dealer-hand (game)
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
      (insert (bj-card-face game value suit))
      (insert " "))
    (insert " ⇒  ")
    (insert (number-to-string (bj-dealer-hand-value dealer-hand 'soft)))))

(defun bj-dealer-hand-value (dealer-hand count-method)
  "Calculates DEALER-HAND cards total value based on COUNT-METHOD."
  (let* ((cards (slot-value dealer-hand 'cards))
         (hide-down-card (slot-value dealer-hand 'hide-down-card))
	 (total 0)
         (card nil))
    (dotimes (x (length cards))
      (if (not (and hide-down-card (= x 0)))
          (progn
	    (setf card (nth x cards))
	    (setf total (+ total (bj-card-val card count-method total))))))
    (if (and (eq count-method 'soft) (> total 21))
        (setf total (bj-dealer-hand-value dealer-hand 'hard)))
    total))

(defun bj-draw-player-hands (game)
  "Draw GAME players hands."
  (let* ((player-hands (slot-value game 'player-hands))
	 (player-hand nil))
    (dotimes (x (length player-hands))
      (setf player-hand (nth x player-hands))
      (bj-draw-player-hand game player-hand x))))

(defun bj-draw-player-hand (game player-hand index)
  "Draw the GAME PLAYER-HAND by INDEX."
  (insert (bj-player-hand-cards game player-hand))
  (insert (bj-player-hand-money game player-hand index))
  (insert (bj-player-hand-status player-hand))
  (insert "\n\n"))

(defun bj-player-hand-cards (game player-hand)
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
      (setf out (concat out (bj-card-face game value suit)))
      (setf out (concat out " ")))
    (setf out (concat out " ⇒  "))
    (setf out (concat out (number-to-string (bj-player-hand-value cards 'soft)) "  "))
    out))

(defun bj-player-hand-status (player-hand)
  "Return PLAYER-HAND status."
  (let* ((cards (slot-value player-hand 'cards))
	 (status (slot-value player-hand 'status)))
    (pcase status
      ('lost (if (bj-player-hand-is-busted cards)
		 "Busted!"
	       "Lost!"))
      ('won (if (bj-hand-is-blackjack cards)
		"Blackjack!"
	      "Won!"))
      ('push "Push")
      ('unknown ""))))

(defun bj-player-hand-money (game player-hand index)
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
    (setf out (concat out "$" (bj-format-money (/ bet 100.0))))
    (if (and
	 (not played)
	 (= index current-hand))
	(setf out (concat out " ⇐")))
    (setf out (concat out "  "))
    out))

(defun bj-player-hand-value (cards count-method)
  "Calculates CARDS total value based on COUNT-METHOD."
  (let* ((total 0)
	 (card nil))
    (dotimes (x (length cards))
      (setf card (nth x cards))
      (setf total (+ total (bj-card-val card count-method total))))
    (if (and (eq count-method 'soft) (> total 21))
        (setf total (bj-player-hand-value cards 'hard)))
    total))

(defun bj-card-val (card count-method total)
  "Calculates CARD value based on COUNT-METHOD and running hand TOTAL."
  (let* ((value (1+ (slot-value card 'value))))
    (if (> value 9)
        (setf value 10))
    (if (and (eq count-method 'soft) (eq value 1) (< total 11))
        (setf value 11))
    value))

(defun bj-card-face (game value suit)
  "Return GAME card face based on VALUE and SUIT."
  (let* ((face nil))
    (if (eq (slot-value game 'face-type) 'unicode)
	(setq face (slot-value game 'faces-unicode))
      (setq face (slot-value game 'faces-ascii)))
    (aref (aref face value) suit)))

(defun bj-is-ace (card)
  "Is the CARD an ace?"
  (= 0 (slot-value card 'value)))

(defun bj-is-ten (card)
  "Is the CARD a 10 value?"
  (> (slot-value card 'value) 8))

(defun bj-normalize-current-bet (game)
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

(defun bj-load-saved-game (game)
  "Load persisted GAME state."
  (let* ((content nil)
	 (parts '()))
    (ignore-errors
      (with-temp-buffer
	(insert-file-contents "bj.txt")
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

(defun bj-save (game)
  "Persist GAME state."
  (ignore-errors
    (with-temp-file "bj.txt"
      (insert (format "%s|%s|%s|%s|%s"
		      (slot-value game 'num-decks)
		      (slot-value game 'deck-type)
		      (slot-value game 'face-type)
		      (slot-value game 'money)
		      (slot-value game 'current-bet))))))

(defun bj ()
  "Run Blackjack."
  (interactive)
  (let* ((debug-on-error t))
    (let* ((buffer-name "blackjack")
	   (game (bj-game)))
      (bj-load-saved-game game)
      (get-buffer-create buffer-name)
      (switch-to-buffer buffer-name)
      (with-current-buffer buffer-name
	(while (not (slot-value game 'quitting))
	  (bj-deal-new-hand game))))
    (quit-window)))

(provide 'bj)
;;; bj.el ends here
