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

(defclass bj-game ()
  ((shoe :initarg :shoe :initform '() :type list)
   (dealer-hand :initarg :dealer-hand :initform '() :type list)
   (player-hands :initarg :player-hands :initform '() :type list)
   (num-decks :initarg :num-decks :initform 1 :type integer)
   (money :initarg :money :initform 10000 :type integer)
   (current-bet :initarg :current-bet :initform 500 :type integer)
   (current-player-hand :initarg :current-player-hand :initform 0 :type integer)
   (quitting :initarg :quitting :initform nil :type boolean)
   (faces :initarg :faces :initform '[["A♠" "A♥" "A♣" "A♦"]
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
   (shuffle-specs :initarg :shuffle-specs :initform '[80 81 82 84 86 89 92 95] :type array)
   (cards-per-deck :initarg :cards-per-deck :initform 52 :type integer)
   (min-bet :initarg :min-bet :initform 500 :type integer)
   (max-bet :initarg :min-bet :initform 100000000 :type integer)
   (max-player-hands :initarg :min-bet :initform 7 :type integer)))

(defclass bj-card ()
  ((value :initarg :value :initform 0 :type integer)
   (suit :initarg :suit :initform 0 :type integer)))

(defclass bj-hand ()
  ((cards :initarg :cards :initform '() :type list)
   (played :initarg :played :initform nil :type boolean)))

(defclass bj-player-hand (bj-hand)
  ((bet :initarg :bet :initform 0 :type integer)
   (status :initarg :status :initform 'unknown :type symbol)
   (payed :initarg :payed :initform nil :type boolean)
   (stood :intiarg :stood :initform nil :type boolean)))

(defclass bj-dealer-hand (bj-hand)
  ((hide-down-card :initarg :hide-down-card :initform t :type boolean)))

(defun bj-deal-new-hand (game)
  "Deal new GAME hands."
  (let ((shoe (slot-value game 'shoe))
	(player-hand nil)
	(dealer-hand nil))

    (if (bj-need-to-shuffle shoe)
        (bj-shuffle shoe))

    (setf (slot-value game 'player-hands) '())
    (setf player-hand (bj-player-hand :bet (slot-value game 'current-bet)))
    (bj-deal-cards shoe (slot-value player-hand 'cards) 2)
    (push player-hand (slot-value game 'player-hands))
    (setf dealer-hand (bj-dealer-hand))
    (bj-deal-cards shoe (slot-value dealer-hand 'cards) 2)
    (setf (slot-value game 'dealer-hand) dealer-hand)

    (if (and (bj-dealer-upcard-is-ace game) (bj-hand-is-blackjack player-hand))
        (progn
          (bj-draw-hands game)
          (bj-ask-insurance game))
      (if (bj-player-hand-done player-hand)
          (progn
            (setf (slot-value dealer-hand 'hide-down-card) nil)
            (bj-pay-hands game)
            (bj-draw-hands game)
            (bj-ask-bet-action game))
        (progn
          (bj-draw-hands game)
          (bj-ask-hand-action game)
          (bj-save game))))))

(defun bj-deal-cards (shoe hand count)
  "Deal COUNT cards into HAND from SHOE."
  (let ((card nil))
    (dotimes (x count)
      (setf card (car shoe))
      (push card hand)
      (setf shoe (remove card shoe)))))

(defun bj-pay-hands (game)
  "Pay GAME player hands."
  (let ((dealer-hand-value (bj-dealer-hand-value game 'soft))
	(dealer-busted (bj-dealer-hand-is-busted game))
	(player-hands (slot-value game 'player-hands)))
    (dotimes (x (length player-hands))
      (bj-pay-player-hand game (nth x player-hands) dealer-hand-value dealer-busted))
    (bj-normalize-current-bet)
    (bj-save-game)))

(defun bj-pay-player-hand (game player-hand dealer-hand-value dealer-hand-busted)
  "Pay GAME PLAYER-HAND based on DEALER-HAND-VALUE and DEALER-HAND-BUSTED."
  (if (not (slot-value player-hand 'payed))
      (progn
        (setf (slot-value player-hand 'payed) t)
        (let ((player-hand-value nil)
	      (money (slot-value game 'money))
          (setf player-hand-value (bj-player-hand-value (slot-value player-hand 'cards) 'soft))
          (if (bj-player-hand-won player-hand-value dealer-hand-value dealer-hand-busted)
              (bj-pay-won-hand money player-hand)
            (if (bj-player-hand-lost player-hand-value dealer-hand-value)
                (bj-collect-lost-hand money player-hand)
              (setf (slot-value player-hand 'status) 'push)))))))

(defun bj-collect-lost-hand (money player-hand)
  "Collect bet into MONEY from losing PLAYER-HAND."
  (setf money (- money (slot-value player-hand 'bet)))
  (setf (slot-value player-hand 'status) 'lost))

(defun bj-pay-won-hand (money player-hand)
  "Pay winning PLAYER-HAND bet into MONEY."
  (let ((bet (slot-value player-hand 'bet)))
    (if (bj-hand-is-blackjack (slot-value player-hand 'cards))
	(setf bet (* 1.5 bet)))
    (setf money (+ money bet))
    (setf (slot-value player-hand 'status) 'won)))

(defun bj-player-hand-lost (player-hand-value dealer-hand-value)
  "Return non-nil if PLAYER-HAND-VALUE < DEALER-HAND-VALUE."
  (if (< player-hand-value dealer-hand-value)
      t))

(defun bj-player-hand-won (player-hand-value dealer-hand-value dealer-hand-busted)
  "Return non-nil if PLAYER-HAND-VALUE > DEALER-HAND-VALUE && !DEALER-HAND-BUSTED."
  (if
      (or
       dealer-hand-busted
       (> player-hand-value dealer-hand-value))
      t))

(defun bj-player-hand-done (player-hand)
  "Return non-nil when PLAYER-HAND is done."
  (if (bj-no-more-actions player-hand)
      (progn
        (setf (slot-value player-hand 'played) t)
        (if
            (and
             (not (slot-value player-hand 'payed))
             (bj-player-hand-is-busted (slot-value player-hand 'cards)))
            (collect-busted-hand player-hand)))
    t))

(defun collect-busted-hand (player-hand)
  "Collect bet from PLAYER-HAND."
  (setf (slot-value player-hand 'payed) t)
  (setf (slot-value player-hand 'status) t)
  (setf bj-money (- bj-money (slot-value player-hand 'bet))))

(defun bj-no-more-actions (player-hand)
  "Return non-nil when PLAYER-HAND has no more actions."
  (let ((cards (slot-value player-hand 'cards)))
    (if
        (or
         (slot-value player-hand 'played)
         (slot-value player-hand 'stood)
         (bj-hand-is-blackjack cards)
         (bj-player-hand-is-busted cards)
         (= 21 (bj-player-hand-value cards 'soft))
         (= 21 (bj-player-hand-value cards 'hard)))
        t)))

(defun bj-need-to-shuffle ()
  "Are shoe cards nearly exhausted?"
  (let ((cards nil) (used nil) (spec nil))
    (setf cards (length (cdr bj-shoe)))
    (if (> cards 0)
        (progn
          (setf used (- (* bj-num-decks BJ-CARDS-PER-DECK) cards))
          (setf spec (aref BJ-SHUFFLE-SPECS (1- bj-num-decks)))
          (> (* 100 (/ (float used) cards)) spec))
      t)))

(defun bj-shuffle ()
  "Create and add cards to the shoe."
  (let ((cards nil))
    (dotimes (n bj-num-decks)
      (dotimes (suit 4)
        (dotimes (value 13)
          (push (bj-card :value value :suit suit) cards))))
    (setf cards (bj-shuffle-loop cards))
    (setf bj-shoe cards)))

(defun bj-shuffle-loop (cards)
  "Shuffle CARDS."
  (dotimes (x (* 7 (length cards)))
    (setf cards (bj-move-rand-card cards)))
  cards)

(defun bj-move-rand-card (cards)
  "Move a random card to the top of the shoe CARDS."
  (let ((rand (random (length cards)))
        (card nil)
        (new-cards nil))
    (setf card (nth rand cards))
    (setf cards (remove card cards))
    (setf new-cards (cons card cards))
    new-cards))

(defun bj-draw-hands ()
  "Draw dealer and player hands."
  (erase-buffer)
  (insert "\n  Dealer:\n")
  (bj-draw-dealer-hand)
  (insert "\n\n  Player:\n")
  (bj-draw-player-hands)
  (insert "\n\n  "))

(defun bj-hit ()
  "Deal a new card to the current player hand."
  (interactive)
  (let ((player-hand nil) (cards nil) (card nil))
    (setf player-hand (bj-get-current-player-hand))
    (setf cards (assq 'cards player-hand))
    (setf card (bj-deal-cards 1))
    (setf cards (cons card cards))
    (setf player-hand (delq (assq 'cards player-hand) player-hand))
    (setf player-hand (cons `(cards . cards) player-hand))
    (setf bj-player-hands `((bj-current-player-hand . player-hand)))))

(defun bj-dbl ()
  "Double the current player hand."
  (interactive)
  (let ((player-hand nil) (cards nil) (card nil))
    (setf player-hand (bj-get-current-player-hand))
    (setf cards (assq 'cards player-hand))
    (setf card (bj-deal-cards 1))
    (setf cards (cons card cards))
    (setf player-hand (delq (assq 'cards player-hand) player-hand))
    (setf player-hand (cons `(cards . cards) player-hand))
    (setf player-hand (delq (assq 'stood player-hand) player-hand))
    (setf player-hand (cons `(stood . t) player-hand))
    (setf bj-player-hands `((bj-current-player-hand . player-hand)))))

(defun bj-stand ()
  "End the current player hand."
  (let ((player-hand nil))
    (setf player-hand (bj-get-current-player-hand))
    (setf player-hand (delq (assq 'stood player-hand) player-hand))
    (setf player-hand (cons `(stood . t) player-hand))
    (setf bj-player-hands `((bj-current-player-hand . player-hand)))))

(defun bj-split ()
  "Split the current player hand."
  (let ((player-hand nil) (cards nil))
    (setf player-hand (bj-get-current-player-hand))
    (setf cards (assq 'cards player-hand))
    ; TODO
    ))

(defun bj-can-hit ()
  "Return non-nil if the current player hand can hit."
  (let ((player-hand nil) (cards nil))
    (setf player-hand (bj-get-current-player-hand))
    (setf cards (slot-value player-hand 'cards))
    (if (not (or
              (slot-value player-hand 'played)
              (slot-value player-hand 'stood)
              (eq (bj-player-hand-value cards 'soft) 21)
              (bj-hand-is-blackjack cards)
              (bj-player-hand-is-busted cards)))
        t)))

(defun bj-can-stand ()
  "Return non-nil if the current player hand can stand."
  (let ((player-hand nil) (cards nil))
    (setf player-hand (bj-get-current-player-hand))
    (setf cards (slot-value player-hand 'cards))
    (if (not (or
              (slot-value player-hand 'stood)
              (bj-player-hand-is-busted cards)
              (bj-hand-is-blackjack cards)))
        t)))

(defun bj-can-split ()
  "Return non-nil if the current player hand can split."
  (let ((player-hand nil) (cards nil))
    (setf player-hand (bj-get-current-player-hand))
    (setf cards (slot-value player-hand 'cards))
    (if (and
         (not (slot-value player-hand 'stood))
         (< (length bj-player-hands) 7)
         (>= bj-money (+ (bj-all-bets) (slot-value player-hand 'bet)))
         (eq (length cards) 2))
        (let ((card-0 nil) (card-1 nil))
          (setf card-0 (nth 0 cards))
          (setf card-1 (nth 1 cards))
          (if (eq (slot-value card-0 'value) (slot-value card-1 'value))
              t)))))

(defun bj-can-dbl ()
  "Return non-nil if the current player hand can double."
  (let ((player-hand nil) (cards nil))
    (setf player-hand (bj-get-current-player-hand))
    (setf cards (slot-value player-hand 'cards))
    (if (and
         (>= bj-money (+ (bj-all-bets) (slot-value player-hand 'bet)))
         (not (or (slot-value player-hand 'stood) (not (eq 2 (length cards)))
                  (bj-hand-is-blackjack cards))))
        t)))

(defun bj-get-current-player-hand ()
  "Return current player hand."
  (let ((player-hand nil))
    (setf player-hand (nth bj-current-player-hand bj-player-hands))
    player-hand))

(defun bj-all-bets ()
  "Sum of all player hand bets."
  (let ((total 0))
    (dotimes (x (length bj-player-hands))
      (setf total (+ total (slot-value (nth x bj-player-hands) 'bet))))
    total))

(defun bj-ask-hand-action ()
  "Ask action for current hand."
  (let ((read-answer-short t) (actions nil))
    (if (bj-can-hit)
        (setf actions (cons '("hit" ?h "deal a new card") actions)))
    (if (bj-can-stand)
        (setf actions (cons '("stand" ?s "end hand") actions)))
    (if (bj-can-split)
        (setf actions (cons '("split" ?p "split hand") actions)))
    (if (bj-can-dbl)
        (setf actions (cons '("double" ?d "deal a new card and end hand") actions)))
    (setf actions (cons '("help" ?? "show help") actions))
    (read-answer "Hand Action " actions)))

(defun bj-ask-insurance ()
  "Ask about insuring hand."
  (let ((read-answer-short t))
        (read-answer "Insurance "
                     '(("yes" ?y "insure hand")
                       ("no" ?n "no insurance")
                       ("help" ?? "show help")))))

(defun bj-ask-bet-action ()
  "Ask about next bet action."
  (let ((read-answer-short t))
        (read-answer "Game Action "
                     '(("deal" ?d "deal new hand")
                       ("bet" ?b "change current bet")
                       ("options" ?o "change game options")
                       ("quit" ?q "quit blackjack")
                       ("help" ?? "show help")))))

(defun bj-player-hand-is-busted (cards)
  "Return non-nil if hand CARDS value is more than 21."
  (if (> (bj-player-hand-value cards 'soft) 21)
      t))

(defun bj-dealer-hand-is-busted ()
  "Return non-nil if hand CARDS value is more than 21."
  (if (> (bj-dealer-hand-value 'soft) 21)
      t))

(defun bj-hand-is-blackjack (cards)
  "Return non-nil if hand CARDS is blackjack."
  (if (eq 2 (length cards))
      (let ((card-0 nil) (card-1 nil))
        (setf card-0 (nth 0 cards))
        (setf card-1 (nth 1 cards))
        (if (or
             (and
              (bj-is-ace card-0)
              (bj-is-ten card-1))
             (and
              (bj-is-ace card-1)
              (bj-is-ten card-0)))
            t))))

(defun bj-dealer-upcard-is-ace ()
  "Return non-nil if dealer upcard is an ace."
  (let ((cards nil) (card nil))
    (setf cards (slot-value bj-dealer-hand 'cards))
    (setf card (nth 1 cards))
    (bj-is-ace card)))

(defun bj-draw-dealer-hand ()
  "Draw the dealer hand."
  (let ((cards nil)
        (card nil)
        (suit nil)
        (value nil)
        (hide-down-card (slot-value bj-dealer-hand 'hide-down-card)))
    (setf cards (slot-value bj-dealer-hand 'cards))
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
      (insert (bj-card-face value suit))
      (insert " "))
    (insert " ⇒  ")
    (insert (number-to-string (bj-dealer-hand-value 'soft)))))

(defun bj-dealer-hand-value (count-method)
  "Calculates CARDS total value based on COUNT-METHOD."
  (let ((total 0)
        (card nil)
        (cards (slot-value bj-dealer-hand 'cards))
        (hide-down-card (slot-value bj-dealer-hand 'hide-down-card)))
    (dotimes (x (length cards))
      (if (not (and hide-down-card (= x 0)))
          (progn
            (setf card (nth x cards))
            (setf total (+ total (bj-card-value card count-method total))))))
    (if (and (eq count-method 'soft) (> total 21))
        (setf total (bj-dealer-hand-value 'hard)))
    total))

(defun bj-draw-player-hands ()
  "Draw players hands."
  (let ((player-hand nil))
    (dotimes (x (length bj-player-hands))
      (setf player-hand (nth x bj-player-hands))
      (bj-draw-player-hand player-hand))))

(defun bj-draw-player-hand (player-hand)
  "Draw the PLAYER-HAND."
  (let ((cards nil) (card nil) (suit nil) (value nil))
    (setf cards (slot-value player-hand 'cards))
    (insert "  ")
    (dotimes (x (length cards))
      (setf card (nth x cards))
      (setf value (slot-value card 'value))
      (setf suit (slot-value card 'suit))
      (insert (bj-card-face value suit))
      (insert " "))
    (insert " ⇒  ")
    (insert (number-to-string (bj-player-hand-value cards 'soft)))))

(defun bj-player-hand-value (cards count-method)
  "Calculates CARDS total value based on COUNT-METHOD."
  (let ((total 0) (card nil))
    (dotimes (x (length cards))
      (setf card (nth x cards))
      (setf total (+ total (bj-card-value card count-method total))))
    (if (and (eq count-method 'soft) (> total 21))
        (setf total (bj-player-hand-value cards 'hard)))
    total))

(defun bj-card-value (card count-method total)
  "Calculates CARD value based on COUNT-METHOD and running hand TOTAL."
  (let ((value nil))
    (setf value (1+ (slot-value card 'value)))
    (if (> value 9)
        (setf value 10))
    (if (and (eq count-method 'soft) (eq value 1) (< total 11))
        (setf value 11))
    value))

(defun bj-card-face (value suit)
  "Return card face based on VALUE and SUIT."
  (aref (aref BJ-FACES value) suit))

(defun bj-is-ace (card)
  "Is the CARD an ace?"
  (= 0 (slot-value card 'value)))

(defun bj-is-ten (card)
  "Is the CARD a 10 value?"
  (> 8 (slot-value card 'value)))

(defun bj-normalize-current-bet ()
  "Normalize current bet to a known range and available funds."
  (if (< bj-current-bet BJ-MIN-BET)
      (setf bj-current-bet BJ-MIN-BET)
    (if (> bj-current-bet BJ-MAX-BET)
        (setf bj-current-bet BJ-MAX-BET)))
  (if (> bj-current-bet bj-money)
      (setf bj-current-bet bj-money)))

(defun bj-save-game ()
  "Persist game state."
  ;; TODO
  )


(defun bj-quit ()
  "Quit."
  ;; TODO
  )

(defun bj ()
  "Run Blackjack."
  (interactive)
  (let ((debug-on-error t))
    (let ((buffer-name "bj") (game bj-game))
      (get-buffer-create buffer-name)
      (switch-to-buffer buffer-name)
      (with-current-buffer buffer-name
	(while (not (slot-value game 'quitting))
	  (bj-deal-new-hand game))))))

(provide 'bj)
;;; bj.el ends here
