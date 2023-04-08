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

(defconst BJ-FACES '[["A♠" "A♥" "A♣" "A♦"]
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
                     ["??"]])

(defconst BJ-SHUFFLE-SPECS '[80 81 82 84 86 89 92 95])
(defconst BJ-CARDS-PER-DECK 52)
(defconst BJ-MIN-BET 500)
(defconst BJ-MAX-BET 100000000)

(defvar bj-shoe nil)
(defvar bj-player-hands nil)
(defvar bj-dealer-hand nil)
(defvar bj-num-decks 1)
(defvar bj-money 10000)
(defvar bj-current-bet 500)
(defvar bj-current-player-hand 0)
(defvar bj-max-player-hands 7)

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

(defclass bj-dealer-hnd (bj-hand)
  ((hide-down-card :initarg :hide-down-card :initform t :type boolean)))

(defun bj-deal-cards (count)
  "Deal COUNT cards."
  (let ((dealt nil) (card nil))
    (dotimes (x count)
      (setf card (car bj-shoe))
      (setf dealt (cons card dealt))
      (setf bj-shoe (remove card bj-shoe)))
    dealt))

(defun bj-deal-new-hand ()
  "Deal new hands."
  (interactive)
  (let ((player-hand nil))
    (if (bj-need-to-shuffle)
        (bj-shuffle))
    (setf bj-player-hands nil)
    (setf player-hand (bj-player-hand :cards (bj-deal-cards 2)
                                      :bet bj-current-bet))
    (push player-hand bj-player-hands)
    (setf bj-dealer-hand (bj-dealer-hnd :cards (bj-deal-cards 2)))
    (if (and (bj-dealer-upcard-is-ace) (bj-hand-is-blackjack (nth 0 bj-player-hands)))
        (progn
          (bj-draw-hands)
          (bj-ask-insurance))
      (if (bj-player-hand-done player-hand)
          (progn
            (setf (slot-value bj-dealer-hand 'hide-down-card) nil)
            (bj-pay-hands)
            (bj-draw-hands)
            (bj-ask-bet-action))
        (progn
          (bj-draw-hands)
          (bj-ask-hand-action)
          (bj-save-game))))))

(defun bj-pay-hands ()
  "Pay player hands."
  (let ((dealer-hand-value nil) (dealer-busted))
    (setf dealer-hand-value (bj-dealer-hand-value 'soft))
    (setf dealer-busted (bj-dealer-hand-is-busted))
    (dotimes (x (length bj-player-hands))
      (bj-pay-player-hand (nth x bj-player-hands) dealer-hand-value dealer-busted))
    (bj-normalize-current-bet)
    (bj-save-game)))

(defun bj-pay-player-hand (player-hand dealer-hand-value dealer-hand-busted)
  "Pay PLAYER-HAND based on DEALER-HAND-VALUE and DEALER-HAND-BUSTED."
  (if (not (slot-value player-hand 'payed))
      (progn
        (setf (slot-value player-hand 'payed) t)
        (let ((player-hand-value nil))
          (setf player-hand-value (bj-player-hand-value (slot-value player-hand 'cards) 'soft))
          (if (bj-player-hand-won player-hand-value dealer-hand-value dealer-hand-busted)
              (bj-pay-won-hand player-hand)
            (if (bj-player-hand-lost player-hand-value dealer-hand-value)
                (bj-collect-lost-hand player-hand)
              (setf (slot-value player-hand 'status) 'push)))))))

(defun bj-collect-lost-hand (player-hand)
  "Collect bet from losing PLAYER-HAND."
  (setf bj-money (- bj-money (slot-value player-hand 'bet)))
  (setf (slot-value player-hand 'status) 'lost))

(defun bj-pay-won-hand (player-hand)
  "Pay winning PLAYER-HAND."
  (if (bj-hand-is-blackjack (slot-value player-hand 'cards))
      (setf (slot-value player-hand 'bet) (* 1.5 (slot-value player-hand 'bet))))
  (setf bj-money (+ bj-money (slot-value player-hand 'bet)))
  (setf (slot-value player-hand 'status) 'won))

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
    (let ((buffer-name "bj"))
      (get-buffer-create buffer-name)
      (switch-to-buffer buffer-name)
      (with-current-buffer buffer-name
        (bj-deal-new-hand)))))

(provide 'bj)
;;; bj.el ends here
