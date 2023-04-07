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
  ((value :initarg :value
         :initform 0
         :type integer
         :documentation "the card value")
   (suit :initarg :suit
         :initform 0
         :type integer
         :documentation "the card suit")))

(cl-defmethod bj-ace-card ((c bj-card))
  "Is the card C an ace?"
  (= 0 (slot-value c 'value)))

(cl-defmethod bj-ten-card ((c bj-card))
  "Is the card C a 10 value?"
  (> 8 (slot-value c 'value)))

(defclass bj-hand ()
  ((cards :initarg :cards
          :initform '()
          :type list
          :documentation "the hand cards")
   (blackjack :initarg :blackjack
              :initform nil
              :type boolean
              :documentation "hand is blackjack")
   (played :initarg :played
           :initform nil
           :type boolean
           :documentation "hand has been played")))

(cl-defmethod bj-busted-hand ((h bj-hand))
  "Is the hand H busted?"
  (> ((bj-value-hand h) 21)))

(cl-defmethod bj-value-hand ((h bj-hand))
  "Value of hand H."
  0)

(defclass bj-player-hand (bj-hand)
  ((bet :initarg :bet
        :initform 0
        :type integer
        :documentation "the player hand bet")
   (status :initarg :status
           :initform :unknown
           :type symbol
           :documentation "current player hand status")
   (payed :initarg :payed
          :initform nil
          :type boolean
          :documentation "player hand has been payed")
   (stood :intiarg :stood
          :initform nil
          :type boolean
          :documentation "player hand stood")))

(defclass bj-dealer-hand (bj-hand)
  ((hide-down-card :initarg :hide-down-card
                   :initform 't
                   :type boolean
                   :documentation "first card is face down")))

(defvar bj-shoe nil)
(defvar bj-player-hands nil)
(defvar bj-dealer-hand nil)
(defvar bj-cards-per-deck 52)
(defvar bj-num-decks 1)
(defvar bj-money 10000)
(defvar bj-current-bet 500)
(defvar bj-current-player-hand 0)
(defvar bj-max-player-hands 7)

(defvar bj-faces '[["A♠" "A♥" "A♣" "A♦"]
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

(defvar bj-shuffle-specs '[80 81 82 84 86 89 92 95])

(defun bj-deal-cards (count)
  "Deal COUNT cards."
  (let ((dealt nil))
    (dotimes (x count)
      (push (car bj-shoe) dealt)
      (setf bj-shoe (remove 0 bj-shoe)))
    dealt))

(defun bj-deal-hands ()
  "Deal new hands."
  (interactive)
  (if (bj-need-to-shuffle)
      (bj-shuffle))

  (setf bj-player-hands nil)
  (push
   (bj-player-hand :cards (bj-deal-cards 2)
                   :bet bj-current-bet)
   bj-player-hands)

  (setf bj-dealer-hand (bj-dealer-hand :cards (bj-deal-cards 2)))

  (bj-draw-hands)
  ;;(bj-draw-bet-options)
  (bj-ask-hand-action))

(defun bj-need-to-shuffle ()
  "Are shoe cards nearly exhausted?"
  (let ((cards nil) (used nil) (spec nil))
    (setq cards (length (cdr bj-shoe)))
    (if (> cards 0)
        (progn
          (setq used (- (* bj-num-decks bj-cards-per-deck) cards))
          (setq spec (aref bj-shuffle-specs (1- bj-num-decks)))
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
    (setf card (assq rand cards))
    (setf cards (delq (assq rand cards) cards))
    (setf new-cards (cons card cards))
    new-cards))

(defun bj-quit ()
  "Quit.")

(defun bj-draw-hands ()
  "Top-level draw."
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
  (interactive)
  (let ((player-hand nil))
    (setf player-hand (bj-get-current-player-hand))
    (setf player-hand (delq (assq 'stood player-hand) player-hand))
    (setf player-hand (cons `(stood . t) player-hand))
    (setf bj-player-hands `((bj-current-player-hand . player-hand)))))

(defun bj-split ()
  "Split the current player hand."
  (interactive)(let ((player-hand nil) (cards nil))
    (setf player-hand (bj-get-current-player-hand))
    (setf cards (assq 'cards player-hand))
    ;; TODO
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

(defun bj-ask-bet-actions ()
  "Ask next action."
  (let ((read-answer-short t))
        (read-answer "Game Action "
                     '(("deal" ?d "deal a new hand")
                       ("bet" ?b "change bet")
                       ("options" ?o "change options")
                       ("quit" ?q "quit")
                       ("help" ?? "show help")))))

(defun bj-player-hand-is-busted (cards)
  "Return non-nil if hand CARDS value is more than 21."
  (if (> (bj-player-hand-value cards 'soft) 21)
      t))

(defun bj-dealer-hand-is-busted (cards)
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
              (bj-ace-card card-0)
              (bj-ten-card card-1))
             (and
              (bj-ace-card card-1)
              (bj-ten-card card-0)))
            t))))

(defun bj-dealer-upcard-is-ace ()
  "Return non-nil if dealer upcard is an ace."
  (let ((cards nil) (card nil))
    (setf cards (cdr (assq 'cards bj-dealer-hand)))
    (setf card (cdr (assq 0 cards)))
    (bj-ace-card card)))

(defun bj-draw-dealer-hand ()
  "Draw the dealer hand."
  (let ((cards nil) (card nil) (suit nil) (value nil))
    (setf cards (slot-value bj-dealer-hand 'cards))
    (insert "  ")
    (dotimes (x (length cards))
      (setf card (nth x cards))
      (if (and (eq x 0) (slot-value bj-dealer-hand 'hide-down-card))
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
  (let ((total 0) (card nil) (cards (slot-value bj-dealer-hand 'cards)))
    (dotimes (x (length cards))
      (if (not (and (eq x 0) (slot-value bj-dealer-hand 'hide-down-card)))
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

;; (defun bj-p (label x)
;;   "LABEL is printed with X."
;;   (move-end-of-line 0)
;;   (insert (format "\n%s%s" label x)))

(defun bj-card-face (value suit)
  "Return card face based on VALUE and SUIT."
  (aref (aref bj-faces value) suit))

(defun bj ()
  "The game of Blackjack."
  (interactive)
  (let ((debug-on-error t))
    (let ((buffer-name "bj"))
      (get-buffer-create buffer-name)
      (switch-to-buffer buffer-name)
      (with-current-buffer buffer-name
        (bj-deal-hands)))))

(provide 'bj)
;;; bj.el ends here
