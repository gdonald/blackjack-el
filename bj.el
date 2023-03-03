;;; bj.el --- The game of Blackjack

;; Copyright (C) 2022 Greg Donald

;; Author: Greg Donald <gdonald@gmail.com>
;; Version: 1.0
;; Package-Requires: ()
;; Keywords: games
;; URL: https://https://github.com/gdonald/bj-el

;;; Commentary:
;;; This package lets you play Blackjack in Emacs.

;;; Code:

(defvar bj-shoe nil)
(defvar bj-player-hands nil)
(defvar bj-dealer-hand nil)
(defvar bj-hide-down-card t)
(defvar bj-cards-per-deck 52)
(defvar bj-num-decks 1)
(defvar bj-money 10000)
(defvar bj-current-bet 500)
(defvar bj-current-player-hand 0)
(defvar bj-max-player-hands 7)

(defvar bj-faces '((0 . ((0 . "A♠") (1 . "A♥") (2 . "A♣") (3 . "A♦")))
                   (1 . ((0 . "2♠") (1 . "2♥") (2 . "2♣") (3 . "2♦")))
                   (2 . ((0 . "3♠") (1 . "3♥") (2 . "3♣") (3 . "3♦")))
                   (3 . ((0 . "4♠") (1 . "4♥") (2 . "4♣") (3 . "4♦")))
                   (4 . ((0 . "5♠") (1 . "5♥") (2 . "5♣") (3 . "5♦")))
                   (5 . ((0 . "6♠") (1 . "6♥") (2 . "6♣") (3 . "6♦")))
                   (6 . ((0 . "7♠") (1 . "7♥") (2 . "7♣") (3 . "7♦")))
                   (7 . ((0 . "8♠") (1 . "8♥") (2 . "8♣") (3 . "8♦")))
                   (8 . ((0 . "9♠") (1 . "9♥") (2 . "9♣") (3 . "9♦")))
                   (9 . ((0 . "T♠") (1 . "T♥") (2 . "T♣") (3 . "T♦")))
                   (10 . ((0 . "J♠") (1 . "J♥") (2 . "J♣") (3 . "J♦")))
                   (11 . ((0 . "Q♠") (1 . "Q♥") (2 . "Q♣") (3 . "Q♦")))
                   (12 . ((0 . "K♠") (1 . "K♥") (2 . "K♣") (3 . "K♦")))
                   (13 . ((0 . "??")))))

(defvar bj-shuffle-specs '((8 . 95)
                           (7 . 92)
                           (6 . 89)
                           (5 . 86)
                           (4 . 84)
                           (3 . 82)
                           (2 . 81)
                           (1 . 80)))

(defun bj-card-is-ace (card)
  "Return non-nil if CARD is an ace."
  (eq 0 (cdr card)))

(defun bj-card-is-ten (card)
  "Return non-nil if CARD is ten or greater."
  (> (cdr card) 8))

(defun bj-deal-cards (count)
  "Deal COUNT cards."
  (let ((card nil) (dealt nil))
    (dotimes (x count)
      (setf card (car bj-shoe))
      (push `(,x . ,(cdr card)) dealt)
      (setf bj-shoe (delq (assq (car card) bj-shoe) bj-shoe)))
    dealt))

(defun bj-deal-hands ()
  "Deal new hands."
  (interactive)
  (if (bj-need-to-shuffle)
      (bj-shuffle))
  (setf bj-player-hands `((0 . ((cards . ,(bj-deal-cards 2))
                                (bet . ,bj-current-bet)
                                (status . nil)
                                (played . nil)
                                (payed . nil)
                                (stood . nil)))))
  (setf bj-dealer-hand `((cards . ,(bj-deal-cards 2))))
  (setf bj-hide-down-card t)
  (bj-draw-hands)
  (bj-draw-bet-options))

(defun bj-need-to-shuffle ()
  "Are shoe cards nearly exhausted?"
  (let ((cards nil) (used nil) (spec nil))
    (setq cards (length (cdr bj-shoe)))
    (if (> cards 0)
        (progn
          (setq used (- (* bj-num-decks bj-cards-per-deck) cards))
          (setq spec (cdr (assq bj-num-decks bj-shuffle-specs)))
          (> (* 100 (/ (float used) cards)) spec))
      t)))

(defun bj-shuffle ()
  "Create and add cards to the shoe."
  (let ((cards nil)
        (x 0))
    (dotimes (n bj-num-decks)
      (dotimes (suit 4)
        (dotimes (value 13)
          (push `(,x . (,value . ,suit)) cards)
          (setf x (1+ x)))))
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
  (insert "\n\n  Player:\n\n")
  (bj-draw-player-hands)
  (insert "\n\n  "))

(defun bj-ask-hand-action ()
  "Ask action for current hand."
  (let ((read-answer-short t))
    (read-answer "Hand Action "
		 '(("hit"    ?h "hit")
		   ("stand"  ?s "stand")
		   ("split"  ?p "split")
		   ("double" ?d "double")
		   ("help"   ?? "show help")))))

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
    (setf cards (assq 'cards player-hand))
    (if (not (or
              (assq 'played player-hand)
              (assq 'stood player-hand)
              (eq (bj-player-hand-value cards) 21)
              (bj-hand-is-blackjack cards)
              (bj-player-hand-is-busted cards)))
        t)))

(defun bj-can-stand ()
  "Return non-nil if the current player hand can stand."
  (let ((player-hand nil) (cards nil))
    (setf player-hand (bj-get-current-player-hand))
    (setf cards (assq 'cards player-hand))
    (if (not (or
              (assq 'stood player-hand)
              (bj-player-hand-is-busted cards)
              (bj-hand-is-blackjack cards)))
        t)))

(defun bj-can-split ()
  "Return non-nil if the current player hand can split."
  (let ((player-hand nil) (cards nil))
    (setf player-hand (bj-get-current-player-hand))
    (setf cards (assq 'cards player-hand))
    (if (and
         (not (assq 'stood player-hand))
         (< (length bj-player-hands) 7)
         (>= bj-money (+ (bj-all-bets) (assq 'bet player-hand)))
         (eq (length cards) 2))
        (let ((card-0 nil) (card-1 nil))
          (setf card-0 (assq 0 cards))
          (setf card-1 (assq 1 cards))
          (if (eq (cdr card-0) (cdr card-1))
              t)))))

(defun bj-can-dbl ()
  "Return non-nil if the current player hand can double."
  (let ((player-hand nil) (cards nil))
    (setf player-hand (bj-get-current-player-hand))
    (setf cards (assq 'cards player-hand))
    (if (and
         (>= bj-money (+ (bj-all-bets) (assq 'bet player-hand)))
         (not (or (assq 'stood player-hand) (not (eq 2 (length cards)))
                  (bj-hand-is-blackjack cards))))
        t)))

(defun bj-get-current-player-hand ()
  "Return current player hand."
  (let ((player-hand nil))
    (setf player-hand (assq bj-current-player-hand bj-player-hands))
    player-hand))

(defun bj-all-bets ()
  "Sum of all player hand bets."
  (let ((total 0) (player-hand))
    (dotimes (x (length bj-player-hands))
      (setf player-hand (cdr (assq x bj-player-hands)))
      (setf total (+ total (assq 'bet player-hand))))
    total))

(defun bj-draw-player-hand-actions ()
  "Draw player hand actions."
  (if (bj-can-hit)
      (let ((map (make-sparse-keymap)))
        (define-key map [mouse-1] 'bj-hit)
        (insert (propertize "[Hit]" 'keymap map 'mouse-face 'highlight 'help-echo "Hit") "  ")))
  (if (bj-can-stand)
      (let ((map (make-sparse-keymap)))
        (define-key map [mouse-1] 'bj-stand)
        (insert (propertize "[Stand]" 'keymap map 'mouse-face 'highlight 'help-echo "Stand") "  ")))
  (if (bj-can-split)
      (let ((map (make-sparse-keymap)))
        (define-key map [mouse-1] 'bj-split)
        (insert (propertize "[Split]" 'keymap map 'mouse-face 'highlight 'help-echo "Split") "  ")))
  (if (bj-can-dbl)
      (let ((map (make-sparse-keymap)))
        (define-key map [mouse-1] 'bj-dbl)
        (insert (propertize "[Double]" 'keymap map 'mouse-face 'highlight 'help-echo "Double") "  "))))

(defun bj-draw-bet-options ()
  "Draw bet options."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'bj-deal-hands)
    (insert (propertize "[Deal Hand]" 'keymap map 'mouse-face 'highlight 'help-echo "Deal Hand") "  "))

  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'bj-new-bet)
    (insert (propertize "[Change Bet]" 'keymap map 'mouse-face 'highlight 'help-echo "Change Bet") "  "))

  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'bj-game-otions)
    (insert (propertize "[Options]" 'keymap map 'mouse-face 'highlight 'help-echo "Options") "  "))

  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'bj-quit)
    (insert (propertize "[Quit]" 'keymap map 'mouse-face 'highlight 'help-echo "Quit") "  ")))

(defun bj-player-hand-is-busted (cards)
  "Return non-nil if hand CARDS value is more than 21."
  (if (> (bj-player-hand-value cards 'soft) 21)
      t))

(defun bj-dealer-hand-is-busted (cards)
  "Return non-nil if hand CARDS value is more than 21."
  (if (> (bj-dealer-hand-value cards 'soft) 21)
      t))

(defun bj-hand-is-blackjack (cards)
  "Return non-nil if hand CARDS is blackjack."
  (if (eq 2 (length cards))
      (let ((card-0 nil) (card-1 nil))
        (setf card-0 (assq 0 cards))
        (setf card-1 (assq 1 cards))
        (if (or
             (and
              (bj-card-is-ace card-0)
              (bj-card-is-ten card-1))
             (and
              (bj-card-is-ace card-1)
              (bj-card-is-ten card-0)))
            t))))

(defun bj-dealer-upcard-is-ace ()
  "Return non-nil if dealer upcard is an ace."
  (let ((cards nil) (card nil))
    (setf cards (cdr (assq 'cards bj-dealer-hand)))
    (setf card (cdr (assq 0 cards)))
    (bj-card-is-ace card)))

(defun bj-draw-dealer-hand ()
  "Draw the dealer hand."
  (let ((cards nil) (card nil) (suit nil) (value nil))
    (setf cards (cdr (assq 'cards bj-dealer-hand)))
    (insert "  ")
    (dotimes (x (length cards))
      (setf card (cdr (assq x cards)))
      (if (and (eq x 1) bj-hide-down-card)
          (progn
            (setf value 13)
            (setf suit 0))
        (progn
          (setf value (car card))
          (setf suit (cdr card))))
      (insert (bj-card-face value suit))
      (insert " "))
    (insert " ⇒  ")
    (insert (number-to-string (bj-dealer-hand-value cards 'soft)))))

(defun bj-dealer-hand-value (cards count-method)
  "Calculates CARDS total value based on COUNT-METHOD."
  (let ((total 0) (card nil))
    (dotimes (x (length cards))
      (if (not (and (eq x 1) bj-hide-down-card))
          (progn
            (setf card (cdr (assq x cards)))
            (setf total (+ total (bj-card-value card count-method total))))))
    (if (and (eq count-method 'soft) (> total 21))
        (setf total (bj-dealer-hand-value cards 'hard)))
    total))

(defun bj-draw-player-hands ()
  "Draw players hands."
  (let ((player-hand nil))
    (dotimes (x (length bj-player-hands))
      (setf player-hand (cdr (assq x bj-player-hands)))
      (bj-draw-player-hand player-hand))))

(defun bj-draw-player-hand (player-hand)
  "Draw the PLAYER-HAND."
  (let ((cards nil) (card nil) (suit nil) (value nil))
    (setf cards (cdr (assq 'cards player-hand)))
    (insert "  ")
    (dotimes (x (length cards))
      (setf card (cdr (assq x cards)))
      (setf value (car card))
      (setf suit (cdr card))
      (insert (bj-card-face value suit))
      (insert " "))
    (insert " ⇒  ")
    (insert (number-to-string (bj-player-hand-value cards 'soft)))))

(defun bj-player-hand-value (cards count-method)
  "Calculates CARDS total value based on COUNT-METHOD."
  (let ((total 0) (card nil))
    (dotimes (x (length cards))
      (setf card (cdr (assq x cards)))
      (setf total (+ total (bj-card-value card count-method total))))
    (if (and (eq count-method 'soft) (> total 21))
        (setf total (bj-player-hand-value cards 'hard)))
    total))

(defun bj-card-value (card count-method total)
  "Calculates CARD value based on COUNT-METHOD and running hand TOTAL."
  (let ((value nil))
    (setf value (1+ (car card)))
    (if (> value 9)
        (setf value 10))
    (if (and (eq count-method 'soft) (eq value 1) (< total 11))
        (setf value 11))
    value))

(defun bj-p (label x)
  "LABEL is printed with X."
  (move-end-of-line 0)
  (insert (format "\n%s%s" label x)))

(defun bj-card-face (value suit)
  "Return card face based on VALUE and SUIT."
  (let ((faces nil) (face nil))
    (setf faces (cdr (assq value bj-faces)))
    (setf face (cdr (assq suit faces)))
    face))

(defun bj ()
  "The game of Blackjack."
  (interactive)
  (let ((buffer-name "bj"))
    (get-buffer-create buffer-name)
    (switch-to-buffer buffer-name)
    (with-current-buffer buffer-name
      (bj-deal-hands))))

(provide 'bj)
;;; bj.el ends here
