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

(defvar bj-game (bj-make-game))

(defun bj ()
  "The game of Blackjack."
  (interactive)
  (let ((buffer-name "bj"))
    (get-buffer-create buffer-name)
    (switch-to-buffer buffer-name)
    (with-current-buffer buffer-name
      (bj-deal-hands))))

(defun bj-make-game ()
  "Create initial game state."
  '((cards-per-deck . 52)
    (num-decks . 1)
    (money . 10000)
    (current-bet . 500)
    (faces . ((0 . ((0 . "🂡") (1 . "🂱") (2 . "🃁") (3 . "🃑")))
              (1 . ((0 . "🂢") (1 . "🂲") (2 . "🃂") (3 . "🃒")))
              (2 . ((0 . "🂣") (1 . "🂳") (2 . "🃃") (3 . "🃓")))
              (3 . ((0 . "🂤") (1 . "🂴") (2 . "🃄") (3 . "🃔")))
              (4 . ((0 . "🂥") (1 . "🂵") (2 . "🃅") (3 . "🃕")))
              (5 . ((0 . "🂦") (1 . "🂶") (2 . "🃆") (3 . "🃖")))
              (6 . ((0 . "🂧") (1 . "🂷") (2 . "🃇") (3 . "🃗")))
              (7 . ((0 . "🂨") (1 . "🂸") (2 . "🃈") (3 . "🃘")))
              (8 . ((0 . "🂩") (1 . "🂹") (2 . "🃉") (3 . "🃙")))
              (9 . ((0 . "🂪") (1 . "🂺") (2 . "🃊") (3 . "🃚")))
              (10 . ((0 . "🂫") (1 . "🂻") (2 . "🃋") (3 . "🃛")))
              (11 . ((0 . "🂭") (1 . "🂽") (2 . "🃍") (3 . "🃝")))
              (12 . ((0 . "🂮") (1 . "🂾") (2 . "🃎") (3 . "🃞")))
              (13 . ((0 . "🂠")))))
    (shuffle-specs . ((8 . 95)
                      (7 . 92)
                      (6 . 89)
                      (5 . 86)
                      (4 . 84)
                      (3 . 82)
                      (2 . 81)
                      (1 . 80)))))

(defun bj-deal-cards (count)
  "Deal COUNT cards."
  (let ((cards nil) (card nil) (dealt nil))
    (dotimes (x count)
      (setf cards (cdr (assq 'shoe bj-game)))
      (setf card (car cards))
      (push `(,x . ,(cdr card)) dealt)
      (setf cards (delq (assq (car card) cards) cards))
      (setf bj-game (delq (assq 'shoe bj-game) bj-game))
      (setf bj-game (cons `(shoe . ,cards) bj-game)))
    `((bj-game . ,bj-game) (dealt . ,dealt))))

(defun bj-deal-hands ()
  "Deal new hands."
  (interactive)
  (if (bj-need-to-shuffle)
      (bj-shuffle))
  (let ((dealer-cards nil) (player-cards nil) (result nil))
    (setf result (bj-deal-cards 2))
    (setf bj-game (cdr (assq 'bj-game result)))
    (setf bj-game (cons `(player-hands . ((0 . ((cards . ,(cdr (assq 'dealt result))))))) bj-game))
    (setf result (bj-deal-cards 2))
    (setf bj-game (cdr (assq 'bj-game result)))
    (setf bj-game (cons `(dealer-hand . ((cards . ,(cdr (assq 'dealt result))) (hide-down-card . t))) bj-game)))
  (bj-draw-hands))

(defun bj-need-to-shuffle ()
  "Are shoe cards nearly exhausted?"
  (let ((cards nil) (used nil) (spec nil) (decks nil))
    (setq cards (length (cdr (assq 'shoe bj-game))))
    (if (> cards 0)
        (progn
          (setq decks (cdr (assq 'num-decks bj-game)))
          (setq used (- (* decks (cdr (assq 'cards-per-deck bj-game))) cards))
          (setq spec (cdr (assq decks (assq 'shuffle-specs bj-game))))
          (> (* 100 (/ (float used) cards)) spec))
      t)))

(defun bj-shuffle ()
  "Create and add cards to the shoe."
  (setf bj-game (delq (assq 'shoe bj-game) bj-game))
  (let ((cards nil)
        (x 0))
    (dotimes (n (cdr (assq 'num-decks bj-game)))
      (dotimes (suit 4)
        (dotimes (value 13)
          (push `(,x . (,value . ,suit)) cards)
          (setf x (1+ x)))))
    (setf cards (bj-shuffle-loop cards))
    (setf bj-game (cons `(shoe . ,cards) bj-game))))

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

(defun bj-hit ()
  "Deal a new card to the current player hand."
  (interactive))

(defun bj-stand ()
  "End the current player hand."
  (interactive))

(defun bj-split ()
  "Split the current player hand."
  (interactive))

(defun bj-dbl ()
  "Double the current player hand."
  (interactive))

(defun bj-can-hit ()
  "Return non-nil if the current player hand can hit."
  t)

(defun bj-can-stand ()
  "Return non-nil if the current player hand can stand."
  t)

(defun bj-can-split ()
  "Return non-nil if the current player hand can split."
  t)

(defun bj-can-dbl ()
  "Return non-nil if the current player hand can double."
  t)

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

(defun bj-draw-bet-options()
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

(defun bj-draw-dealer-hand ()
  "Draw the dealer hand."
  (let ((dealer-hand nil) (hide-down-card . nil) (cards nil) (card nil) (suit nil) (value nil))
    (setf dealer-hand (assq 'dealer-hand bj-game))
    (setf hide-down-card (cdr (assq 'hide-down-card dealer-hand)))
    (setf cards (cdr (assq 'cards dealer-hand)))
    (insert "  ")
    (dotimes (x (length cards))
      (setf card (cdr (assq x cards)))
      (if (and (eq x 1) hide-down-card)
          (progn
            (setf value 13)
            (setf suit 0))
        (progn
          (setf value (car card))
          (setf suit (cdr card))))
      (insert (bj-card-face value suit))
      (insert " "))
    (insert " ⇒  ")
    (insert (number-to-string (bj-dealer-hand-value cards 'soft hide-down-card)))))

(defun bj-dealer-hand-value (cards count-method hide-down-card)
  "Calculates CARDS total value based on COUNT-METHOD and HIDE-DOWN-CARD."
  (let ((total 0) (card nil))
    (dotimes (x (length cards))
      (if (not (and (eq x 1) hide-down-card))
          (progn
            (setf card (cdr (assq x cards)))
            (setf total (+ total (bj-card-value card count-method total))))))
    (if (and (eq count-method 'soft) (> total 21))
        (setf total (bj-dealer-hand-value cards 'hard)))
    total))

(defun bj-draw-player-hands ()
  "Draw players hands."
  (let ((player-hand nil) (player-hands nil))
    (setf player-hands (cdr (assq 'player-hands bj-game)))
    (dotimes (x (length player-hands))
      (setf player-hand (cdr (assq x player-hands)))
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

(defun bj-card-face (value suit)
  "Return card face based on VALUE and SUIT."
  (let ((faces nil) (face nil))
    (setf faces (cdr (assq 'faces bj-game)))
    (setf faces (cdr (assq value faces)))
    (setf face (cdr (assq suit faces)))
    face))

(defun bj-p (label x)
  "LABEL is printed with X."
  (move-end-of-line 0)
  (insert (format "\n%s%s" label x)))

(provide 'bj)
;;; bj.el ends here
