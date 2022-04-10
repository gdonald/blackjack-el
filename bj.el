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

(defun bj ()
  "The game of Blackjack."
  (interactive)
  (let ((buffer-name "bj"))
    (get-buffer-create buffer-name)
    (switch-to-buffer buffer-name)
    (with-current-buffer buffer-name
      (let ((game (bj-make-game)))
        (setf game (bj-deal-hands game))))))

(defun bj-make-game ()
  "Create initial game state."
  '((cards-per-deck . 52)
    (num-decks . 1)
    (money . 10000)
    (current-bet . 500)
    (faces . ((0 . ((0 . "A♠") (1 . "A♥") (2 . "A♣") (3 . "A♦")))
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
    (shuffle-specs . ((8 . 95)
                      (7 . 92)
                      (6 . 89)
                      (5 . 86)
                      (4 . 84)
                      (3 . 82)
                      (2 . 81)
                      (1 . 80)))))

(defun bj-deal-cards (game count)
  "Deal COUNT cards using GAME."
  (let ((cards nil) (card nil) (dealt nil))
    (dotimes (x count)
      (setf cards (cdr (assq 'shoe game)))
      (setf card (car cards))
      (push `(,x . ,(cdr card)) dealt)
      (setf cards (delq (assq (car card) cards) cards))
      (setf game (delq (assq 'shoe game) game))
      (setf game (cons `(shoe . ,cards) game)))
    `((game . ,game) (dealt . ,dealt))))

(defun bj-deal-hands (game)
  "Deal new hands from GAME."
  (interactive)
  (bj-p "game: " game)
  (if (bj-need-to-shuffle game)
      (setf game (bj-shuffle game)))
  (let ((dealer-cards nil) (player-cards nil) (result nil))
    (setf result (bj-deal-cards game 2))
    (setf game (cdr (assq 'game result)))
    (setf game (cons `(player-hands . ((0 . ((cards . ,(cdr (assq 'dealt result))))))) game))
    (setf result (bj-deal-cards game 2))
    (setf game (cdr (assq 'game result)))
    (setf game (cons `(dealer-hand . ((cards . ,(cdr (assq 'dealt result))) (hide-down-card . t))) game)))
  (bj-draw-hands game)
  (bj-draw-bet-options game))

(defun bj-need-to-shuffle (game)
  "Are GAME shoe cards nearly exhausted?"
  (let ((cards nil) (used nil) (spec nil) (decks nil))
    (setq cards (length (cdr (assq 'shoe game))))
    (if (> cards 0)
        (progn
          (setq decks (cdr (assq 'num-decks game)))
          (setq used (- (* decks (cdr (assq 'cards-per-deck game))) cards))
          (setq spec (cdr (assq decks (assq 'shuffle-specs game))))
          (> (* 100 (/ (float used) cards)) spec))
      t)))

(defun bj-shuffle (game)
  "Create and add cards to the GAME shoe."
  (setf game (delq (assq 'shoe game) game))
  (let ((cards nil)
        (x 0))
    (dotimes (n (cdr (assq 'num-decks game)))
      (dotimes (suit 4)
        (dotimes (value 13)
          (push `(,x . (,value . ,suit)) cards)
          (setf x (1+ x)))))
    (setf cards (bj-shuffle-loop cards))
    (setf game (cons `(shoe . ,cards) game))))

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

(defun bj-quit (game)
  "Quit using GAME.")

(defun bj-draw-hands (game)
  "Top-level draw using GAME."
  (erase-buffer)
  (insert "\n  Dealer:\n")
  (bj-draw-dealer-hand game)
  (insert "\n\n  Player:\n\n")
  (bj-draw-player-hands game)
  (insert "\n\n  "))

(defun bj-hit (game)
  "Deal a new card to the current GAME player hand."
  (interactive))

(defun bj-stand (game)
  "End the current GAME player hand."
  (interactive))

(defun bj-split (game)
  "Split the current GAME player hand."
  (interactive))

(defun bj-dbl (game)
  "Double the current GAME player hand."
  (interactive))

(defun bj-can-hit (game)
  "Return non-nil if the current GAME player hand can hit."
  t)

(defun bj-can-stand (game)
  "Return non-nil if the current GAME player hand can stand."
  t)

(defun bj-can-split (game)
  "Return non-nil if the current GAME player hand can split."
  t)

(defun bj-can-dbl (game)
  "Return non-nil if the current GAME player hand can double."
  t)

(defun bj-all-bets (game)
  "Sum of all GAME player hand bets."
  (let ((total 0) (player-hands nil) (player-hand))
    (setf player-hands (cdr (assq 'player-hands game)))
    (dotimes (x (length player-hands))
      (setf player-hand (cdr (assq x player-hands)))
      ;; TODO
      )))

(defun bj-draw-player-hand-actions (game)
  "Draw GAME player hand actions."
  (if (bj-can-hit game)
      (let ((map (make-sparse-keymap)))
        (define-key map [mouse-1] `(bj-hit ,game))
        (insert (propertize "[Hit]" 'keymap map 'mouse-face 'highlight 'help-echo "Hit") "  ")))
  (if (bj-can-stand game)
      (let ((map (make-sparse-keymap)))
        (define-key map [mouse-1] `(bj-stand ,game))
        (insert (propertize "[Stand]" 'keymap map 'mouse-face 'highlight 'help-echo "Stand") "  ")))
  (if (bj-can-split game)
      (let ((map (make-sparse-keymap)))
        (define-key map [mouse-1] `(bj-split ,game))
        (insert (propertize "[Split]" 'keymap map 'mouse-face 'highlight 'help-echo "Split") "  ")))
  (if (bj-can-dbl game)
      (let ((map (make-sparse-keymap)))
        (define-key map [mouse-1] `(bj-dbl ,game))
        (insert (propertize "[Double]" 'keymap map 'mouse-face 'highlight 'help-echo "Double") "  "))))

(defun bj-draw-bet-options (game)
  "Draw GAME bet options."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] `(bj-deal-hands ,game))
    (insert (propertize "[Deal Hand]" 'keymap map 'mouse-face 'highlight 'help-echo "Deal Hand") "  "))

  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] `(bj-new-bet ,game))
    (insert (propertize "[Change Bet]" 'keymap map 'mouse-face 'highlight 'help-echo "Change Bet") "  "))

  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] `(bj-game-otions ,game))
    (insert (propertize "[Options]" 'keymap map 'mouse-face 'highlight 'help-echo "Options") "  "))

  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] `(bj-quit ,game))
    (insert (propertize "[Quit]" 'keymap map 'mouse-face 'highlight 'help-echo "Quit") "  ")))

(defun bj-draw-dealer-hand (game)
  "Draw the GAME dealer hand."
  (let ((dealer-hand nil) (hide-down-card . nil) (cards nil) (card nil) (suit nil) (value nil))
    (setf dealer-hand (assq 'dealer-hand game))
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
      (insert (bj-card-face game value suit))
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

(defun bj-draw-player-hands (game)
  "Draw GAME players hands."
  (let ((player-hand nil) (player-hands nil))
    (setf player-hands (cdr (assq 'player-hands game)))
    (dotimes (x (length player-hands))
      (setf player-hand (cdr (assq x player-hands)))
      (bj-draw-player-hand game player-hand))))

(defun bj-draw-player-hand (game player-hand)
  "Draw the GAME PLAYER-HAND."
  (let ((cards nil) (card nil) (suit nil) (value nil))
    (setf cards (cdr (assq 'cards player-hand)))
    (insert "  ")
    (dotimes (x (length cards))
      (setf card (cdr (assq x cards)))
      (setf value (car card))
      (setf suit (cdr card))
      (insert (bj-card-face game value suit))
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

(defun bj-card-face (game value suit)
  "Return GAME card face based on VALUE and SUIT."
  (let ((faces nil) (face nil))
    (setf faces (cdr (assq 'faces game)))
    (setf faces (cdr (assq value faces)))
    (setf face (cdr (assq suit faces)))
    face))

(defun bj-p (label x)
  "LABEL is printed with X."
  (move-end-of-line 0)
  (insert (format "\n%s%s" label x)))

(provide 'bj)
;;; bj.el ends here
