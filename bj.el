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
        (setf game (bj-deal-hands game))
        (bj-draw game)))))

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

(defun bj-deal-cards (game count)
  "Deal COUNT cards from GAME."
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
  "Deal new GAME hands."
  (if (bj-need-to-shuffle game)
      (setf game (bj-shuffle game)))
  (let ((dealer-cards nil)
        (player-cards nil)
        (result nil))
    (setf result (bj-deal-cards game 2))
    (setf game (cdr (assq 'game result)))
    (setf game (cons `(player-hands . (0 . (cards . ,(cdr (assq 'dealt result))))) game))
    (setf result (bj-deal-cards game 2))
    (setf game (cdr (assq 'game result)))
    (setf game (cons `(dealer-hand . (cards . ,(cdr (assq 'dealt result)))) game)))
  game)

(defun bj-need-to-shuffle (game)
  "Are GAME cards nearly exhausted?"
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
  "Create and add GAME cards to the shoe."
  (setf game (delq (assq 'shoe game) game))
  (let ((cards nil)
        (x 0))
    (dotimes (n (cdr (assq 'num-decks game)))
      (dotimes (suit 4)
        (dotimes (value 13)
          (push `(,x . (,value . ,suit)) cards)
          (setf x (1+ x)))))
    (setf cards (bj-shuffle-loop cards))
    (setf game (cons `(shoe . ,cards) game)))
  game)

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
  "GAME quit.")

(defun bj-draw (game)
  "Draw GAME."
  ;; (erase-buffer)
  (insert "\n  Dealer:\n")
  (bj-draw-dealer-hand game)
  (insert "\n\n  Player:\n\n")
  (insert "  ")

  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'blackjack-deal-hand)
    (insert (propertize "[Deal Hand]" 'keymap map 'mouse-face 'highlight 'help-echo "Deal Hand") "  "))

  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'blackjack-quit)
    (insert (propertize "[Quit]" 'keymap map 'mouse-face 'highlight 'help-echo "Quit") "  ")))

(defun bj-draw-dealer-hand (game)
  "Draw the GAME dealer hand."
  (let ((dealer-hand nil) (cards nil) (card nil) (suit nil) (value nil))
    (setf dealer-hand (assq 'dealer-hand game))
    (setf cards (cdr (cdr dealer-hand)))
    (insert "  ")
    (dotimes (x (length cards))
      (setf card (cdr (assq x cards)))
      (setf value (car card))
      (setf suit (cdr card))
      (insert (bj-card-face game value suit))
      (insert " "))
    (insert " ⇒  ")
    (insert (number-to-string (bj-dealer-hand-value cards 'soft)))))

(defun bj-dealer-hand-value (cards count-method)
  "Calculates CARDS total value based on COUNT-METHOD."
  (let ((total 0) (card nil))
    (dotimes (x (length cards))
      (setf card (cdr (assq x cards)))
      (setf total (+ total (bj-card-value card count-method total))))
    (if (and (eq count-method 'soft) (> total 21))
        (setf total (bj-dealer-hand-value cards 'hard)))
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
  "Return card face from GAME based on VALUE and SUIT."
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
