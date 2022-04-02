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
      (let ((game (bj-create-game)))
        (setf game (bj-deal-hand game))
        (bj-draw game)))))

(defun bj-create-game ()
  "Create initial game state."
  (interactive)
  '((dealer-hand . (cards . nil))
    (player-hands . nil)
    (num-decks . 1)
    (money . 10000)
    (current-bet . 500)))

(defun bj-deal-hand (game)
  "Deal new GAME hands."
  (if (bj-need-to-shuffle game)
      (setf game (bj-shuffle game)))

  ;; deal dealer hand

  ;; deal player hand

  game)

(defun bj-need-to-shuffle (game)
  "Are GAME cards nearly exhausted?"
  t)

(defun bj-shuffle (game)
  "Create and add GAME cards to the show."
  (let ((cards nil)
        (x 0))
    (dotimes (suit 4)
      (dotimes (value 13)
        (push `(,x . (,value . ,suit)) cards)
        (setf x (1+ x))))
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
    (setf new-cards (setf cards (delq (assq rand cards) cards)))
    (setf new-cards (cons card cards))
    new-cards))

(defun bj-quit (game)
  "GAME quit.")

(defun bj-draw (game)
  "Draw GAME."
  ;; (erase-buffer)
  (insert "\n  Dealer:")
  (insert "\n\n  Player:\n\n")
  (insert "  ")

  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'blackjack-deal-hand)
    (insert (propertize "[Deal Hand]" 'keymap map 'mouse-face 'highlight 'help-echo "Deal Hand") "  "))

  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'blackjack-quit)
    (insert (propertize "[Quit]" 'keymap map 'mouse-face 'highlight 'help-echo "Quit") "  ")))

(defun bj-p (label x)
  "LABEL is printed with X."
  (move-end-of-line 0)
  (insert (format "\n%s%s" label x)))

(provide 'bj)
;;; bj.el ends here
