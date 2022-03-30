;;; blackjack.el --- The game of Blackjack

;; Copyright (C) 2022 Greg Donald

;; Author: Greg Donald <gdonald@gmail.com>
;; Version: 1.0
;; Package-Requires: ()
;; Keywords: games
;; URL: https://https://github.com/gdonald/blackjack-el

;;; Commentary:
;;; This package lets you play Blackjack in Emacs.

;;; Code:
(defun blackjack ()
  "The game of Blackjack."
  (interactive)
  (let ((buffer-name "blackjack"))
    (get-buffer-create buffer-name)
    (switch-to-buffer buffer-name)
    (with-current-buffer buffer-name
      (defvar blackjack-game '((shoe nil)
                   (dealer-hand '(:cards nil))
                   (player-hands nil)
                   (num-decks 1)
                   (money 10000)
                   (current-bet 500)))
      (blackjack-deal-hand blackjack-game)
      (blackjack-draw blackjack-game))))

(defun blackjack-draw (game)
  "GAME top level draw function."
  (erase-buffer)
  (insert "\n  Dealer:")
  (insert "\n\n  Player:\n\n")
  (insert "  ")

  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'blackjack-deal-hand)
    (insert (propertize "[Deal Hand]" 'keymap map 'mouse-face 'highlight 'help-echo "Deal Hand") "  "))

  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'blackjack-quit)
    (insert (propertize "[Quit]" 'keymap map 'mouse-face 'highlight 'help-echo "Quit") "  "))


  )

(defun blackjack-need-to-shuffle (game)
  "GAME are we almost out of cards?"
  t

  )

(defun blackjack-shuffle (game)
  "GAME re-shuffle the shoe."
  (setcdr (assq 'shoe game) '())

  (let ((cards '()))
    (dotimes (suit 4)
      (dotimes (value 13)
        (setf cards (cons cards '(value . suit)))
        )
      )
    )
  )


(defun blackjack-deal-hand (game)
  "GAME deal a new hand."

  )

(defun blackjack-quit (game)
  "GAME quit."

  )


(provide 'blackjack)
;;; blackjack.el ends here
