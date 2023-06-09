;;; blackjack.el --- The game of Blackjack -*- lexical-binding: t; -*-

;; Copyright (C) 2022, 2023 Greg Donald
;; SPDX-License-Identifier: GPL-3.0-only
;; Author: Greg Donald <gdonald@gmail.com>
;; Version: 1.0.3
;; Package-Requires: ((emacs "26.2") (dash "2.19.1"))
;; Keywords: card game games blackjack 21
;; URL: https://github.com/gdonald/blackjack-el

;;; Commentary:
;; This package lets you play Blackjack in Emacs.

;;; Code:

(require 'cl-lib)
(require 'eieio)

(eval-when-compile
  (require 'dash) ; for `--dotimes'
)

(eval-and-compile
  (declare-function -partial "dash")
  (autoload #'-partial "dash")
  (declare-function -table "dash")
  (autoload #'-table "dash")
)

(defconst blackjack--buffer-name "* Blackjack *"
  "The buffer name.")

(defgroup blackjack nil
  "Customization group for Blackjack."
  :group 'games)

(defcustom blackjack-stand-key "s"
  "Key to press to stand on current hand."
  :type '(string) :group 'blackjack)

(defcustom blackjack-deal-double-key "d"
  "Key to press to deal or double a hand."
  :type '(string) :group 'blackjack)

(defcustom blackjack-bet-back-key "b"
  "Key to press to change bet or go back."
  :type '(string) :group 'blackjack)

(defcustom blackjack-quit-key "q"
  "Key to press to quit."
  :type '(string) :group 'blackjack)

(defcustom blackjack-options-key "o"
  "Key to press to view options menus."
  :type '(string) :group 'blackjack)

(defcustom blackjack-num-decks-no-insurance-key "n"
  "Key to press to decline insurance or change number of decks."
  :type '(string) :group 'blackjack)

(defcustom blackjack-deck-type-key "t"
  "Key to press to change deck type."
  :type '(string) :group 'blackjack)

(defcustom blackjack-face-type-key "f"
  "Key to press to change face type."
  :type '(string) :group 'blackjack)

(defcustom blackjack-hit-key "h"
  "Key to press to hit a hand."
  :type '(string) :group 'blackjack)

(defcustom blackjack-split-key "p"
  "Key to press to split a hand."
  :type '(string) :group 'blackjack)

(defcustom blackjack-deck-regular-key "1"
  "Key to press to switch to a regular deck."
  :type '(string) :group 'blackjack)

(defcustom blackjack-deck-aces-key "2"
  "Key to press to switch to a deck of aces."
  :type '(string) :group 'blackjack)

(defcustom blackjack-deck-jacks-key "3"
  "Key to press to switch to a deck of jacks."
  :type '(string) :group 'blackjack)

(defcustom blackjack-deck-aces-jacks-key "4"
  "Key to press to switch to a deck of aces and jacks."
  :type '(string) :group 'blackjack)

(defcustom blackjack-deck-sevens-key "5"
  "Key to press to switch to a deck of sevens."
  :type '(string) :group 'blackjack)

(defcustom blackjack-deck-eights-key "6"
  "Key to press to switch to a deck of eights."
  :type '(string) :group 'blackjack)

(defcustom blackjack-insurance-key "y"
  "Key to press to insure hand."
  :type '(string) :group 'blackjack)

(defcustom blackjack-face-type-regular-key "r"
  "Key to press to to choose regular face type."
  :type '(string) :group 'blackjack)

(defcustom blackjack-face-type-alternate-key "a"
  "Key to press to choose alternate face type."
  :type '(string) :group 'blackjack)

(defcustom blackjack-persist-file
  (file-name-concat user-emacs-directory "blackjack.txt")
  "File to persist blackjack game state to."
  :type '(file) :group 'blackjack)

(defcustom blackjack-currency "$"
  "Currency to display.

Can be a single-character currency symbol such as \"$\", \"€\" or \"£\", or a
3-character currency code as per ISO 4217."
  :type '(string)
  :group 'blackjack)

(defclass blackjack-card ()
  ((id :initarg :id :initform 0 :type integer)
   (value :initarg :value :initform 0 :type integer)
   (suit :initarg :suit :initform 0 :type integer)))

(defclass blackjack-hand ()
  ((cards :initarg :cards :initform '() :type list)
   (played :initarg :played :initform nil :type boolean)))

(defclass blackjack-player-hand (blackjack-hand)
  ((id :initarg :id :initform 0 :type integer)
   (bet :initarg :bet :initform 0 :type integer :accessor bet-on)
   (status :initarg :status :initform 'unknown :type symbol)
   (paid :initarg :paid :initform nil :type boolean)
   (stood :intiarg :stood :initform nil :type boolean)))

;; Unlike the 'game' object's 'current-bet' slot, the 'player-hand' object's
;; 'bet' slot is only ever initialized or mutated by our software, never
;; directly by user interaction.  We can thus rely on integer cents values as the
;; only kind of string passed in, since all such calls are internal.
(cl-defmethod (setf bet-on) ((bet string) (hand blackjack-player-hand))
  (setf (bet-on hand) (string-to-number bet)))

(cl-defmethod (setf bet-on) ((bet float) (hand blackjack-player-hand))
  (setf (bet-on hand) (truncate bet)))

(defclass blackjack-dealer-hand (blackjack-hand)
  ((hide-down-card :initarg :hide-down-card :initform t :type boolean)))

(defclass blackjack-game ()
  ((id :initarg :id :initform 0 :type integer)
   (shoe :initarg :shoe :initform '() :type list)
   (dealer-hand :initarg :dealer-hand :initform nil :type atom)
   (player-hands :initarg :player-hands :initform '() :type list)
   (num-decks :initarg :num-decks :initform 1 :type integer
              :accessor decks-count-of)
   (deck-type :initarg :deck-type :initform 'regular :type symbol
              :accessor deck-type-of)
   (face-type :initarg :face-type :initform 'regular :type symbol
              :accessor face-type-of)
   (money :initarg :money :initform 10000 :type integer :accessor user-money-for)
   ;; 2023-05-09 PH Could/should slot 'current-bet' instead be a
   ;; synthetic/calculated/derived property, via a defmethod which accesses the
   ;; 'bet' slot of the 'current-player-hand'-th element of slot 'player-hands'?
   ;; I need to get clarity on when/whether it is useful/necessary to have two
   ;; separate representations of the same thing.  For instance, is there ever a
   ;; time when it is useful/necessary for them to hold different values, even
   ;; temporarily?
   (current-bet :initarg :current-bet :initform 500 :type integer
                :accessor active-bet-of)
   (current-player-hand :initarg :current-player-hand :initform 0 :type integer)
   (current-menu :initarg :current-menu :initform 'game :type symbol)
   (faces-regular :initarg :faces :initform '[["A♠" "A♥" "A♣" "A♦"]
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
   (faces-alternate :initarg :faces2 :initform '[["🂡" "🂱" "🃁" "🃑"]
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
   (max-bet :initarg :max-bet :initform 100000000 :type integer)
   (max-player-hands :initarg :max-player-hands :initform 7 :type integer)
   (quitting :initarg :quitting :initform nil :type boolean)))

;; String literal bet values entered via user interaction will typically be
;; dollar values (integer or float), not the integer cents value we record.
(cl-defmethod (setf active-bet-of) ((bet string) (game blackjack-game))
  ;; Leave it to the method dispatcher to distinguish between integer and float
  ;; values.
  (setf (active-bet-of game) (* 100 (string-to-number bet))))

(cl-defmethod (setf active-bet-of) ((bet float) (game blackjack-game))
  (setf (active-bet-of game) (truncate bet)))

(cl-defmethod (setf decks-count-of) ((count string) (game blackjack-game))
  (setf (decks-count-of game) (string-to-number count)))

(cl-defmethod (setf deck-type-of) ((deck-type string) (game blackjack-game))
  (setf (deck-type-of game) (intern deck-type)))

(cl-defmethod (setf face-type-of) ((face-type string) (game blackjack-game))
  (setf (face-type-of game) (intern face-type)))

;; The user's stash of cash is only ever initialized or mutated by our software,
;; never directly by user interaction.  We can thus rely on integer cents values
;; as the only kind of string passed in, since all such calls are internal.
(cl-defmethod (setf user-money-for) ((money string) (game blackjack-game))
  (setf (user-money-for game) (string-to-number money)))

(cl-defmethod (setf user-money-for) ((money float) (game blackjack-game))
  (setf (user-money-for game) (truncate money)))

;; (cl-defmethod cl-print-object ((obj blackjack-card) stream)
;;   "Print OBJ to STREAM."
;;   (princ
;;    (format "#<%s id: %d value: %s suit: %s>"
;;            (eieio-class-name (eieio-object-class obj))
;;            (slot-value obj 'id)
;;            (slot-value obj 'value)
;;            (slot-value obj 'suit))
;;    stream))

;; (cl-defmethod cl-print-object ((obj blackjack-player-hand) stream)
;;   "Print OBJ to STREAM."
;;   (princ
;;    (format "#<%s id: %d cards: %s played: %s status: %s paid: %s stood: %s bet: %s>"
;;            (eieio-class-name (eieio-object-class obj))
;;            (slot-value obj 'id)
;;            (slot-value obj 'cards)
;;            (slot-value obj 'played)
;;            (slot-value obj 'status)
;;            (slot-value obj 'paid)
;;            (slot-value obj 'stood)
;;            (slot-value obj 'bet))
;;    stream))

;; (cl-defmethod cl-print-object ((obj blackjack-dealer-hand) stream)
;;   "Print OBJ to STREAM."
;;   (princ
;;    (format "#<%s cards: %s played: %s hide-down-card: %s>"
;;            (eieio-class-name (eieio-object-class obj))
;;            (slot-value obj 'cards)
;;            (slot-value obj 'played)
;;            (slot-value obj 'hide-down-card))
;;    stream))

(defun blackjack--deal-new-hand (game)
  "Deal new GAME hands."
  (interactive)
  (when (blackjack--valid-menu-action-p game 'game)
    (with-slots
        (current-bet player-hands current-player-hand current-menu dealer-hand)
        game
      (when (blackjack--need-to-shuffle-p game)
        (blackjack--shuffle game))
      (setf player-hands nil)
      (setf dealer-hand (blackjack-dealer-hand))
      (let ((player-hand
             (blackjack-player-hand :id (blackjack--next-id game)
                                    :bet current-bet)))
        (--dotimes 2
          (blackjack--deal-card game player-hand)
          (blackjack--deal-card game dealer-hand))
        (push player-hand player-hands)
        (setf current-player-hand 0)
        (if (and
             (blackjack--dealer-upcard-is-ace-p dealer-hand)
             (not (blackjack--hand-is-blackjack-p (slot-value player-hand 'cards))))
            (progn
              (setf current-menu 'insurance)
              (blackjack--draw-hands game)
              (blackjack--ask-insurance-action game))
          (if (blackjack--player-hand-done-p game player-hand)
              (progn
                (setf (slot-value dealer-hand 'hide-down-card) nil)
                (blackjack--pay-hands game)
                (setf current-menu 'game)
                (blackjack--draw-hands game)
                (blackjack--ask-game-action game))
            (setf current-menu 'hand)
            (blackjack--draw-hands game)
            (blackjack--save game)
            (blackjack--ask-hand-action game)))))))

(defun blackjack--deal-card (game hand)
  "Deal a card into HAND from GAME shoe."
  (interactive)
  (with-slots (shoe) game
    (with-slots (cards) hand
      (cl-callf append cards `(,(pop shoe))))))

(defun blackjack--next-id (game)
  "Return next GAME object id."
  (cl-incf (slot-value game 'id)))

(defun blackjack--pay-hands (game)
  "Pay player GAME hands."
  (with-slots (dealer-hand player-hands) game
    (let ((dealer-hand-value (blackjack--dealer-hand-value dealer-hand 'soft))
          (dealer-busted (blackjack--dealer-hand-is-busted-p dealer-hand)))
      (dolist (player-hand player-hands)
        (blackjack--pay-player-hand
         game player-hand dealer-hand-value dealer-busted))
      (blackjack--normalize-current-bet game)
      (blackjack--save game))))

(defun blackjack--pay-player-hand (game player-hand dealer-hand-value
                                        dealer-hand-busted)
  "Pay GAME PLAYER-HAND based on DEALER-HAND-VALUE and DEALER-HAND-BUSTED."
  (with-slots (paid cards status) player-hand
    (unless paid
      (setf paid t)
      (let ((player-hand-value (blackjack--player-hand-value cards 'soft)))
        (if (blackjack--player-hand-won player-hand-value dealer-hand-value dealer-hand-busted)
            (blackjack--pay-won-hand game player-hand)
          (if (blackjack--player-hand-lost-p player-hand-value dealer-hand-value)
              (blackjack--collect-lost-hand game player-hand)
            (setf status 'push)))))))

(defun blackjack--collect-lost-hand (game player-hand)
  "Collect bet from losing GAME PLAYER-HAND."
  (with-slots (bet status) player-hand
    (cl-decf (user-money-for game) bet)
    (setf status 'lost)))

(defun blackjack--pay-won-hand (game player-hand)
  "Pay winning GAME PLAYER-HAND bet."
  (with-slots (cards status) player-hand
    (when (blackjack--hand-is-blackjack-p cards)
      (cl-callf * (bet-on player-hand) 1.5))
    (cl-incf (user-money-for game) (bet-on player-hand))
    (setf status 'won)))

(defun blackjack--player-hand-lost-p (player-hand-value dealer-hand-value)
  "Return non-nil if PLAYER-HAND-VALUE < DEALER-HAND-VALUE."
  (< player-hand-value dealer-hand-value))

(defun blackjack--player-hand-won (player-hand-value dealer-hand-value dealer-hand-busted)
  "Return non-nil if PLAYER-HAND-VALUE > DEALER-HAND-VALUE && !DEALER-HAND-BUSTED."
  (or dealer-hand-busted
      (> player-hand-value dealer-hand-value)))

(defun blackjack--player-hand-done-p (game player-hand)
  "Return non-nil when GAME PLAYER-HAND is done."
  (when (blackjack--no-more-actions-p player-hand)
    (with-slots (played paid cards) player-hand
      (setf played t)
      (when (and (not paid)
                 (blackjack--player-hand-is-busted-p cards))
        (blackjack--collect-busted-hand game player-hand)))
    t))

(defun blackjack--collect-busted-hand (game player-hand)
  "Collect bet from GAME PLAYER-HAND."
  (with-slots (paid status bet) player-hand
    (setf paid t
          status 'lost)
    (cl-decf (user-money-for game) bet)))

(defun blackjack--no-more-actions-p (player-hand)
  "Return non-nil when PLAYER-HAND has no more actions."
  (with-slots (cards played stood) player-hand
    (or
     played
     stood
     (blackjack--hand-is-blackjack-p cards)
     (blackjack--player-hand-is-busted-p cards)
     (= 21 (blackjack--player-hand-value cards 'soft))
     (= 21 (blackjack--player-hand-value cards 'hard)))))

(defun blackjack--need-to-shuffle-p (game)
  "Return non-nil when GAME shoe is exhausted."
  (with-slots (shoe shuffle-specs num-decks) game
    (let ((cards-count (length shoe)))
      (if (> cards-count 0)
          (let ((used (- (blackjack--total-cards game) cards-count))
                (spec (aref shuffle-specs (1- num-decks))))
            (> (* 100 (/ (float used) cards-count)) spec))
        t))))

(defun blackjack--total-cards (game)
  "Return total number of cards per GAME shoe."
  (with-slots (cards-per-deck num-decks) game
    (* cards-per-deck num-decks)))

(defun blackjack--shuffle (game &optional card-values skip-shuffle)
  "Build a GAME shoe using CARD-VALUES if provided and shuffle unless SKIP-SHUFFLE."
  (blackjack--build-cards game
                          (or card-values (blackjack--card-values game)))
  (unless skip-shuffle
    (cl-callf blackjack--shuffle-loop (slot-value game 'shoe))))

(defun blackjack--card-values (game)
  "Return card values from GAME deck-type."
  (pcase (deck-type-of game)
    ('regular (number-sequence 0 12))
    ('aces '(0))
    ('jacks '(10))
    ('aces-jacks '(0 10))
    ('sevens '(6))
    ('eights '(7))))

(defun blackjack--build-cards (game values)
  "Populate GAME shoe with card VALUES."
  (with-slots (shoe) game
    (setf shoe nil)
    (let ((total-cards (blackjack--total-cards game))
          (reversed-values (reverse values)))
      (while (length< shoe total-cards)
        (dotimes (suit 4)
          (dolist (value reversed-values)
            (when (length< shoe total-cards)
              (push (blackjack-card :id (blackjack--next-id game) :value value
                                    :suit suit)
                    shoe))))))))

(defun blackjack--shuffle-loop (shoe)
  "Shuffle SHOE."
  (--dotimes (* 7 (length shoe))
    (setq shoe (blackjack--move-rand-card shoe)))
  shoe)

(defun blackjack--move-rand-card (shoe)
  "Move a random card to the top of the SHOE."
  (let ((card (seq-random-elt shoe)))
    (setq shoe (cl-remove card shoe :count 1))
    (push card shoe)
    shoe))

(defun blackjack--draw-hands (game)
  "Draw GAME dealer hand and player hands."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (blackjack--update-header game)
    (insert "\n  Dealer:\n")
    (blackjack--draw-dealer-hand game)
    (insert "\n\n  Player:\n")
    (blackjack--draw-player-hands game)))

(defun blackjack--format-money (money)
  "Format MONEY."
  (format "%s%.2f" blackjack-currency (/ money 100.0)))

(defun blackjack--more-hands-to-play-p (game)
  "Return non-nil when there are more split GAME hands to play."
  (with-slots (current-player-hand player-hands) game
    (length> player-hands (1+ current-player-hand))))

(defun blackjack--play-more-hands (game)
  "Advance to next split GAME player hand."
  (with-slots (current-player-hand current-menu) game
    (cl-incf current-player-hand)
    (let ((player-hand (blackjack--current-player-hand game)))
      (blackjack--deal-card game player-hand)
      (if (blackjack--player-hand-done-p game player-hand)
          (blackjack--process game)
        (setf current-menu 'hand)
        (blackjack--draw-hands game)
        (blackjack--ask-hand-action game)))))

(defun blackjack--need-to-play-dealer-hand-p (game)
  "Return non-nil when playing the GAME dealer hand is required."
  (--some
   (with-slots (cards) it
     (not (or (blackjack--player-hand-is-busted-p cards)
              (blackjack--hand-is-blackjack-p cards))))
   (slot-value game 'player-hands)))

(defun blackjack--dealer-hand-counts (dealer-hand)
  "Return soft and hard counts for DEALER-HAND."
  (mapcar
   (-partial #'blackjack--dealer-hand-value dealer-hand) '(soft hard)))

(defun blackjack--deal-required-cards (game)
  "Deal GAME dealer-hand required cards."
  (with-slots (dealer-hand) game
    (while
        (-let (((soft-count hard-count)
                (blackjack--dealer-hand-counts dealer-hand)))
          (and (< soft-count 18) (< hard-count 17)))
      (blackjack--deal-card game dealer-hand))))

(defun blackjack--play-dealer-hand (game)
  "Player GAME dealer hand."
  (let ((playing (blackjack--need-to-play-dealer-hand-p game)))
    (with-slots (dealer-hand current-menu) game
      (with-slots (cards hide-down-card played) dealer-hand
        (when (or playing
                  (blackjack--hand-is-blackjack-p cards))
          (setf hide-down-card nil))
        (when playing
          (blackjack--deal-required-cards game))
        (setf played t)
        (blackjack--pay-hands game)
        (setf current-menu 'game)
        (blackjack--draw-hands game)
        (blackjack--ask-game-action game)))))

(defun blackjack--process (game)
  "Handle more split GAME hands to play."
  (if (blackjack--more-hands-to-play-p game)
      (blackjack--play-more-hands game)
    (blackjack--play-dealer-hand game)))

(defun blackjack--hit (game)
  "Deal a new card to the current GAME player hand."
  (let ((player-hand (blackjack--current-player-hand game)))
    (blackjack--deal-card game player-hand)
    (if (blackjack--player-hand-done-p game player-hand)
        (blackjack--process game)
      (setf (slot-value game 'current-menu) 'hand)
      (blackjack--draw-hands game)
      (blackjack--ask-hand-action game))))

(defun blackjack--double (game)
  "Double the current GAME player hand bet and deal a single card."
  (let ((player-hand (blackjack--current-player-hand game)))
    (blackjack--deal-card game player-hand)
    (setf (slot-value player-hand 'played) t)
    (cl-callf * (bet-on player-hand) 2)
    (if (blackjack--player-hand-done-p game player-hand)
        (blackjack--process game)
      (blackjack--ask-hand-action game))))

(defun blackjack--stand (game)
  "End the current GAME player hand."
  (with-slots (stood played) (blackjack--current-player-hand game)
    (setf stood t
          played t))
  (blackjack--process game))

(defun blackjack--split (game)
  "Split the current GAME player hand."
  (with-slots (player-hands current-bet current-player-hand current-menu) game
    (let ((player-hand)
          (card)
          (hand
           (blackjack-player-hand :id (blackjack--next-id game) :bet current-bet))
          (x (length player-hands)))
      (cl-callf append player-hands `(,hand))
      (while (> x current-player-hand)
        (setq player-hand (nth (1- x) player-hands))
        (setq hand (nth x player-hands))
        (setf (slot-value hand 'cards) (slot-value player-hand 'cards))
        (cl-decf x))
      (setq player-hand (nth current-player-hand player-hands))
      (setq hand (nth (1+ current-player-hand) player-hands))
      (with-slots ((hand-cards cards)) hand
        (with-slots ((player-hand-cards cards)) player-hand
          (setq card (nth 1 player-hand-cards))
          (setf hand-cards `(,card))
          (cl-callf2 cl-remove card player-hand-cards :count 1)
          (blackjack--deal-card game player-hand)
          (if (blackjack--player-hand-done-p game player-hand)
              (blackjack--process game)
            (setf current-menu 'hand)
            (blackjack--draw-hands game)
            (blackjack--ask-hand-action game)))))))

(defun blackjack--valid-menu-action-p (game action)
  "Return non-nil if the GAME menu ACTION can be performed."
  (eq (slot-value game 'current-menu) action))

(cl-defmacro blackjack--hand-can-pred ((action) form)
  "Build a test for whether ACTION is available using FORM, a boolean expression.

The generated predicate takes a single parameter GAME, which is available within
the scope of FORM.

FORM executes in a lexical environment with local variables bound to slots of
GAME and the current player hand (using the `with-slots' macro)."
  (declare (indent defun))
  `(defun ,(intern (format "blackjack--hand-can-%s-p" action)) (game)
     ,(substring-no-properties
       (format "Return non-nil if the GAME current hand can %s." action))
     (when (blackjack--valid-menu-action-p game 'hand)
       (with-slots (money player-hands) game
         (with-slots (bet cards played stood) (blackjack--current-player-hand game)
           ,form)))))

(blackjack--hand-can-pred (hit) ; => blackjack--hand-can-hit-p
  (not
   (or
    played
    stood
    (= 21 (blackjack--player-hand-value cards 'hard))
    (blackjack--hand-is-blackjack-p cards)
    (blackjack--player-hand-is-busted-p cards))))

(blackjack--hand-can-pred (stand) ; => blackjack--hand-can-stand-p
  (not
   (or
    stood
    (blackjack--player-hand-is-busted-p cards)
    (blackjack--hand-is-blackjack-p cards))))

(blackjack--hand-can-pred (split) ; => blackjack--hand-can-split-p
  (and
   (not stood)
   (length< player-hands 7)
   (>= money (+ (blackjack--all-bets game) bet))
   (length= cards 2)
   (-let (((card-0 card-1) cards))
     (= (slot-value card-0 'value) (slot-value card-1 'value)))))

(blackjack--hand-can-pred (double) ; => blackjack--hand-can-double-p
  (and
   (>= money (+ (blackjack--all-bets game) bet))
   (not
    (or
     stood
     (not (length= cards 2))
     (blackjack--hand-is-blackjack-p cards)))))

(defun blackjack--current-player-hand (game)
  "Return current GAME player hand."
  (with-slots (current-player-hand player-hands) game
    (nth current-player-hand player-hands)))

(defun blackjack--all-bets (game)
  "Return the sum of all GAME player hand bets."
  (cl-reduce #'+ (slot-value game 'player-hands) :key #'bet-on))

(defun blackjack--insure-hand (game)
  "Insure the current GAME player hand."
  (interactive)
  (when (blackjack--valid-menu-action-p game 'insurance)
    (let ((player-hand (blackjack--current-player-hand game)))
      (with-slots (current-menu) game
        (with-slots (played paid status) player-hand
          (cl-callf / (bet-on player-hand) 2)
          (setf played t
                paid t
                status 'lost)
          (cl-decf (user-money-for game) (bet-on player-hand)))
        (setf current-menu 'game)
        (blackjack--draw-hands game)
        (blackjack--ask-game-action game)))))

(defun blackjack--no-insurance (game)
  "Decline GAME player hand insurance."
  (interactive)
  (with-slots (dealer-hand current-menu) game
    (with-slots ((dealer-hand-cards cards) hide-down-card) dealer-hand
      (when (blackjack--valid-menu-action-p game 'insurance)
        (if (blackjack--hand-is-blackjack-p dealer-hand-cards)
            (progn
              (setf hide-down-card nil)
              (blackjack--pay-hands game)
              (setf current-menu 'game)
              (blackjack--draw-hands game)
              (blackjack--ask-game-action game))
          (let ((player-hand (blackjack--current-player-hand game)))
            (if (blackjack--player-hand-done-p game player-hand)
                (blackjack--play-dealer-hand game)
              (setf current-menu 'hand)
              (blackjack--draw-hands game)
              (blackjack--ask-hand-action game))))))))

(defun blackjack--ask-new-bet (game)
  "Update the current GAME player bet."
  (interactive)
  (when (blackjack--valid-menu-action-p game 'game)
    (setf (active-bet-of game) (blackjack--new-bet-menu))
    (blackjack--normalize-current-bet game)
    (blackjack--save game)
    (setf (slot-value game 'current-menu) 'game)
    (blackjack--deal-new-hand game)))

(defun blackjack--new-bet-menu ()
  "Get new bet."
  (read-string "Bet amount: "))

(defun blackjack--player-hand-is-busted-p (cards)
  "Return non-nil if CARDS value is more than 21."
  (> (blackjack--player-hand-value cards 'soft) 21))

(defun blackjack--dealer-hand-is-busted-p (dealer-hand)
  "Return non-nil if DEALER-HAND cards value is more than 21."
  (> (blackjack--dealer-hand-value dealer-hand 'soft) 21))

(defun blackjack--hand-is-blackjack-p (cards)
  "Return non-nil if hand CARDS is blackjack."
  (and (length= cards 2)
       (seq-some #'blackjack--is-ace-p cards)
       (seq-some #'blackjack--is-ten-p cards)))

(defun blackjack--dealer-upcard-is-ace-p (dealer-hand)
  "Return non-nil if DEALER-HAND up-card is an ace."
  (blackjack--is-ace-p (nth 1 (slot-value dealer-hand 'cards))))

(defconst blackjack--down-card '(13 0)
  "Hidden card location in faces array.")

(defun blackjack--dealer-hand-to-str (game)
  "Return GAME dealer-hand as a string."
  (let ((str "  "))
    (with-slots (dealer-hand) game
      (with-slots (hide-down-card cards) dealer-hand
        (seq-map-indexed
         (lambda (card index)
           "Represent the CARD at INDEX in the hand of the dealer."
           (cl-callf concat str
             (apply #'blackjack--card-face game
                    (if (and (= index 0) hide-down-card)
                        blackjack--down-card
                      (with-slots (value suit) card
                        `(,value ,suit))))
             " "))
         cards))
      (concat
       str
       " ⇒  "
       (number-to-string (blackjack--dealer-hand-value dealer-hand 'soft))))))

(defun blackjack--draw-dealer-hand (game)
  "Draw the GAME dealer-hand."
  (insert (blackjack--dealer-hand-to-str game)))

(defun blackjack--dealer-hand-value (dealer-hand count-method)
  "Calculates DEALER-HAND cards total value based on COUNT-METHOD."
  (let ((total 0))
    (with-slots (hide-down-card cards) dealer-hand
      (seq-map-indexed
       (lambda (card index)
         "Value the CARD at INDEX in the dealer's hand."
         (unless (and (= index 0) hide-down-card)
           (cl-incf total (blackjack--card-val card count-method total))))
       cards))
    (if (and (eq count-method 'soft) (> total 21))
        (blackjack--dealer-hand-value dealer-hand 'hard)
      total)))

(defun blackjack--draw-player-hands (game)
  "Draws GAME player hands."
  (seq-map-indexed (-partial #'blackjack--draw-player-hand game)
                   (slot-value game 'player-hands)))

(defun blackjack--draw-player-hand (game player-hand index)
  "Draws a single GAME PLAYER-HAND using an INDEX."
  (insert (blackjack--player-hand-to-str game player-hand index)))

(defun blackjack--player-hand-to-str (game player-hand index)
  "Draw the GAME PLAYER-HAND using an INDEX."
  (concat (blackjack--player-hand-cards game player-hand)
          (blackjack--player-hand-money game player-hand index)
          (blackjack--player-hand-status player-hand)
          "\n\n"))

(defun blackjack--player-hand-cards (game player-hand)
  "Draw GAME PLAYER-HAND cards."
  (with-slots (cards) player-hand
    (format "  %s  ⇒  %s  "
            (mapconcat
             (lambda (card)
               "Present how CARD is to be drawn."
               (with-slots (value suit) card
                 (blackjack--card-face game value suit)))
             cards
             " ")
            (number-to-string (blackjack--player-hand-value cards 'soft)))))

(defun blackjack--player-hand-status (player-hand)
  "Return PLAYER-HAND status."
  (with-slots (cards status) player-hand
    (pcase status
      ('lost (if (blackjack--player-hand-is-busted-p cards)
                 "Busted!"
               "Lost!"))
      ('won (if (blackjack--hand-is-blackjack-p cards)
                "Blackjack!"
              "Won!"))
      ('push "Push")
      ('unknown ""))))

(defun blackjack--player-hand-money (game player-hand index)
  "Return GAME PLAYER-HAND money using an INDEX."
  (with-slots (current-player-hand) game
    (with-slots (played status bet) player-hand
      (concat
       (when (eq status 'lost) "-")
       (when (eq status 'won) "+")
       (blackjack--format-money bet)
       (when (and
              (not played)
              (= index current-player-hand))
         " ⇐")
       "  "))))

(defun blackjack--player-hand-value (cards count-method)
  "Calculates CARDS total value based on COUNT-METHOD."
  (let ((total 0))
    (dolist (card cards)
      (cl-incf total (blackjack--card-val card count-method total)))
    (if (and (eq count-method 'soft) (> total 21))
        (blackjack--player-hand-value cards 'hard)
      total)))

(defun blackjack--card-val (card count-method total)
  "Calculates CARD value based on COUNT-METHOD and running hand TOTAL."
  (let ((value (1+ (slot-value card 'value))))
    (cl-callf min value 10)
    (if (and (eq count-method 'soft) (= value 1) (< total 11))
        11
      value)))

(defun blackjack--card-face (game value suit)
  "Return GAME card face based on VALUE and SUIT."
  (let ((face
         (with-slots (face-type faces-alternate faces-regular) game
           (if (eq face-type 'alternate)
               faces-alternate
             faces-regular))))
    (aref (aref face value) suit)))

(defun blackjack--is-ace-p (card)
  "Return non-nil if CARD is an ace."
  (= 0 (slot-value card 'value)))

(defun blackjack--is-ten-p (card)
  "Return non-nil if CARD has a value of 10."
  (> (slot-value card 'value) 8))

(defmacro blackjack--limit-to-range (lowest proposed highest)
  "Constrain PROPOSED value within LOWEST and HIGHEST limits."
  `(progn
     (cl-callf max ,proposed ,lowest)
     (cl-callf min ,proposed ,highest)))

(defun blackjack--normalize-current-bet (game)
  "Normalize current GAME player bet."
  (with-slots (min-bet max-bet money) game
    (blackjack--limit-to-range min-bet (active-bet-of game) max-bet)
    (cl-callf min (active-bet-of game) money)))

(defun blackjack--persist-file-name ()
  "Resolve the persist file including all abbreviations and symlinks."
  (file-truename (expand-file-name blackjack-persist-file)))

(defun blackjack--load-saved-game (game)
  "Load persisted GAME state."
  (when-let ((content
              (ignore-errors
                (with-temp-buffer
                  (insert-file-contents-literally (blackjack--persist-file-name))
                  (buffer-string))))
             (parts (split-string content "|"))
             ((length= parts 5)))
    (setf (decks-count-of game) (pop parts))
    (setf (deck-type-of game) (pop parts))
    (setf (face-type-of game) (pop parts))
    (setf (user-money-for game) (pop parts))
    (setf (active-bet-of game) (pop parts))))

(defun blackjack--save (game)
  "Persist GAME state."
  (ignore-errors
    (with-temp-file (blackjack--persist-file-name)
      (insert
       (with-slots (num-decks deck-type face-type money current-bet) game
         (format "%s|%s|%s|%s|%s"
                 ;; Here we render the current bet as a dollar value (float)
                 ;; rather than the cents integer value we use internally, so
                 ;; that it can be read back in subsequently in the same way as
                 ;; it would be for an interactive user menu selection made
                 ;; during play.
                 num-decks deck-type face-type money (/ current-bet 100.0)))))))

(defun blackjack--quit (game)
  "Quit Blackjack GAME."
  (interactive)
  (when (blackjack--valid-menu-action-p game 'game)
    (setf (slot-value game 'quitting) t)
    (quit-window)))

(defun blackjack--header (game)
  "Return GAME header."
  (with-slots (money current-menu) game
    (format "  Blackjack %s  %s"
            (blackjack--format-money money)
            (pcase current-menu
              ('game (blackjack--game-header-menu))
              ('hand (blackjack--hand-menu game))
              ('options (blackjack--options-header-menu))
              ('deck-type (blackjack--deck-type-header-menu))
              ('face-type (blackjack--face-type-header-menu))
              ('num-decks (blackjack--num-decks-header-menu))
              ('bet (blackjack--bet-menu))
              ('insurance (blackjack--insurance-header-menu))))))

(defun blackjack--update-header (game)
  "Update GAME header."
  (setq header-line-format (blackjack--header game)))

(defun blackjack--game-header-menu ()
  "Return game menu string."
  (format "(%s) deal new hand  (%s) change bet  (%s) options  (%s) quit"
          blackjack-deal-double-key
          blackjack-bet-back-key
          blackjack-options-key
          blackjack-quit-key))

(defun blackjack--ask-game-action (game)
  "Ask about next GAME action."
  (setf (slot-value game 'current-menu) 'game)
  (blackjack--update-header game)
  (pcase (blackjack-game-actions-menu)
    ("deal" nil)
    ("bet" (blackjack--ask-new-bet game))
    ("options" (blackjack--ask-game-options game))
    ("quit" (blackjack--quit game))))

(defun blackjack--read-short-answer (prompt answer-list)
  "PROMPT for one of ANSWER-LIST accepting single-character answers."
  (let ((read-answer-short t))
    (read-answer prompt answer-list)))

(defun blackjack-game-actions-menu ()
  "Bet actions menu for GAME."
  (blackjack--read-short-answer
   "Game Actions: "
   `(("deal" ,blackjack-deal-double-key "deal new hand")
     ("bet" ,blackjack-bet-back-key "change current bet")
     ("options" ,blackjack-options-key "change game options")
     ("quit" ,blackjack-quit-key "quit blackjack")
     ("help" ?? "show help"))))

(defun blackjack--options-header-menu ()
  "Return option menu string."
  (format "(%s) number of decks  (%s) deck type  (%s) face type  (%s) go back"
          blackjack-num-decks-no-insurance-key
          blackjack-deck-type-key
          blackjack-face-type-key
          blackjack-bet-back-key))

(defun blackjack--ask-game-options (game)
  "Ask about which GAME option to update."
  (setf (slot-value game 'current-menu) 'options)
  (blackjack--update-header game)
  (pcase (blackjack--game-options-menu)
    ("number-decks" (blackjack--ask-new-number-decks game))
    ("deck-type" (blackjack--ask-new-deck-type game))
    ("face-type" (blackjack--ask-new-face-type game))
    ("back" (blackjack--ask-game-action game))))

(defun blackjack--game-options-menu ()
  "GAME options menu."
  (blackjack--read-short-answer
   "Options: "
   `(("number-decks" ,blackjack-num-decks-no-insurance-key "change number of decks")
     ("deck-type" ,blackjack-deck-type-key "change the deck type")
     ("face-type" ,blackjack-face-type-key "change the card face type")
     ("back" ,blackjack-bet-back-key "go back to previous menu")
     ("help" ?? "show help"))))

(defun blackjack--deck-type-header-menu ()
  "Return deck type menu string."
  (format
   "(%s) regular  (%s) aces  (%s) jacks  (%s) aces & jacks  (%s) sevens  (%s) eights"
   blackjack-deck-regular-key
   blackjack-deck-aces-key
   blackjack-deck-jacks-key
   blackjack-deck-aces-jacks-key
   blackjack-deck-sevens-key
   blackjack-deck-eights-key))

(defun blackjack--ask-new-deck-type (game)
  "Ask for new GAME deck type."
  (setf (slot-value game 'current-menu) 'deck-type)
  (blackjack--update-header game)
  (setf (deck-type-of game) (blackjack-deck-type-menu))
  (blackjack--normalize-num-decks game)
  (blackjack--shuffle-save-deal-new-hand game))

(defun blackjack--normalize-num-decks (game)
  "Normalize GAME num-decks."
  (when (eq (slot-value game 'deck-type) 'aces)
    (cl-callf min (decks-count-of game) 2))
  (blackjack--limit-to-range 1 (decks-count-of game) 8))

(defun blackjack-deck-type-menu ()
  "New GAME deck type menu."
  (blackjack--read-short-answer
   "Deck Type: "
   `(("regular" ,blackjack-deck-regular-key "regular deck")
     ("aces" ,blackjack-deck-aces-key "deck of aces")
     ("jacks" ,blackjack-deck-jacks-key "deck of jacks")
     ("aces-jacks" ,blackjack-deck-aces-jacks-key "deck of aces and jacks")
     ("sevens" ,blackjack-deck-sevens-key "deck of sevens")
     ("eights" ,blackjack-deck-eights-key "deck of eights")
     ("help" ?? "show help"))))

(defun blackjack--face-type-header-menu ()
  "Return face type menu string."
  (format "(%s) A♠ regular  (%s) 🂡 alternate"
          blackjack-face-type-regular-key
          blackjack-face-type-alternate-key))

(defun blackjack--ask-new-face-type (game)
  "Ask for new GAME face type."
  (setf (slot-value game 'current-menu) 'face-type)
  (blackjack--update-header game)
  (setf (face-type-of game) (blackjack--face-type-menu))
  (blackjack--save-deal-new-hand game))

(defun blackjack--face-type-menu ()
  "New GAME face type menu."
  (blackjack--read-short-answer
   "Card Face Type: "
   `(("regular" ,blackjack-face-type-regular-key "use regular face type")
     ("alternate" ,blackjack-face-type-alternate-key "use alternate face type")
     ("help" ?? "show help"))))

(defun blackjack--insurance-header-menu ()
  "Return insurance menu string."
  (format "(%s) insure hand  (%s) refuse insurance"
          blackjack-insurance-key
          blackjack-num-decks-no-insurance-key))

(defun blackjack--ask-insurance-action (game)
  "Ask about insuring GAME hand."
  (pcase (blackjack--ask-insurance-menu)
    ("yes" (blackjack--insure-hand game))
    ("no" (blackjack--no-insurance game))
    ("help" ?? "show help")))

(defun blackjack--ask-insurance-menu ()
  "Ask about insuring GAME hand."
  (blackjack--read-short-answer
   "Hand Insurance: "
   `(("yes" ,blackjack-insurance-key "insure hand")
     ("no" ,blackjack-num-decks-no-insurance-key "refuse insurance")
     ("help" ?? "show help"))))

(defun blackjack--num-decks-header-menu ()
  "Return number of decks menu string."
  "Number of decks (1-8)")

(defun blackjack--bet-menu ()
  "Return bet menu string."
  "New bet")

(defun blackjack--new-number-decks-prompt ()
  "Get new number of decks value."
  (read-string "Enter number of decks: "))

(defun blackjack--hand-menu (game)
  "Return GAME hand menu string."
  (string-trim-right
   (cl-labels ((blackjack--menu-item-maybe ((pred key label))
                (if (funcall pred game) (format "(%s) %s  " key label) "")))
     (mapconcat
      #'blackjack--menu-item-maybe
      `((blackjack--hand-can-hit-p    ,blackjack-hit-key         "hit")
        (blackjack--hand-can-stand-p  ,blackjack-stand-key       "stand")
        (blackjack--hand-can-split-p  ,blackjack-split-key       "split")
        (blackjack--hand-can-double-p ,blackjack-deal-double-key "double"))
      ""))))

(defun blackjack--ask-hand-action (game)
  "Ask hand action for GAME."
  (pcase (blackjack--hand-actions-menu game)
    ("stand" (if (blackjack--hand-can-stand-p game)
                 (blackjack--stand game)
               (blackjack--ask-hand-action game)))
    ("hit" (if (blackjack--hand-can-hit-p game)
               (blackjack--hit game)
             (blackjack--ask-hand-action game)))
    ("split" (if (blackjack--hand-can-split-p game)
                 (blackjack--split game)
               (blackjack--ask-hand-action game)))
    ("double" (if (blackjack--hand-can-double-p game)
                  (blackjack--double game)
                (blackjack--ask-hand-action game)))))

(defun blackjack--hand-actions-menu (game)
  "Hand actions menu for GAME."
  (blackjack--read-short-answer "Hand Action "
   (remq nil
    (cl-labels ((blackjack--action-maybe ((pred &rest action))
                 (when (funcall pred game) action)))
      (mapcar
       #'blackjack--action-maybe
       `((blackjack--hand-can-hit-p "hit" ,blackjack-hit-key "deal a new card")
         (blackjack--hand-can-stand-p "stand" ,blackjack-stand-key "end current hand with no further actions")
         (blackjack--hand-can-split-p "split" ,blackjack-split-key "split hand into two hands")
         (blackjack--hand-can-double-p "double" ,blackjack-deal-double-key "double bet, deal a new card, and end hand")
         (always "help" ?? "show help")))))))

(defun blackjack--show-options-menu (game)
  "Switch to GAME options menu."
  (interactive)
  (when (blackjack--valid-menu-action-p game 'game)
    (setf (slot-value game 'current-menu) 'options)
    (blackjack--update-header game)
    (blackjack--ask-game-options game)))

(defun blackjack--show-num-decks-menu (game)
  "Switch to GAME number of decks menu."
  (setf (slot-value game 'current-menu) 'num-decks)
  (blackjack--update-header game)
  (blackjack--ask-new-number-decks game))

(defun blackjack--ask-new-number-decks (game)
  "Get new number of GAME decks."
  (with-slots (current-menu) game
    (setf current-menu 'num-decks)
    (blackjack--update-header game)
    (setf (decks-count-of game) (blackjack--new-number-decks-prompt))
    (blackjack--normalize-num-decks game)
    (blackjack--save game)
    (blackjack--shuffle game)
    (setf current-menu 'game)
    (blackjack--deal-new-hand game)))

(defun blackjack--show-game-menu (game)
  "Switch to GAME menu."
  (interactive)
  (setf (slot-value game 'current-menu) 'game)
  (blackjack--update-header game))

(dolist (menu '(deck-type face-type))
  (let ((func-name (intern (format "blackjack--show-%s-menu" menu))))
    (fset func-name
          (lambda (game)
            (interactive)
            (when (blackjack--valid-menu-action-p game 'options)
              (setf (slot-value game 'current-menu) menu)
              (blackjack--update-header game)
              (funcall (intern (format "blackjack--ask-%s-action" menu))))))))

(defun blackjack--save-deal-new-hand (game)
  "Save GAME and deal new hand."
  (blackjack--save game)
  (setf (slot-value game 'current-menu) 'game)
  (blackjack--deal-new-hand game))

(defun blackjack--shuffle-save-deal-new-hand (game)
  "Shuffle, save, and deal new GAME hand."
  (blackjack--shuffle game)
  (blackjack--save-deal-new-hand game))

(defvar blackjack-minor-mode-map
  (make-sparse-keymap)
  "Blackjack minor mode keymap.")

(define-minor-mode blackjack-minor-mode
  "Blackjack minor mode.

\\{blackjack-minor-mode-map}"
  :group 'blackjack
  :lighter " blackjack")

(define-derived-mode blackjack-mode special-mode "Blackjack"
  "Blackjack game mode."
  :group 'blackjack)

(defmacro until (condition &rest forms)
  "Repeat FORMS until CONDITION evaluates to nil.

\(fn CONDITION FORMS...)"
  (declare (indent 1))
  `(while (not ,condition) ,@forms))

(defun blackjack--init ()
  "Initialize game state."
  (let ((game (blackjack-game)))
    (blackjack--load-saved-game game)
    (blackjack--normalize-num-decks game)
    (until (slot-value game 'quitting)
      (blackjack--deal-new-hand game))))

;;;###autoload
(defun blackjack ()
  "Run Blackjack."
  (interactive)
  (let ((debug-on-error t))
    (get-buffer-create blackjack--buffer-name)
    (switch-to-buffer blackjack--buffer-name)
    (with-current-buffer blackjack--buffer-name
      (add-hook 'blackjack-mode-hook #'blackjack-minor-mode)
      (blackjack-mode)
      (blackjack--init))))

(provide 'blackjack)
;;; blackjack.el ends here
