;;; blackjack --- Tests for blackjack -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'blackjack)
(require 'buttercup)

(describe "blackjack-card"
          :var ((card (blackjack-card)))
          (it "has a default id"
              (expect (slot-value card 'id) :to-be 0))
          (it "has a default value"
              (expect (slot-value card 'value) :to-be 0))
          (it "has a default suit"
              (expect (slot-value card 'suit) :to-be 0)))

(describe "blackjack-player-hand"
          :var ((player-hand (blackjack-player-hand)))
          (it "has a default id"
              (expect (slot-value player-hand 'id) :to-be 0))
          (it "initially has no cards"
              (expect (slot-value player-hand 'cards) :to-be '()))
          (it "has a default bet"
              (expect (slot-value player-hand 'bet) :to-be 0))
          (it "has a default status"
              (expect (slot-value player-hand 'status) :to-be 'unknown))
          (it "is initially not played"
              (expect (slot-value player-hand 'played) :to-be nil))
          (it "is initially not payed"
              (expect (slot-value player-hand 'payed) :to-be nil))
          (it "is initially not stood"
              (expect (slot-value player-hand 'stood) :to-be nil)))

(describe "blackjack-dealer-hand"
          :var ((dealer-hand (blackjack-dealer-hand)))
          (it "initially has no cards"
              (expect (slot-value dealer-hand 'cards) :to-be '()))
          (it "is initially not played"
              (expect (slot-value dealer-hand 'played) :to-be nil))
          (it "initially has a hidden down card"
              (expect (slot-value dealer-hand 'hide-down-card) :to-be t)))

(describe "blackjack--game"
          (describe "before initialization"
                    (it "is nil"
                        (expect blackjack--game :to-be nil)))
          (describe "once initialized"
                    (before-all
                     (setq blackjack--game (blackjack-game)))
                    (after-all
                     (setq blackjack--game nil))
                    (it "has a default id"
                        (expect (slot-value blackjack--game 'id) :to-be 0))
                    (it "initially has an empty shoe"
                        (expect (slot-value blackjack--game 'shoe) :to-be '()))
                    (it "initially has no dealer-hand"
                        (expect (slot-value blackjack--game 'dealer-hand) :to-be nil))
                    (it "initially has no player hands"
                        (expect (slot-value blackjack--game 'player-hands) :to-be '()))
                    (it "has a default num-decks"
                        (expect (slot-value blackjack--game 'num-decks) :to-be 1))
                    (it "has a default deck-type"
                        (expect (slot-value blackjack--game 'deck-type) :to-be 'regular))
                    (it "has a default face-type"
                        (expect (slot-value blackjack--game 'face-type) :to-be 'regular))
                    (it "has default money"
                        (expect (slot-value blackjack--game 'money) :to-be 10000))
                    (it "has default current-bet"
                        (expect (slot-value blackjack--game 'current-bet) :to-be 500))
                    (it "has a default current-player-hand"
                        (expect (slot-value blackjack--game 'current-player-hand) :to-be 0))
                    (it "has default current-menu"
                        (expect (slot-value blackjack--game 'current-menu) :to-be 'game))
                    (it "has regular faces"
                        (expect (slot-value blackjack--game 'faces-regular) :to-equal '[["A♠" "A♥" "A♣" "A♦"]
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
                                                                                        ["??"]]))
                    (it "has alternate faces"
                        (expect (slot-value blackjack--game 'faces-alternate) :to-equal '[["🂡" "🂱" "🃁" "🃑"]
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
                                                                                          ["🂠"]]))
                    (it "has shuffle specs"
                        (expect (slot-value blackjack--game 'shuffle-specs) :to-equal '[80 81 82 84 86 89 92 95]))
                    (it "has a cards-per-deck value"
                        (expect (slot-value blackjack--game 'cards-per-deck) :to-be 52))
                    (it "has a minimum bet value"
                        (expect (slot-value blackjack--game 'min-bet) :to-be 500))
                    (it "has a maximum bet value"
                        (expect (slot-value blackjack--game 'max-bet) :to-be 100000000))
                    (it "has a maximum player hands value"
                        (expect (slot-value blackjack--game 'max-player-hands) :to-be 7))))

(describe "blackjack--deal-new-hand"
          (describe "with a regular deck"
                    (before-all
                     (setq blackjack--game (blackjack-game))
                     (blackjack--deal-new-hand)
                     (setq player-hands (slot-value blackjack--game 'player-hands))
                     (setq dealer-hand (slot-value blackjack--game 'dealer-hand))
                     (setq shoe (slot-value blackjack--game 'shoe)))
                    (after-all
                     (setq blackjack--game nil))
                    (it "deals cards"
                        (expect (slot-value blackjack--game 'deck-type) :to-be 'regular)
                        (expect (length player-hands) :to-be 1)
                        (expect (length (slot-value (nth 0 player-hands) 'cards)) :to-be 2)
                        (expect (length (slot-value dealer-hand 'cards)) :to-be 2)
                        (expect (length shoe) :to-be 48)))

          (describe "with an aces deck"
                    (before-all
                     (setq blackjack--game (blackjack-game :deck-type 'aces))
                     (blackjack--deal-new-hand)
                     (setq player-hands (slot-value blackjack--game 'player-hands))
                     (setq dealer-hand (slot-value blackjack--game 'dealer-hand))
                     (setq shoe (slot-value blackjack--game 'shoe)))
                    (after-all
                     (setq blackjack--game nil))
                    (it "shows insurance menu"
                        (expect (slot-value blackjack--game 'deck-type) :to-be 'aces)
                        (expect (length player-hands) :to-be 1)
                        (expect (length (slot-value (nth 0 player-hands) 'cards)) :to-be 2)
                        (expect (length (slot-value dealer-hand 'cards)) :to-be 2)
                        (expect (length shoe) :to-be 48)
                        (expect (slot-value blackjack--game 'current-menu) :to-be 'insurance)))

          (describe "player is dealt a blackjack"
                    (before-all
                     (setq blackjack--game (blackjack-game :deck-type 'jacks))
                     (blackjack--shuffle)
                     (setq shoe (slot-value blackjack--game 'shoe))
                     (setq card (first shoe))
                     (setf (slot-value card 'value) 0)
                     (blackjack--deal-new-hand)
                     (setq player-hands (slot-value blackjack--game 'player-hands))
                     (setq dealer-hand (slot-value blackjack--game 'dealer-hand)))
                    (after-all
                     (setq blackjack--game nil))
                    (it "shows game menu"
                        (expect (slot-value blackjack--game 'deck-type) :to-be 'jacks)
                        (expect (length player-hands) :to-be 1)
                        (expect (length (slot-value (nth 0 player-hands) 'cards)) :to-be 2)
                        (expect (length (slot-value dealer-hand 'cards)) :to-be 2)
                        ;(expect (length (slot-value blackjack--game 'shoe)) :to-be 48)
                        (expect (slot-value blackjack--game 'current-menu) :to-be 'game))))


(provide 'test-blackjack)

;;; test-blackjack.el ends here

