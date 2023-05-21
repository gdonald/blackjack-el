;;; blackjack --- Tests for blackjack -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(load-file "./tests/test-helper.el")
(require 'blackjack)

(setf card-A (blackjack-card :value 0)
      card-6 (blackjack-card :value 5)
      card-7 (blackjack-card :value 6)
      card-T (blackjack-card :value 9))

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
          :var ((game (blackjack-game)))

          (it "has a default id"
              (expect (slot-value game 'id) :to-be 0))

          (it "initially has an empty shoe"
              (expect (slot-value game 'shoe) :to-be '()))

          (it "initially has no dealer-hand"
              (expect (slot-value game 'dealer-hand) :to-be nil))

          (it "initially has no player hands"
              (expect (slot-value game 'player-hands) :to-be '()))

          (it "has a default num-decks"
              (expect (slot-value game 'num-decks) :to-be 1))

          (it "has a default deck-type"
              (expect (slot-value game 'deck-type) :to-be 'regular))

          (it "has a default face-type"
              (expect (slot-value game 'face-type) :to-be 'regular))

          (it "has default money"
              (expect (slot-value game 'money) :to-be 10000))

          (it "has default current-bet"
              (expect (slot-value game 'current-bet) :to-be 500))

          (it "has a default current-player-hand"
              (expect (slot-value game 'current-player-hand) :to-be 0))

          (it "has default current-menu"
              (expect (slot-value game 'current-menu) :to-be 'game))

          (it "has regular faces"
              (expect (slot-value game 'faces-regular) :to-equal '[["A♠" "A♥" "A♣" "A♦"]
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
              (expect (slot-value game 'faces-alternate) :to-equal '[["🂡" "🂱" "🃁" "🃑"]
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
              (expect (slot-value game 'shuffle-specs) :to-equal '[80 81 82 84 86 89 92 95]))

          (it "has a cards-per-deck value"
              (expect (slot-value game 'cards-per-deck) :to-be 52))

          (it "has a minimum bet value"
              (expect (slot-value game 'min-bet) :to-be 500))

          (it "has a maximum bet value"
              (expect (slot-value game 'max-bet) :to-be 100000000))

          (it "has a maximum player hands value"
              (expect (slot-value game 'max-player-hands) :to-be 7)))

(describe "blackjack--deal-new-hand"
          (after-each
           (setq player-hands (slot-value game 'player-hands))
           (expect (length player-hands) :to-be 1)
           (setq player-hand (nth 0 player-hands))
           (expect (length (slot-value player-hand 'cards)) :to-be 2)
           (setq dealer-hand (slot-value game 'dealer-hand))
           (expect (length (slot-value dealer-hand 'cards)) :to-be 2)
           (setq shoe (slot-value game 'shoe))
           (expect (length shoe) :to-be 48))

          (describe "with a deck of jacks"
                    (it "player has 20, shows hand actions"
                        (spy-on 'blackjack--ask-hand-action)
                        (setq game (blackjack-game :deck-type 'jacks))
                        (blackjack--deal-new-hand game)
                        (expect (slot-value game 'deck-type) :to-be 'jacks)
                        (expect (slot-value game 'current-menu) :to-be 'hand)))

          (describe "with a deck of aces"
                    (it "dealer upcard is an A, shows insurance actions"
                        (spy-on 'blackjack--ask-insurance-action)
                        (setq game (blackjack-game :deck-type 'aces))
                        (blackjack--deal-new-hand game)
                        (expect (slot-value game 'deck-type) :to-be 'aces)
                        (expect (slot-value game 'current-menu) :to-be 'insurance)))

          (describe "with T, T, A, T"
                    (it "player has blackjack, shows game actions"
                        (spy-on 'blackjack--ask-game-action)
                        (setq game (blackjack-game))
                        (blackjack--shuffle game '(9 9 0 9) t)
                        (blackjack--deal-new-hand game)
                        (expect (slot-value game 'deck-type) :to-be 'regular)
                        (expect (slot-value game 'current-menu) :to-be 'game))))

(describe "blackjack--player-hand-lost-p"
          (it "returns non-nil when player hand value is less than dealer hand value"
              (expect (blackjack--player-hand-lost-p 2 3) :to-be t))

          (it "returns nil when player hand value is equal to dealer hand value"
              (expect (blackjack--player-hand-lost-p 2 2) :to-be nil))

          (it "returns nil when player hand value is greater than dealer hand value"
              (expect (blackjack--player-hand-lost-p 3 2) :to-be nil)))

(describe "blackjack--card-values"
          :var ((game (blackjack-game)))

          (it "returns '(0 1 2 3 4 5 6 7 8 9 10 11 12) for a regular deck-type"
              (expect (blackjack--card-values game) :to-equal '(0 1 2 3 4 5 6 7 8 9 10 11 12)))

          (it "returns '(0) for an aces deck-type"
              (setf (slot-value game 'deck-type) 'aces)
              (expect (blackjack--card-values game) :to-equal '(0)))

          (it "returns '(10) for a jack deck-type"
              (setf (slot-value game 'deck-type) 'jacks)
              (expect (blackjack--card-values game) :to-equal '(10)))

          (it "returns '(0 10) for an aces-jacks deck-type"
              (setf (slot-value game 'deck-type) 'aces-jacks)
              (expect (blackjack--card-values game) :to-equal '(0 10)))

          (it "returns '(6) for a sevens deck-type"
              (setf (slot-value game 'deck-type) 'sevens)
              (expect (blackjack--card-values game) :to-equal '(6)))

          (it "returns '(7) for an eights deck-type"
              (setf (slot-value game 'deck-type) 'eights)
              (expect (blackjack--card-values game) :to-equal '(7))))

(describe "blackjack--pay-player-hand"
          :var ((game (blackjack-game)))

          (before-each
           (setf player-hand (blackjack-player-hand)))

          (it "ignores payed player hands"
              (spy-on 'blackjack--player-hand-value)
              (setf (slot-value player-hand 'payed) t)
              (blackjack--pay-player-hand game player-hand 0 nil)
              (expect 'blackjack--player-hand-value :not :to-have-been-called))

          (it "pays unpayed player hands"
              (blackjack--pay-player-hand game player-hand 0 nil)
              (expect (slot-value player-hand 'payed) :to-be t))

          (describe "with a player hand value of 18"

                    (before-each
                     (setf (slot-value player-hand 'cards) (list card-A card-7)))

                    (it "pays winning player hands"
                        (spy-on 'blackjack--pay-won-hand :and-call-through)
                        (blackjack--pay-player-hand game player-hand 0 nil)
                        (expect 'blackjack--pay-won-hand :to-have-been-called)
                        (expect (slot-value player-hand 'status) :to-be 'won))

                    (it "collects losing player hands"
                        (spy-on 'blackjack--collect-lost-hand :and-call-through)
                        (blackjack--pay-player-hand game player-hand 19 nil)
                        (expect 'blackjack--collect-lost-hand :to-have-been-called)
                        (expect (slot-value player-hand 'status) :to-be 'lost))

                    (it "pays player hand when dealer busts"
                        (spy-on 'blackjack--pay-won-hand :and-call-through)
                        (blackjack--pay-player-hand game player-hand 0 t)
                        (expect 'blackjack--pay-won-hand :to-have-been-called)
                        (expect (slot-value player-hand 'status) :to-be 'won))

                    (it "result is a push when hands are equal"
                        (spy-on 'blackjack--pay-won-hand)
                        (spy-on 'blackjack--collect-lost-hand)
                        (blackjack--pay-player-hand game player-hand 18 nil)
                        (expect 'blackjack--pay-won-hand :not :to-have-been-called)
                        (expect 'blackjack--collect-lost-hand :not :to-have-been-called)
                        (expect (slot-value player-hand 'status) :to-be 'push))))

(describe "blackjack--player-hand-done-p"
          :var ((game (blackjack-game))
                (player-hand (blackjack-player-hand)))

          (before-each
           (spy-on 'blackjack--no-more-actions-p :and-return-value t))
          
          (it "sets the player hand to played"
              (expect (blackjack--player-hand-done-p game player-hand) :to-be t)
              (expect (slot-value player-hand 'played) :to-be t))

          (it "collects a busted player hand"
              (spy-on 'blackjack--player-hand-is-busted-p :and-return-value t)
              (spy-on 'blackjack--collect-busted-hand)
              (expect (blackjack--player-hand-done-p game player-hand) :to-be t)
              (expect 'blackjack--collect-busted-hand :to-have-been-called)))

(describe "blackjack--collect-busted-hand"
          :var ((game (blackjack-game))
                (player-hand (blackjack-player-hand :bet 500)))

          (it "collects a busted player hand"
              (blackjack--collect-busted-hand game player-hand)
              (expect (slot-value player-hand 'payed) :to-be t)
              (expect (slot-value player-hand 'status) :to-be 'lost)
              (expect (slot-value game 'money) :to-be 9500)))

(describe "blackjack--more-hands-to-play-p"
          :var ((game (blackjack-game)))

          (it "returns nil when there are no more hands to play"
              (expect (blackjack--more-hands-to-play-p game) :to-be nil))

          (it "returns non-nil when there are more hands to play"
              (setf (slot-value game 'player-hands) (list (blackjack-player-hand) (blackjack-player-hand)))
              (expect (blackjack--more-hands-to-play-p game) :to-be t)))

(describe "blackjack--play-more-hands"
          :var ((game (blackjack-game :deck-type 'jacks))
                (player-hand-0 (blackjack-player-hand))
                (player-hand-1 (blackjack-player-hand))
                (cards-0 (list card-A card-T))
                (cards-1 '()))

          (before-all
           (blackjack--shuffle game))

          (before-each
           (spy-on 'blackjack--draw-hands)
           (setf (slot-value game 'current-player-hand) 0))

          (after-each
           (expect (slot-value game 'current-player-hand) :to-be 1))

          (it "increments current-player-hand and continues"
              (push card-T cards-1)
              (setf (slot-value player-hand-0 'cards) cards-0)
              (setf (slot-value player-hand-1 'cards) cards-1)
              (setf (slot-value game 'player-hands) (list player-hand-0 player-hand-1))
              (spy-on 'blackjack--ask-hand-action)
              (blackjack--play-more-hands game)
              (expect 'blackjack--ask-hand-action :to-have-been-called))

          (it "increments current-player-hand and processes"
              (push card-A cards-1)
              (setf (slot-value player-hand-0 'cards) cards-0)
              (setf (slot-value player-hand-1 'cards) cards-1)
              (setf (slot-value game 'player-hands) (list player-hand-0 player-hand-1))
              (spy-on 'blackjack--process)
              (blackjack--play-more-hands game)
              (expect 'blackjack--process :to-have-been-called)))

(describe "blackjack--need-to-play-dealer-hand-p"
          :var ((game (blackjack-game))
                (player-hand (blackjack-player-hand)))

          (before-each
           (setf cards (list card-T)))

          (it "returns non-nil when player hand is not busted or blackjack"
              (push card-T cards)
              (setf (slot-value player-hand 'cards) cards)
              (setf (slot-value game 'player-hands) (list player-hand))
              (expect (blackjack--need-to-play-dealer-hand-p game) :to-be t))

          (describe "returns nil when"
                    (after-each
                     (setf (slot-value player-hand 'cards) cards)
                     (setf (slot-value game 'player-hands) (list player-hand))
                     (expect (blackjack--need-to-play-dealer-hand-p game) :to-be nil))

                    (it "player hand is blackjack"
                        (push card-A cards))

                    (it "player hand is busted"
                        (push card-T cards)
                        (push card-T cards))))

(describe "blackjack--dealer-hand-counts"
          :var ((dealer-hand (blackjack-dealer-hand :cards (list card-A card-A))))

          (it "when hiding down card returns (11 1)"
              (expect (slot-value dealer-hand 'hide-down-card) :to-be t)
              (expect (blackjack--dealer-hand-counts dealer-hand) :to-equal '(11 1)))

          (it "when showing down card returns (12 2)"
              (setf (slot-value dealer-hand 'hide-down-card) nil)
              (expect (blackjack--dealer-hand-counts dealer-hand) :to-equal '(12 2))))

(describe "blackjack--deal-required-cards"
          :var ((game (blackjack-game :deck-type 'aces))
                (dealer-hand (blackjack-dealer-hand :hide-down-card nil)))

          (before-all
           (blackjack--shuffle game))

          (after-each
           (setf (slot-value game 'dealer-hand) dealer-hand)
           (blackjack--deal-required-cards game)
           (expect (length (slot-value (slot-value game 'dealer-hand) 'cards)) :to-be 3))
          
          (it "with a hard count deals 1 more card"
              (setf (slot-value dealer-hand 'cards) (list card-T card-6)))

          (it "with a soft-counted ace deals 1 more card"
              (setf (slot-value dealer-hand 'cards) (list card-A card-6))))

(describe "blackjack--play-dealer-hand"
          :var ((game (blackjack-game))
                (dealer-hand (blackjack-dealer-hand)))

          (before-each
           (setf (slot-value game 'dealer-hand) dealer-hand)
           (spy-on 'blackjack--ask-game-action))
          
          (it "is always set to played and player hands payed"
              (spy-on 'blackjack--pay-hands)
              (blackjack--play-dealer-hand game)
              (expect (slot-value dealer-hand 'hide-down-card) :to-be t)
              (expect (slot-value game 'current-menu) :to-be 'game)
              (expect (slot-value dealer-hand 'played) :to-be t)
              (expect 'blackjack--pay-hands :to-have-been-called))

          (it "dealer hand is dealt required cards"
              (spy-on 'blackjack--deal-required-cards)
              (spy-on 'blackjack--need-to-play-dealer-hand-p :and-return-value t)
              (blackjack--play-dealer-hand game)
              (expect (slot-value dealer-hand 'hide-down-card) :to-be nil)
              (expect 'blackjack--deal-required-cards :to-have-been-called))

          (it "unhides down card when blackjack"
              (spy-on 'blackjack--hand-is-blackjack-p :and-return-value t)
              (blackjack--play-dealer-hand game)
              (expect (slot-value dealer-hand 'hide-down-card) :to-be nil)))

(describe "blackjack--process"
          :var ((game (blackjack-game)))
          
          (it "plays more player hands when more hands to play"
              (spy-on 'blackjack--more-hands-to-play-p :and-return-value t)
              (spy-on 'blackjack--play-more-hands)
              (blackjack--process game)
              (expect 'blackjack--play-more-hands :to-have-been-called))

          (it "plays dealer hand when no more hands to play"
              (spy-on 'blackjack--more-hands-to-play-p :and-return-value nil)
              (spy-on 'blackjack--play-dealer-hand)
              (blackjack--process game)
              (expect 'blackjack--play-dealer-hand :to-have-been-called)))

(provide 'test-blackjack)

;;; test-blackjack.el ends here

