;;; blackjack --- Tests for blackjack -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(load-file "./tests/test-helper.el")
(require 'blackjack)

(setf card-A (blackjack-card :value 0)
      card-2 (blackjack-card :value 1)
      card-4 (blackjack-card :value 3)
      card-6 (blackjack-card :value 5)
      card-7 (blackjack-card :value 6)
      card-9 (blackjack-card :value 8)
      card-T (blackjack-card :value 9)
      card-J (blackjack-card :value 10))

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

          (it "is initially not paid"
              (expect (slot-value player-hand 'paid) :to-be nil))

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

          (it "ignores paid player hands"
              (spy-on 'blackjack--player-hand-value)
              (setf (slot-value player-hand 'paid) t)
              (blackjack--pay-player-hand game player-hand 0 nil)
              (expect 'blackjack--player-hand-value :not :to-have-been-called))

          (it "pays unpaid player hands"
              (blackjack--pay-player-hand game player-hand 0 nil)
              (expect (slot-value player-hand 'paid) :to-be t))

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
              (expect (slot-value player-hand 'paid) :to-be t)
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

          (it "is always set to played and player hands paid"
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

(describe "blackjack--hit"
          :var ((game (blackjack-game))
                (player-hand (blackjack-player-hand)))

          (before-all
           (blackjack--shuffle game))

          (before-each
           (spy-on 'blackjack--draw-hands)
           (spy-on 'blackjack--ask-hand-action)
           (setf (slot-value game 'player-hands) (list player-hand)))

          (it "adds a card to the current player hand"
              (spy-on 'blackjack--deal-card)
              (blackjack--hit game)
              (expect 'blackjack--deal-card :to-have-been-called)
              (expect (slot-value game 'current-menu) :to-be 'hand))

          (it "processes done hand"
              (spy-on 'blackjack--player-hand-done-p :and-return-value t)
              (spy-on 'blackjack--process)
              (blackjack--hit game)
              (expect 'blackjack--process :to-have-been-called)))

(describe "blackjack--double"
          :var ((game (blackjack-game))
                (player-hand (blackjack-player-hand :bet 500)))

          (before-all
           (blackjack--shuffle game))

          (before-each
           (setf (slot-value game 'player-hands) (list player-hand)))

          (it "doubles bet and takes one card"
              (spy-on 'blackjack--play-dealer-hand)
              (spy-on 'blackjack--deal-card)
              (blackjack--double game)
              (expect 'blackjack--deal-card :to-have-been-called)
              (expect (slot-value player-hand 'bet) :to-be 1000)
              (expect (slot-value player-hand 'played) :to-be t))

          (it "asks hand action when not done"
              (spy-on 'blackjack--ask-hand-action)
              (spy-on 'blackjack--player-hand-done-p :and-return-value nil)
              (blackjack--double game)
              (expect 'blackjack--ask-hand-action :to-have-been-called)))

(describe "blackjack--stand"
          :var ((game (blackjack-game))
                (player-hand (blackjack-player-hand)))

          (before-each
           (setf (slot-value game 'player-hands) (list player-hand)))

          (it "ends the hand"
              (spy-on 'blackjack--process)
              (blackjack--stand game)
              (expect 'blackjack--process :to-have-been-called)
              (expect (slot-value player-hand 'stood) :to-be t)
              (expect (slot-value player-hand 'played) :to-be t)))

(describe "blackjack--split"
          :var ((game (blackjack-game :deck-type 'aces))
                (player-hand (blackjack-player-hand)))

          (before-all
           (blackjack--shuffle game))

          (before-each
           (spy-on 'blackjack--draw-hands)
           (setf (slot-value player-hand 'cards) (list card-A card-A))
           (setf (slot-value game 'player-hands) (list player-hand)))

          (it "splits into two hands"
              (spy-on 'blackjack--ask-hand-action)
              (blackjack--split game)
              (expect (length (slot-value game 'player-hands)) :to-be 2)
              (expect (slot-value game 'current-menu) :to-be 'hand))

          (it "processes done hand"
              (spy-on 'blackjack--player-hand-done-p :and-return-value t)
              (spy-on 'blackjack--process)
              (blackjack--split game)
              (expect 'blackjack--process :to-have-been-called)))

(describe "blackjack--hand-can-hit-p"
          :var ((game (blackjack-game))
                (player-hand (blackjack-player-hand)))

          (it "returns nil for non-hand menu"
              (expect (blackjack--hand-can-hit-p game) :to-be nil))

          (describe "when menu is 'hand"

                    (before-each
                     (setf (slot-value game 'current-menu) 'hand)
                     (setf (slot-value game 'player-hands) (list player-hand)))

                    (it "returns non-nil for a hand that can be hit"
                        (expect (blackjack--hand-can-hit-p game) :to-be t))

                    (it "returns nil for a played hand"
                        (setf (slot-value player-hand 'played) t)
                        (expect (blackjack--hand-can-hit-p game) :to-be nil))

                    (it "returns nil for a stood hand"
                        (setf (slot-value player-hand 'stood) t)
                        (expect (blackjack--hand-can-hit-p game) :to-be nil))

                    (it "returns nil for a hand total of 21"
                        (setf (slot-value player-hand 'cards) (list card-7 card-7 card-7))
                        (setf (slot-value game 'player-hands) (list player-hand))
                        (expect (blackjack--hand-can-hit-p game) :to-be nil))

                    (it "returns nil for a blackjack hand"
                        (setf (slot-value player-hand 'cards) (list card-A card-T))
                        (setf (slot-value game 'player-hands) (list player-hand))
                        (expect (blackjack--hand-can-hit-p game) :to-be nil))

                    (it "returns nil for a busted hand"
                        (setf (slot-value player-hand 'cards) (list card-T card-T card-T))
                        (setf (slot-value game 'player-hands) (list player-hand))
                        (expect (blackjack--hand-can-hit-p game) :to-be nil))))

(describe "blackjack--hand-can-stand-p"
          :var ((game (blackjack-game))
                (player-hand (blackjack-player-hand)))

          (it "returns nil for non-hand menu"
              (expect (blackjack--hand-can-stand-p game) :to-be nil))

          (describe "when menu is 'hand"

                    (before-each
                     (setf (slot-value game 'current-menu) 'hand)
                     (setf (slot-value game 'player-hands) (list player-hand)))

                    (it "returns non-nil for a hand that can be stand"
                        (expect (blackjack--hand-can-stand-p game) :to-be t))

                    (it "returns nil for a stood hand"
                        (setf (slot-value player-hand 'stood) t)
                        (expect (blackjack--hand-can-stand-p game) :to-be nil))

                    (it "returns nil for a blackjack hand"
                        (setf (slot-value player-hand 'cards) (list card-A card-T))
                        (setf (slot-value game 'player-hands) (list player-hand))
                        (expect (blackjack--hand-can-stand-p game) :to-be nil))

                    (it "returns nil for a busted hand"
                        (setf (slot-value player-hand 'cards) (list card-T card-T card-T))
                        (setf (slot-value game 'player-hands) (list player-hand))
                        (expect (blackjack--hand-can-stand-p game) :to-be nil))))

(describe "blackjack--hand-can-split-p"
          :var ((game (blackjack-game))
                (player-hand (blackjack-player-hand :bet 500)))

          (it "returns nil for non-hand menu"
              (expect (blackjack--hand-can-split-p game) :to-be nil))

          (describe "when menu is 'hand"

                    (before-each
                     (setf (slot-value game 'current-menu) 'hand)
                     (setf (slot-value player-hand 'cards) (list card-A card-A))
                     (setf (slot-value game 'player-hands) (list player-hand)))

                    (after-each
                     (setf (slot-value game 'money) 1000)
                     (setf (slot-value player-hand 'stood) nil))

                    (it "returns non-nil for a hand that can be split"
                        (expect (blackjack--hand-can-split-p game) :to-be t))

                    (it "returns nil for a stood hand"
                        (setf (slot-value player-hand 'stood) t)
                        (expect (blackjack--hand-can-split-p game) :to-be nil))

                    (it "returns nil for too many split hands"
                        (setf (slot-value game 'player-hands)
                              (make-list (slot-value game 'max-player-hands)
                                         (blackjack-player-hand :cards (list card-A card-A))))
                        (expect (blackjack--hand-can-split-p game) :to-be nil))

                    (it "returns nil when not enough money"
                        (setf (slot-value game 'money) 999)
                        (expect (blackjack--hand-can-split-p game) :to-be nil))

                    (it "returns nil for not having two cards"
                        (setf (slot-value player-hand 'cards) (list card-T card-T card-T))
                        (setf (slot-value game 'player-hands) (list player-hand))
                        (expect (blackjack--hand-can-split-p game) :to-be nil))

                    (it "returns nil for not having matching cards"
                        (setf (slot-value player-hand 'cards) (list card-A card-T))
                        (setf (slot-value game 'player-hands) (list player-hand))
                        (expect (blackjack--hand-can-split-p game) :to-be nil))))

(describe "blackjack--hand-can-double-p"
          :var ((game (blackjack-game))
                (player-hand (blackjack-player-hand :bet 500)))

          (it "returns nil for non-hand menu"
              (expect (blackjack--hand-can-double-p game) :to-be nil))

          (describe "when menu is 'hand"

                    (before-each
                     (setf (slot-value game 'current-menu) 'hand)
                     (setf (slot-value player-hand 'cards) (list card-4 card-7))
                     (setf (slot-value game 'player-hands) (list player-hand)))

                    (after-each
                     (setf (slot-value game 'money) 1000)
                     (setf (slot-value player-hand 'stood) nil))

                    (it "returns non-nil for a hand that can be doubled"
                        (expect (blackjack--hand-can-double-p game) :to-be t))

                    (it "returns nil for a stood hand"
                        (setf (slot-value player-hand 'stood) t)
                        (expect (blackjack--hand-can-double-p game) :to-be nil))

                    (it "returns nil when not enough money"
                        (setf (slot-value game 'money) 999)
                        (expect (blackjack--hand-can-double-p game) :to-be nil))

                    (it "returns nil for not having two cards"
                        (setf (slot-value player-hand 'cards) (list card-4 card-4 card-4))
                        (setf (slot-value game 'player-hands) (list player-hand))
                        (expect (blackjack--hand-can-double-p game) :to-be nil))

                    (it "returns nil when hand is blackjack"
                        (setf (slot-value player-hand 'cards) (list card-A card-T))
                        (setf (slot-value game 'player-hands) (list player-hand))
                        (expect (blackjack--hand-can-double-p game) :to-be nil))))

(describe "blackjack--current-player-hand"
          :var ((game (blackjack-game))
                (player-hand-0 (blackjack-player-hand :cards (list card-A card-4)))
                (player-hand-1 (blackjack-player-hand :cards (list card-T card-T))))

          (before-each
           (setf (slot-value game 'player-hands) (list player-hand-0 player-hand-1))
           (setf (slot-value game 'current-player-hand) 1))

          (it "returns the current player-hand"
              (expect (blackjack--current-player-hand game) :to-be player-hand-1)))

(describe "blackjack--all-bets"
          :var ((game (blackjack-game))
                (player-hand-0 (blackjack-player-hand :bet 1000))
                (player-hand-1 (blackjack-player-hand :bet 500)))

          (before-each
           (setf (slot-value game 'player-hands) (list player-hand-0 player-hand-1)))

          (it "returns the sum of all player hand bets"
              (expect (blackjack--all-bets game) :to-be 1500)))

(describe "blackjack--insure-hand"
          :var ((game (blackjack-game))
                (player-hand (blackjack-player-hand :bet 500)))

          (before-each
           (spy-on 'blackjack--draw-hands)
           (spy-on 'blackjack--ask-game-action)
           (setf (slot-value game 'player-hands) (list player-hand)))

          (it "returns nil unless menu is 'insurance"
              (expect (blackjack--insure-hand game) :to-be nil))

          (describe "when menu is 'insurance"

                    (before-each
                     (setf (slot-value game 'current-menu) 'insurance))

                    (it "surrender half of hand bet"
                        (blackjack--insure-hand game)
                        (expect (slot-value player-hand 'bet) :to-be 250)
                        (expect (slot-value player-hand 'paid) :to-be t)
                        (expect (slot-value player-hand 'played) :to-be t)
                        (expect (slot-value player-hand 'status) :to-be 'lost)
                        (expect (slot-value game 'money) :to-be 9750)
                        (expect (slot-value game 'current-menu) :to-be 'game))))

(describe "blackjack--no-insurance"
          :var ((game (blackjack-game))
                (player-hand (blackjack-player-hand))
                (dealer-hand (blackjack-dealer-hand)))

          (before-each
           (blackjack--shuffle game))

          (it "returns nil unless menu is 'insurance"
              (setf (slot-value dealer-hand 'cards) (list card-A card-T))
              (setf (slot-value game 'dealer-hand) dealer-hand)
              (expect (blackjack--no-insurance game) :to-be nil))

          (describe "when menu is 'insurance"
                    (before-each
                     (setf (slot-value game 'current-menu) 'insurance)
                     (setf (slot-value player-hand 'cards) (list card-A card-4))
                     (setf (slot-value game 'player-hands) (list player-hand)))

                    (describe "when dealer has blackjack"
                              (before-each
                               (spy-on 'blackjack--pay-hands)
                               (spy-on 'blackjack--ask-game-action)
                               (setf (slot-value dealer-hand 'cards) (list card-A card-T))
                               (setf (slot-value game 'dealer-hand) dealer-hand))

                              (it "asks next game action"
                                  (blackjack--no-insurance game)
                                  (expect (slot-value dealer-hand 'hide-down-card) :to-be nil)
                                  (expect 'blackjack--pay-hands :to-have-been-called)
                                  (expect (slot-value game 'current-menu) :to-be 'game)))

                    (describe "when dealer does not have blackjack"
                              (before-each
                               (spy-on 'blackjack--ask-hand-action)
                               (setf (slot-value dealer-hand 'cards) (list card-4 card-7))
                               (setf (slot-value game 'dealer-hand) dealer-hand))

                              (describe "when player hand is done"
                                        (before-each
                                         (spy-on 'blackjack--play-dealer-hand)
                                         (spy-on 'blackjack--player-hand-done-p :and-return-value t))

                                        (it "plays dealer hand"
                                            (blackjack--no-insurance game)
                                            (expect 'blackjack--play-dealer-hand :to-have-been-called)))

                              (describe "when player hand is not done"
                                        (it "asks hand action"
                                            (blackjack--no-insurance game)
                                            (expect (slot-value game 'current-menu) :to-be 'hand))))))

(describe "blackjack--ask-new-bet"
          :var ((game (blackjack-game)))

          (it "returns nil when menu is not 'game"
              (setf (slot-value game 'current-menu) 'hand)
              (blackjack--ask-new-bet game)
              (expect (blackjack--ask-new-bet game) :to-be nil))

          (describe "when menu is 'game"
                    (before-each
                     (setf (slot-value game 'current-menu) 'game)
                     (spy-on 'blackjack--new-bet-menu :and-return-value "5")
                     (spy-on 'blackjack--deal-new-hand)
                     (spy-on 'blackjack--normalize-current-bet)
                     (spy-on 'blackjack--persist-state))

                    (it "asks for and sets new bet value"
                        (blackjack--ask-new-bet game)
                        (expect 'blackjack--deal-new-hand :to-have-been-called)
                        (expect (slot-value game 'current-bet) :to-be 500)
                        (expect 'blackjack--normalize-current-bet :to-have-been-called)
                        (expect 'blackjack--persist-state :to-have-been-called)
                        (expect (slot-value game 'current-menu) :to-be 'game))))

(describe "blackjack--new-bet-menu"
          (it "asks for a new bet amount"
              (spy-on 'read-string)
              (blackjack--new-bet-menu)
              (expect 'read-string :to-have-been-called)))

(describe "blackjack--player-hand-is-busted-p"
          (describe "when soft hand count is less than 21"
                    (it "returns nil"
                        (expect (blackjack--player-hand-is-busted-p (list card-A card-A)) :to-be nil)))

          (describe "when soft hand count is 21"
                    (it "returns nil"
                        (expect (blackjack--player-hand-is-busted-p (list card-A card-T)) :to-be nil)))

          (describe "when soft hand count is greater than 21"
                    (it "returns non-nil"
                        (expect (blackjack--player-hand-is-busted-p (list card-T card-T card-T)) :to-be t))))

(describe "blackjack--dealer-hand-is-busted-p"
          :var ((dealer-hand (blackjack-dealer-hand)))

          (describe "with a hidden down card"
                    (before-each
                     (setf (slot-value dealer-hand 'hide-down-card) t))

                    (after-each
                     (expect (blackjack--dealer-hand-is-busted-p dealer-hand) :to-be nil))

                    (describe "when soft hand count is less than 21"
                              (it "returns nil"
                                  (setf (slot-value dealer-hand 'cards) (list card-A card-A))))

                    (describe "when soft hand count is 21"
                              (it "returns nil"
                                  (setf (slot-value dealer-hand 'cards) (list card-A card-T))))

                    (describe "when soft hand count is greater than 21"
                              (it "returns nil"
                                  (setf (slot-value dealer-hand 'cards) (list card-T card-T card-T)))))

          (describe "with a unhidden down card"
                    (before-each
                     (setf (slot-value dealer-hand 'hide-down-card) nil))

                    (describe "when soft hand count is less than 21"
                              (it "returns nil"
                                  (setf (slot-value dealer-hand 'cards) (list card-A card-A))
                                  (expect (blackjack--dealer-hand-is-busted-p dealer-hand) :to-be nil)))

                    (describe "when soft hand count is 21"
                              (it "returns nil"
                                  (setf (slot-value dealer-hand 'cards) (list card-A card-T))
                                  (expect (blackjack--dealer-hand-is-busted-p dealer-hand) :to-be nil)))

                    (describe "when soft hand count is greater than 21"
                              (it "returns non-nil "
                                  (setf (slot-value dealer-hand 'cards) (list card-T card-T card-T))
                                  (expect (blackjack--dealer-hand-is-busted-p dealer-hand) :to-be t)))))

(describe "blackjack--hand-is-blackjack-p"
          (describe "with two aces"
                    (it "returns nil"
                        (expect (blackjack--hand-is-blackjack-p (list card-A card-A)) :to-be nil)))

          (describe "with an ace and a ten"
                    (it "returns non-nil"
                        (expect (blackjack--hand-is-blackjack-p (list card-A card-T)) :to-be t)))

          (describe "with a ten and an ace"
                    (it "returns non-nil"
                        (expect (blackjack--hand-is-blackjack-p (list card-T card-A)) :to-be t)))

          (describe "with three cards"
                    (it "returns nil"
                        (expect (blackjack--hand-is-blackjack-p (list card-A card-T card-T)) :to-be nil))))

(describe "blackjack--dealer-upcard-is-ace-p"
          :var ((dealer-hand (blackjack-dealer-hand)))

          (describe "when upcard is an ace"
                    (it "returns non-nil"
                        (setf (slot-value dealer-hand 'cards) (list card-T card-A))
                        (expect (blackjack--dealer-upcard-is-ace-p dealer-hand) :to-be t)))

          (describe "when upcard is not an ace"
                    (it "returns nil"
                        (setf (slot-value dealer-hand 'cards) (list card-A card-T))
                        (expect (blackjack--dealer-upcard-is-ace-p dealer-hand) :to-be nil))))

(describe "blackjack--down-card"
          (it "is a constant"
              (expect blackjack--down-card :to-equal '(13 0))))

(describe "blackjack--dealer-hand-to-str"
          :var ((game (blackjack-game))
                (dealer-hand (blackjack-dealer-hand)))

          (describe "with an ace and a ten"
                    (it "returns dealer hand cards as a string"
                        (setf (slot-value dealer-hand 'cards) (list card-A card-T))
                        (setf (slot-value game 'dealer-hand) dealer-hand)
                        (expect (blackjack--dealer-hand-to-str game) :to-equal "  ?? T♠  ⇒  10")))

          (describe "with unhidden down card"
                    (it "returns dealer hand cards as a string"
                        (setf (slot-value dealer-hand 'hide-down-card) nil)
                        (setf (slot-value dealer-hand 'cards) (list card-A card-T))
                        (setf (slot-value game 'dealer-hand) dealer-hand)
                        (expect (blackjack--dealer-hand-to-str game) :to-equal "  A♠ T♠  ⇒  21"))))

(describe "blackjack--draw-dealer-hand"
          :var ((game (blackjack-game))
                (dealer-hand (blackjack-dealer-hand)))

          (before-each
           (setf (slot-value game 'dealer-hand) dealer-hand))

          (it "inserts dealer hand"
              (spy-on 'insert)
              (spy-on 'blackjack--dealer-hand-to-str :and-return-value "  ?? T♠  ⇒  10")
              (blackjack--draw-dealer-hand game)
              (expect 'blackjack--dealer-hand-to-str :to-have-been-called)
              (expect 'insert :to-have-been-called-with "  ?? T♠  ⇒  10")))

(describe "blackjack--dealer-hand-value"
          :var ((dealer-hand (blackjack-dealer-hand)))

          (before-each
           (setf (slot-value dealer-hand 'cards) (list card-A card-T)))

          (describe "when soft count"
                    (describe "with down card hidden"
                              (it "returns 10"
                                  (setf (slot-value dealer-hand 'hide-down-card) t)
                                  (expect (blackjack--dealer-hand-value dealer-hand 'soft) :to-be 10)))

                    (describe "with down card unhidden"
                              (it "returns 21"
                                  (setf (slot-value dealer-hand 'hide-down-card) nil)
                                  (expect (blackjack--dealer-hand-value dealer-hand 'soft) :to-be 21))))

          (describe "when hard count"
                    (describe "with down card hidden"
                              (it "returns 10"
                                  (setf (slot-value dealer-hand 'hide-down-card) t)
                                  (expect (blackjack--dealer-hand-value dealer-hand 'hard) :to-be 10)))

                    (describe "with down card unhidden"
                              (it "returns 21"
                                  (setf (slot-value dealer-hand 'hide-down-card) nil)
                                  (expect (blackjack--dealer-hand-value dealer-hand 'hard) :to-be 11)))))

(describe "blackjack--draw-player-hands"
          :var ((game (blackjack-game))
                (player-hand-0 (blackjack-player-hand))
                (player-hand-1 (blackjack-player-hand)))

          (before-each
           (setf (slot-value game 'player-hands) (list player-hand-0 player-hand-1)))

          (it "calls blackjack--draw-player-hand"
              (spy-on 'blackjack--draw-player-hand)
              (blackjack--draw-player-hands game)
              (expect 'blackjack--draw-player-hand :to-have-been-called-times 2)))

(describe "blackjack--player-hand-to-str"
          :var ((game (blackjack-game))
                (player-hand (blackjack-player-hand :bet 500)))

          (it "returns player hand as a string"
              (setf (slot-value player-hand 'cards) (list card-A card-T))
              (setf (slot-value game 'player-hands) (list player-hand))
              (expect (blackjack--player-hand-to-str game player-hand 0) :to-equal "  A♠ T♠  ⇒  21  $5.00 ⇐  \n\n")))

(describe "blackjack--draw-player-hand"
          :var ((game (blackjack-game))
                (player-hand (blackjack-player-hand)))

          (it "inserts player hand string"
              (spy-on 'insert)
              (spy-on 'blackjack--player-hand-to-str :and-return-value "  A♠ T♠  ⇒  21  $5.00 ⇐  \n\n")
              (blackjack--draw-player-hand game player-hand 0)
              (expect 'insert :to-have-been-called-with "  A♠ T♠  ⇒  21  $5.00 ⇐  \n\n")))

(describe "blackjack--player-hand-to-str"
          :var ((game (blackjack-game))
                (player-hand (blackjack-player-hand :cards (list card-A card-T) :bet 500)))

          (it "returns the player hand as a string"
              (expect (blackjack--player-hand-to-str game player-hand 0) :to-equal "  A♠ T♠  ⇒  21  $5.00 ⇐  \n\n")))

(describe "blackjack--player-hand-cards"
          :var ((game (blackjack-game))
                (player-hand (blackjack-player-hand :cards (list card-A card-T))))

          (it "returns the player hand cards as a string"
              (expect (blackjack--player-hand-cards game player-hand) :to-equal "  A♠ T♠  ⇒  21  ")))

(describe "blackjack--player-hand-status"
          :var ((player-hand (blackjack-player-hand)))

          (describe "unknown"
                    (it "returns the player hand status"
                        (expect (blackjack--player-hand-status player-hand) :to-equal "")))

          (describe "push"
                    (it "returns the player hand status"
                        (setf (slot-value player-hand 'status) 'push)
                        (expect (blackjack--player-hand-status player-hand) :to-equal "Push")))

          (describe "blackjack"
                    (it "returns the player hand status"
                        (setf (slot-value player-hand 'status) 'won)
                        (spy-on 'blackjack--hand-is-blackjack-p :and-return-value t)
                        (expect (blackjack--player-hand-status player-hand) :to-equal "Blackjack!")))

          (describe "won"
                    (it "returns the player hand status"
                        (setf (slot-value player-hand 'status) 'won)
                        (spy-on 'blackjack--hand-is-blackjack-p :and-return-value nil)
                        (expect (blackjack--player-hand-status player-hand) :to-equal "Won!")))

          (describe "lost"
                    (it "returns the player hand status"
                        (setf (slot-value player-hand 'status) 'lost)
                        (spy-on 'blackjack--player-hand-is-busted-p :and-return-value nil)
                        (expect (blackjack--player-hand-status player-hand) :to-equal "Lost!")))

          (describe "busted"
                    (it "returns the player hand status"
                        (setf (slot-value player-hand 'status) 'lost)
                        (spy-on 'blackjack--player-hand-is-busted-p :and-return-value t)
                        (expect (blackjack--player-hand-status player-hand) :to-equal "Busted!"))))

(describe "blackjack--player-hand-money"
          :var ((game (blackjack-game))
                (player-hand (blackjack-player-hand :bet 500)))

          (it "returns current player hand money as a string"
              (expect (blackjack--player-hand-money game player-hand 0) :to-equal "$5.00 ⇐  "))

          (it "returns non-current player hand money as a string"
              (expect (blackjack--player-hand-money game player-hand 1) :to-equal "$5.00  "))

          (it "returns lost player hand money as a string"
              (setf (slot-value player-hand 'status) 'lost)
              (expect (blackjack--player-hand-money game player-hand 0) :to-equal "-$5.00 ⇐  "))

          (it "returns won player hand money as a string"
              (setf (slot-value player-hand 'status) 'won)
              (expect (blackjack--player-hand-money game player-hand 0) :to-equal "+$5.00 ⇐  "))

          (it "returns played current player hand money as a string"
              (setf (slot-value player-hand 'played) t)
              (setf (slot-value player-hand 'status) 'unknown)
              (expect (blackjack--player-hand-money game player-hand 0) :to-equal "$5.00  ")))

(describe "blackjack--player-hand-value"
          :var ((cards))

          (it "returns 13"
              (setf cards (list card-A card-2))
              (expect (blackjack--player-hand-value cards 'soft) :to-equal 13))

          (it "returns 3"
              (setf cards (list card-A card-2))
              (expect (blackjack--player-hand-value cards 'hard) :to-equal 3))

          (it "returns 21"
              (setf cards (list card-A card-T))
              (expect (blackjack--player-hand-value cards 'soft) :to-equal 21))

          (it "returns 11"
              (setf cards (list card-A card-T))
              (expect (blackjack--player-hand-value cards 'hard) :to-equal 11))

          (it "returns 12 with forced hard count"
              (setf cards (list card-A card-A card-T))
              (expect (blackjack--player-hand-value cards 'soft) :to-equal 12)))


(describe "blackjack--card-val"
          (it "returns 11"
              (expect (blackjack--card-val card-A 'soft 0) :to-equal 11))

          (it "returns 1"
              (expect (blackjack--card-val card-A 'hard 0) :to-equal 1))

          (it "returns 1 with forced hard count"
              (expect (blackjack--card-val card-A 'soft 11) :to-equal 1))

          (it "returns 10"
              (expect (blackjack--card-val card-T 'soft 0) :to-equal 10)))

(describe "blackjack--card-face"
          :var ((game (blackjack-game)))

          (it "returns A♠"
              (expect (blackjack--card-face game 0 0) :to-equal "A♠"))

          (it "returns 🂡"
              (setf (slot-value game 'face-type) 'alternate)
              (expect (blackjack--card-face game 0 0) :to-equal "🂡"))

          (it "returns ??"
              (setf (slot-value game 'face-type) 'regular)
              (expect (blackjack--card-face game 13 0) :to-equal "??"))

          (it "returns 🂠"
              (setf (slot-value game 'face-type) 'alternate)
              (expect (blackjack--card-face game 13 0) :to-equal "🂠")))

(describe "blackjack--is-ace-p"
          (it "returns true"
              (expect (blackjack--is-ace-p card-A) :to-be t))

          (it "returns false"
              (expect (blackjack--is-ace-p card-T) :to-be nil)))

(describe "blackjack--is-ten-p"
          (it "10 returns true"
              (expect (blackjack--is-ten-p card-T) :to-be t))

          (it "Jack returns true"
              (expect (blackjack--is-ten-p card-J) :to-be t))

          (it "9 returns false"
              (expect (blackjack--is-ten-p card-9) :to-be nil)))

(describe "blackjack--normalize-current-bet"
          :var ((game (blackjack-game)))

          (it "does not change current bet"
              (blackjack--normalize-current-bet game)
              (expect (slot-value game 'current-bet) :to-equal 500))

          (it "change current bet to minimum"
              (setf (slot-value game 'min-bet) 1000)
              (blackjack--normalize-current-bet game)
              (expect (slot-value game 'current-bet) :to-equal 1000))

          (it "change current bet to maximum"
              (setf (slot-value game 'max-bet) 1000)
              (setf (slot-value game 'current-bet) 10000)
              (blackjack--normalize-current-bet game)
              (expect (slot-value game 'current-bet) :to-equal 1000))

          (it "change current bet to money"
              (setf (slot-value game 'max-bet) 100000000)
              (setf (slot-value game 'money) 1000)
              (setf (slot-value game 'current-bet) 2000)
              (blackjack--normalize-current-bet game)
              (expect (slot-value game 'current-bet) :to-equal 1000)))

(describe "blackjack--persist-file-name"
          (it "returns a file name"
              (setf result (string-match-p
                            "/.emacs.d/blackjack.txt\\'"
                            (blackjack--persist-file-name)))
              (expect result :not :to-be nil)))

(describe "blackjack--persisted-state"
          (it "returns persisted state as a string"
              (spy-on 'buffer-string :and-call-through)
              (expect (blackjack--persisted-state) :to-equal "1|regular|regular|10000|500")
              (expect 'buffer-string :to-have-been-called)))

(describe "blackjack--load-persisted-state"
          :var ((game (blackjack-game)))

          (it "loads saved gamed state"
              (spy-on 'blackjack--persisted-state :and-return-value "8|aces|alternate|20000|1000")
              (blackjack--load-persisted-state game)
              (expect (slot-value game 'num-decks) :to-be 8)
              (expect (slot-value game 'deck-type) :to-be 'aces)
              (expect (slot-value game 'face-type) :to-be 'alternate)
              (expect (slot-value game 'money) :to-be 20000)
              (expect (slot-value game 'current-bet) :to-be 1000)))

(describe "blackjack--quit"
          :var ((game (blackjack-game)))

          (it "calls quit-window"
              (spy-on 'quit-window)
              (blackjack--quit game)
              (expect 'quit-window :to-have-been-called)
              (expect (slot-value game 'quitting) :to-be t)))

(describe "blackjack--header"
          :var ((game (blackjack-game)))

          (it "returns formatted game menu"
              (expect (blackjack--header game) :to-equal
                      "  Blackjack $100.00  (d) deal new hand  (b) change bet  (o) options  (q) quit"))

          (it "returns formatted hand menu"
              (setf (slot-value player-hand 'cards) (list card-T card-4))
              (setf (slot-value game 'player-hands) (list player-hand))
              (setf (slot-value game 'current-menu) 'hand)
              (expect (blackjack--header game) :to-equal
                      "  Blackjack $100.00  (h) hit  (s) stand  (d) double"))

          (it "returns formatted options menu"
              (setf (slot-value game 'current-menu) 'options)
              (expect (blackjack--header game) :to-equal
                      "  Blackjack $100.00  (n) number of decks  (t) deck type  (f) face type  (b) go back"))

          (it "returns formatted deck type menu"
              (setf (slot-value game 'current-menu) 'deck-type)
              (expect (blackjack--header game) :to-equal
                      "  Blackjack $100.00  (1) regular  (2) aces  (3) jacks  (4) aces & jacks  (5) sevens  (6) eights"))

          (it "returns formatted face type menu"
              (setf (slot-value game 'current-menu) 'face-type)
              (expect (blackjack--header game) :to-equal
                      "  Blackjack $100.00  (r) A♠ regular  (a) 🂡 alternate"))

          (it "returns formatted number of decks menu"
              (setf (slot-value game 'current-menu) 'num-decks)
              (expect (blackjack--header game) :to-equal
                      "  Blackjack $100.00  Number of decks (1-8)"))

          (it "returns formatted bet menu"
              (setf (slot-value game 'current-menu) 'bet)
              (expect (blackjack--header game) :to-equal
                      "  Blackjack $100.00  New bet"))

          (it "returns formatted insurance menu"
              (setf (slot-value game 'current-menu) 'insurance)
              (expect (blackjack--header game) :to-equal
                      "  Blackjack $100.00  (y) insure hand  (n) refuse insurance")))

(describe "blackjack--update-header"
          :var ((game (blackjack-game)))

          (it "sets the header"
              (blackjack--update-header game)
              (expect header-line-format :to-equal
                      "  Blackjack $100.00  (d) deal new hand  (b) change bet  (o) options  (q) quit")))

(describe "blackjack--game-header-menu"
          (it "returns game menu as a string"
              (expect (blackjack--game-header-menu) :to-equal
                      "(d) deal new hand  (b) change bet  (o) options  (q) quit")))

(provide 'test-blackjack)

;;; test-blackjack.el ends here

