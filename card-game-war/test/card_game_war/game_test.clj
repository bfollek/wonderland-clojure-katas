(ns card-game-war.game-test
  (:require [clojure.test :refer :all]
            [clojure.set :as set]
            [card-game-war.game :refer :all]))

(deftest test-valid-card?
  (binding [*verbose* false]
    (let [valid-card? #'card-game-war.game/valid-card?]
      (testing "good card"
        (is (valid-card? [:club 7])))
      (testing "bad suit"
        (is (not (valid-card? [:bad-suit :ace]))))
      (testing "bad rank"
        (is (not (valid-card? [:spade 99])))))))

(deftest test-deal-cards
  (binding [*verbose* false]
    (let [deal-cards #'card-game-war.game/deal-cards
          hands (deal-cards)]
      (testing "deal right # of hands"
        (is (= 2 (count hands))))
      (testing "deal right # of cards"
        (is (and (= 26 (count (first hands))) (= 26 (count (second hands))))))
      (testing "deal unique cards"
        (let [set1 (set (first hands))
              set2 (set (second hands))]
          (is (empty? (set/intersection set1 set2))))))))

(deftest test-cards->winner
  (binding [*verbose* false]
    (let [cards->winner #'card-game-war.game/cards->winner
          cards [[:club :ace] [:diamond 5] [:spade 7] [:heart 9]]
          opponent-card [:club :king]
          new-cards (cards->winner cards opponent-card)]
      (testing "winner has the right # of cards"
        (is (= (count new-cards) (inc (count cards)))))
      (testing "winner's card in right place"
        (is (= (first cards) (last new-cards))))
      (testing "opponent's card in right place"
        (is (= opponent-card (first (take-last 2 new-cards))))))))

(deftest test-play-round
  (binding [*verbose* false]
    (testing "bad card"
      (let [q [:spade :queen] bad [:foo 10000]]
        (is (thrown? AssertionError (play-round q bad)))))
    (testing "queens are higher rank than jacks"
      (let [q [:spade :queen] j [:spade :jack]]
        (is (= q (play-round q j)))))
    (testing "kings are higher rank than queens"
      (let [q [:spade :queen] k [:spade :king]]
        (is (= k (play-round q k)))))
    (testing "aces are higher rank than kings"
      (let [a [:spade :ace] k [:spade :king]]
        (is (= a (play-round a k)))))
    (testing "eights are higher rank than sevens"
      (let [seven [:spade 7] eight [:spade 8]]
        (is (= eight (play-round seven eight)))))
    (testing "if the ranks are equal, clubs beat spades"
      (let [a-s [:spade :ace] a-c [:club :ace]]
        (is (= a-c (play-round a-s a-c)))))
    (testing "if the ranks are equal, diamonds beat clubs"
      (let [a-d [:diamond :ace] a-c [:club :ace]]
        (is (= a-d (play-round a-d a-c)))))
    (testing "if the ranks are equal, hearts beat diamonds"
      (let [a-d [:diamond :ace] a-h [:heart :ace]]
        (is (= a-h (play-round a-d a-h)))))))

(deftest test-play-game
  (binding [*verbose* false]
    (testing "Cards that should end game predictably"
      (let [winning-cards [[:diamond 5] [:spade 7] [:heart 9]
                           [:heart :ace] [:heart 10] [:diamond :ace]]
            losing-cards [[:club :ace] [:heart 8] [:club 4]
                          [:diamond :king] [:spade 3] [:heart 4]]]
        (is (= (count winning-cards) (count losing-cards))
            "Make sure cards balance")
        (is (= :player1 (play-game winning-cards losing-cards)))
        (is (= :player2 (play-game losing-cards winning-cards))
            "Order of play doesn't matter")))
    (testing "the player loses when they run out of cards"
      (let [player1-cards [[:spade 7]]
            player2-cards []]
        (is (= :player1 (play-game player1-cards player2-cards))))
      (let [player1-cards []
            player2-cards [[:club 2]]]
        (is (= :player2   (play-game player1-cards player2-cards)))))))
