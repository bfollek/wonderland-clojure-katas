(ns card-game-war.game
  "wonderland card-game-war kata.")

(def ^:private suits
  "Card suits."
  [:spade :club :diamond :heart])

(def ^:private ranks
  "Card ranks."
  [2 3 4 5 6 7 8 9 10 :jack :queen :king :ace])

(def ^:private cards
  "Fresh deck of cards."
  (for [suit suits
        rank ranks]
    [suit rank]))

(def ^:dynamic *verbose*
  "Should we print game info?"
  true)

(defn- show
  "println wrapper that checks *verbose*"
  [s]
  (when *verbose* (println s)))

(defn- deal-cards
  "Shuffle, deal."
  []
  (partition 26 (shuffle cards)))

(defn- valid-card?
  "Make sure suit and rank values are elements of the respective vectors."
  [[suit rank]]
  (and (some (partial = suit) suits) (some (partial = rank) ranks)))

(defn- show-play
  "Show the cards played."
  [card1 card2]
  (show (format "Player 1 played %s, Player 2 played %s" card1 card2)))

(defn play-round
  "Return the winning card."
  [[suit1 rank1 :as card1] [suit2 rank2 :as card2]]
  {:pre [(valid-card? card1) (valid-card? card2)]}
  (show-play card1 card2)
  (let [rank1-idx (.indexOf ranks rank1)
        rank2-idx (.indexOf ranks rank2)]
    ;; Use rank indexes if they're different, else use suit indexes.
    (let [args (if (not= rank1-idx rank2-idx)
                 [rank1-idx rank2-idx]
                 [(.indexOf suits suit1) (.indexOf suits suit2)])]
      (if (apply > args) card1 card2))))

(defn- show-scoreboard
  "Show the number of cards each player has."
  [player1-cards player2-cards]
  (let [cnt1 (count player1-cards)
        cnt2 (count player2-cards)
        plural1 (if (= 1 cnt1) "" "s")
        plural2 (if (= 1 cnt2) "" "s")]
    (show (format "Player 1 has %d card%s, Player 2 has %d card%s"
                  cnt1 plural1 cnt2 plural2))))

(defn- show-winning-card
  "Show which card wins the round."
  [card]
  (show (str card " wins")))

(defn- cards->winner
  "Winner adds the opponent's card to the bottom of their remaining cards,
  followed by the card they just played."
  [winner-cards opponent-card]
  ;; https://stuartsierra.com/2015/04/26/clojure-donts-concat
  ;; (concat) is probably safe for a single deck. For ten decks,
  ;; we might need to switch to vectors.
  (concat (rest winner-cards) [opponent-card (first winner-cards)]))

(defn play-game
  "Return :player1 if player1 wins, else :player2."
  [player1-cards player2-cards]
  (loop [cards1 player1-cards cards2 player2-cards]
    (show-scoreboard cards1 cards2)
    (cond
      (empty? cards1) :player2 ; If player-1 has no cards, then player-2 wins
      (empty? cards2) :player1
      :else (let [c1 (first cards1)
                  c2 (first cards2)
                  winning-card (play-round c1 c2)]
              (show-winning-card winning-card)
              (if (= c1 winning-card)
                (recur (cards->winner cards1 c2) (rest cards2))
                (recur (rest cards1) (cards->winner cards2 c1)))))))

(defn- show-winner
  "Show the game winner."
  [winner]
  (show (str winner " wins the game!")))

(defn start-game
  "Shuffle, deal, play."
  []
  (let [[player1-cards player2-cards] (deal-cards)
        winner (play-game player1-cards player2-cards)]
    (show-winner winner)
    winner))
