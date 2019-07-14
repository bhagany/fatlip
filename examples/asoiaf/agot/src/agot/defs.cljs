(ns agot.defs
  (:require [clojure.string :as s]
            [fatlip.core :as fatlip]
            [fatlip.debug :as debug]
            [reagent.core :as r]))

(def input
  ;; A Game of Thrones
  ;; 1. Prologue - Will I
  [{:duration 10
    :groups [{:characters [:will :waymar :gared]}]}
   {:duration 10
    :groups [{:characters [:will :waymar :gared]
              :event "Encounter with Others"
              :path-mods {:will #{:dead}
                          :waymar #{:undead}}}
             ;; Characters Intros
             {:characters [:ned :robb :bran :jon :theon]}
             {:characters [:grey-wind :lady :nymeria :summer :shaggydog :ghost]}]}
   ;; 2. Bran I
   {:duration 10
    :groups [{:characters [:ned :robb :bran :jon :theon :gared
                           :grey-wind :lady :nymeria :summer :shaggydog :ghost]
              :path-mods {:gared #{:dead}}
              :event "Execution of Gared, finding of the wolves"}
             ;; Characters Intros
             {:characters [:cat :sansa :arya :rickon]}]}
   ;; 3. Catelyn I
   {:duration 10
    :groups [{:characters [:ned :cat :robb :sansa :arya :bran :rickon :jon
                           :theon :grey-wind :lady :nymeria :summer :shaggydog :ghost]}
             ;; Characters Intros
             {:characters [:jorah]}
             {:characters [:drogo]}
             {:characters [:daenerys :viserys :illyrio]}]}
   ;; 4. Daenerys I
   {:duration 10
    :groups [;; Characters Intros
             {:characters [:benjen]}
             {:characters [:robert-b :cersei :jaime :tyrion :joffrey :myrcella :tommen :sandor]}
             {:characters [:daenerys :viserys :illyrio :jorah :drogo]
              :event "Daenerys sold to Drogo"}]}
   ;; 4. Ned I
   {:duration 10
    :groups [{:characters [:ned :cat :robb :sansa :arya :bran :rickon :jon
                           :theon :grey-wind :lady :nymeria :summer :shaggydog :ghost :benjen
                           :robert-b :cersei :jaime :tyrion :joffrey :myrcella :tommen :sandor]
              :event "Robert visits Winterfell"}]}
   ;; 9. Bran II
   {:duration 10
    :groups [{:characters [:ned :cat :robb :sansa :arya :bran :rickon :jon
                           :theon :grey-wind :lady :nymeria :summer
                           :shaggydog :ghost :benjen :robert-b :cersei
                           :jaime :tyrion :joffrey :myrcella :tommen :sandor]
              :event "The things I do for love"}]}
   ;; 12. Daenerys II
   {:duration 10
    :groups [{:characters [:daenerys :viserys :illyrio :jorah :drogo]
              :event "Drogo and Daenerys Married"}]}
   ;; 15. Catelyn III
   {:duration 10
    :groups [{:characters [:cat :robb :bran :rickon :theon :grey-wind :summer :shaggydog]}
             ;; Character Intros
             {:characters [:renly :barristan :ilyn]}]}
   ;; 16-17. Sansa I, Eddard III
   {:duration 20
    :groups [{:characters [:ned :sansa :arya :robert-b :cersei :jaime :joffrey :myrcella
                           :tommen :sandor :renly :barristan :ilyn :lady :nymeria]
              :path-mods {:lady #{:dead}}
              :event "Children Fight Near the Trident"}
             ;; Character Intros
             {:characters [:petyr :varys :pycelle]}]}
   ;; 19. Catelyn IV
   {:duration 10
    :groups [;; Character Intros
             {:characters [:jeor :aemon]}

             {:characters [:cat :petyr :varys :pycelle]}
             ]}
   ;; 20. Jon III
   {:duration 10
    :groups [{:characters [:benjen :jon :ghost :tyrion :jeor :aemon]}]}
   ;; 21. Eddard IV
   {:duration 10
    :groups [{:characters [:ned :sansa :arya :robert-b :cersei :jaime :joffrey :myrcella
                           :tommen :sandor :renly :barristan :ilyn :cat :petyr :varys :pycelle]}
             {:characters [:benjen]
              :path-mods {:benjen #{:disappeared}}}]}
   ;; 25. Bran IV
   {:duration 10
    :groups [{:characters [:robb :bran :rickon :theon :grey-wind :summer :shaggydog :tyrion]}
             ;; Character Intro
             {:characters [:sam]}]}
   ;; 27. Jon IV
   {:duration 10
    :groups [{:characters [:jon :ghost :jeor :aemon :sam]}
             ;; Character Intro
             {:characters [:gendry]}]}
   ;; 28. Eddard VI
   {:duration 10
    :groups [{:characters [:ned :sansa :arya :robert-b :cersei :jaime :joffrey :myrcella
                           :tommen :sandor :renly :barristan :ilyn :petyr :varys
                           :pycelle :gendry]
              :event "Hand's Tourney"}]}
   ;; 29. Catelyn V
   {:duration 10
    :groups [{:characters [:cat :tyrion]}
             ;; Character Intros
             {:characters [:gregor]}
             {:characters [:beric]}
             {:characters [:loras]}]}
   ;; 30. Sansa II
   {:duration 10
    :groups [{:characters [:ned :sansa :arya :robert-b :cersei :jaime :joffrey :myrcella
                           :tommen :sandor :renly :barristan :ilyn :petyr :varys
                           :pycelle :gendry :gregor :beric :loras]}]}
   ;; 33. Arya III
   {:duration 10
    :groups [{:characters [:ned :sansa :arya :robert-b :cersei :jaime :joffrey :myrcella
                           :tommen :sandor :renly :barristan :ilyn :petyr :varys
                           :pycelle :gendry :beric :loras :illyrio]}
             ;; Character Intros
             {:characters [:lysa :robert-a]}
             {:characters [:brynden]}]}
   ;; 35. Catelyn IV
   {:duration 30
    :groups [{:characters [:cat :lysa :robert-a :brynden :tyrion]}]}
   ;;*** 36. Eddard IX Jaime attacks Eddard?
   ;; 45. Sansa II
   {:duration 10
    :groups [{:characters [:ned :sansa :arya :robert-b :cersei :jaime :joffrey :myrcella
                           :tommen :sandor :renly :barristan :ilyn :petyr :varys
                           :pycelle :gendry :beric :loras]
              :event "Beric sent out to bring justice to Gregor"}]}
   ;; 47. Daenerys V
   {:duration 10
    :groups [{:characters [:daenerys :viserys :jorah :drogo]
              :path-mods {:viserys #{:dead}}
              :event "Viserys Crowned"}]}
   ;; 48, 50-52. Eddard XIII, Eddard XIV, Arya IV, Sansa IV
   {:duration 40
    :groups [{:characters [:ned :sansa :arya :robert-b :cersei :joffrey :myrcella
                           :tommen :sandor :barristan :ilyn :petyr :varys :pycelle
                           :gendry :renly :loras]
              :path-mods {:robert-b #{:dead}}
              :event "King Robert gored and killed, Starks betrayed"}]}
   ;; 53. Jon VII
   {:duration 10
    :groups [{:characters [:jon :ghost :jeor :aemon :sam]
              :event "Wight Attack at Castle Black"}]}
   ;; 54. Bran VI
   {:duration 10
    :groups [{:characters [:robb :bran :rickon :theon :grey-wind :summer :shaggydog]
              :event "Robb calls his banners"}]}
   ;; Ambush at Mummer's ford, related in ASOS 18 - Arya III
   {:duration 10
    :groups [{:characters [:beric :gregor]
              :path-mods {:beric #{:undead}}}]}
   ;; 56. Catelyn VIII
   {:duration 10
    :groups [{:characters [:robb :grey-wind :theon :cat :brynden]}
             ;; Character Intro
             {:characters [:tywin]}]}
   ;; 57. Tyrion VII
   {:duration 10
    :groups [{:characters [:tyrion :tywin :gregor]}]}
   ;; 58. Sansa V
   {:duration 40
    :groups [{:characters [:ned :sansa :arya :cersei :joffrey :myrcella
                           :tommen :sandor :barristan :ilyn :petyr :varys :pycelle
                           :gendry]}
             ;; Character Intro
             {:characters [:mirri]}]}
   ;; 62. Daenerys VII
   {:duration 10
    :groups [{:characters [:daenerys :jorah :drogo :mirri]
              :event "Sack of the Lhazareen town"}]}
   ;; 63. Tyrion VIII
   {:duration 10
    :groups [{:characters [:tyrion :tywin :gregor]
              :event "Battle of the Green Fork"}]}
   ;; 64. Catelyn X
   {:duration 10
    :groups [{:characters [:robb :grey-wind :theon :cat :brynden :jaime]
              :event "Battle of the Whispering Wood"}]}
   ;; 65. Daenerys VIII
   {:duration 10
    :groups [{:characters [:daenerys :jorah :drogo :mirri]
              :event "Drogo falls off his horse, Dany goes into labor"}]}
   ;; 66. Arya V
   {:duration 10
    :groups [{:characters [:ned :sansa :arya :cersei :joffrey :myrcella
                           :tommen :sandor :barristan :ilyn :petyr :varys :pycelle :gendry]
              :path-mods {:ned #{:dead}}
              :event "Ned beheaded at the Great Sept of Baelor"}]}
   ;; 69. Daenerys IX
   {:duration 10
    :groups [{:characters [:daenerys :jorah :drogo :mirri]
              :path-mods {:drogo #{:dead}}
              :event "The breaking of Drogo's khalasar"}]}
   ;; 70, 72. Tyrion IX, Catelyn XI
   {:duration 10
    :groups [{:characters [:robb :grey-wind :theon :cat :brynden :jaime]
              :event "Battle of the Camps; Robb proclaimed King in the North"}]}
   ;; 73. Daenerys X
   {:duration 10
    :groups [{:characters [:daenerys :jorah :mirri :drogon :rhaegal :viserion]
              :path-mods {:mirri #{:dead}}
              :event "Mother of Dragons"}]}
   ;; Wrap up
   {:duration 10
    :groups [{:characters [:daenerys :jorah :drogon :rhaegal :viserion]}
             {:characters [:robb :grey-wind :theon :cat :brynden :jaime]}
             {:characters [:sansa :arya :cersei :joffrey :myrcella :tommen :sandor
                           :barristan :ilyn :petyr :varys :pycelle :gendry]}
             {:characters [:tyrion :tywin :gregor]}
             {:characters [:jon :ghost :jeor :aemon :sam]}
             {:characters [:illyrio]}
             {:characters [:beric]}
             {:characters [:nymeria]}
             {:characters [:loras :renly]}
             {:characters [:bran :rickon :summer :shaggydog]}
             {:characters [:lysa :robert-a]}]}])

;;*** Missed Robb leaving for the Westerlands, and Theon and Asha sailing for the North
;;*** Was Margaery at the Hand's Tourney?
;;*** When did Tommen return to KL after Battle of the Blackwater?
;;*** When is Petyr sent to the Vale?
;;*** Guess at all of Beric's deaths?


(def characters
  ;; Candidates for inclusion:
  ;; Edmure Tully
  ;; Walder Frey
  ;; Roose Bolton
  ;; Gilly
  ;; Ygritte
  ;; Oberyn Martell
  ;; Podrick Payne
  ;; Mance Rayder
  ;; Ramsay Snow
  ;; Moqorro
  ;; Tormund
  ;; Kevan Lannister
  {:ned {:name "Eddard Stark"
         :group :pov
         :family :stark}
   :cat {:name "Catelyn Stark"
         :group :pov
         :family :stark}
   :robb {:name "Robb Stark"
          :family :stark}
   :sansa {:name "Sansa Stark"
           :group :pov
           :family :stark}
   :arya {:name "Arya Stark"
          :group :pov
          :family :stark}
   :bran {:name "Bran Stark"
          :group :pov
          :family :stark}
   :rickon {:name "Rickon Stark"
            :family :stark}
   :jon {:name "Jon Snow"
         :group :pov
         :family :stark}
   :meera {:name "Meera Reed"
           :family :stark}
   :jojen {:name "Jojen Reed"
           :family :stark}

   :grey-wind {:name "Grey Wind"
               :family :wolf}
   :lady {:name "Lady"
          :family :wolf}
   :nymeria {:name "Nymeria"
             :family :wolf}
   :summer {:name "Summer"
            :family :wolf}
   :shaggydog {:name "Shaggydog"
               :family :wolf}
   :ghost {:name "Ghost"
           :family :wolf}

   :robert-b {:name "Robert Baratheon"
              :family :baratheon}
   :joffrey {:name "Joffrey Baratheon"
             :family :baratheon}
   :myrcella {:name "Myrcella Baratheon"
              :family :baratheon}
   :tommen {:name "Tommen Baratheon"
            :family :baratheon}
   :stannis {:name "Stannis Baratheon"
             :family :baratheon}
   :renly {:name "Renly Baratheon"
           :family :baratheon}
   :davos {:name "Davos Seaworth"
           :group :pov
           :family :baratheon}
   :brienne {:name "Brienne of Tarth"
             :family :baratheon}

   :tywin {:name "Tywin Lannister"
           :family :lannister}
   :cersei {:name "Cersei Lannister"
            :group :pov
            :family :lannister}
   :jaime {:name "Jaime Lannister"
           :group :pov
           :family :lannister}
   :tyrion {:name "Tyrion Lannister"
            :group :pov
            :family :lannister}
   :sandor {:name "Sandor Clegane"
            :family :lannister}
   :gregor {:name "Gregor Clegane"
            :family :lannister}
   :ilyn {:name "Ilyn Payne"
          :family :lannister}

   :daenerys {:name "Daenerys Targaryen"
              :group :pov
              :family :targaryen}
   :viserys {:name "Viserys Targaryen"
             :family :targaryen}

   :sam {:name "Samwell Tarly"
         :group :pov
         :family :nights-watch}
   :aemon {:name "Maester Aemon"
           :family :nights-watch}
   :jeor {:name "Jeor Mormont"
          :family :nights-watch}
   :benjen {:name "Benjen Stark"
            :family :nights-watch}
   :will {:name "Will"
          :group :pov
          :family :nights-watch}
   :waymar {:name "Waymar Royce"
            :family :nights-watch}
   :gared {:name "Gared"
           :family :nights-watch}

   :theon {:name "Theon Greyjoy"
           :group :pov
           :family :greyjoy}
   :asha {:name "Asha Greyjoy"
          :group :pov
          :family :greyjoy}
   :aeron {:name "Aeron Greyjoy"
           :group :pov
           :family :greyjoy}
   :victarion {:name "Victarion Greyjoy"
               :group :pov
               :family :greyjoy}
   :euron {:name "Euron Greyjoy"
           :family :greyjoy}

   :barristan {:name "Barristan Selmy"
               :family :baratheon}
   :arys {:name "Arys Oakheart"
          :family :baratheon}

   :petyr {:name "Petyr Baelish"
           :family :other}
   :varys {:name "Varys"
           :family :other}
   :pycelle {:name "Pycelle"
             :family :other}
   :illyrio {:name "Illyrio Mopatis"
             :family :other}
   :jorah {:name "Jorah Mormont"
           :family :other}
   :drogo {:name "Drogo"
           :family :other}
   :mirri {:name "Mirri Maz Duur"
           :family :other}
   :melisandre {:name "Melisandre"
                :family :other}
   :gendry {:name "Gendry"
            :family :other}
   :jaqen {:name "Jaqen H'ghar"
           :family :other}
   :coldhands {:name "Coldhands"
               :family :other}
   :pate {:name "Pate"
          :family :other}
   :varamyr {:name "Varamyr"
             :family :other}

   :loras {:name "Loras Tyrell"
           :family :tyrell}
   :margaery {:name "Margaery Tyrell"
              :family :tyrell}
   :beric {:name "Beric Dondarrion"
           :family :tyrell}

   :areo {:name "Areo Hotah"
          :family :martell}
   :doran {:name "Doran Martell"
           :family :martell}
   :arianne {:name "Arianne Martell"
             :family :martell}
   :alleras {:name "Alleras"
             :family :martell}
   :quentyn {:name "Quentyn Martell"
             :family :martell}

   :brynden {:name "Brynden Tully"
             :family :arryn}
   :lysa {:name "Lysa Arryn"
          :family :arryn}
   :robert-a {:name "Robert Arryn"
              :family :arryn}

   :drogon {:name "Drogon"
            :family :dragon}
   :rhaegal {:name "Rhaegal"
             :family :dragon}
   :viserion {:name "Viserion"
              :family :dragon}})

(def colors {:stark "#7c7c7c"
             :wolf "#956b41"
             :baratheon "#ffe557"
             :lannister "#ff1414"
             :targaryen "#ac1717"
             :dragon "#ac1717"
             :nights-watch "#000000"
             :greyjoy "#b15bc9"
             :martell "#ff7a32"
             :other "#4940ff"
             :tyrell "#31c105"
             :arryn "#23d0f5"})

(def character-colors (->> characters
                           (map (fn [[k v]] [k (-> v :family colors)]))
                           (into {})))

(def pov-characters (->> characters
                         (filter (fn [[k v]] (= :pov (:group v))))
                         (map first)
                         (into #{})))

(defn data->path
  [data]
  (case (:type data)
    :h (str "H" (:x data))
    :m (str "M" (:x data) " " (:y data))
    :l (str "L" (:x data) " " (:y data))
    :a (let [{:keys [radius sweep x y]} data]
          (str "A" (s/join " " [radius radius "0 0" sweep x y])))))

(defn agot-component
  []
  (let [plot-data (fatlip/plot input)
        {:keys [min-x min-y max-x max-y]} plot-data
        width (- max-x min-x)
        height (- max-y min-y)]
    [:svg {:viewBox (s/join " " [min-x min-y width height])}
     (map #(let [{:keys [plots character]} %
                 d (s/join " " (map data->path plots))
                 stroke (get character-colors character)
                 fill "none"]
             ^{:key character}
             [:path {:d d :class character :stroke stroke :fill fill
                     :stroke-width (when (pov-characters character) "2")}])
          (:plots plot-data))]))
