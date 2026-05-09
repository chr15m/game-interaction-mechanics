(ns main
  (:require
    [reagent.core :as r]
    [reagent.dom :as rdom]))

(defonce state
  (r/atom
    {:coins 99
     :inventory {"🥔" 0 "🪵" 0 "💎" 0 "🥕" 0 "🍅" 0 "🐟" 0 "🍗" 0}
     :slots {"🥔" [nil nil nil]
             "🥕" [nil nil nil nil nil]
             "🍅" [nil nil nil nil nil nil nil]}
     :tree-progress 0
     :rock-progress 0
     :catch-games {:fishing {:pos 0 :dir 1 :target-start 40 :target-width 20 :speed 1.5}
                   :hunting {:pos 50 :dir -1 :target-start 70 :target-width 15 :speed 2.5}}
     :cooldowns {}}))

(def hold-interval (atom nil))

(defonce animation-interval-ref (atom nil))

(when @animation-interval-ref
  (js/clearInterval @animation-interval-ref))

(reset! animation-interval-ref
  (js/setInterval
   (fn []
     (swap! state
            (fn [s]
              (-> s
                  (update :catch-games
                          (fn [games]
                            (reduce-kv
                             (fn [m k v]
                               (let [new-pos (+ (:pos v) (* (:dir v) (:speed v)))
                                     [final-pos final-dir]
                                     (cond
                                       (>= new-pos 100) [100 -1]
                                       (<= new-pos 0) [0 1]
                                       :else [new-pos (:dir v)])]
                                 (assoc m k (assoc v :pos final-pos :dir final-dir))))
                             {} games)))
                  (update :cooldowns
                          (fn [cds]
                            (reduce-kv
                             (fn [m k v]
                               (let [new-v (- v 0.1)]
                                 (if (> new-v 0)
                                   (assoc m k new-v)
                                   m)))
                             {} cds)))))))
   20))

(js/console.log @state)

(defn e [c]
  [:span {:class "emoji"} c])

(defn start-gathering [id progress-key speed]
  (when-not (or @hold-interval (get-in @state [:cooldowns id]))
    (reset! hold-interval
            (js/setInterval
             (fn []
               (swap! state update progress-key
                      (fn [p]
                        (if (>= p 100)
                          100
                          (+ p speed)))))
             20))))

(defn stop-gathering [id progress-key reward-emoji]
  (when @hold-interval
    (js/clearInterval @hold-interval)
    (reset! hold-interval nil))
  (when (>= (get @state progress-key) 100)
    (swap! state update-in [:inventory reward-emoji] inc)
    (swap! state assoc-in [:cooldowns id] 100))
  (swap! state assoc progress-key 0))

(defn handle-catch [game-id reward-emoji]
  (let [game (get-in @state [:catch-games game-id])
        pos (:pos game)
        start (:target-start game)
        end (+ start (:target-width game))
        on-cooldown? (get-in @state [:cooldowns game-id])]
    (when-not on-cooldown?
      (when (and (>= pos start) (<= pos end))
        (swap! state update-in [:inventory reward-emoji] inc))
      (swap! state assoc-in [:cooldowns game-id] 100))))

(defn handle-click [emoji]
  (swap! state
         (fn [current-state]
           (let [coins (:coins current-state)
                 slots (get-in current-state [:slots emoji])
                 on-cooldown? (get-in current-state [:cooldowns emoji])]
             (if (and (> coins 0) (some nil? slots) (not on-cooldown?))
               (let [slot-index (.indexOf slots nil)
                     new-slots (assoc slots slot-index "🪙")
                     is-full? (not (some nil? new-slots))]
                 (if is-full?
                   (-> current-state
                       (update :coins dec)
                       (update-in [:inventory emoji] (fnil inc 0))
                       (assoc-in [:slots emoji] (vec (repeat (count slots) nil)))
                       (assoc-in [:cooldowns emoji] 100))
                   (-> current-state
                       (update :coins dec)
                       (assoc-in [:slots emoji] new-slots))))
               current-state)))))

(defn component:hud []
  [:div {:class "hud"} "🪙 " (:coins @state)])

(defn component:inventory []
  [:div {:class "inventory-hud"}
   (for [[item count] (:inventory @state)
         :when (> count 0)]
     ^{:key item}
     [:div {:class "inventory-item"}
      [e item]
      (when (> count 1)
        [:div {:class "badge"} count])])])

(defn component:cooldown-indicator [id emoji-char]
  (let [cd (get-in @state [:cooldowns id])
        radius 40
        circumference (* 2 js/Math.PI radius)
        offset (- (* circumference (- 1 (/ cd 100))))]
    [:div {:style {:position "relative" :display "flex" :justify-content "center" :align-items "center"}}
     [:span {:style {:opacity 0.5}} [e emoji-char]]
     [:svg {:class "circular-progress"
            :viewBox "0 0 100 100"}
      [:circle {:cx 50 :cy 50 :r radius
                :fill "none"
                :stroke "var(--c-fg)"
                :stroke-width 12
                :stroke-linecap "round"
                :stroke-dasharray circumference
                :stroke-dashoffset offset}]]]))

(defn component:buy-slide [emoji]
  ^{:key emoji}
  [:section {:class "slide big"}
   (if (get-in @state [:cooldowns emoji])
     [component:cooldown-indicator emoji emoji]
     [:<>
      [:div {:class "slots"}
       (let [slots-vec (get-in @state [:slots emoji])
             slots-per-row 5
             rows (partition-all slots-per-row slots-vec)]
         (map-indexed
          (fn [row-idx row]
            [:div {:class "slot-row" :key row-idx}
             (let [row-vec (vec row)
                   n (count row-vec)
                   middle (/ (dec n) 2.0)]
               (map-indexed
                (fn [col-idx slot]
                  (let [dist (js/Math.abs (- col-idx middle))
                        y-offset (* dist dist 0.2)
                        style {:transform (str "translateY(" y-offset "em)")}]
                    (if slot
                      [:span
                       {:class "emoji filled" :style style :key col-idx}
                       "🪙"]
                      [:span
                       {:class "emoji" :style style :key col-idx}
                       "⚪"])))
                row-vec))])
          rows))]
      [:div {:style {:cursor "pointer"}
             :on-click #(handle-click emoji)}
       [e emoji]]])])

(defn component:catch-slide [game-id display-emoji reward-emoji]
  (let [game (get-in @state [:catch-games game-id])]
    ^{:key game-id}
    [:section {:class "slide big"}
     (if (get-in @state [:cooldowns game-id])
       [component:cooldown-indicator game-id display-emoji]
       [:<>
        [:div {:class "catch-wrapper"}
         [:div {:class "catch-finger"
                :style {:left (str (+ (:target-start game) (/ (:target-width game) 2)) "%")}}
          [e "👇"]]
         [:div {:class "catch-container"
                :on-mouse-down #(handle-catch game-id reward-emoji)
                :on-touch-start (fn [ev] (.preventDefault ev) (handle-catch game-id reward-emoji))}
          [:div {:class "catch-target"
                 :style {:left (str (:target-start game) "%")
                         :width (str (:target-width game) "%")}}]
          [:div {:class "catch-indicator"
                 :style {:left (str (:pos game) "%")}}]]]
        [:div {:style {:cursor "pointer"}
               :on-mouse-down #(handle-catch game-id reward-emoji)
               :on-touch-start (fn [ev] (.preventDefault ev) (handle-catch game-id reward-emoji))}
         [e display-emoji]]])]))

(defn component:gather-slide [id progress-key base-emoji reward-emoji speed]
  ^{:key id}
  [:section {:class "slide big"}
   (if (get-in @state [:cooldowns id])
     [component:cooldown-indicator id base-emoji]
     [:<>
      [:div {:class "progress-container"}
       [:div {:class "progress-bar" :style {:width (str (get @state progress-key) "%")}}]]
      [:div {:style {:cursor "pointer"}
             :on-mouse-down #(start-gathering id progress-key speed)
             :on-mouse-up #(stop-gathering id progress-key reward-emoji)
             :on-mouse-leave #(stop-gathering id progress-key reward-emoji)
             :on-touch-start (fn [ev] (.preventDefault ev) (start-gathering id progress-key speed))
             :on-touch-end #(stop-gathering id progress-key reward-emoji)}
       [e (if (>= (get @state progress-key) 100) reward-emoji base-emoji)]]])])

(defn component:app [_state]
  [:<>
   [component:hud]
   [component:inventory]
   (if-not (:started @state)
     [:section {:class "slide home"}
      [:h1 [e "👇️"]]
      [:p
       [:button {:class "cta"
                 :on-click #(swap! state assoc :started true)}
        [e "🎮️"]]]]
     [:<>
      [component:buy-slide "🥔"]
      [component:gather-slide "tree" :tree-progress "🌳" "🪵" 2]
      [component:catch-slide :fishing "🎣" "🐟"]
      [component:buy-slide "🥕"]
      [component:gather-slide "rock" :rock-progress "🪨" "💎" 0.8]
      [component:catch-slide :hunting "🐗" "🍗"]])])
      [component:buy-slide "🍅"]

(rdom/render [component:app state]
          (.getElementById js/document "app"))
