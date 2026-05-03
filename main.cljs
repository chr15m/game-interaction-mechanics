(ns main
  (:require
    [reagent.core :as r]
    [reagent.dom :as rdom]))

(def state
  (r/atom
    {:coins 99
     :inventory {"🥔" 0 "🥕" 0 "🍅" 0}
     :slots {"🥔" [nil nil nil]
             "🥕" [nil nil nil nil nil]
             "🍅" [nil nil nil nil nil nil nil]}}))

(js/console.log @state)

(defn e [c]
  [:span {:class "emoji"} c])

(defn handle-click [emoji]
  (swap! state
         (fn [current-state]
           (let [coins (:coins current-state)
                 slots (get-in current-state [:slots emoji])]
             (if (and (> coins 0) (some nil? slots))
               (let [slot-index (.indexOf slots nil)
                     new-slots (assoc slots slot-index "🪙")
                     is-full? (not (some nil? new-slots))]
                 (if is-full?
                   (-> current-state
                       (update :coins dec)
                       (update-in [:inventory emoji] (fnil inc 0))
                       (assoc-in [:slots emoji] (vec (repeat (count slots) nil))))
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

(defn component:app [_state]
  [:<>
   [component:hud]
   [component:inventory]
   (if-not (:started @state)
     [:section {:class "slide home"}
      [:h1 [e "🥔"]]
      [:h2 "Harvest"]
      [:p
       [:button {:class "cta"
                 :on-click #(swap! state assoc :started true)}
        [e "▶"]]]]
     [:<>
      (for [emoji (keys (:slots @state))]
        ^{:key emoji}
        [:section {:class "slide big"}
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
         [e emoji]]])])])

(rdom/render [component:app state]
          (.getElementById js/document "app"))
