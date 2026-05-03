(ns main
  (:require
    [reagent.core :as r]
    [reagent.dom :as rdom]))

(def state
  (r/atom
    {:coins 99
     :inventory {"🥔" 0 "🪵" 0 "🥕" 0 "🍅" 0}
     :slots {"🥔" [nil nil nil]
             "🥕" [nil nil nil nil nil]
             "🍅" [nil nil nil nil nil nil nil]}
     :tree-progress 0}))

(def hold-interval (atom nil))

(js/console.log @state)

(defn e [c]
  [:span {:class "emoji"} c])

(defn start-chopping []
  (when-not @hold-interval
    (reset! hold-interval
            (js/setInterval
             (fn []
               (swap! state update :tree-progress
                      (fn [p]
                        (if (>= p 100)
                          100
                          (+ p 2)))))
             20))))

(defn stop-chopping []
  (when @hold-interval
    (js/clearInterval @hold-interval)
    (reset! hold-interval nil))
  (when (>= (:tree-progress @state) 100)
    (swap! state update-in [:inventory "🪵"] inc))
  (swap! state assoc :tree-progress 0))

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
     (let [buy-slide
           (fn [emoji]
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
               [e emoji]]])]
       [:<>
        (buy-slide "🥔")
        ^{:key "tree"}
        [:section {:class "slide big"}
         [:div {:class "progress-container"}
          [:div {:class "progress-bar" :style {:width (str (:tree-progress @state) "%")}}]]
         [:div {:style {:cursor "pointer"}
                :on-mouse-down start-chopping
                :on-mouse-up stop-chopping
                :on-mouse-leave stop-chopping
                :on-touch-start (fn [ev] (.preventDefault ev) (start-chopping))
                :on-touch-end stop-chopping}
          [e (if (>= (:tree-progress @state) 100) "🪵" "🌳")]]]
        (buy-slide "🥕")
        (buy-slide "🍅")]))])

(rdom/render [component:app state]
          (.getElementById js/document "app"))
