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
     :cooldowns {}
     :flying-coins {}
     :flying-items {}
     :ghosts {}
     :particles {}
     :last-click {}}))

(def hold-interval (atom nil))

(defonce animation-interval-ref
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
                               (let [current (if (map? v) (:current v) v)
                                     max-val (if (map? v) (:max v) v)
                                     new-v (- current 0.1)]
                                 (if (> new-v 0)
                                   (assoc m k {:current new-v :max max-val})
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

(defn get-event-coords [ev fallback-id]
  (let [cx (when ev
             (or (.-clientX ev)
                 (when (and (.-changedTouches ev) (> (.-length (.-changedTouches ev)) 0))
                   (.-clientX (aget (.-changedTouches ev) 0)))
                 (when (and (.-touches ev) (> (.-length (.-touches ev)) 0))
                   (.-clientX (aget (.-touches ev) 0)))))
        cy (when ev
             (or (.-clientY ev)
                 (when (and (.-changedTouches ev) (> (.-length (.-changedTouches ev)) 0))
                   (.-clientY (aget (.-changedTouches ev) 0)))
                 (when (and (.-touches ev) (> (.-length (.-touches ev)) 0))
                   (.-clientY (aget (.-touches ev) 0)))))]
    (if (and cx cy)
      [cx cy]
      (let [el (when fallback-id (js/document.getElementById fallback-id))
            rect (when el (.getBoundingClientRect el))]
        (if rect
          [(+ (.-left rect) (/ (.-width rect) 2)) (+ (.-top rect) (/ (.-height rect) 2))]
          [(/ (.-innerWidth js/window) 2) (/ (.-innerHeight js/window) 2)])))))

(defn animate-item! [sx sy emoji cb]
  (let [id (str (js/Math.random))]
    (swap! state assoc-in [:flying-items id] {:x sx :y sy :tx sx :ty sy :emoji emoji :duration 0.5})
    (js/requestAnimationFrame
     (fn []
       (js/requestAnimationFrame
        (fn []
          (let [dest-el (js/document.getElementById (str "inventory-" emoji))
                rect (when dest-el (.getBoundingClientRect dest-el))
                ex (if rect (+ (.-left rect) (/ (.-width rect) 2)) (/ (.-innerWidth js/window) 2))
                ey (if rect (+ (.-top rect) (/ (.-height rect) 2)) (.-innerHeight js/window))]
            (swap! state update-in [:flying-items id] assoc :tx ex :ty ey))))))
    (js/setTimeout
     (fn []
       (swap! state update :flying-items dissoc id)
       (when cb (cb)))
     500)))

(defn spawn-ghost! [el-id emoji]
  (let [el (js/document.getElementById el-id)
        rect (when el (.getBoundingClientRect el))
        cx (if rect (+ (.-left rect) (/ (.-width rect) 2)) (/ (.-innerWidth js/window) 2))
        cy (if rect (+ (.-top rect) (/ (.-height rect) 2)) (/ (.-innerHeight js/window) 2))
        id (str (js/Math.random))]
    (swap! state assoc-in [:ghosts id] {:x cx :y cy :emoji emoji})
    (js/setTimeout #(swap! state update :ghosts dissoc id) 2000)))

(def particle-emojis ["✨" "⭐" "🌟" "💫" "💥"])

(defn spawn-particles! [cx cy]
  (let [n 12
        new-particles (into {}
                            (for [_ (range n)
                                  :let [id (str (js/Math.random))]]
                              [id {:x cx
                                   :y cy
                                   :emoji (rand-nth particle-emojis)
                                   :jump (+ (* (js/Math.random) 2) 0.5)
                                   :direction (* (- (js/Math.random) 0.5) 2)
                                   :spin (- (js/Math.random) 0.5)
                                   :size (+ (* (js/Math.random) 0.5) 1.0)
                                   :delay (* (js/Math.random) 1)}]))]
    (swap! state update :particles merge new-particles)
    (js/setTimeout #(swap! state update :particles (fn [ps] (reduce dissoc ps (keys new-particles)))) 2500)))

(defn stop-gathering [_ev id progress-key reward-emoji cooldown-time]
  (when @hold-interval
    (js/clearInterval @hold-interval)
    (reset! hold-interval nil))
  (let [progress (get @state progress-key)]
    (when (> progress 0)
      (if (>= progress 100)
        (let [el (js/document.getElementById (str "progress-" id))
              rect (when el (.getBoundingClientRect el))
              px (if rect (.-right rect) (/ (.-innerWidth js/window) 2))
              py (if rect (+ (.-top rect) (/ (.-height rect) 2)) (/ (.-innerHeight js/window) 2))]
          (swap! state update-in [:inventory reward-emoji] inc)
          (swap! state assoc-in [:cooldowns id] {:current cooldown-time :max cooldown-time})
          (spawn-particles! px py)
          (animate-item! px py reward-emoji nil))
        (spawn-ghost! (str "progress-" id) reward-emoji))))
  (swap! state assoc progress-key 0))

(defn handle-catch [_ev game-id reward-emoji cooldown-time]
  (let [game (get-in @state [:catch-games game-id])
        pos (:pos game)
        start (:target-start game)
        end (+ start (:target-width game))
        on-cooldown? (get-in @state [:cooldowns game-id])]
    (when-not on-cooldown?
      (if (and (>= pos start) (<= pos end))
        (let [el (js/document.getElementById (str "catch-" (name game-id)))
              rect (when el (.getBoundingClientRect el))
              target-center (+ start (/ (:target-width game) 2))
              px (if rect (+ (.-left rect) (* (.-width rect) (/ target-center 100.0))) (/ (.-innerWidth js/window) 2))
              py (if rect (+ (.-top rect) (/ (.-height rect) 2)) (/ (.-innerHeight js/window) 2))]
          (swap! state update-in [:inventory reward-emoji] inc)
          (spawn-particles! px py)
          (animate-item! px py reward-emoji nil))
        (spawn-ghost! (str "catch-" (name game-id)) reward-emoji))
      (swap! state assoc-in [:cooldowns game-id] {:current cooldown-time :max cooldown-time}))))

(defonce slot-timeouts (atom {}))

(defn clear-slot-timeout [emoji]
  (when-let [tid (get @slot-timeouts emoji)]
    (js/clearTimeout tid)
    (swap! slot-timeouts dissoc emoji)))

(defn animate-coin! [sx sy ex ey duration cb]
  (let [id (str (js/Math.random))]
    (swap! state assoc-in [:flying-coins id] {:x sx :y sy :tx sx :ty sy :duration duration})
    (js/requestAnimationFrame
     (fn []
       (js/requestAnimationFrame
        (fn []
          (swap! state update-in [:flying-coins id] assoc :tx ex :ty ey)))))
    (js/setTimeout
     (fn []
       (swap! state update :flying-coins dissoc id)
       (cb))
     (* duration 1000))))

(defn refund-coins [emoji]
  (let [curr @state
        slots (get-in curr [:slots emoji])
        last-click (get-in curr [:last-click emoji] {:x (/ (.-innerWidth js/window) 2) :y (/ (.-innerHeight js/window) 2)})
        cx (:x last-click)
        cy (:y last-click)
        inserted-count (count (remove nil? slots))]
    (when (> inserted-count 0)
      (spawn-ghost! (str "slots-" emoji) emoji))
    (swap! state assoc-in [:slots emoji] (vec (repeat (count slots) nil)))
    (swap! slot-timeouts dissoc emoji)
    (doseq [[idx slot-val] (map-indexed vector slots)]
      (when slot-val
        (let [slot-id (str "slot-" emoji "-" idx)
              slot-el (js/document.getElementById slot-id)
              rect (when slot-el (.getBoundingClientRect slot-el))
              sx (if rect (+ (.-left rect) (/ (.-width rect) 2)) cx)
              sy (if rect (+ (.-top rect) (/ (.-height rect) 2)) cy)
              duration (+ 0.25 (* (js/Math.random) 0.25))]
          (animate-coin! sx sy cx cy duration
            (fn []
              (swap! state update :coins inc))))))))

(defn set-slot-timeout [emoji]
  (clear-slot-timeout emoji)
  (swap! slot-timeouts assoc emoji
         (js/setTimeout #(refund-coins emoji) 1500)))

(defn handle-click [ev emoji cooldown-time]
  (let [current-state @state
        coins (:coins current-state)
        slots (get-in current-state [:slots emoji])
        on-cooldown? (get-in current-state [:cooldowns emoji])
        [cx cy] (get-event-coords ev (str "slots-" emoji))]
    (swap! state assoc-in [:last-click emoji] {:x cx :y cy})
    (when (and (> coins 0) (some nil? slots) (not on-cooldown?))
      (let [slot-index (.indexOf slots nil)
            slot-id (str "slot-" emoji "-" slot-index)
            slot-el (js/document.getElementById slot-id)
            rect (when slot-el (.getBoundingClientRect slot-el))
            ex (if rect (+ (.-left rect) (/ (.-width rect) 2)) cx)
            ey (if rect (+ (.-top rect) (/ (.-height rect) 2)) cy)]
        (swap! state
               (fn [s]
                 (-> s
                     (update :coins dec)
                     (assoc-in [:slots emoji slot-index] :pending))))
        (set-slot-timeout emoji)
        (animate-coin! cx cy ex ey 0.5
          (fn []
            (let [curr @state
                  curr-slots (get-in curr [:slots emoji])]
              (when (= :pending (nth curr-slots slot-index nil))
                (let [new-slots (assoc curr-slots slot-index "🪙")
                      is-full? (not (some #(not= % "🪙") new-slots))]
                  (if is-full?
                    (do
                      (clear-slot-timeout emoji)
                      (swap! state
                             (fn [s]
                               (-> s
                                   (update-in [:inventory emoji] (fnil inc 0))
                                   (assoc-in [:slots emoji] (vec (repeat (count curr-slots) nil)))
                                   (assoc-in [:cooldowns emoji] {:current cooldown-time :max cooldown-time}))))
                      (spawn-particles! ex ey)
                      (animate-item! ex ey emoji nil))
                    (do
                      (set-slot-timeout emoji)
                      (swap! state assoc-in [:slots emoji] new-slots))))))))))))

(defn component:hud []
  [:div {:class "hud"} "🪙 " (:coins @state)])

(defn component:inventory []
  [:div {:class "inventory-hud"}
   (for [[item count] (:inventory @state)
         :when (> count 0)]
     ^{:key item}
     [:div {:class "inventory-item" :id (str "inventory-" item)}
      [e item]
      (when (> count 1)
        [:div {:class "badge"} count])])])

(defn component:cooldown-indicator [id emoji-char]
  (let [cd-val (get-in @state [:cooldowns id])
        current (if (map? cd-val) (:current cd-val) cd-val)
        max-val (if (map? cd-val) (:max cd-val) 100)
        radius 40
        circumference (* 2 js/Math.PI radius)
        offset (- (* circumference (- 1 (/ current max-val))))]
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

(defn component:buy-slide [emoji cooldown-time]
  ^{:key emoji}
  [:section {:class "slide big"}
   (if (get-in @state [:cooldowns emoji])
     [component:cooldown-indicator emoji emoji]
     [:<>
      [:div {:class "slots" :id (str "slots-" emoji)}
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
                        style {:transform (str "translateY(" y-offset "em)")}
                        slot-id (str "slot-" emoji "-" (+ (* row-idx slots-per-row) col-idx))]
                    (if (= slot "🪙")
                      [:span
                       {:class "emoji filled" :style style :key col-idx :id slot-id}
                       "🪙"]
                      [:span
                       {:class "emoji" :style style :key col-idx :id slot-id}
                       "⚪"])))
                row-vec))])
          rows))]
      [:div {:style {:cursor "pointer"}
             :on-click #(handle-click % emoji cooldown-time)}
       [e emoji]]])])

(defn component:catch-slide [game-id display-emoji reward-emoji cooldown-time]
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
                :id (str "catch-" (name game-id))
                :on-mouse-down #(handle-catch % game-id reward-emoji cooldown-time)
                :on-touch-start (fn [ev] (.preventDefault ev) (handle-catch ev game-id reward-emoji cooldown-time))}
          [:div {:class "catch-target"
                 :style {:left (str (:target-start game) "%")
                         :width (str (:target-width game) "%")}}]
          [:div {:class "catch-indicator"
                 :style {:left (str (:pos game) "%")}}]]]
        [:div {:style {:cursor "pointer"}
               :on-mouse-down #(handle-catch % game-id reward-emoji cooldown-time)
               :on-touch-start (fn [ev] (.preventDefault ev) (handle-catch ev game-id reward-emoji cooldown-time))}
         [e display-emoji]]])]))

(defn component:gather-slide [id progress-key base-emoji reward-emoji speed cooldown-time]
  ^{:key id}
  [:section {:class "slide big"}
   (if (get-in @state [:cooldowns id])
     [component:cooldown-indicator id base-emoji]
     [:<>
      [:div {:class "progress-container" :id (str "progress-" id)}
       [:div {:class "progress-bar" :style {:width (str (get @state progress-key) "%")}}]]
      [:div {:style {:cursor "pointer"}
             :on-mouse-down #(start-gathering id progress-key speed)
             :on-mouse-up #(stop-gathering % id progress-key reward-emoji cooldown-time)
             :on-mouse-leave #(stop-gathering % id progress-key reward-emoji cooldown-time)
             :on-touch-start (fn [ev] (.preventDefault ev) (start-gathering id progress-key speed))
             :on-touch-end #(stop-gathering % id progress-key reward-emoji cooldown-time)}
       [e (if (>= (get @state progress-key) 100) reward-emoji base-emoji)]]])])

(defn component:flying-coins []
  [:div {:class "flying-coins-layer"}
   (for [[id coin] (:flying-coins @state)]
     ^{:key id}
     [:div {:class "flying-coin"
            :style {:left (str (:x coin) "px")
                    :top (str (:y coin) "px")
                    :transition (str "transform " (:duration coin) "s ease-in")
                    :transform (str "translate(" (- (:tx coin) (:x coin)) "px, " (- (:ty coin) (:y coin)) "px)")}}
      "🪙"])])

(defn component:flying-items []
  [:div {:class "flying-items-layer"}
   (for [[id item] (:flying-items @state)]
     ^{:key id}
     [:div {:class "flying-item"
            :style {:left (str (:x item) "px")
                    :top (str (:y item) "px")
                    :transition (str "transform " (:duration item) "s ease-in")
                    :transform (str "translate(" (- (:tx item) (:x item)) "px, " (- (:ty item) (:y item)) "px)")}}
      [e (:emoji item)]])])

(defn component:ghosts []
  [:div {:class "ghost-layer"}
   (for [[id ghost] (:ghosts @state)]
     ^{:key id}
     [:div {:class "ghost-wrapper"
            :style {:left (str (:x ghost) "px")
                    :top (str (:y ghost) "px")}}
      [:div {:class "ghost-emoji"} [e (:emoji ghost)]]])])

(defn component:particles []
  [:div {:class "particle-layer"}
   (for [[id p] (:particles @state)]
     ^{:key id}
     [:div {:class "particle-wrapper"
            :style {:left (str (:x p) "px")
                    :top (str (:y p) "px")
                    "--particle-jump" (:jump p)
                    "--particle-direction" (:direction p)
                    "--particle-spin" (:spin p)
                    "--particle-size" (:size p)
                    "--particle-delay" (str (:delay p) "s")}}
      [:div {:class "particle-inner"} [e (:emoji p)]]])])

(defn component:app [_state]
  [:<>
   [component:particles]
   [component:ghosts]
   [component:flying-items]
   [component:flying-coins]
   [component:hud]
   [component:inventory]
   (if-not (:started @state)
     [:section {:class "slide home"}
      [:h1 [e "👇️"]]
      [:p
       [:button {:class "cta"
                 :on-click #(swap! state assoc :started true)}
        [e "🎯"]]]]
     [:<>
      [component:buy-slide "🥔" 56.25]
      [component:gather-slide "tree" :tree-progress "🌳" "🪵" 2 56.25]
      [component:catch-slide :fishing "🎣" "🐟" 56.25]
      [component:buy-slide "🥕" 112.5]
      [component:gather-slide "rock" :rock-progress "🪨" "💎" 0.8 112.5]
      [component:catch-slide :hunting "🐗" "🍗" 112.5]
      [component:buy-slide "🍅" 112.5]])])

(rdom/render [component:app state]
          (.getElementById js/document "app"))
