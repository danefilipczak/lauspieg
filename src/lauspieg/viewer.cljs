(ns lauspieg.viewer
  (:require [clojure.pprint] 
            [geilausp.utils.math :as utils.math]
            [geilausp.ast.ast :as ast]
            [rational.core :as r]
            [react :as react] 
            [rum.core :as rum]))

(rum/defc pprint [x]
  [:pre
   (with-out-str (clojure.pprint/pprint x))])

(defn spy [x]
  (.log js/console (print-str x))
  x)
(defn pitch-set [x]
  ;;eventually pitch/pitch-set, but that isn't stable at this time because the protocol implememntation isn't clj-safe
  ;;
  (cond (set? x)
        x
        (number? x)
        #{x}
        :else #{}))

(defn nearest-era [ast path]
  (if (ast.era? (get-in ast (ast/ast-path path)))
    (get-in ast (ast/ast-path path))
    (get-in ast (ast/ast-path (pop path)))))

(def highlight-color "#AADE91")
(def contrast-color #_:lightgrey :whitesmoke)

(rum/defc piano-roll [{:keys [width height era active-path]}]
  (let [ast (ast/standard-interpretation era)
        pitches (sort (mapcat (comp pitch-set ast/value) (ast/values ast)))
        min-pitch (first pitches)
        max-pitch (last pitches)
        stave (reverse (range min-pitch (inc max-pitch)))
        height-per-stave (float (/ height (count stave)))
        last-end (last (sort (map (comp r/realize ast/end) (ast/values ast))))
        rescale-tv-time (fn [tv-time] (float (utils.math/map-range (r/realize tv-time) 0 last-end 0 width)))
        nn->i (zipmap stave (range))
        nn->height (fn [nn]
                     (assert (int? nn))
                     (* (nn->i nn) height-per-stave))]
    [:svg {:width width :height height}
     (let [values (ast/values ast)
           {active true non-active false} (group-by #(ast/sub-path? active-path (:path %)) values)
           draw-things (fn [things active?]
                         (for [v things
                               :let [pitch-set (pitch-set (ast/value v))
                                     color (or (:color (ast/attrs v)) (when active? highlight-color))]
                               nn pitch-set
                               :when nn]
                           [:rect
                            {:key (print-str (conj (:path v) nn))
                             :stroke (if active? :black :gainsboro)
                             :stroke-width 0.5
                             :fill (or color :black)
                             :x (rescale-tv-time (ast/start v))
                             :y (nn->height nn)
                             :width (rescale-tv-time (ast/duration v))
                             :height height-per-stave}]))] 
       (concat (for [[i nn] (partition 2 (interleave (range) stave))]
                 [:rect
                  {:key nn
                   :stroke :gainsboro
                   :stroke-width 0.5
                   :fill (if (contains?
                              #{0 2 4 5 7 9 11}
                              (int (mod nn 12)))
                           :white
                           contrast-color)
                   :x 0
                   :y (nn->height nn)
                   :width width
                   :height height-per-stave}])
               (draw-things non-active false)
               (draw-things active true)))]))

(defn bracket [from to height]
  (let [attrs {:stroke :black}]
    [:g
     [:line
      (merge
       attrs
       {:x1 from
        :y1 0
        :x2 to
        :y2 0})]
     [:line
      (merge
       attrs
       {:x1 from
        :y1 0
        :x2 from
        :y2 height})]
     [:line
      (merge
       attrs
       {:x1 to
        :y1 0
        :x2 to
        :y2 height})]]))

(defn translate [x y & children]
  (into [:g {:transform (str "translate(" x " " y ")")}] children))

(defn extract-voices [node]
  (apply
   merge-with
   concat
   {(:voice node) [node]}
   (map extract-voices (ast/children node))))

(defn lexicographic-comparator [vector-a vector-b]
  (loop [v1 vector-a
         v2 vector-b]
    (cond
      (empty? v1) (if (empty? v2) 0 -1)
      (empty? v2) 1
      :else
      (let [cmp (compare (first v1) (first v2))]
        (if (zero? cmp)
          (recur (rest v1) (rest v2))
          cmp)))))

#_(defn resizable-div* [initial-height _component _attrs]
  (let [min-height 100
        size (reagent.core/atom {:width 0 :height initial-height})
        resize-observer (js/ResizeObserver. (fn [entries _]
                                              (when-let [entry (first entries)]
                                                (let [dom-rect (.-contentRect entry)]
                                                  entry
                                                  dom-rect
                                                  (reset! size {:width (.-width dom-rect)
                                                                :height (.-height dom-rect)})))))]
    (fn [_initial-height component attrs]
      (let [my-ref (react/useRef)]
        (react/useEffect
         (fn []
           (when (.-current my-ref)
             (.observe resize-observer (.-current my-ref)))
           (fn []
             (.disconnect resize-observer)))
         (clj->js []))

        [:div.vertical-resize
         {:ref my-ref
          :style {:height (:height @size)
                  :min-height min-height}}
         [component (merge attrs @size)]]))))

#_(defn resizable-div [initial-height component attrs]
  [:f> resizable-div* initial-height component attrs])

(defn inspector [{:keys [era active-path width] :as attrs}]
  (let [ast (ast/standard-interpretation era)
        parent-path (cond-> active-path (not-empty active-path) pop)
        parent-era (get-in ast (ast/ast-path parent-path))]
    [:div {:style {:width "100%"
                   :height "26px"
                   :position :relative}}
     (doall
      (for [child (ast/children parent-era)
            :let [display (cond
                            (ast.era? child) (:tag child)
                            (set? (ast/value child)) #{}
                            :else (ast/value child))
                  active? (= active-path (:path child))]]
        [:div
         {:style {:position :absolute
                  :top 0
                  :width (str (* 100 (r/realize (ast/duration child))) "%")
                  :left (str (* 100 (r/realize (ast/start child))) "%")
                  :text-decoration (when active? "underline")}}
         [:span
          {:style {:font-family "monospace" :font-size "12px"}}
          (print-str display)]]))]))

(rum/defc visualize-era [{:keys [era width] :as attrs}]
  [:<>
   #_[resizable-div 100 voice-tree attrs]
   ;; this inspector should instead be an excel-style single line read/edit window. 
   ;; the children of eras should be represented by ...n where n is the count of the children. clicking on that will expand it one level. 
   ;; trickily, the view should also be edit-able so that values can be manually entered.
   #_[inspector attrs] 
   (piano-roll (merge attrs {:width 500 :height 200}))])

(rum/defc buttons [ast active-path]
  [:<>
   [:button
    {:on-click #(when-let [d (ast/down ast @active-path)]
                  (reset! active-path d))
     :disabled (not (ast/down ast @active-path))}
    "down"]
   [:button
    {:on-click #(when-let [d (ast/up ast @active-path)]
                  (reset! active-path d))
     :disabled (not (ast/up ast @active-path))}
    "up"]
   [:button
    {:on-click #(when-let [d (ast/left ast @active-path)]
                  (reset! active-path d))
     :disabled (not (ast/left ast @active-path))}
    "left"]
   [:button
    {:on-click #(when-let [d (ast/right ast @active-path)]
                  (reset! active-path d))
     :disabled (not (ast/right ast @active-path))}
    "right"]])

(rum/defcs editor < (rum/local [] ::active-path) 
  [state era on-change]
  (let [active-path (::active-path state)]
    (let [ast (ast/standard-interpretation era)
          current-node (get-in ast (ast/ast-path @active-path))]
      [:<>
       (visualize-era
        {:era era
         :on-change on-change
         :active-path @active-path})
       (buttons ast active-path)
       [:pre (print-str @active-path)] 
       (pprint current-node)])))

(def era1 (atom [:era 2 3 [:chain #{4 5 6} #{4 5 6}] [:heap 2
                                                      [:era  2 3]
                                                      [2 3]
                                                      1 3 5 7 13]]))

(rum/defc root < rum/reactive []
  [:<>
   [:<>
    [:div.notebook
     [:h1 "UI ideas"]
     [:p "hello you tricky devil, is this thing on?"]]
    (editor (rum/react era1) (fn [new-era]
                               (reset! era1 new-era)))]
   #_[:<>
      [:div.notebook]
      [editor @era2 (fn [new-era]
                      (reset! era new-era))]
      [:pre (print-str @era2)]]
   #_[:<>
      [:div.notebook]
      [editor @era3 (fn [new-era]
                      (reset! era new-era))]
      [:pre (print-str @era3)]
      [:div.notebook
       [:h2 "Notes for the future"]
       [:p "If we can go no further right, try to go down"]
       [:p "If we can go no further left, try to go up"]]]])


(comment
  ;; todo -- navigation
  ;; cross siblings - 'horizontal'
  ;; accross levels - 'vertical'
  ;; the basic ops are up, down, left right

  ;; todo -- zooming
  ;; you can zoom up to the current level that you're at
  ;; moving to a sibling shifts the zoom to the sibling
  ;; moving to a parent shifts the zoom to the parent

  ;; value editing - read string a str, number, or set

  ;; cycle era type

  ;; todo - editing
  ;; slup
  ;; barf


  ;; transpose by semitone
  ;; variation - transpose by octave 


  (resizable-div
   (vizualize-era [[:graft {:color "green"} 2] 4 [5]]))

  (ast/standard-interpretation [1 2 3]))