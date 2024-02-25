(ns lauspieg.container
  (:require [rum.core :as rum]
            [lauspieg.state :refer [**]]
            [lauspieg.viewer]))

(rum/defcs jared < rum/reactive (rum/local nil ::start)
  [s size update-size orientation container-tag separator-tag before?]
  (assert (#{:vertical :horizonal} orientation))
  (let [start (::start s)
        coordinate (fn [e] (if (= :vertical orientation)
                             (.-clientX e)
                             (.-clientY e)))
        listen (fn [e]
                 (.stopPropagation e)
                 (.preventDefault e)
                 (let [delta (- (coordinate e) @start)]
                   (update-size ((if before? - +) size delta))))]
    (letfn [(mouseup [e]
              (.stopPropagation e)
              (.preventDefault e)
              (.removeEventListener
               js/document
               "mousemove"
               listen)
              (.removeEventListener
               js/document
               "mouseup"
               mouseup))]
      (cond->> [[container-tag #_:div.left-container.column-container.side-container
                {:style {(if (= :vertical orientation) :width :height) size}}]
               [separator-tag #_:div.container-separator.column-container.vertical-separator
                {:onMouseDown (fn [e]
                                (.stopPropagation e)
                                (.preventDefault e)
                                (reset! start (coordinate e))
                                (.addEventListener js/document "mousemove" listen)
                                (.addEventListener
                                 js/document
                                 "mouseup"
                                 mouseup))}]]
        before? reverse
        :always (into [:<>]))))
  )

;;todo - this is what I wanted to write
#_(def *left-width
    (state/register [::left-width] 200 :malli-schema :conform-fn))
(def *left-width (rum/cursor-in ** [::left-width]))

(rum/defc left < rum/reactive
  [] 
  (jared
   (or (rum/react *left-width) 250) ;;todo remove me
   (fn [width]
     (reset! *left-width width))
   :vertical
   :div.left-container.column-container.side-container
   :div.container-separator.column-container.vertical-separator
   false))

(def *right-width (rum/cursor-in ** [::right-width]))

(rum/defc right < rum/reactive
  [] 
  (jared
   (or (rum/react *right-width) 250) ;;todo remove me
   (fn [width]
     (reset! *right-width width))
   :vertical
   :div.right-container.column-container.side-container
   :div.container-separator.column-container.vertical-separator
   true))

(def *bottom-height (rum/cursor-in ** [::bottom-height]))

(rum/defc bottom < rum/reactive
  []
  (jared
   (or (rum/react *bottom-height) 250) ;;todo remove me
   (fn [width]
     (reset! *bottom-height width))
   :horizonal
   :div.bottom-container
   :div.container-separator.horizontal-separator
   true))

(rum/defc arrow
  [direction]
  (let [deg (case direction
              :up 360
              :down 180
              :left 270
              :right 90)]
    [:svg {:xmlns "http://www.w3.org/2000/svg"
           :viewBox "0 0 340 512"
           :width 30
           :height 30 
           :style {:cursor :pointer}}
     [:path {:d "M182.6 137.4c-12.5-12.5-32.8-12.5-45.3 0l-128 128c-9.2 9.2-11.9 22.9-6.9 34.9s16.6 19.8 29.6 19.8H288c12.9 0 24.6-7.8 29.6-19.8s2.2-25.7-6.9-34.9l-128-128z"
             :style {:transform (str "rotate(" deg "deg)")
                     :transform-origin "50% 50%"
                     #_#_"transition" "transform 0.5s ease 0s"
                     }}]]))

(def *left-open? (rum/cursor-in ** [::left-open?]))
(def *right-open? (rum/cursor-in ** [::right-open?]))
(def *bottom-open? (rum/cursor-in ** [::bottom-open?]))

(rum/defc root < rum/reactive
  []
  (let [#_#__ (rum/use-effect!
               (fn []
                 (.addEventListener js/document "keydown" js/console.log)
                 #(.removeEventListener js/document "keydown" js/console.log))
               [])] 
    [:div.page-container
     [:div {:style {:position :absolute
                    :top 0
                    :line-height 0
                    :left 0}
            :on-click #(swap! *left-open? not)} 
      (arrow (if (rum/react *left-open?) :right :down))] 
     [:div {:style {:position :absolute
                    :top 0
                    :line-height 0
                    :right 0}
            :on-click #(swap! *right-open? not)}
      (arrow (if (rum/react *right-open?) :left :down))]
     [:div {:style {:position :absolute
                    :bottom 0
                    :line-height 0
                    :right 0}
            :on-click #(swap! *bottom-open? not)}
      (arrow (if (rum/react *bottom-open?) :up :left))]
     [:div.top-container
    ;;   <svg 
    ;;   xmlns="http://www.w3.org/2000/svg" 
    ;;   viewBox="0 0 320 512"> 
    ;;   <path d="M182.6 137.4c-12.5-12.5-32.8-12.5-45.3 0l-128 128c-9.2 9.2-11.9 22.9-6.9 34.9s16.6 19.8 29.6 19.8H288c12.9 0 24.6-7.8 29.6-19.8s2.2-25.7-6.9-34.9l-128-128z"/></svg>
      (if (rum/react *left-open?)
        (left)
        [:div {:style {:width 30
                       :border-right "1px solid"}}])
      [:div.center-container.column-container
       (lauspieg.viewer/root)]
      (if (rum/react *right-open?)
        (right)
        [:div {:style {:width 30
                       :border-left "1px solid"}}])]
     (if (rum/react *bottom-open?)
       (bottom)
       [:div {:style {:height 30
                      :border-top "1px solid"}}])]))