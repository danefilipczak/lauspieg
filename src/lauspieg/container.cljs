(ns lauspieg.container
  (:require [rum.core :as rum]))

(rum/defc root
  []
  [:div.page-container
   [:div.top-container
    (when true
      [:<>
       [:div.left-container.column-container.side-container]
       [:div.container-separator.column-container]])
    [:div.center-container.column-container]
    (when true
      [:<>
       [:div.container-separator.column-container]
       [:div.right-container.column-container.side-container]])]
   (when true
     [:div.bottom-container])])