(ns lauspieg.app
  (:require [rum.core :as rum]
            [lauspieg.viewer]))

(rum/defc hello-rum [text]
  [:div text])

(defn mount []
  (rum/mount (lauspieg.viewer/root) (js/document.getElementById "root"))
  #_(r.dom/render [lauspieg.viewer/root] ))

(defn ^:dev/after-load re-render []
  (mount))


(defn init [] 
  (println "Hello World") 
  (mount))

(comment
  
  (js/alert "hi")
  )