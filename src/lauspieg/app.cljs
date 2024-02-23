(ns lauspieg.app
  (:require [rum.core :as rum]
            [lauspieg.viewer]
            [lauspieg.container :as container]))

(rum/defc hello-rum [text]
  [:div text])

(defn mount []
  #_(rum/mount (lauspieg.viewer/root) (js/document.getElementById "root"))
  (rum/mount (lauspieg.container/root) (js/document.getElementById "root"))
  )

(defn ^:dev/after-load re-render []
  (mount))


(defn init [] 
  (println "Hello World") 
  (mount))

(comment
  
  (js/alert "hi")
  )