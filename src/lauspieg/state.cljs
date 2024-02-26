(ns lauspieg.state
  (:require [rum.core :as rum])
  (:import goog.async.Debouncer))

(defonce ** (atom {}))

(defn swap**! [f & args]
  (apply swap! ** f args))

(defn spy [v]
  (println v)
  v)

;;todo full spec of appstate, make validating function within swapp**!
;; can this be done via add-watch?

#_(def *right-width (state/register
                     ::right-width
                     250
                     #_(derived-validator
                        [state/*vw]
                    ;; malli validation code for doing a derived validation
                        )
                     (derived-conformer
                      [state/*vw]
                      (fn [vw v]
                        (min (* (/ max-right-percentage 100) vw)
                             (max minimum-right-width v))))))

(defn derived-conformer 
  "atoms is a vector of watchable things.
   conforming-fn is a fn taking the derefed values of watchables, then the current value to be conformed."
  [atoms conforming-fn] 
  [::derived-conformer atoms conforming-fn])

(defn derived-conformer* [conformer key re-conform]
  (assert (= ::derived-conformer (first conformer)))
  (let [[_ atoms cfn] conformer 
        gen-fn (fn [] 
                 (fn [v]
                   (apply cfn (concat (map deref atoms) [(spy v)]))))
        reset-conformer (fn [_k _r _os _ns] 
                          (swap! ** assoc-in [key ::conformer] (gen-fn))
                          (re-conform))]
    (doseq [a atoms]
      (add-watch a (keyword (str (name key) "derived-conformer")) reset-conformer))
    ;; in addition to reseting the conformer, we need to actually conform the value
    (gen-fn)))

(defn register [key initial-value validator conformer]
  (let [cursor (rum/cursor-in ** [key ::value])
        reset-fn (fn [v] (reset! cursor ((get-in @** [key ::conformer]) v)))
        re-conform (fn [] (reset-fn (get-in @** [key ::value]))) 
        conforming-fn (if (fn? conformer)
                        conformer
                        (derived-conformer* conformer key re-conform))]
    (swap! ** assoc key {::value initial-value
                         ::validator validator
                         ::conformer conforming-fn}) 
    [cursor 
     reset-fn]))

#_(def *left-width
    (state/register [::left-width] 200 :malli-schema :conform-fn))

(def *vw (rum/cursor-in ** [::vw]))

(def *vh (rum/cursor-in ** [::vh]))

(defn reset-view* [] 
  (reset! *vw (.-innerWidth js/window))
  (reset! *vh (.-innerHeight js/window)))

(def rv-debouncer (Debouncer. reset-view* 300))
(defn reset-view [] (.fire rv-debouncer))

(defn init []
  (reset-view)
  (.addEventListener js/window "resize" reset-view))

(init)