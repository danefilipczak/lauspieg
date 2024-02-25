(ns lauspieg.state)

(defonce ** (atom {}))

(defn swap**! [f & args]
  (apply swap! ** f args))

;;todo full spec of appstate, make validating function within swapp**!
;; can this be done via add-watch?