(defn exception->map
  [^Throwable ex-obj]
  (merge
    {:type (str (type ex-obj))
     :message (str (.getMessage ex-obj))
     :stack-trace (mapv str (.getStackTrace ex-obj))}
    (when-let [cause (.getCause ex-obj)]
      {:cause (exception->map cause)})))

(def debug (atom []))

(fn [state time-left]
  (try
    (let [posn (:global-coords state)
          _ (println posn)
          [real-x real-y] posn
          x 3
          y 3
          arena (:arena state)
          {:keys [orientation hp]} (get-in arena [y x :contents])
          neighs (fn [[x y]]
                   #{[(inc x) y]
                     [(dec x) y]
                     [x (inc y)]
                     [x (dec y)]})
          best-path
          (loop [q (conj (clojure.lang.PersistentQueue/EMPTY) [[x y]])
                 visited #{}]
            (if (empty? q)
              nil
              (let [path (peek q)
                    [x' y'] (peek path)
                    q (pop q)
                    visited' (conj visited [x' y'])
                    type (get-in arena [y' x' :contents :type])]
                (cond
                  (visited [x' y']) (recur q visited')
                  (= "food" type) path
                  (not (or (= "open" type)
                           (= [x y] [x' y']))) (recur q visited')
                  :else
                  (recur
                   (into q (for [p (neighs [x' y'])]
                             (conj path p)))
                   visited')))))]
      (if best-path
        (let [[x' y'] (second best-path)
              delta [(- x' x) (- y' y)]
              delta->dir {[1 0] "e" [0 1] "s" [-1 0] "w" [0 -1] "n"}
              turn-action
              (case [orientation (delta->dir delta)]
                (["e" "s"] ["s" "w"] ["w" "n"] ["n" "e"]) :right
                (["e" "w"] ["s" "n"] ["w" "e"] ["n" "s"]) :about-face
                (["e" "n"] ["n" "w"] ["w" "s"] ["s" "e"]) :left
                nil)]
          {:command
           (if turn-action
             {:action :turn
              :metadata {:direction turn-action}}
             {:action :move
              :metadata {}})
           :state
           {:path best-path}})
        (rand-nth
          [{:command {:action :move
                      :metadata {}}
            :state {}}
           {:command {:action :turn
                      :metadata {:direction :left}}
            :state {}}])))
    (catch Throwable t
      {:command
       {:action :move
        :metadata {}}
       :state
       {:exception (exception->map t)
        :debugger @debug}})))
