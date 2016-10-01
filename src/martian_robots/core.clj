(ns martian-robots.core
  (:gen-class))

(def directions {:N [ 1  0]
                 :E [ 0  1]
                 :S [-1  0]
                 :W [ 0 -1]})

(def rotations {:N [:W :E]
                :E [:N :S]
                :S [:E :W]
                :W [:S :N]})

(defn- move [{:keys [location direction] :as robot} op]
  (let [offset (get directions direction)]
    (->> location
         (interleave offset)
         (partition 2)
         (map (partial apply op))
         (assoc robot :location))))

(defn move-forward [robot]
  (move robot +))

(defn- rotate [robot direction]
  (->> (get-in rotations [(:direction robot) direction])
       (assoc robot :direction)))

(defn rotate-left [robot]
  (rotate robot 0))

(defn rotate-right [robot]
  (rotate robot 1))

(def instruction-fn {:F move-forward
                     :R rotate-right
                     :L rotate-left})

(defn wrong-location? [{:keys [size] :as planet} location]
  (or (some #(< % 0) location)
      (->> location
           (interleave size)
           (partition 2)
           (map (fn [[size coordinate]]
                  (> coordinate size)))
           (some true?))
      false))

(defn robot-lost? [planet {:keys [location] :as robot}]
  (wrong-location? planet location))

(defn fence? [{:keys [fences] :as planet} location]
  (contains? fences location))

(defn check-lost-robot [planet {:keys [location] :as robot} execution-result]
  (if (robot-lost? planet execution-result)
    (assoc robot :lost (not (fence? planet location)))
    execution-result))

(defn execution-fn [{:keys [lost] :as robot} instruction]
  (if-not lost
    (get instruction-fn instruction)
    identity))

(defn execute-instruction [planet robot instruction]
  (if-let [execute (execution-fn robot instruction)]
    (->> robot
         execute
         (check-lost-robot planet robot))
    (do
      (println "What do you mean by '" instruction "' ?")
      robot)))

(defn run-robot [planet {:keys [instructions] :as robot}]
  (reduce (partial execute-instruction planet)
          robot
          instructions))

(defn add-fence [planet {:keys [lost location] :as robot}]
  (if lost
    (update-in planet [:fences] conj location)
    planet))

(defn run-planet [{:keys [robots] :as planet}]
  (let [result-planet (assoc planet :robots [])]
    (reduce (fn [planet robot]
              (let [result-robot (run-robot planet robot)]
                (-> planet
                    (update-in [:robots] conj result-robot)
                    (add-fence result-robot))))
            result-planet
            robots)))

(defn inspect-planet [planet description]
  (println "== " description " ==")
  (println "Size:  " (:size planet))
  (println "Fences:" (:fences planet))
  (println "Robots:")
  (doseq [robot (:robots planet)]
    (println " - Name:         " (:name robot))
    (println " - Location:     " (:location robot))
    (println " - Direction:    " (:direction robot))
    (println " - Instructions: " (:instructions robot))
    (when (:lost robot) (println " - LOST!"))
    (println ""))
  (println "======================\n")
  planet)

(def mars {:size   [5 3]
           :fences #{}
           :robots [{:name         "Bender"
                     :location     [1 1]
                     :direction    :E
                     :instructions [:R :F :R :F :R :F :R :F]}
                    {:name         "Calculon"
                     :location     [3 2]
                     :direction    :N
                     :instructions [:F :R :R :F :L :L :F :F :R :R :F :L :L]}
                    {:name         "Donbot"
                     :location     [0 3]
                     :direction    :W
                     :instructions [:L :L :F :F :F :L :F :L :F :L]}]})

(defn -main
  [& args]
  (-> mars
      (inspect-planet "Initial state")
      run-planet
      (inspect-planet "Final state")))
