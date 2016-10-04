(ns martian-robots.core
  (:gen-class))

(def directions
  "Offset for move the robot based on it's direction"
  {:N [ 0  1]
   :E [ 1  0]
   :S [ 0 -1]
   :W [-1 0]})

(def rotations
  "The key is the current direction and the values are the direction
   in which the robot will be if it turns left (first value) or
   right (second value)"
  {:N [:W :E]
   :E [:N :S]
   :S [:E :W]
   :W [:S :N]})

(defn- move
  "Apply a function to each pair of coordinates from the robot's location and
   the `directions` hashmap. It works with a 3D coordinate system with no
   code changes"
  [{:keys [location direction] :as robot} op]
  (let [offset (get directions direction)]
    (->> location
         (interleave offset)
         (partition 2)
         (map (partial apply op))
         (assoc robot :location))))

(defn move-forward
  [robot]
  (move robot +))

(defn- rotate [robot direction]
  (->> (get-in rotations [(:direction robot) direction])
       (assoc robot :direction)))

(defn rotate-left [robot]
  (rotate robot 0))

(defn rotate-right [robot]
  (rotate robot 1))

(def instruction-fn
  "Relation between the instructions and the handler functions"
  {:F move-forward
   :R rotate-right
   :L rotate-left})

(defn wrong-location?
  "It returns true if the location is outside the planet boundaries.
   It consider that the coordinates system could be more than 2
   dimensions, as for the `move` function"
  [{:keys [size] :as planet} location]
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

(defn check-lost-robot
  "It checks if the execution-result (the robot after the execution of the
   instruction) is outside the boundaries. If so it returns the initial
   state of the robot (before the execution of the instruction) and
   add the `:lost` field in case there isn't a fence in the location."
  [planet {:keys [location] :as robot} execution-result]
  (if (robot-lost? planet execution-result)
    (assoc robot :lost (not (fence? planet location)))
    execution-result))

(defn execution-fn
  "Returns the execution function the `instruction`.
   In case the robot is lost the `identity` function is returned
   so that no action are executed on the robot"
  [{:keys [lost] :as robot} instruction]
  (if-not lost
    (get instruction-fn instruction)
    identity))

(defn execute-instruction
  "Execute the instruction and check if the robot is lost.
   It prints a message in case the instruction is not understood
   and does not change the robot state"
  [planet robot instruction]
  (if-let [execute (execution-fn robot instruction)]
    (->> robot
         execute
         (check-lost-robot planet robot))
    (do
      (println "What do you mean by '" instruction "' ?")
      robot)))

(defn run-robot
  "It execute all the instructions for a robot and return the final state"
  [planet {:keys [instructions] :as robot}]
  (reduce (partial execute-instruction planet)
          robot
          instructions))

(defn add-fence [planet {:keys [lost location] :as robot}]
  "If the robot is lost it add a fence to the planet at the robot location"
  (if lost
    (update-in planet [:fences] conj location)
    planet))

(defn run-planet
  "It execute all the logic for every robots on the planet and update
   it's state on every execution"
  [{:keys [robots] :as planet}]
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
