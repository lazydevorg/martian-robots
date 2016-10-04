(ns martian-robots.core-test
  (:require [clojure.test :refer :all]
            [martian-robots.core :refer :all]))

(deftest move-forward-test
  (testing "move north"
    (is (= {:location [0 1]
            :direction :N}
           (move-forward {:location [0 0]
                          :direction :N}))))
  (testing "move east"
    (is (= {:location [1 0]
            :direction :E}
           (move-forward {:location [0 0]
                          :direction :E}))))
  (testing "move south"
    (is (= {:location [0 0]
            :direction :S}
           (move-forward {:location [0 1]
                          :direction :S}))))
  (testing "move west"
    (is (= {:location [0 0]
            :direction :W}
           (move-forward {:location [1 0]
                          :direction :W})))))

(deftest rotation-test
  (testing "rotate right from N to E"
    (is (= {:direction :E}
           (rotate-right {:direction :N}))))
  (testing "rotate left from W to S"
    (is (= {:direction :S}
           (rotate-left {:direction :W})))))

(deftest robot-lost-test
  (testing "robot not lost"
    (is (= false
           (robot-lost? {:size [5 5]}
                        {:location [4 4]}))))
  (testing "robot lost"
    (is (= true
           (robot-lost? {:size [5 5]}
                        {:location [5 6]})))
    (is (= true
           (robot-lost? {:size [5 5]}
                        {:location [-1 0]})))))

(deftest check-lost-robot-test
  (let [planet {:size [5 5]}]
    (testing "robot not lost"
      (let [robot {:location [5 5]}
            execution-result {:location [4 5]}]
        (is (= {:location [4 5]}
               (check-lost-robot planet robot execution-result)))))
    (testing "robot lost"
      (let [robot {:location [5 5]}
            execution-result {:location [6 5]}]
        (is (= {:location [5 5]
                :lost true}
               (check-lost-robot planet robot execution-result)))))
    (testing "robot lost because of a fence"
      (let [planet {:size [5 5]
                    :fences #{[5 2]}}
            robot {:location [5 2]}
            execution-result {:location [6 2]}]
        (is (= {:location [5 2]
                :lost false}
               (check-lost-robot planet robot execution-result)))))))

(deftest execution-fn-test
  (testing "get the forward function"
    (is (= move-forward
           (execution-fn {:lost false} :F))))
  (testing "get the identity function if lost"
    (is (= identity
           (execution-fn {:lost true} :F)))))

(deftest run-planet-test
  (let [planet {:size [5 5]
                :fences #{}
                :robots [{:location [5 5]
                          :direction :E
                          :instructions [:F]}
                         {:location [4 4]
                          :direction :N
                          :instructions [:F :R :F :F]}
                         {:location [2 3]
                          :direction :S
                          :instructions [:L :F :L :F :L :F :L :F]}]}]
    (is (= (run-planet planet)
           {:size [5 5]
            :fences #{[5 5]}
            :robots [{:location [5 5]
                      :direction :E
                      :instructions [:F]
                      :lost true}
                     {:location [5 5]
                      :direction :E
                      :instructions [:F :R :F :F]
                      :lost false}
                     {:location [2 3]
                      :direction :S
                      :instructions [:L :F :L :F :L :F :L :F]}]}))))
