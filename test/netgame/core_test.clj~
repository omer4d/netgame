(ns netgame.core-test
  (:require [clojure.test :refer :all]
            [netgame.core :refer [def-net-struct write-bin read-bin write-diff apply-diff]])
  (:import [java.io DataInputStream DataOutputStream ByteArrayInputStream ByteArrayOutputStream]))

(defn simple-diff [type from to type-opts]
  (let [bos (new ByteArrayOutputStream)
        dos (new DataOutputStream bos)]
    (write-diff type dos from to type-opts)
    (. bos toByteArray)))

(defn simple-apply-diff [type old diff type-opts]
  (let [bis (new ByteArrayInputStream diff)
        dis (new DataInputStream bis)]
    (apply-diff type dis old type-opts)))

(defn simple-write-bin [type x type-opts]
  (let [bos (new ByteArrayOutputStream)
        dos (new DataOutputStream bos)]
    (write-bin type dos x type-opts)
    (. bos toByteArray)))

(defn simple-read-bin [type data type-opts]
  (let [bis (new ByteArrayInputStream data)
        dis (new DataInputStream bis)]
    (read-bin type dis type-opts)))

(defn bin-echo [type x type-opts]
  (simple-read-bin type (simple-write-bin type x type-opts) type-opts))

(def-net-struct Flat
  :net
  [(a int)
   (b short)
   (c short)]
  :server
  [x y (z int)])

(def-net-struct Nested
  :net
  [(x int)
   (y int)
   (z Flat)])

(def-net-struct Foo
  :net [(x int)])

(deftest test-delta-updates
  (let [from (Flat. 1 2 3 :foo :foo 777)
        to (Flat. 5 2 5 :bar :bar 888)
        cfrom1 (FlatClient. 1 2 3)
        cfrom2 (FlatClient. 0 0 0)
        diff (simple-diff 'Flat from to nil)]
    (testing "Flat struct diff"
      (is (= (count diff) 7))
      (is (= (simple-apply-diff 'Flat cfrom1 diff nil) (FlatClient. 5 2 5)))
      (is (= (simple-apply-diff 'Flat cfrom2 diff nil) (FlatClient. 5 0 5)))))
  
  (let [x (Flat. 1 2 3 0 0 0)
        y (Flat. 1 2 3 1 1 1)
        diff1 (simple-diff 'Flat x x nil)
        diff2 (simple-diff 'Flat x y nil)
        cfrom (FlatClient. 0 0 0)]
    (testing "Flat struct diff edge cases"
      (is (= (count diff1) 1))
      (is (= (count diff2) 1))
      (is (= (simple-apply-diff 'Flat cfrom diff1 nil) cfrom))
      (is (= (simple-apply-diff 'Flat cfrom diff2 nil) cfrom))))

  (let [from (Nested. 1 2 (Flat. 1 2 3 :foo :foo 777))
        to (Nested. 1 3 (Flat. 5 2 5 :bar :bar 888))
        cfrom (NestedClient. 1 2 (FlatClient. 1 2 3))
        diff (simple-diff 'Nested from to nil)]
    (testing "Nested struct diff"
      (is (= (count diff) 12))
      (is (= (simple-apply-diff 'Nested cfrom diff nil) (NestedClient. 1 3 (FlatClient. 5 2 5))))))
  
  (let [x (Nested. 1 2 (Flat. 1 2 3 :foo :foo 777))
        y (Nested. 1 2 (Flat. 1 2 3 :bar :bar 888))
        cfrom (NestedClient. 0 0 (FlatClient. 0 0 0))
        diff1 (simple-diff 'Nested x x nil)
        diff2 (simple-diff 'Nested x y nil)]
    (testing "Nested struct diff edge cases"
      (is (= (count diff1) 1))
      (is (= (count diff2) 2))
      (is (= (simple-apply-diff 'Nested cfrom diff1 nil) cfrom))
      (is (= (simple-apply-diff 'Nested cfrom diff2 nil) cfrom)))))

(deftest test-struct-full-updates
  (testing "Full update flat struct"
    (is (= (bin-echo 'Flat (Flat. 1 2 3 :foo :baz 777) nil) (FlatClient. 1 2 3))))
  (testing "Full update nested struct"
    (is (= (bin-echo 'Nested (Nested. 7 8 (Flat. 1 2 3 :foo :baz 777)) nil) (NestedClient. 7 8 (FlatClient. 1 2 3))))))

(deftest test-short-map-delta-updates
  (testing "short-map diff (deletion)"
    (let [from {1 (Foo. 1), 2 (Foo. 2), 3 (Foo. 3), 4 (Foo. 4)}
          from-client {1 (FooClient. 1), 2 (FooClient. 2), 3 (FooClient. 3), 4 (FooClient. 4)}
          to {}
          diff (simple-diff 'short-map from to {:elem-type 'Foo})]
      (is (= (count diff) 14))
      (is (= (simple-apply-diff 'short-map from-client diff {:elem-type 'Foo}) {}))))
  
  (testing "short-map diff (insertion)"
    (let [foo1 (Foo. 1)
          from {1 foo1}
          from-client {1 (FooClient. 1)}
          to {1 foo1, 2 (Foo. 2), 3 (Foo. 3)}
          diff (simple-diff 'short-map from to {:elem-type 'Foo})]
      (is (= (count diff) 18))
      (is (= (simple-apply-diff 'short-map from-client diff {:elem-type 'Foo}) {1 (FooClient. 1), 2 (FooClient. 2), 3 (FooClient. 3)}))))
  
  (testing "short-map diff (insert+delete+update)"
    (let [from {1 (Foo. 1), 2 (Foo. 2), 3 (Foo. 3), 4 (Foo. 4)}
          from-client {1 (FooClient. 1), 2 (FooClient. 2), 3 (FooClient. 3), 4 (FooClient. 4)}
          to (-> from (dissoc 2) (assoc 5 (Foo. 5)) (assoc 1 (Foo. 777)))
          diff (simple-diff 'short-map from to {:elem-type 'Foo})]
      (is (= (count diff) 21))
      (is (= (simple-apply-diff 'short-map from-client diff {:elem-type 'Foo}) {1 (FooClient. 777), 5 (FooClient. 5), 3 (FooClient. 3), 4 (FooClient. 4)})))))

(deftest test-short-map-full-update
  (testing "short-map full update"
    (is (= (bin-echo 'short-map {1 (Foo. 1), 2 (Foo. 2), 3 (Foo. 3), 4 (Foo. 4)} {:elem-type 'Foo})
           {1 (FooClient. 1), 2 (FooClient. 2), 3 (FooClient. 3), 4 (FooClient. 4)}))))

(deftest test-short-map-nesting
  (testing "nested short-map full update"
    (is (= (bin-echo 'short-map
                     {10 {1 (Foo. 1), 2 (Foo. 2), 3 (Foo. 3)}
                      20 {4 (Foo. 4), 5 (Foo. 5), 6 (Foo. 6)}
                      30 {7 (Foo. 7), 8 (Foo. 8), 9 (Foo. 9)}}
                     {:elem-type 'short-map :elem-type-opts {:elem-type 'Foo}})
           {10 {1 (FooClient. 1), 2 (FooClient. 2), 3 (FooClient. 3)}
            20 {4 (FooClient. 4), 5 (FooClient. 5), 6 (FooClient. 6)}
            30 {7 (FooClient. 7), 8 (FooClient. 8), 9 (FooClient. 9)}})))

  (testing "nested short-map diff"
    (let [from {10 {1 (Foo. 1), 2 (Foo. 2), 3 (Foo. 3)}
                20 {4 (Foo. 4), 5 (Foo. 5), 6 (Foo. 6)}
                30 {7 (Foo. 7), 8 (Foo. 8), 9 (Foo. 9)}}
          
          from-client {10 {1 (FooClient. 1), 2 (FooClient. 2), 3 (FooClient. 3)}
                       20 {4 (FooClient. 4), 5 (FooClient. 5), 6 (FooClient. 6)}
                       30 {7 (FooClient. 7), 8 (FooClient. 8), 9 (FooClient. 9)}}
          
          to (-> from
                 (update-in [10] dissoc 1)
                 (assoc-in [20 777] (Foo. 777))
                 (assoc-in [30 9] (Foo. 999)))
          
          diff (simple-diff 'short-map from to {:elem-type 'short-map :elem-type-opts {:elem-type 'Foo}})]
      (is (= (simple-apply-diff 'short-map from-client diff {:elem-type 'short-map :elem-type-opts {:elem-type 'Foo}})
             {10 {2 (FooClient. 2), 3 (FooClient. 3)}
              20 {4 (FooClient. 4), 5 (FooClient. 5), 6 (FooClient. 6), 777 (FooClient. 777)}
              30 {7 (FooClient. 7), 8 (FooClient. 8), 9 (FooClient. 999)}})))))
