(ns cinder.core
  (:require
    [clojure.java.io :as io]
    [parcera.core :as parcera]
    [clojure.walk :as walk]
    [clojure.string :as string]))

(defn until-unchanged
  "Call (f (f init)) until the return value of f is no different to the last
  run."
  [f init]
  (let [v (f init)]
    (if (= v init)
      v
      (recur f v))))

(defn remove-vertical-alignment
  [ast]
  (walk/postwalk
    (fn [x]
      (if (and (seq? x)
               (= :whitespace (first x))
               (re-matches #"  +" (second x)))
        [:whitespace " "]
        x))
    ast))

(defn refresh-ast-pos
  [ast]
  (parcera/ast (parcera/code ast)))

(defn map-align-keys
  [map-ast]
  (if-not (get-in (meta map-ast) [::parcera/start :column])
    map-ast
    (into
      [:map]
      (let [inside-map-column (inc (get-in (meta map-ast) [::parcera/start :column]))]
        (loop [row -1
               finding-next-line? true
               [elem & elems] (rest map-ast)
               new-list []]
          (if elem
            (if finding-next-line?
              (if (= row (get-in (meta elem) [::parcera/start :row]))
                (recur row true elems (conj new-list elem))
                (recur (get-in (meta elem) [::parcera/start :row]) false (cons elem elems) new-list))
              ;;
              (let [row-elems (take-while
                                (fn [x]
                                  (= row (get-in (meta x) [::parcera/start :row])))
                                (cons elem elems))]
                (if-let [non-whitespace (first (filter #(and (seq? %) (not= :whitespace (first %))) row-elems))]
                  (let [non-whitespace-col (get-in (meta elem) [::parcera/start :column])]
                    (if (> non-whitespace-col inside-map-column)
                      (recur
                        row
                        true
                        (cons elem elems)
                        (loop [[x & xs] (reverse new-list)
                               new-list ()]
                          (if (and (seq? x)
                                   (= :whitespace (first x)))
                            (vec
                              (apply conj new-list
                                     [:whitespace
                                      (string/replace-first
                                        (second x)
                                        (re-pattern
                                          (format " {%d}$" (- non-whitespace-col inside-map-column)))
                                        "")]
                                     xs))
                            (recur xs (vec (conj new-list x))))))
                      (recur row true elems (conj new-list elem))))
                  (recur row true elems (conj new-list elem)))))
            new-list))))))

(defn align-map-keys
  [ast]
  ;; postwalk doesn't retain meta
  (walk/prewalk
    (fn [x]
      (if (and (sequential? x)
               (= :map (first x)))
        (map-align-keys x)
        x))
    ast))

(comment
  (print
    (parcera/code
      (until-unchanged
        (comp align-map-keys refresh-ast-pos remove-vertical-alignment)
        (parcera/ast (slurp "corpus/core.clj"))))))

(defn -main
  [& [file]]
  (print
    (parcera/code
      (until-unchanged
        (comp align-map-keys refresh-ast-pos remove-vertical-alignment)
        (parcera/ast (slurp file))))))
