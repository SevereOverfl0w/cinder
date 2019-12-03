(ns cinder.core
  (:require
    [clojure.string :as string]
    [parcera.core :as parcera]))

;; adapted from clojure.walk, but preserves metadata
;; adapted from potemkin

(defn walk
  "Like `clojure.walk/walk`, but preserves metadata."
  [inner outer form]
  (let [x (cond
            (list? form) (outer (apply list (map inner form)))
            (instance? clojure.lang.IMapEntry form) (outer (vec (map inner form)))
            (seq? form) (outer (doall (map inner form)))
            (coll? form) (outer (into (empty form) (map inner form)))
            :else (outer form))]
    (if (instance? clojure.lang.IObj x)
      (with-meta x (merge (meta form) (meta x)))
      x)))

(defn postwalk
  "Like `clojure.walk/postwalk`, but preserves metadata."
  [f form]
  (walk (partial postwalk f) f form))

(defn prewalk
  "Like `clojure.walk/prewalk`, but preserves metadata."
  [f form]
  (walk (partial prewalk f) identity (f form)))

;; end walkers

(defn refresh-ast-pos
  [ast]
  (parcera/ast (parcera/code ast)))

(defn until-unchanged
  "Call (f (f init)) until the return value of f is no different to the last
  run."
  [f init]
  (let [v (f init)]
    (if (= v init)
      v
      (recur f v))))

(defn dedent
  [ast n]
  (concat
    (empty ast)
    (take 1 ast)
    (prewalk
      (fn [x]
        (if (and (sequential? x)
                 (= :whitespace (first x))
                 (re-matches
                   #".*\n +$"
                   (second x)))
          [:whitespace (string/replace (second x)
                                       (re-pattern (format " {0,%d}$" n))
                                       "")]
          x))
      (rest ast))))

(defn vertical-alignment-whitespace?
  [ast]
  (and
    (= :whitespace (first ast))
    (> (get-in (meta ast) [::parcera/start :column]) 0)
    (boolean (re-matches #" {2,}" (second ast)))))

(defn remove-vertical-alignment
  [ast]
  (let [to-dedent (atom #{})]
    (as-> ast $
      (prewalk
        (fn [x]
          (if (and (seq? x)
                   (vertical-alignment-whitespace? x))
            (do
              (swap! to-dedent conj (assoc (::parcera/start (meta x))
                                           :amount (dec (count (second x)))))
              [:whitespace " "])
            x))
        $)
      (reduce
        (fn [ast to-dedent]
          (prewalk
            (fn [x]
              (if (and
                    (seq? x)
                    (= (get-in (meta x) [::parcera/start :row])
                       (:row to-dedent))
                    (> (get-in (meta x) [::parcera/start :column] -1)
                       (:column to-dedent)))
                (dedent x (:amount to-dedent))
                x))
            ast))
        $
        @to-dedent))))

(comment
  (remove-vertical-alignment
    (refresh-ast-pos
      (remove-vertical-alignment
        (parcera/ast (slurp "corpus/core.clj")))))

  (print
    (parcera/code
      (until-unchanged
        (comp refresh-ast-pos remove-vertical-alignment)
        (parcera/ast (slurp "corpus/core.clj"))))))

(defn- invalid-code
  [ast]
  (map (comp ::parcera/start meta)
       (filter
         #(= ::parcera/failure (first %))
         (filter seq?
                 (tree-seq seq? next ast)))))

(defn -main
  [& args]
  (let [[opts files]
        (split-with #(and (string/starts-with? % "-")
                          (not= "--" %))
                    args)
        files (if (= "--" (first files))
                (rest files)
                files)
        opts (set opts)]
    (doseq [file files]
      (try
        (let [input-ast (parcera/ast (slurp file))]
          (if-let [invalids (seq (invalid-code input-ast))]
            (binding [*out* *err*]
              (doseq [invalid invalids]
                (printf "%s:%s:%s: Invalid code\n" file (:row invalid) (:column invalid)))
              (flush)
              (System/exit 1))
            (let [output-ast (until-unchanged
                               (fn [ast]
                                 (-> ast
                                     remove-vertical-alignment
                                     refresh-ast-pos))
                               input-ast)]
              (cond->> (parcera/code output-ast)
                (contains? opts "--fix")
                (spit file)

                (and (not (contains? opts "--fix"))
                     (not (contains? opts "--silent")))
                print))))
        (catch Exception e
          (throw (ex-info
                   (str "Failure while processing file " file)
                   {:file file}
                   e)))))))
