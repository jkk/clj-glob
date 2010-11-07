(ns org.satta.glob
  (:use [clojure.java.io :only [as-file]])
  (:import [java.io File StringReader]))

;; TODO: make Windows-friendly

(def
  ^{:doc "Start directory for absolute path globbing"}
  *root-file* (as-file "/"))

(def
  ^{:doc "Start directory for relative path globbing"}
  *cwd-file* (as-file "."))

(defn- glob->regex
  "Takes a glob-format string and returns a regex."
  [s]
  (loop [stream s
	 re ""
	 curly-depth 0]
    (let [[c j] stream]
        (cond
         (nil? c) (re-pattern (str (if (= \. (first s)) "" "(?=[^\\.])") re))
         (= c \\) (recur (nnext stream) (str re c j) curly-depth)
         (= c \/) (recur (next stream) (str re (if (= \. j) c "/(?=[^\\.])"))
                         curly-depth)
         (= c \*) (recur (next stream) (str re "[^/]*") curly-depth)
         (= c \?) (recur (next stream) (str re "[^/]") curly-depth)
         (= c \{) (recur (next stream) (str re \() (inc curly-depth))
         (= c \}) (recur (next stream) (str re \)) (dec curly-depth))
         (and (= c \,) (< 0 curly-depth)) (recur (next stream) (str re \|) curly-depth)
         (#{\. \( \) \| \+ \^ \$ \@ \%} c) (recur (next stream) (str re \\ c) curly-depth)
         :else (recur (next stream) (str re c) curly-depth)))))

(defn glob
  "Returns a list of java.io.File instances that match the given glob pattern.
  Ignores dot files unless explicitly included.

  Examples: (glob \"*.{jpg,gif}\") (glob \".*\") (glob \"/usr/*/se*\")"
  [pattern]
  (let [abs-path? (= \/ (first pattern))
        start-dir (if abs-path? *root-file* *cwd-file*)
        patterns (map glob->regex
                      (.split (if abs-path? (subs pattern 1) pattern) "/"))
        expand (fn [re dir]
                 (filter #(re-matches re (.getName %)) (.listFiles dir)))]
    (reduce #(mapcat (partial expand %2) %1) [start-dir] patterns)))