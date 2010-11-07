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
  (let [stream (StringReader. s)]
    (loop [i (.read stream)
           re ""
           curly-depth 0]
      (let [c (if (= i -1) nil (char i))
            j (.read stream)]
        (cond
         (= i -1) (re-pattern (str (if (= \. (first s)) "" "(?=[^\\.])") re))
         (= c \\) (recur (.read stream) (str re c (char j)) curly-depth)
         (= c \/) (recur j (str re (if (= \. (char j)) c "/(?=[^\\.])"))
                         curly-depth)
         (= c \*) (recur j (str re "[^/]*") curly-depth)
         (= c \?) (recur j (str re "[^/]") curly-depth)
         (= c \{) (recur j (str re \() (inc curly-depth))
         (= c \}) (recur j (str re \)) (dec curly-depth))
         (and (= c \,) (< 0 curly-depth)) (recur j (str re \|) curly-depth)
         (#{\. \( \) \| \+ \^ \$ \@ \%} c) (recur j (str re \\ c) curly-depth)
         :else (recur j (str re c) curly-depth))))))

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