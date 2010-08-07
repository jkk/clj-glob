(ns org.satta.glob
  (:use [clojure.contrib.io :only [as-file]])
  (:import [java.io File StringReader]))

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

(defn- abs-path?
  "Returns true if the specified path is absolute, false otherwise."
  [path]
  (or
    (= \/ (first path))      ; /home/qertoip
    (= \: (second path))))   ; c:/windows

(defn- start-dir
  "Returns a start directory for recursive traversal as File instance."
  [path]
  (as-file
    (if (abs-path? path)
      (if (= \: (second path))
        (subs path 0 3)        ; c:/
        "/")                   ; /
      ".")))                   ; .

(defn- first-slash
  "Returns index of the first slash, plus 1"
  [s]
  (inc (.indexOf s "/")))

(defn glob
  "Returns a list of java.io.File instances that match the given glob pattern.
  Ignores dot files unless explicitly included.

  Examples: (glob \"*.{jpg,gif}\") (glob \".*\") (glob \"/usr/*/se*\")"
  [pattern]
  (let [abs-path? (abs-path? pattern)
        start-dir (start-dir pattern)
        splitted  (.split (if abs-path? (subs pattern (first-slash pattern)) pattern) "/")
        patterns  (map glob->regex splitted)
        expand    (fn [re dir]
                    (filter #(re-matches re (.getName %)) (.listFiles dir)))]
    (reduce #(mapcat (partial expand %2) %1) [start-dir] patterns)))
