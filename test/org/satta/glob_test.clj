(ns org.satta.glob-test
  (:use [org.satta.glob] :reload-all)
  (:use [clojure.test])
  (:import [java.io File]))

;; glob->regex ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def glob->regex #'org.satta.glob/glob->regex)

(defn- matches?
  "Whether a glob pattern matches a path string"
  [pattern path]
  (not (nil? (re-matches (glob->regex pattern) path))))

(deftest test-glob->regex
  (testing "General matching"
    (are [pattern path] (matches? pattern path)
         "abcd"        "abcd"
         "a*d"         "abcd"
         "a??d"        "abcd"
         "*"           "abcd"
         "foo.bar"     "foo.bar"
         "foo.*"       "foo.bar"
         "*.bar"       "foo.bar"
         "*foo.*"      "foo.bar"
         "foo*"        "foo.bar"
         "*.{bar,baz}" "foo.bar"
         "*.{bar,baz}" "foo.baz"
         "{foo,bar}"   "foo"
         "{foo,bar}"   "bar"
         "foo,bar"     "foo,bar"
         "*,*"         "foo,bar"
         "foo\\*bar"   "foo*bar"
         ".()|+^$@%"   ".()|+^$@%"
         "foo/bar.*"   "foo/bar.baz"
         "foo/*.baz"   "foo/bar.baz"
         "*/*"         "foo/bar.baz"
         ".*.foo"      ".bar.foo"
         ".*bar.foo"   ".bar.foo"
         ".*/bar"      ".foo/bar"
         "foo/.*"      "foo/.bar"
         "foo.[ch]"    "foo.c"
         "foo.[ch]"    "foo.h"
         "foo.[c-h]"   "foo.c"
         "foo.[c-h]"   "foo.e"
         "foo.[c-h]"   "foo.h"))
  (testing "Dot files ignored"
    (are [pattern path] (not (matches? pattern path))
         "*"           ".foo"
         "*.*"         ".foo"
         "*.foo"       ".bar.foo"
         "*.bar.foo"   ".bar.foo"
         "foo/*"       "foo/.bar"
         "?bar.foo"    ".bar.foo"))
  (testing "Character ranges that shouldn't match"
    (are [pattern path] (not (matches? pattern path))
         "foo.[ch]"    "foo.a"
         "foo.[ch]"    "foo.d"
         "foo.[c-h]"   "foo.b"
         "foo.[c-h]"   "foo.i")))

;; glob ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn mock-fs
  "Takes a tree of vectors and returns a minimal, fake file/dir hierarchy.
  Only getName and listFiles will return reliable (fake) results."
  [node]
  (if (vector? node)
    (proxy [File] [(first node)]
      (listFiles [] (into-array File (map mock-fs (rest node)))))
    (File. node)))

(deftest test-mock-fs
  (let [fs (mock-fs ["/" ["foo" "subfoo1" "subfoo2"] "bar" "baz"])]
    (is (= fs (File. "/")))
    (is (= (seq (.listFiles fs))
           [(File. "foo") (File. "bar") (File. "baz")]))
    (is (= (seq (.listFiles (first (.listFiles fs))))
           [(File. "subfoo1") (File. "subfoo2")]))
    (is (= (map #(.getName %) (.listFiles fs))
           ["foo" "bar" "baz"]))))

(def shallow-fs
     (mock-fs
      ["." "cat.jpg" "dog.jpg" "lol.gif" ".hidden"]))

(def deep-fs
     (mock-fs
      ["/"
       ["usr"
        ["bin" "awk" "bzip" "cd" "diff" "sed" "segedit" "xargs"]
        ["lib" "pam" "samba" "java"]
        ["sbin" "arp" "fdisk" "sendmail" "tcpdump"]
        ["share"
         ["man" "man1" "man2" "man3"]]]]))

(defn glob*
  "Glob with a fake filesystem. Also returns seq of file names rather
  than File instances."
  [pattern start-dir]
  (binding [*root-file* start-dir
	    *cwd-file* start-dir]
    (map #(.getName %) (glob pattern))))

(deftest test-glob
  (testing "Simple, shallow (single-level) matching"
    (are [pattern files] (= (glob* pattern shallow-fs) files)
         "*"            ["cat.jpg" "dog.jpg" "lol.gif"]
         "*.*"          ["cat.jpg" "dog.jpg" "lol.gif"]
         ".*"           [".hidden"]
         "*.jpg"        ["cat.jpg" "dog.jpg"]
         "*.{jpg,gif}"  ["cat.jpg" "dog.jpg" "lol.gif"]))
  (testing "Deep (multi-level) matching"
    (are [pattern files] (= (glob* pattern deep-fs) files)
         "/*"         ["usr"]
         "/usr/*"     ["bin" "lib" "sbin" "share"]
         "/usr/*/se*" ["sed" "segedit" "sendmail"]
         "/*/*/a*"    ["awk" "arp"]
         "/*/*/*/*"   ["man1" "man2" "man3"])))