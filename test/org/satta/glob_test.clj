(ns org.satta.glob-test
  (:use [org.satta.glob] :reload-all)
  (:use [clojure.test]))

;; TODO: test glob, maybe with some kind of temp file/dir scaffolding?

(def glob->regex #'org.satta.glob/glob->regex)

(defn- matches?
  "Whether a glob pattern matches a path string"
  [pattern path]
  (not (nil? (re-matches (glob->regex pattern) path))))

(deftest glob->regex-matches-correctly
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
       "foo/bar.*"   "foo/bar.baz"
       "foo/*.baz"   "foo/bar.baz"
       "*/*"         "foo/bar.baz"
       ".*.foo"      ".bar.foo"
       ".*bar.foo"   ".bar.foo"
       ".*/bar"        ".foo/bar"
       "foo.[ch]"    "foo.c"
       "foo.[ch]"    "foo.h"
       "foo.[c-h]"   "foo.c"
       "foo.[c-h]"   "foo.e"
       "foo.[c-h]"   "foo.f"
       "foo.[c-h]"   "foo.h"))

(deftest glob->regex-ignores-dotfiles
  (are [pattern path] (not (matches? pattern path))
       "a*d"         "abc"
       "*.foo"       ".bar.foo"
       "*.bar.foo"   ".bar.foo"
       "?bar.foo"    ".bar.foo"))

(deftest glob->regex-char-range-nonmatch
  (are [pattern path] (not (matches? pattern path))
       "foo.[ch]"    "foo.a"
       "foo.[ch]"    "foo.d"
       "foo.[c-h]"   "foo.b"
       "foo.[c-h]"   "foo.i"))