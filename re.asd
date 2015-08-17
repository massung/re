(defpackage :re-asd
  (:use :cl :asdf))

(in-package :re-asd)

(defsystem :re
  :name "re"
  :version "1.0"
  :author "Jeffrey Massung"
  :license "Apache 2.0"
  :description "Lua-style string pattern matching."
  :serial t
  :components ((:file "re"))
  :depends-on ("parse"))
