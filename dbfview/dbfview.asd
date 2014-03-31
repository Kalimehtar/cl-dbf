(defpackage #:dbfview-system
  (:use #:cl #:asdf))
(in-package #:dbfview-system)

(defsystem dbfview
  :description "DBF Viewer"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.1"
  :license "BSD"
  :depends-on (cl-dbf gtk-cffi-ext)
  :components
  ((:file package)
   (:file src :depends-on (package))))