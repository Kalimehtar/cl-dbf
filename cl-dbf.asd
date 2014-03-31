(defpackage #:cl-dbf-system
  (:use #:cl #:asdf))
(in-package #:cl-dbf-system)

(defsystem #:cl-dbf
  :description "DBF files reader/writer"
  :author "Roman Klochkov <kalimehtar@mail.ru>"
  :version "0.2.0"
  :license "BSD"
  :depends-on (#:com.gigamonkeys.binary-data #:flexi-streams)
  :components
  ((:file #:package)
   (:file #:src :depends-on (#:package))
   (:file #:conses :depends-on (#:src))))
