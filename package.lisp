(defpackage #:cl-dbf
  (:use #:cl #:binary-data #:com.gigamonkeys.binary-data.common-datatypes)
  (:export 
   #:code-page
   #:dbase3-memo
   #:dbase4-memo
   #:dbopen
   #:field-type
   #:fields
   #:goto-bof
   #:goto-record
   #:name
   #:read-field-datum
   #:read-record
   #:records-count
   #:translate-field-datum
   #:visual-foxpro-memo
   #:with-db
   #:with-db-memo))

