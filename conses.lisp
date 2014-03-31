;; (c) Roman Klochkov, kalimehtar@mail.ru
;; 


(in-package #:cl-dbf)


(defun dbf-to-conses-of-strings (filename &key external-format)
  "FILENAME is a name of dbf file to open.
Returns a list (field-names . record-values), 
where values are strings.
EXTERNAL-FORMAT is passed to flexi-streams:octets-to-string"
  (with-db db filename
    (when external-format
      (setf (slot-value db 'external-format) external-format)
      (cons (mapcar #'name (fields db))
            (loop 
               :for rec = (read-record db)
               :while rec
               :collect rec)))))