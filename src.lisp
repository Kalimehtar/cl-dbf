;; (c) Roman Klochkov, kalimehtar@mail.ru
;;     Rafael Jesús Alcántara Pérez, <ralcantara@dedaloingenieros.com>
;; 
;; Status: Alpha
;; for now you can do something like
;; (with-open-file (stream filename)
;;  (let ((driver (dbopen stream)))
;;    (read-record driver)
;;    (read-record driver)
;;    ....))

(in-package #:cl-dbf)

;;;
;;; Visual FoxPro table field flags (position 18).
;;;

(defparameter +dbt-memo-end-marker+ #x1A
  "Marker for end of text in memo fields.")
(defparameter +visual-foxpro-column-flag-system+ #x1 
  "System column (not visible to user).")
(defparameter +visual-foxpro-column-flag-can-be-null+ #x2 
  "Column can store null values.")
(defparameter +visual-foxpro-column-flag-binary+ #x4 
  "Binary column (for CHAR and MEMO only).")
(defparameter +visual-foxpro-column-flag-binary-and-can-be-null+ #x6 
  "When a field is binary and can be NULL (INTEGER, CURRENCY and 
CHARACTER/MEMO fields).")
(defparameter +visual-foxpro-column-flag-autoincrement+ #xC 
  "Column is autoincrementing.")

;;;
;;; Binary types utilities. See `flexi-streams' package.
;;;

(define-binary-type unsigned-integer-le (bytes bits-per-byte)
  (:reader (in)
    (loop with value = 0
       for low-bit from 0 to (* bits-per-byte (1- bytes)) by bits-per-byte do
         (setf (ldb (byte bits-per-byte low-bit) value) (read-byte in))
       finally (return value)))
  (:writer (out value)
    (loop for low-bit from 0 to (* bits-per-byte (1- bytes)) by bits-per-byte
       do (write-byte (ldb (byte bits-per-byte low-bit) value) out))))

(define-binary-type l1 () (unsigned-integer-le :bytes 1 :bits-per-byte 8))
(define-binary-type l2 () (unsigned-integer-le :bytes 2 :bits-per-byte 8))
(define-binary-type l3 () (unsigned-integer-le :bytes 3 :bits-per-byte 8))
(define-binary-type l4 () (unsigned-integer-le :bytes 4 :bits-per-byte 8))

(define-binary-type discard (length)
  (:reader (in)
           (dotimes (i length)
             (read-byte in))
           nil)
  (:writer (out dummy)
           (declare (ignore dummy))
           (dotimes (i length)
             (write-byte 0 out))))

;;;
;;; xBase binary classes code.
;;;

(defclass xbase-common ()
  (stream external-format))

(define-tagged-binary-class dbf-header (xbase-common)
  ((db-type u1))
  (:dispatch (select-db-driver db-type)))

(define-binary-class dbase3-header (dbf-header)
   ((year u1)
    (month u1)
    (day u1)
    (records-count l4)
    (header-size l2)
    (record-size l2)
    (reserved l2)
    (transaction u1)
    (code u1)
    (multi-user (discard :length 12))
    (indexed u1)
    (code-page u1)
    (reserved2 u2)))

(define-condition in-padding () ())

(define-binary-type db-field-name (length)
  (:reader (in)
    (let ((first-byte (read-byte in)))
      (when (= first-byte #xd) (signal 'in-padding))
      (let* ((rest (read-value 'iso-8859-1-string in :length (1- length)))
             (raw-dbf-field-name (concatenate 'string (string (code-char first-byte)) rest))
             (nul-char-position (position #\nul raw-dbf-field-name)))
        (if nul-char-position
            (subseq raw-dbf-field-name 0 nul-char-position)
            raw-dbf-field-name))))
  (:writer (out id)
    (write-value 'iso-8859-1-string out id :length (length id))
    (dotimes (i (- length (length id)))
             (write-byte 0 out))))

(define-binary-class xbase-field ()
  ((name (db-field-name :length 11))
   (field-type u1)
   (reserved u4)
   (size u1)
   (precision u1)
   (reserved2 u2)
   (workspace u1)
   (multi-user u2)
   (set-fields u1)
   (reserved3 (discard :length 7))
   (index u1)))

(define-binary-class visual-foxpro-field ()
  ((name (db-field-name :length 11))
   (field-type u1)
   (reserved u4)
   (size u1)
   (precision u1)
   (flags u1)
   (autoincrement-next-value u4)
   (autoincrement-step-value u1)
   (reserved2 (discard :length 8))))

(defun read-field (field-class in)
  (handler-case (read-value field-class in)
    (in-padding () nil)))

(define-binary-type xbase-fields (length)
  (:reader 
   (in)
   (loop with to-read = (- length 32)
      while (plusp to-read)
      for field = (read-field 'xbase-field in)
      while field
      do (decf to-read 32)
      collect field
      finally (assert (null field))))
  (:writer 
   (out frames)
   (loop with to-write = length
      for frame in frames
      do (write-value 'dbase3-field out frame)
        (decf to-write (+ 6 (size frame)))
      finally (loop repeat to-write do (write-byte 0 out)))))

(define-binary-type visual-foxpro-fields (length)
  (:reader 
   (in)
   (loop with to-read = (- length 32)
      while (plusp to-read)
      for field = (read-field 'visual-foxpro-field in)
      while field
      do (decf to-read 32)
      collect field
      finally (assert (null field))))
  (:writer 
   (out frames)
   (loop with to-write = length
      for frame in frames
      do (write-value 'visual-foxpro-field out frame)
        (decf to-write (+ 6 (size frame)))
      finally (loop repeat to-write do (write-byte 0 out)))))

(define-binary-class dbase3 (dbase3-header)
  ((fields (xbase-fields :length header-size))))

(define-binary-class dbase4 (dbase3-header)
  ((fields (xbase-fields :length header-size))))

(define-binary-class foxbase (dbase3-header)
  ((fields (xbase-fields :length header-size))))

(define-binary-class visual-foxpro (dbase3-header)
  ((fields (visual-foxpro-fields :length header-size))))

;;;
;;; Memo fields related classes.
;;;

(defclass xbase-memo-common (xbase-common)
  ((code-page :reader code-page)))

(define-binary-class dbt-header (xbase-memo-common)
  ((next-available-block u4)))

(define-binary-class dbase3-memo (dbt-header)
  ((reserved1 (discard :length 508))))

(define-binary-class dbase4-memo (dbt-header)
  ((record-size l4)
   (reserved2 (discard :length 504))))

(define-binary-class visual-foxpro-memo (xbase-memo-common)
  ((next-available-block u4)
   (reserved1 u2)
   (record-size u2)
   (reserved2 (discard :length 504))))

(defmethod header-size ((object xbase-memo-common))
  512)

(defmethod record-size ((object xbase-memo-common))
  512)

;;;
;;; Utilities.
;;;

(defun select-db-driver (db-type)
  (case db-type
    (#x2 'foxbase)
    ((#x3 #x83) 'dbase3)
    ((#x4 #x7B #x8B #x8E) 'dbase4)
    ((#x30 #x31 #x32 #xF5) 'visual-foxpro)
    (t 'dbase3)))
   
(defun dbopen (stream)
  (assert (and (input-stream-p stream) (output-stream-p stream)))
  (file-position stream 0)
  (let ((db (read-value 'dbf-header stream)))
    (setf (slot-value db 'stream) stream)
    db))

(defun dbopen-memo (stream type code-page)
  (assert (and (input-stream-p stream) (output-stream-p stream)))
  (file-position stream 0)
  (let ((memo (read-value type stream)))
    (setf (slot-value memo 'stream) stream)
    (setf (slot-value memo 'code-page) code-page)
    memo))

(defun goto-bof (driver)
  (file-position (slot-value driver 'stream) (header-size driver)))

(defgeneric goto-record (driver n)
  (:documentation "Moves the stream to the record `n'.")
  (:method ((driver dbase3-header) n)
    (file-position (slot-value driver 'stream)
                   (+ (header-size driver) (* n (record-size driver)))))
  (:method ((driver xbase-memo-common) n)
    "In memo files, the header is accesible via block numbers. So
    it is up to the database engine to avoid using blocks that
    overlaps the header."
    (file-position (slot-value driver 'stream)
                   (* n (record-size driver)))))

(defun external-format (driver)
  (or (and (slot-boundp driver 'external-format) (slot-value driver 'external-format))
      (case (code-page driver)
        (2    '(:code-page :id 850))
        (3    '(:code-page :id 1252))
        (#x64 '(:code-page :id 852))
        (#x65 '(:code-page :id 865))
        (#x66 '(:code-page :id 866))
        (#xC8 '(:code-page :id 1250))
        (#xC9 '(:code-page :id 1251))
        (t '(:code-page :id 437)))))

;;; FIXME Join first and third methods.
(defgeneric translate-field-datum (driver field datum)
  (:method ((driver dbase3-header) field datum)
    (with-slots (stream) driver
      (case (code-char (field-type field))
        ((#\I #\M) datum)
        (t
         (flexi-streams:octets-to-string datum :external-format (external-format driver))))))
  (:method ((driver xbase-memo-common) field datum)
    (declare (ignore field))
    (flexi-streams:octets-to-string datum :external-format (external-format driver))))

(defgeneric read-field-datum (driver field &key translate)
  (:documentation "Reads raw data from current `driver' `stream'
  position and then, it uses `translate' for returning the real field
  datum.")
  (:method ((driver dbase3-header) field &key (translate #'translate-field-datum))
    (with-slots (stream) driver
      (case (code-char (field-type field))
        (#\I (funcall translate driver field (read-value 'l4 stream)))
        (#\M (let ((s (make-array (size field) :element-type '(unsigned-byte 8))))
               (read-sequence s stream)
               (handler-case
                   (let ((memo-block-index
                          (parse-integer (flexi-streams:octets-to-string s :external-format (external-format driver)))))
                     (when (plusp memo-block-index)
                       (funcall translate driver field memo-block-index)))
                 (parse-error () nil))))
        (t (let ((s (make-array (size field) :element-type '(unsigned-byte 8))))
             (read-sequence s stream)
             (funcall translate driver field s))))))
  (:method ((driver dbase3-memo) field &key (translate #'translate-field-datum))
    (with-slots (stream) driver
      (let ((memo-value-pieces
             (loop
                :with memo-block-size := (record-size driver)
                :with buffer := (make-array memo-block-size :element-type (stream-element-type stream))
                :for read-length := (read-sequence buffer stream)
                :for terminator-position := (position +dbt-memo-end-marker+ buffer :end read-length)
                :if (zerop read-length)
                  :return memo-value-pieces
                :else
                  :if terminator-position
                    :collect (subseq buffer 0 terminator-position) :into memo-value-pieces
                    :and :return memo-value-pieces
                  :else
                    :collect (subseq buffer 0 read-length) :into memo-value-pieces)))
        (funcall translate driver field (apply #'concatenate (cons 'vector memo-value-pieces))))))
  (:method ((driver dbase4-memo) field &key (translate #'translate-field-datum))
    (with-slots (stream) driver
      (let ((memo-value-pieces
             (loop
                :with memo-block-size := (record-size driver)
                :with buffer := (make-array memo-block-size :element-type (stream-element-type stream))
                :for read-length := (read-sequence buffer stream)
                :for terminator-position := (position +dbt-memo-end-marker+ buffer :end read-length)
                :if (zerop read-length)
                  :return memo-value-pieces
                :else
                  :if terminator-position
                    :collect (subseq buffer 8 terminator-position) :into memo-value-pieces
                    :and :return memo-value-pieces
                  :else
                    :collect (subseq buffer 8 read-length) :into memo-value-pieces)))
        (funcall translate driver field (apply #'concatenate (cons 'vector memo-value-pieces))))))
  (:method ((driver visual-foxpro-memo) field &key (translate #'translate-field-datum))
    (with-slots (stream) driver
      (read-value 'l4 stream)
      (let* ((size (read-value 'u4 stream))
             (datum (make-array size :element-type '(unsigned-byte 8))))
        (read-sequence datum stream)
        (funcall translate driver field datum))))
  (:method ((driver visual-foxpro) field &key (translate #'translate-field-datum))
    (with-slots (stream) driver
      (case (code-char (field-type field))
        ((#\I #\M) (let ((memo-block-index (read-value 'l4 stream)))
                     (when (plusp memo-block-index)
                       (funcall translate driver field memo-block-index))))
        (t (let ((s (make-array (size field) :element-type '(unsigned-byte 8))))
             (read-sequence s stream)
             (funcall translate driver field s)))))))

(defmethod read-record ((driver dbase3-header) &key (translate #'translate-field-datum))
  "Return record value as list and move to the next record.
When eof, return nil. Deleted records skipped."
  (with-slots (stream) driver
    (case (read-byte stream nil :eof)
      (32 (loop 
             :for field :in (fields driver)
             :collect (read-field-datum driver field :translate translate)))
      (:eof nil)
      (t ; deleted record, skip and read again
       (file-position stream
                      (+ (file-position stream)
                         (1- (record-size driver))))
       (read-record driver :translate translate)))))

(defmacro with-db (db filespec &body body)
  (let ((stream (gensym)))
    `(with-open-file (,stream ,filespec :direction :io 
                              :element-type 'unsigned-byte
                              :if-exists :overwrite)
       (let ((,db (dbopen ,stream)))
         ,@body))))

(defmacro with-db-memo (db filespec type code-page &body body)
  (let ((stream (gensym)))
    `(with-open-file (,stream ,filespec :direction :io
                              :element-type 'unsigned-byte
                              :if-exists :overwrite)
       (let ((,db (dbopen-memo ,stream ,type ,code-page)))
         ,@body))))
