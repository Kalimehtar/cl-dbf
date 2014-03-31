(in-package #:dbfview)

(defclass lisp-model-dbf (gtk-cffi-ext:lisp-model-list)
  ((db :initarg :db)))

(defmethod gtk-cffi-ext:get-value ((lisp-model lisp-model-dbf) iter n value)
  (let ((db (slot-value lisp-model 'db)))
    (cl-dbf:goto-record db (gtk-cffi-ext:iter->index iter))
    (gtk-cffi-ext:set-value value (cl-dbf:read-record db) n)))

(defmethod gtk-cffi-ext:lisp-model-length ((lisp-model lisp-model-dbf))
  (cl-dbf:records-count (slot-value lisp-model 'db)))

(defvar *codepages* #(("Auto" 0)
                      ("850" 2)
                      ("1251" 3)
                      ("852" #x64)
                      ("865" #x65)
                      ("866" #x66)
                      ("1250" #xC8)
                      ("1251" #xC9)
                      ("437" #xFF)))

(defun codepage ()
  (second (aref *codepages* 
                (multiple-value-bind (iter found) 
                    (gtk-cffi:active-iter (gtk-cffi:object-by-id :codepage))
                  (if found
                      (gtk-cffi-ext:iter->index iter)
                      0)))))

(defun fill-tree-view (tree-view filename)
  (let* ((s (open filename :direction :io 
                  :element-type 'unsigned-byte
                  :if-exists :overwrite))
         (db (cl-dbf:dbopen s))
         (impl (make-instance 'lisp-model-dbf
                              :columns (loop
                                          :for i :in (cl-dbf:fields db)
                                          :collect :string)
                              :db db)))
    (tg:finalize impl (lambda () (close s)))
    (setf (gtk-cffi:columns tree-view)
          (mapcar #'cl-dbf:name (cl-dbf:fields db)))
    (let ((codepage (codepage)))
      (unless (= 0 codepage)
        (setf (cl-dbf:code-page db) codepage)))
    (setf (gtk-cffi:model tree-view)
          (make-instance 'gtk-cffi-ext:lisp-model 
                         :implementation impl))))

(defun fill-tree-view-array (tree-view filename)
  "Use this instead of FILL-TREE-VIEW if you don't want keep DBF file open"
  (cl-dbf:with-db db filename
    (setf (gtk-cffi:columns tree-view)
          (mapcar #'cl-dbf:name (cl-dbf:fields db)))
    (let* ((empty-rec (loop
                         :for i 
                         :in (cl-dbf:fields db)
                         :collect ""))
           (arr (make-array (cl-dbf:records-count db) 
                            :initial-element empty-rec)))
      (let ((codepage (codepage)))
        (unless (= 0 codepage)
          (setf (cl-dbf:code-page db) codepage)))
      (gtk-cffi-ext:with-progress (:parent (gtk-cffi:object-by-id :window))
        (loop 
           :for i :from 0 to (- (length arr) 1)
           :do (progn
                 (setf (aref arr i) (cl-dbf:read-record db))
                 (when (= (mod i 1000) 0)
                   (gtk-cffi-ext:set-progress (/ i (length arr)))))))
      (setf (gtk-cffi:model tree-view)
            (make-instance 'gtk-cffi-ext:lisp-model 
                           :implementation
                           (make-instance 'gtk-cffi-ext:lisp-model-array 
                                          :columns (loop
                                                      :for i 
                                                      :in (cl-dbf:fields db)
                                                      :collect :string)
                                          :array arr))))))
                       
    

(defun open-file (widget)
  (declare (ignore widget))
  (let ((window (gtk-cffi:object-by-id :window)))
    (let ((open-dialog (make-instance 'gtk-cffi:file-chooser-dialog
                                      :action :open 
                                      :dialog-parent window))
          (filter (make-instance 'gtk-cffi:file-filter)))
      (gtk-cffi:add-pattern filter "*.dbf")
      (gtk-cffi:add-pattern filter "*.DBF")
      (setf (gtk-cffi:filter open-dialog) filter)
      (when (eq (gtk-cffi:run open-dialog) :accept)
        (let ((filename (gtk-cffi:filename open-dialog)))
          (gtk-cffi:destroy open-dialog)
          (fill-tree-view (gtk-cffi:object-by-id :view) 
                          filename))))))
  

(defun combo-box-model ()
  (make-instance 'gtk-cffi-ext:lisp-model 
                 :implementation
                 (make-instance 
                  'gtk-cffi-ext:lisp-model-array 
                  :columns '(:string :int)
                  :array *codepages*)))

(defun debug-press ()
  (let ((combo (gtk-cffi:object-by-id :codepage)))
    (format t "~A~%"
            (second (aref *codepages* 
                          (gtk-cffi-ext:iter->index 
                           (gtk-cffi:active-iter combo)))))))

;    (gtk-cffi-ext:iter->aref (gtk-cffi-ext:implementation 
;                              (gtk-cffi:model combo))
;                             (gtk-cffi:active-iter combo)))))
    

(defun run ()
  (gtk-cffi:gtk-init)
  (let (window)
    (setf window
          (gtk-cffi:gtk-model
            'gtk-cffi:window
            :id :window
            :width 800 :height 800
            :signals '(:destroy :gtk-main-quit)
            ('gtk-cffi:v-box
             :expand nil
             ('gtk-cffi:menu-bar
              ('gtk-cffi:menu-item 
               :label "File"
               :submenu
               (gtk-cffi:gtk-model 
                 'gtk-cffi:menu
                 ('gtk-cffi:menu-item :label "Open"
                             :signals (list :activate #'open-file))
                 ('gtk-cffi:menu-item :label "Quit"
                             :signals (list :activate
                                            (lambda (widget)
                                              (declare (ignore widget))
                                              (gtk-cffi:destroy window)))))))
             ('gtk-cffi:h-box
              ('gtk-cffi:label :text "Codepage:")
              ('gtk-cffi:combo-box 
               :id :codepage
               :model (combo-box-model)
               :active 0
               ('gtk-cffi:cell-renderer-text
                :attributes '((:text 0)))))
              ;('gtk-cffi:button :label "Debug" :signals 
              ;                  (list :clicked
              ;                        (lambda (widget)
              ;                          (declare (ignore widget))
              ;                          (debug-press)))))
             :expand t
             ('gtk-cffi:scrolled-window
              ('gtk-cffi:tree-view :id :view))
             :expand nil
             ('gtk-cffi:statusbar))))
    (gtk-cffi:show window)
    (gtk-cffi:gtk-main)))
    