(asdf:defsystem ningle-bells
  :version "0.1"
  :author "PEA"
  :license "MIT"
  :components ((:module "src"  ; src/  dir
                :components
                ((:file "ningle-bells"))))

  
  :description "Some helper functions for ningle"
  :long-description
  #.(with-open-file (stream
                     (merge-pathnames
                             #p"README.markdown"
                            (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq))))
