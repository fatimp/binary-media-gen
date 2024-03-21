(defsystem :binary-media-gen
    :name :binary-media-gen
    :version "0.1"
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :description "A collection of utilities for generation of binary media"
    :license "2-clause BSD"
    :serial t
    :pathname "src"
    :components ((:file "package")
                 (:file "utils")
                 (:file "voronoi")
                 (:file "value-noise")
                 (:file "ball-packing"))
    :depends-on (:alexandria
                 :serapeum
                 :stateless-iterators
                 :cl-value-noise
                 :array-operations
                 :float-features
                 :vp-trees))
