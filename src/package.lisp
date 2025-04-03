(defpackage binary-media-gen
  (:use #:cl)
  (:local-nicknames (#:sera #:serapeum)
                    (#:alex #:alexandria)
                    (#:n    #:cl-value-noise)
                    (#:ff   #:float-features)
                    (#:vp   #:vp-trees)
                    (#:si   #:stateless-iterators))
  (:export #:mix-*
           #:mix-+
           #:invert
           #:label-components
           #:only-one-cluster
           #:remove-floating-solid
           #:histogram
           #:random-centers
           #:voronoi-diagram
           #:voronoi-type
           #:noise-fill
           #:porosity
           #:ball-packing
           #:ball))
