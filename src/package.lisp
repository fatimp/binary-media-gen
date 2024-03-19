(defpackage binary-media-gen
  (:use #:cl)
  (:local-nicknames (#:sera #:serapeum)
                    (#:alex #:alexandria)
                    (#:n    #:cl-value-noise)
                    (#:ff   #:float-features)
                    (#:si   #:stateless-iterators))
  (:export #:mix-*
           #:mix-+
           #:invert
           #:label-components
           #:histogram
           #:random-centers
           #:voronoi-diagram
           #:voronoi-type
           #:noise-fill
           #:porosity))
