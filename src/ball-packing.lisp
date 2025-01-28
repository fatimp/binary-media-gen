(in-package :binary-media-gen)

(sera:-> ball-packing (alex:positive-fixnum alex:positive-fixnum list single-float)
         (values (simple-array bit) &optional))
(defun ball-packing (side dim centers r)
  "Create a ball packing in a @c(dim)-dimensional cube of a side
@c(side). Centers of the balls are in @c(centers). All balls have a
radius @c(r)."
  (declare (optimize (speed 3)))
  (let ((array (make-array (loop repeat dim collect side) :element-type 'bit))
        (indices (si:power (si:range 0 side) dim))
        (side (float side))
        (tree (vp:make-vp-tree centers #'distance)))
    (si:do-iterator (idx indices)
      (setf (index array idx)
            (if (vp:items-in-ball
                 tree (map '(vector single-float)
                           (lambda (x)
                             (declare (type fixnum x))
                             (/ x side))
                           idx)
                 r #'distance)
                1 0)))
    array))


(sera:-> ball (alex:positive-fixnum alex:positive-fixnum (single-float 0.0 1.0))
         (values (simple-array bit) &optional))
(defun ball (side dim r)
  "Draw a @c(dim)-dimensional ball with radius \\(0 \\le r \\le
0.5\\). Produced array has dimensions (@c(side) @c(side) @c(side))."
  (declare (optimize (speed 3)))
  (let ((array (make-array (loop repeat dim collect side) :element-type 'bit))
        (indices (si:power (si:range 0 side) dim))
        (side (float side)))
    (si:do-iterator (idx indices)
      (setf (index array idx)
            (let ((d (reduce #'+ idx
                             :initial-value 0.0
                             :key (lambda (n)
                                    (declare (type fixnum n))
                                    (expt (/ (- n (/ side 2)) side) 2)))))
              (if (< d (expt r 2)) 1 0))))
    array))
