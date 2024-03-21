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
