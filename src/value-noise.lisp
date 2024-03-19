(in-package :binary-media-gen)

(sera:-> noise (fixnum list)
         (values single-float &optional))
(declaim (inline noise))
(defun noise (seed coords)
  (declare (optimize (speed 3)))
  (ecase (length coords)
    (1 (n:value-noise (first coords) 0.0 0.0
                      :octaves 2 :seed seed))
    (2 (n:value-noise (first coords) (second coords) 0.0
                      :octaves 2 :seed seed))
    (3 (n:value-noise (first coords) (second coords) (third coords)
                      :octaves 2 :seed seed))))

(sera:-> %noise-fill (alex:positive-fixnum
                      (integer 1 3)
                      &key
                      (:mul     single-float)
                      (:thr     single-float)
                      (:seed    fixnum))
         (values (simple-array bit) &optional))
(defun %noise-fill (side dim &key (mul 1.0) (thr 0.5) (seed 123456))
  "Fill a @c(dim)-dimensional array with a side @c(side) with a
thresholded value noise."
  (declare (optimize (speed 3)))
  (let ((array (make-array (loop repeat dim collect side) :element-type 'bit))
        (indices (si:power (si:range 0 side) dim))
        (side (float side)))
    (si:do-iterator (idx indices)
      (setf (index array idx)
            (if (< (noise seed
                          (mapcar
                           (lambda (i)
                             (declare (type alex:non-negative-fixnum i))
                             (let* ((x  (/ i side))
                                    (%x (if (< x 0.5) x (- (1- x)))))
                               (* mul %x)))
                           idx))
                   thr)
                0 1)))
    array))

(sera:-> filter-main-cluster ((simple-array bit))
         (values (simple-array bit) &optional))
(defun filter-main-cluster (array)
  "Remove all clusters of zeros from array with exception of the
largest."
  (let* ((lbls (label-components (invert array)))
         (histogram (histogram lbls))
         (max (reduce #'max histogram))
         (pos (1+ (position max histogram :test #'=))))
    (aops:vectorize* 'bit
        (lbls)
      (if (= lbls pos) 1 0))))

(sera:-> noise-fill (alex:positive-fixnum
                     (integer 1 3)
                     &key
                     (:mul     single-float)
                     (:thr     single-float)
                     (:seed    fixnum))
         (values (simple-array bit) &optional))
(defun noise-fill (side dim &key (mul 1.0) (thr 0.5) (seed 123456))
  (let ((noise (%noise-fill side dim :mul mul :thr thr :seed seed)))
    (filter-main-cluster noise)))
