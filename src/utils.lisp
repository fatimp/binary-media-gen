(in-package :binary-media-gen)

(macrolet ((def-mix (name (a1 a2) expr &optional documentation)
             `(progn
                (sera:-> ,name ((simple-array bit) (simple-array bit))
                         (values (simple-array bit) &optional))
                (defun ,name (,a1 ,a2)
                  ,@(if documentation (list documentation))
                  (declare (optimize (speed 3)))
                  (aops:vectorize* 'bit
                      (,a1 ,a2)
                    ,expr)))))
  (def-mix
      mix-* (a1 a2) (* a1 a2)
      "Mix two binary arrays using @c(*) operation.")
  (def-mix
      mix-+ (a1 a2) (min (+ a1 a2) 1)
      "Mix two binary arrays using @c(+) operation, clamping result to
the range of @c(bit) type."))

(sera:-> invert ((simple-array bit))
         (values (simple-array bit) &optional))
(defun invert (array)
  (declare (optimize (speed 3)))
  (aops:vectorize* 'bit
      (array)
    (- (1- array))))

(sera:-> porosity ((simple-array bit))
         (values single-float &optional))
(defun porosity (array)
  (float (/ (count 0 (aops:flatten array))
            (array-total-size array))))

(sera:-> make-pattern (alex:positive-fixnum)
         (values list &optional))
(defun make-pattern (dim)
  (loop for c in '(-1 1) append
        (loop for d1 below dim collect
              (loop for d2 below dim collect
                    (if (= d1 d2) c 0)))))

(declaim (inline index))
(defun index (array index)
  (let ((rank (array-rank array)))
    (cond
      ((= rank 1)
       (aref array (first index)))
      ((= rank 2)
       (aref array (first index) (second index)))
      ((= rank 3)
       (aref array (first index) (second index) (third index)))
      (t (apply #'aref array index)))))

(declaim (inline (setf index)))
(defun (setf index) (val array index)
  (let ((rank (array-rank array)))
    (cond
      ((= rank 1)
       (setf (aref array (first index)) val))
      ((= rank 2)
       (setf (aref array (first index) (second index)) val))
      ((= rank 3)
       (setf (aref array (first index) (second index) (third index)) val))
      (t (setf (apply #'aref array index) val)))))

(sera:-> label-components ((simple-array bit))
         (values (simple-array fixnum) &optional))
(defun label-components (array)
  "Label connected components in a binary array @c(array). Zeros are
left as is, ones are labeled with a positive label."
  (declare (optimize (speed 3)))
  (let* ((dimensions (array-dimensions array))
         (lbls (make-array dimensions :element-type 'fixnum :initial-element -1))
         (indices (si:indices dimensions))
         (current-label 1)
         (pattern (make-pattern (array-rank array))))
    (declare (type fixnum current-label))
    (si:do-iterator (idx indices)
      (let (queue)
        (flet ((push-in-queue (idx)
                 (cond
                   ;; If an element is a background element, label it as so
                   ((zerop (the bit (index array idx)))
                    (setf  (index lbls idx) 0)
                    0)
                   ;; If the element does not have a label, assign the current label
                   ((= (the fixnum (index lbls idx)) -1)
                    (setf (index lbls idx) current-label)
                    (push idx queue)
                    1)
                   ;; Already has a label - skip
                   (t 0))))
          (loop with delta fixnum = (push-in-queue idx)
                until (null queue) do
                (let ((idx2 (pop queue)))
                  (dolist (offset pattern)
                    (let ((shifted
                           (mapcar
                            (lambda (i o d)
                              (declare (type (signed-byte 32) i o d))
                              (mod (+ i o) d))
                            idx2 offset dimensions)))
                      (push-in-queue shifted))))
                finally (incf current-label delta)))))
    lbls))

(sera:-> histogram ((simple-array fixnum))
         (values (simple-array alex:non-negative-fixnum (*)) &optional))
(defun histogram (array)
  "Compute a histogram for an array of labels."
  (let* ((max-label (reduce #'max (aops:flatten array)))
         (histogram (make-array max-label
                                :element-type 'alex:non-negative-fixnum
                                :initial-element 0)))
    (loop for i below (array-total-size array)
          for x = (row-major-aref array i)
          unless (zerop x) do
          (incf (aref histogram (1- x))))
    histogram))

(sera:-> only-one-cluster ((simple-array bit))
         (values (simple-array bit) &optional))
(defun only-one-cluster (array)
  "Remove all clusters of zeros from an array with exception of the
largest."
  (let* ((lbls (label-components (invert array)))
         (histogram (histogram lbls))
         (max (reduce #'max histogram))
         (pos (1+ (position max histogram :test #'=))))
    (aops:vectorize* 'bit
        (lbls)
      (if (= lbls pos) 0 1))))

;; Ball packings and voronoi diagrams

(deftype point () `(simple-array single-float (*)))

(sera:-> make-point (list)
         (values point &optional))
(declaim (inline make-point))
(defun make-point (coords)
  (make-array (length coords)
              :element-type 'single-float
              :initial-contents coords))

(sera:-> random-centers ((integer 1) (integer 1))
         (values list &optional))
(defun random-centers (n dim)
  "Generate @c(n) points in @c(dim)-dimensional space, which are
distributed independently and uniformly in the range \\([0, 1]\\)."
  (loop repeat n collect
        (make-point
         (loop repeat dim collect (random 1f0)))))

(sera:-> distance (point point)
         (values single-float &optional))
(defun distance (p1 p2)
  "Calculate the Euclidean distance between two points lying on a
torus of arbitrary dimensionality \\(n\\). Coordinates of the points
must be in the range \\([0, 1]^n\\)."
  (declare (optimize (speed 3)))
  (sqrt
   (loop for x1 across p1
         for x2 across p2
         for d = (abs (- x1 x2))
         sum (expt (min d (- (1- d))) 2)
         single-float)))
