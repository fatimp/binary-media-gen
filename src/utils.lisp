(in-package :binary-media-gen)

(deftype point () `(simple-array single-float (*)))

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
  (si:collect (si:power (si:list->iterator '(-1 0 +1)) dim)))

(declaim (inline index))
(defun index (array index)
  (let ((rank (array-rank array)))
    (cond
      ((= rank 1)
       (destructuring-bind (i) index
         (aref array i)))
      ((= rank 2)
       (destructuring-bind (i j) index
         (aref array i j)))
      ((= rank 3)
       (destructuring-bind (i j k) index
         (aref array i j k)))
      (t (apply #'aref array index)))))

(declaim (inline (setf index)))
(defun (setf index) (val array index)
  (let ((rank (array-rank array)))
    (cond
      ((= rank 1)
       (destructuring-bind (i) index
         (setf (aref array i) val)))
      ((= rank 2)
       (destructuring-bind (i j) index
         (setf (aref array i j) val)))
      ((= rank 3)
       (destructuring-bind (i j k) index
         (setf (aref array i j k) val)))
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
