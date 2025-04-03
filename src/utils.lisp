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

(deftype sets-map () '(simple-array alex:non-negative-fixnum (*)))

(sera:-> make-sets-map (alex:non-negative-fixnum)
         (values sets-map &optional))
(defun make-sets-map (n)
  (let ((map (make-array n :element-type 'alex:non-negative-fixnum)))
    (loop for i below n do (setf (aref map i) i))
    map))

(sera:-> sets-find! (sets-map alex:non-negative-fixnum)
         (values alex:non-negative-fixnum &optional))
(defun sets-find! (map id)
  (let ((parent (aref map id)))
    (if (= parent id) id
        (let ((root (sets-find! map parent)))
          (setf (aref map id) root)))))

(sera:-> sets-union! (sets-map alex:non-negative-fixnum alex:non-negative-fixnum)
         (values alex:non-negative-fixnum &optional))
(defun sets-union! (map id1 id2)
  (let ((root1 (sets-find! map id1))
        (root2 (sets-find! map id2)))
    (setf (aref map root2) root1)))

(sera:-> make-pattern (alex:positive-fixnum)
         (values list &optional))
(defun make-pattern (dim)
  (loop for d1 below dim collect
        (loop for d2 below dim collect
              (if (= d1 d2) -1 0))))

(sera:-> add-indices (list list)
         (values list &optional))
(declaim (inline add-indices))
(defun add-indices (idx1 idx2)
  (mapcar
   (lambda (i j)
     (declare (type fixnum i j))
     (+ i j))
   idx1 idx2))

(sera:-> index (simple-array list)
         (values fixnum &optional))
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

(sera:-> (setf index) (fixnum simple-array list)
         (values fixnum &optional))
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

(sera:-> neighbor ((simple-array fixnum) list)
         (values fixnum &optional))
(defun neighbor (array coords)
  (if (some (lambda (x) (< x 0)) coords) -1
      (index array coords)))

(sera:-> non-background ((simple-array fixnum) list)
         (values list fixnum &optional))
(defun non-background (array neighbors)
  (declare (optimize (speed 3)))
  (labels ((%go (neighbors non-background min)
             (declare (type fixnum min))
             (if (null neighbors)
                 (values non-background min)
                 (let ((lbl (neighbor array (car neighbors))))
                   (if (= lbl -1)
                       (%go (cdr neighbors) non-background min)
                       (%go (cdr neighbors)
                            (cons (car neighbors) non-background)
                            (min min lbl)))))))
    (%go neighbors nil most-positive-fixnum)))

(sera:-> label-components! ((simple-array fixnum))
         (values (simple-array fixnum) &optional))
(defun label-components! (image)
  "Label connected components in a binary array @c(array). Zeros are
labeled as @c(-1), ones are labeled with a positive label. This
function modifies its argument."
  (declare (optimize (speed 3)))
  (let ((map (make-sets-map (floor (reduce #'+ (aops:flatten image)) 2)))
        (current-label 0)
        (pattern (make-pattern (array-rank image))))
    (declare (type fixnum current-label))
    (si:do-iterator (idx (si:indices (array-dimensions image)))
      (if (zerop (index image idx))
          (setf (index image idx) -1)
          (multiple-value-bind (non-background min-lbl)
              (non-background
               image (mapcar (lambda (offset) (add-indices idx offset)) pattern))
            (cond
              ((null non-background)
               (setf (index image idx) current-label)
               (incf current-label))
              (t
               (setf (index image idx) min-lbl)
               (dolist (neighbor non-background)
                 (sets-union! map min-lbl (index image neighbor))))))))

    (loop for i below (array-total-size image)
          for x = (row-major-aref image i)
          when (/= x -1) do
          (setf (row-major-aref image i)
                (sets-find! map (row-major-aref image i)))))
  image)

(sera:-> label-components ((simple-array bit))
         (values (simple-array fixnum) &optional))
(defun label-components (array)
  "Label connected components in a binary array @c(array). Zeros are
labeled as @c(-1), ones are labeled with a positive label."
  (label-components!
   (aops:vectorize* 'fixnum
       (array)
     array)))

(sera:-> histogram ((simple-array fixnum))
         (values list &optional))
(defun histogram (array)
  "Compute a histogram for an array of labels."
  (let ((table
         (loop with histogram = (make-hash-table)
               for i below (array-total-size array)
               for x = (row-major-aref array i)
               unless (= x -1) do
               (incf (gethash x histogram 0))
               finally (return histogram)))
        hist)
    (maphash
     (lambda (k v)
       (push (cons v k) hist))
     table)
    hist))

(sera:-> remove-floating-solid ((simple-array bit))
         (values (simple-array bit) &optional))
(defun remove-floating-solid (array)
  "Remove all clusters of ones from an array with exception of the
largest."
  (let* ((lbls (label-components array))
         (histogram (histogram lbls))
         (max (reduce #'max histogram :key #'car))
         (label (cdr (assoc max histogram))))
    (aops:vectorize* 'bit
        (lbls)
      (if (= lbls label) 1 0))))

(sera:-> only-one-cluster ((simple-array bit))
         (values (simple-array bit) &optional))
(defun only-one-cluster (array)
  "Remove all clusters of zeros and ones from an array with exception
of the largest."
  (flet ((pass (array)
           (let* ((lbls (label-components array))
                  (histogram (histogram lbls))
                  (max (reduce #'max histogram :key #'car))
                  (label (cdr (assoc max histogram))))
             (aops:vectorize* 'bit
                 (lbls)
               (if (= lbls label) 0 1)))))
    (pass (pass array))))

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
