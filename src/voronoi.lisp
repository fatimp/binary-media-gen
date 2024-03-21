(in-package :binary-media-gen)

(sera:-> normalize (point)
         (values point &optional))
(defun normalize (p)
  (declare (optimize (speed 3)))
  "Normalize a vector."
  (let ((norm (sqrt (loop for c across p sum (expt c 2) single-float))))
    (if (zerop norm) p
        (map '(vector single-float)
             (lambda (x) (/ x norm))
             p))))

(sera:-> mul-add (point point single-float)
         (values point &optional))
(defun mul-add (a b c)
  "Calculate \\(a + bc\\) where \\(a, b\\) are vectors and \\(c\\) is
a scalar."
  (declare (optimize (speed 3)))
  (map '(vector single-float)
       (lambda (x1 x2)
         (+ x1 (* x2 c)))
       a b))

(sera:-> gradient (point point)
         (values point &optional))
(defun gradient (point center)
  "Calculate gradient of a function \\(f_c(p) = \\rho(c, p)\\) where
\\(\\rho\\) is the function @c(distance), \\(c\\) is a fixed point
@c(center) and \\(p\\) is an arbitrary point."
  (declare (optimize (speed 3)))
  (map '(vector single-float)
       (lambda (x y)
         (let ((d (abs (- x y))))
           (- x (cond
                  ((< d 0.5) y)
                  ((> x 0.5) (1+ y))
                  (t (1- y))))))
       point center))

;; FIXME: faster than VP-trees for a small number of centers (5-15).
(sera:-> nearest-neighbor (point list)
         (values point &optional))
(defun nearest-neighbor (point centers)
  "Return a point from a list which is the closest to the point
@c(point)."
  (declare (optimize (speed 3)))
  (loop with distance single-float = ff:single-float-positive-infinity
        with closest = point
        for center in centers
        for cd = (distance point center)
        when (< cd distance) do
        (setq distance cd
              closest center)
        finally (return closest)))

(deftype voronoi-type () '(member :edges :surface))

(sera:-> on-the-surface-p (list point single-float)
         (values boolean &optional))
(defun on-the-surface-p (centers point Δ)
  "Test if @c(point) lies close (with the distance no more than @c(Δ))
to the surface of Voronoi diagram's cell defined by a list of dots
@c(centers)."
  (declare (optimize (speed 3)))
  (let* ((closest (nearest-neighbor point centers))
         (gradient (normalize (gradient point closest)))
         (p (mul-add point gradient Δ)))
    (not (eq closest (nearest-neighbor p centers)))))

(sera:-> on-the-edge-p (list point single-float alex:positive-fixnum)
         (values boolean &optional))
(defun on-the-edge-p (centers point Δ dim)
  (declare (optimize (speed 3)))
  (let ((close-centers
         (si:collect
             (si:take
              dim (si:unfold
                   (lambda (point)
                     (let* ((center (nearest-neighbor point centers))
                            (gradient (normalize (gradient point center)))
                            (p (mul-add point gradient Δ)))
                       (values center p)))
                   point)))))
        (= (length (remove-duplicates close-centers :test #'eq)) dim)))

(defun check-dimensionality (dim points)
  (unless (every (lambda (point) (= (length point) dim)) points)
    (error "Not all points are of dimensionality ~d" dim)))

(sera:-> in-the-set-p (list point single-float alex:positive-fixnum voronoi-type)
         (values boolean &optional))
(defun in-the-set-p (centers point Δ dim type)
  (declare (optimize (speed 3)))
  (ecase type
    (:edges   (on-the-edge-p centers point Δ dim))
    (:surface (on-the-surface-p centers point Δ))))

(sera:-> voronoi-diagram
         (alex:positive-fixnum alex:positive-fixnum list single-float &optional voronoi-type)
         (values (simple-array bit) &optional))
(defun voronoi-diagram (side dim points Δ &optional (type :edges))
  "Generate @c(dim)-dimensional Voronoi diagram for the points
@c(points) in a cube with a side @c(side). @c(Δ) is a thickness of
cells. @c(types) can be either @c(:edges) or @c(:surface)."
  (declare (optimize (speed 3)))
  (check-dimensionality dim points)
  (let ((array (make-array (loop repeat dim collect side)
                           :element-type 'bit))
        (indices (si:power (si:range 0 side) dim))
        (side (float side)))
    (si:do-iterator (coord indices)
      (setf (index array coord)
            (let ((coords-in-unit-range
                   (mapcar
                    (lambda (x)
                      (declare (type alex:non-negative-fixnum x))
                      (/ x side))
                    coord)))
              (if (in-the-set-p points (make-point coords-in-unit-range) Δ dim type)
                  1 0))))
    array))
