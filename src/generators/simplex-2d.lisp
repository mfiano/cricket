(in-package #:cl-user)

(defpackage #:coherent-noise.generators.simplex-2d
  (:local-nicknames
   (#:gen #:coherent-noise.generators)
   (#:int #:coherent-noise.internal)
   (#:rng #:seedable-rng)
   (#:u #:golden-utils))
  (:use #:cl))

(in-package #:coherent-noise.generators.simplex-2d)

(u:eval-always
  (u:define-constant +skew-factor+ (/ (1- (sqrt 3d0)) 2))

  (u:define-constant +unskew-factor+ (/ (- 3 (sqrt 3d0)) 6))

  (u:define-constant +scale+ 45.23065d0))

(defstruct (simplex-2d
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (table int::+perlin-permutation+ :type (simple-array u:ub8 (512))))

(defun gen:simplex-2d (&key seed)
  (let* ((rng (int::make-rng seed))
         (table (rng:shuffle rng int::+perlin-permutation+)))
    (make-simplex-2d :rng rng
                     :table table)))

(defmethod int:sample ((sampler simplex-2d) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (ignore z w)
           (optimize speed)
           (int::f50 x y z w))
  (flet ((get-simplex (x y)
           (if (> x y)
               (values 1 0)
               (values 0 1)))
         (noise (hash x y)
           (let* ((s (- 0.5 (* x x) (* y y)))
                  (h (logand hash 7))
                  (u (if (< h 4) x y))
                  (v (if (< h 4) y x))
                  (grad (+ (if (zerop (logand h 1)) u (- u))
                           (if (zerop (logand h 2)) (* v 2) (* v -2)))))
             (if (plusp s)
                 (* s s s s grad)
                 0d0))))
    (declare (inline get-simplex noise))
    (u:mvlet* ((table (table sampler))
               (s (* (+ x y) +skew-factor+))
               (i (floor (+ x s)))
               (j (floor (+ y s)))
               (tx (* (+ i j) +unskew-factor+))
               (x1 (- x (- i tx)))
               (y1 (- y (- j tx)))
               (i1 j1 (get-simplex x1 y1))
               (x2 (+ (- x1 i1) +unskew-factor+))
               (y2 (+ (- y1 j1) +unskew-factor+))
               (x3 (+ (1- x1) #.(* +unskew-factor+ 2)))
               (y3 (+ (1- y1) #.(* +unskew-factor+ 2))))
      (float (* (+ (noise (int::lookup table i j) x1 y1)
                   (noise (int::lookup table (+ i i1) (+ j j1)) x2 y2)
                   (noise (int::lookup table (1+ i) (1+ j)) x3 y3))
                +scale+)
             1f0))))
