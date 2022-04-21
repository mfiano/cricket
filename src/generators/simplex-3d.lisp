(in-package #:cl-user)

;;;; 3-dimensional Simplex noise generator

(defpackage #:%cricket.generators.simplex-3d
  (:local-nicknames
   (#:gen #:%cricket.generators)
   (#:int #:%cricket.internal)
   (#:rng #:seedable-rng)
   (#:u #:mfiano-utils))
  (:use #:cl))

(in-package #:%cricket.generators.simplex-3d)

(u:eval-always
  (u:define-constant +skew-factor+ (/ 3d0))

  (u:define-constant +unskew-factor+ (/ 6d0))

  (u:define-constant +scale+ 32d0))

(defstruct (gen:simplex-3d
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (table nil :type (u:ub8a 512)))

(defun gen:simplex-3d (&key seed)
  "Construct a sampler that, when sampled, outputs 3-dimensional Simplex noise values ranging from
-1.0 to 1.0.

`seed`: A string used to seed the random number generator for this sampler, or NIL. If a seed is not
supplied, one will be generated automatically which will negatively affect the reproducibility of
the noise (optional, default: NIL)."
  (let ((rng (int::make-rng seed)))
    (make-simplex-3d :rng rng :table (int::perlin-permute rng))))

(defmethod int:sample ((sampler gen:simplex-3d) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (ignore w)
           (optimize speed)
           (int::f50 x y z w))
  (flet ((get-simplex (x y z)
           (if (>= x y)
               (cond
                 ((>= y z) (values 1 0 0 1 1 0))
                 ((>= x z) (values 1 0 0 1 0 1))
                 (t (values 0 0 1 1 0 1)))
               (cond
                 ((< y z) (values 0 0 1 0 1 1))
                 ((< x z) (values 0 1 0 0 1 1))
                 (t (values 0 1 0 1 1 0)))))
         (grad (hash x y z)
           (let* ((s (- 0.6 (* x x) (* y y) (* z z)))
                  (h (logand hash 15))
                  (u (if (< h 8) x y))
                  (v (case h
                       ((0 1 2 3) y)
                       ((12 14) x)
                       (t z)))
                  (grad (+ (if (zerop (logand h 1)) u (- u))
                           (if (zerop (logand h 2)) v (- v)))))
             (if (plusp s)
                 (* s s s s grad)
                 0d0))))
    (declare (inline get-simplex grad))
    (u:mvlet* ((table (table sampler))
               (s (* (+ x y z) +skew-factor+))
               (i (floor (+ x s)))
               (j (floor (+ y s)))
               (k (floor (+ z s)))
               (tx (* (+ i j k) +unskew-factor+))
               (x1 (- x (- i tx)))
               (y1 (- y (- j tx)))
               (z1 (- z (- k tx)))
               (i1 j1 k1 i2 j2 k2 (get-simplex x1 y1 z1))
               (x2 (+ (- x1 i1) +unskew-factor+))
               (y2 (+ (- y1 j1) +unskew-factor+))
               (z2 (+ (- z1 k1) +unskew-factor+))
               (x3 (+ (- x1 i2) #.(* +unskew-factor+ 2)))
               (y3 (+ (- y1 j2) #.(* +unskew-factor+ 2)))
               (z3 (+ (- z1 k2) #.(* +unskew-factor+ 2)))
               (x4 (+ (1- x1) #.(* +unskew-factor+ 3)))
               (y4 (+ (1- y1) #.(* +unskew-factor+ 3)))
               (z4 (+ (1- z1) #.(* +unskew-factor+ 3)))
               (r1 (grad (int::lookup-wrap table i j k) x1 y1 z1))
               (r2 (grad (int::lookup-wrap table (+ i i1) (+ j j1) (+ k k1)) x2 y2 z2))
               (r3 (grad (int::lookup-wrap table (+ i i2)  (+ j j2) (+ k k2)) x3 y3 z3))
               (r4 (grad (int::lookup-wrap table (1+ i) (1+ j) (1+ k)) x4 y4 z4)))
      (float (* (+ r1 r2 r3 r4) +scale+) 1f0))))
