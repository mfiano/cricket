(in-package #:cl-user)

;;;; 4-dimensional Simplex noise generator

(defpackage #:%cricket.generators.simplex-4d
  (:local-nicknames
   (#:gen #:%cricket.generators)
   (#:int #:%cricket.internal)
   (#:rng #:seedable-rng)
   (#:u #:mfiano-utils))
  (:use #:cl))

(in-package #:%cricket.generators.simplex-4d)

(u:eval-always
  (u:define-constant +skew-factor+ (/ (1- (sqrt 5d0)) 4))

  (u:define-constant +unskew-factor+ (/ (- 5 (sqrt 5d0)) 20))

  (u:define-constant +scale+ 27d0)

  (u:define-constant +table+
      (make-array 256
                  :element-type 'u:ub8
                  :initial-contents '(0 1 2 3 0 1 3 2 0 0 0 0 0 2 3 1 0 0 0 0 0 0 0 0 0 0 0 0 1 2 3
                                      0 0 2 1 3 0 0 0 0 0 3 1 2 0 3 2 1 0 0 0 0 0 0 0 0 0 0 0 0 1 3
                                      2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                                      0 0 0 1 2 0 3 0 0 0 0 1 3 0 2 0 0 0 0 0 0 0 0 0 0 0 0 2 3 0 1
                                      2 3 1 0 1 0 2 3 1 0 3 2 0 0 0 0 0 0 0 0 0 0 0 0 2 0 3 1 0 0 0
                                      0 2 1 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                                      0 0 0 0 0 0 2 0 1 3 0 0 0 0 0 0 0 0 0 0 0 0 3 0 1 2 3 0 2 1 0
                                      0 0 0 3 1 2 0 2 1 0 3 0 0 0 0 0 0 0 0 0 0 0 0 3 1 0 2 0 0 0 0
                                      3 2 0 1 3 2 1 0))
    :test #'equalp))

(defstruct (gen:simplex-4d
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (table nil :type (u:ub8a 512)))

(defun gen:simplex-4d (&key seed)
  "Construct a sampler that, when sampled, outputs 4-dimensional Simplex noise values ranging from
-1.0 to 1.0.

`seed`: A string used to seed the random number generator for this sampler, or NIL. If a seed is not
supplied, one will be generated automatically which will negatively affect the reproducibility of
the noise (optional, default: NIL)."
  (let ((rng (int::make-rng seed)))
    (make-simplex-4d :rng rng :table (int::perlin-permute rng))))

(defmethod int:sample ((sampler gen:simplex-4d) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (optimize speed)
           (int::f50 x y z w))
  (flet ((get-simplex (x y z w)
           (let* ((c1 (if (> x y) 32 0))
                  (c2 (if (> x z) 16 0))
                  (c3 (if (> y z) 8 0))
                  (c4 (if (> x w) 4 0))
                  (c5 (if (> y w) 2 0))
                  (c6 (if (> z w) 1 0))
                  (c (* 4 (+ c1 c2 c3 c4 c5 c6)))
                  (a0 (aref +table+ (+ c 0)))
                  (a1 (aref +table+ (+ c 1)))
                  (a2 (aref +table+ (+ c 2)))
                  (a3 (aref +table+ (+ c 3))))
             (values (if (>= a0 3) 1 0)
                     (if (>= a1 3) 1 0)
                     (if (>= a2 3) 1 0)
                     (if (>= a3 3) 1 0)
                     (if (>= a0 2) 1 0)
                     (if (>= a1 2) 1 0)
                     (if (>= a2 2) 1 0)
                     (if (>= a3 2) 1 0)
                     (if (>= a0 1) 1 0)
                     (if (>= a1 1) 1 0)
                     (if (>= a2 1) 1 0)
                     (if (>= a3 1) 1 0))))
         (grad (hash x y z w)
           (let* ((s (- 0.6 (* x x) (* y y) (* z z) (* w w)))
                  (h (logand hash 31))
                  (u (if (< h 24) x y))
                  (v (if (< h 16) y z))
                  (w (if (< h 8) z w))
                  (grad (+ (if (zerop (logand h 1)) u (- u))
                           (if (zerop (logand h 2)) v (- v))
                           (if (zerop (logand h 4)) w (- w)))))
             (if (plusp s)
                 (* s s s s grad)
                 0d0))))
    (declare (inline get-simplex grad))
    (u:mvlet* ((table (table sampler))
               (s (* (+ x y z w) +skew-factor+))
               (i (floor (+ x s)))
               (j (floor (+ y s)))
               (k (floor (+ z s)))
               (l (floor (+ w s)))
               (tx (* (+ i j k l) +unskew-factor+))
               (x1 (- x (- i tx)))
               (y1 (- y (- j tx)))
               (z1 (- z (- k tx)))
               (w1 (- w (- l tx)))
               (i1 j1 k1 l1 i2 j2 k2 l2 i3 j3 k3 l3 (get-simplex x1 y1 z1 w1))
               (x2 (+ (- x1 i1) +unskew-factor+))
               (y2 (+ (- y1 j1) +unskew-factor+))
               (z2 (+ (- z1 k1) +unskew-factor+))
               (w2 (+ (- w1 l1) +unskew-factor+))
               (x3 (+ (- x1 i2) #.(* +unskew-factor+ 2)))
               (y3 (+ (- y1 j2) #.(* +unskew-factor+ 2)))
               (z3 (+ (- z1 k2) #.(* +unskew-factor+ 2)))
               (w3 (+ (- w1 l2) #.(* +unskew-factor+ 2)))
               (x4 (+ (- x1 i3) #.(* +unskew-factor+ 3)))
               (y4 (+ (- y1 j3) #.(* +unskew-factor+ 3)))
               (z4 (+ (- z1 k3) #.(* +unskew-factor+ 3)))
               (w4 (+ (- w1 l3) #.(* +unskew-factor+ 3)))
               (x5 (+ (1- x1) #.(* +unskew-factor+ 4)))
               (y5 (+ (1- y1) #.(* +unskew-factor+ 4)))
               (z5 (+ (1- z1) #.(* +unskew-factor+ 4)))
               (w5 (+ (1- w1) #.(* +unskew-factor+ 4)))
               (r1 (grad (int::lookup-wrap table i j k l) x1 y1 z1 w1))
               (r2 (grad (int::lookup-wrap table (+ i i1) (+ j j1) (+ k k1) (+ l l1)) x2 y2 z2 w2))
               (r3 (grad (int::lookup-wrap table (+ i i2) (+ j j2) (+ k k2) (+ l l2)) x3 y3 z3 w3))
               (r4 (grad (int::lookup-wrap table (+ i i3) (+ j j3) (+ k k3) (+ l l3)) x4 y4 z4 w4))
               (r5 (grad (int::lookup-wrap table (1+ i) (1+ j) (1+ k) (1+ l)) x5 y5 z5 w5)))
      (float (* (+ r1 r2 r3 r4 r5) +scale+) 1f0))))
