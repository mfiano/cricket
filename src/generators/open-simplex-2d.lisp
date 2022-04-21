(in-package #:cl-user)

;;;; 2-dimensional OpenSimplex noise generator

(defpackage #:%cricket.generators.open-simplex-2d
  (:local-nicknames
   (#:gen #:%cricket.generators)
   (#:int #:%cricket.internal)
   (#:rng #:seedable-rng)
   (#:u #:mfiano-utils))
  (:use #:cl))

(in-package #:%cricket.generators.open-simplex-2d)

(u:eval-always
  (u:define-constant +stretch+ (/ (1- (/ (sqrt 3d0))) 2))

  (u:define-constant +squish+ (/ (1- (sqrt 3d0)) 2))

  (u:define-constant +scale+ (/ 40.7d0))

  (u:define-constant +gradients+
      (let ((data '(5 2 2 5 -5 2 -2 5 5 -2 2 -5 -5 -2 -2 -5)))
        (make-array 16 :element-type 'fixnum :initial-contents data))
    :test #'equalp))

(defstruct (gen:open-simplex-2d
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (table nil :type (u:ub8a 512)))

(declaim (inline %make-state))
(defstruct (state
            (:constructor %make-state)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (sampler nil :type gen:open-simplex-2d)
  (stretch-offset 0d0 :type u:f64)
  (xsb 0 :type fixnum)
  (ysb 0 :type fixnum)
  (dx0 0d0 :type u:f64)
  (dy0 0d0 :type u:f64)
  (dx1 0d0 :type u:f64)
  (dy1 0d0 :type u:f64)
  (dx2 0d0 :type u:f64)
  (dy2 0d0 :type u:f64)
  (dx-ext 0d0 :type u:f64)
  (dy-ext 0d0 :type u:f64)
  (xsv-ext 0 :type fixnum)
  (ysv-ext 0 :type fixnum)
  (xins 0d0 :type u:f64)
  (yins 0d0 :type u:f64)
  (ins 0d0 :type u:f64)
  (value 0d0 :type u:f64))

(declaim (inline make-state))
(defun make-state (sampler x y)
  (let* ((stretch-offset (* (+ x y) +stretch+))
         (xs (+ x stretch-offset))
         (ys (+ y stretch-offset))
         (xsb (floor xs))
         (ysb (floor ys))
         (squish-offset (* (+ xsb ysb) +squish+))
         (dx0 (- x (+ xsb squish-offset)))
         (dy0 (- y (+ ysb squish-offset)))
         (xins (- xs xsb))
         (yins (- ys ysb)))
    (declare (int::f50 xs ys))
    (%make-state :sampler sampler
                 :xsb xsb
                 :ysb ysb
                 :dx0 dx0
                 :dy0 dy0
                 :dx1 (- dx0 1 +squish+)
                 :dy1 (- dy0 +squish+)
                 :dx2 (- dx0 +squish+)
                 :dy2 (- dy0 1 +squish+)
                 :xins xins
                 :yins yins
                 :ins (+ xins yins))))

(declaim (inline extrapolate))
(defun extrapolate (table xsb ysb dx dy)
  (let ((index (logand (int::lookup-wrap table ysb xsb) 14)))
    (+ (* (aref +gradients+ index) dx)
       (* (aref +gradients+ (1+ index)) dy))))

(declaim (inline contribute))
(defun contribute (state dx dy xsb ysb)
  (let ((a (- 2 (* dx dx) (* dy dy))))
    (when (plusp a)
      (incf (value state)
            (* a a a a (extrapolate (table (sampler state)) xsb ysb dx dy))))
    (values)))

(declaim (inline contribute1))
(defun contribute1 (state)
  (let ((xsb (xsb state))
        (ysb (ysb state)))
    (contribute state (dx1 state) (dy1 state) (1+ xsb) ysb)
    (contribute state (dx2 state) (dy2 state) xsb (1+ ysb))))

(declaim (inline contribute2))
(defun contribute2 (state)
  (contribute state (dx0 state) (dy0 state) (xsb state) (ysb state))
  (contribute state (dx-ext state) (dy-ext state) (xsv-ext state) (ysv-ext state)))

(declaim (inline in1))
(defun in1 (state)
  (let ((xins (xins state))
        (yins (yins state))
        (zins (- 1 (ins state)))
        (xsb (xsb state))
        (ysb (ysb state))
        (dx0 (dx0 state))
        (dy0 (dy0 state))
        (sq2 #.(* +squish+ 2)))
    (if (or (> zins xins) (> zins yins))
        (if (> xins yins)
            (psetf (xsv-ext state) (1+ xsb)
                   (ysv-ext state) (1- ysb)
                   (dx-ext state) (1- dx0)
                   (dy-ext state) (1+ dy0))
            (psetf (xsv-ext state) (1- xsb)
                   (ysv-ext state) (1+ ysb)
                   (dx-ext state) (1+ dx0)
                   (dy-ext state) (1- dy0)))
        (psetf (xsv-ext state) (1+ xsb)
               (ysv-ext state) (1+ ysb)
               (dx-ext state) (- dx0 1 sq2)
               (dy-ext state) (- dy0 1 sq2)))
    (values)))

(declaim (inline in2))
(defun in2 (state)
  (let ((xins (xins state))
        (yins (yins state))
        (zins (- 2 (ins state)))
        (xsb (xsb state))
        (ysb (ysb state))
        (dx0 (dx0 state))
        (dy0 (dy0 state))
        (sq2 #.(* +squish+ 2)))
    (if (or (< zins xins) (< zins yins))
        (if (> xins yins)
            (psetf (xsv-ext state) (+ xsb 2)
                   (ysv-ext state) ysb
                   (dx-ext state) (- dx0 2 sq2)
                   (dy-ext state) (- dy0 sq2))
            (psetf (xsv-ext state) xsb
                   (ysv-ext state) (+ ysb 2)
                   (dx-ext state) (- dx0 sq2)
                   (dy-ext state) (- dy0 2 sq2)))
        (psetf (dx-ext state) dx0
               (dy-ext state) dy0
               (xsv-ext state) xsb
               (ysv-ext state) ysb))
    (incf (xsb state))
    (incf (ysb state))
    (psetf (dx0 state) (- dx0 1 sq2)
           (dy0 state) (- dy0 1 sq2))
    (values)))

(defun gen:open-simplex-2d (&key seed)
  "Construct a sampler that, when sampled, outputs 2-dimensional OpenSimplex noise values ranging
from -1.0 to 1.0.

`seed`: A string used to seed the random number generator for this sampler, or NIL. If a seed is not
supplied, one will be generated automatically which will negatively affect the reproducibility of
the noise (optional, default: NIL)."
  (let ((rng (int::make-rng seed)))
    (make-open-simplex-2d :rng rng :table (int::perlin-permute rng))))

(defmethod int:sample ((sampler gen:open-simplex-2d) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (ignore z w)
           (optimize speed)
           (int::f50 x y z w))
  (let ((state (make-state sampler x y)))
    (contribute1 state)
    (if (<= (ins state) 1)
        (in1 state)
        (in2 state))
    (contribute2 state)
    (float (* (value state) +scale+) 1f0)))
