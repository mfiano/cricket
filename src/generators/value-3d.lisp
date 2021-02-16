(in-package #:cl-user)

(defpackage #:coherent-noise.generators.value-3d
  (:local-nicknames
   (#:gen #:coherent-noise.generators)
   (#:int #:coherent-noise.internal)
   (#:rng #:seedable-rng)
   (#:u #:golden-utils))
  (:use #:cl))

(in-package #:coherent-noise.generators.value-3d)

(defstruct (value-3d
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (seed 0 :type u:ub32))

(defun gen:value-3d (&key seed)
  (u:mvlet ((rng seed (int::make-rng seed)))
    (make-value-3d :rng rng
                   :seed seed)))

(defmethod int:sample ((sampler value-3d) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (ignore w)
           (optimize speed)
           (int::f50 x y z w))
  (labels ((in-range (x)
             (logand x #.(1- (expt 2 32))))
           (coord (seed x y z)
             (let ((hash (expt (in-range (* (logxor seed x y z) 668265261)) 2)))
               (* (in-range (logxor hash (ash hash -19)))
                  (/ 2147483648.0)))))
    (declare (inline in-range coord))
    (u:mvlet* ((seed (seed sampler))
               (x0 xs (floor x))
               (xs (int::cubic-curve xs))
               (x0 (in-range (* x0 int::+prime-x+)))
               (x1 (+ x0 int::+prime-x+))
               (y0 ys (floor y))
               (ys (int::cubic-curve ys))
               (y0 (in-range (* y0 int::+prime-y+)))
               (y1 (+ y0 int::+prime-y+))
               (z0 zs (floor z))
               (zs (int::cubic-curve zs))
               (z0 (in-range (* z0 int::+prime-z+)))
               (z1 (+ z0 int::+prime-z+)))
      (float
       (1- (u:lerp zs
                   (u:lerp ys
                           (u:lerp xs (coord seed x0 y0 z0) (coord seed x1 y0 z0))
                           (u:lerp xs (coord seed x0 y1 z0) (coord seed x1 y1 z0)))
                   (u:lerp ys
                           (u:lerp xs (coord seed x0 y0 z1) (coord seed x1 y0 z1))
                           (u:lerp xs (coord seed x0 y1 z1) (coord seed x1 y1 z1)))))
       1f0))))
