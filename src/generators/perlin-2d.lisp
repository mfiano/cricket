(in-package #:cl-user)

;;;; 2-dimensional Perlin ("Improved") noise generator

(defpackage #:%cricket.generators.perlin-2d
  (:local-nicknames
   (#:gen #:%cricket.generators)
   (#:int #:%cricket.internal)
   (#:rng #:seedable-rng)
   (#:u #:mfiano-utils))
  (:use #:cl))

(in-package #:%cricket.generators.perlin-2d)

(defstruct (gen:perlin-2d
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (table nil :type (u:ub8a 512)))

(defun gen:perlin-2d (&key seed)
  "Construct a sampler that, when sampled, outputs 2-dimensional Perlin Improved noise values
ranging from -1.0 to 1.0.

`seed`: A string used to seed the random number generator for this sampler, or NIL. If a seed is not
supplied, one will be generated automatically which will negatively affect the reproducibility of
the noise (optional, default: NIL)."
  (let ((rng (int::make-rng seed)))
    (make-perlin-2d :rng rng :table (int::perlin-permute rng))))

(defmethod int:sample ((sampler gen:perlin-2d) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (ignore z w)
           (optimize speed)
           (int::f50 x y z w))
  (flet ((grad (hash x y)
           (let* ((h (logand hash 7))
                  (u (if (< h 4) x y))
                  (v (if (< h 4) y x)))
             (+ (if (zerop (logand h 1)) u (- u))
                (if (zerop (logand h 2)) v (- v))))))
    (declare (inline grad))
    (u:mvlet* ((table (table sampler))
               (xi xf (floor x))
               (yi yf (floor y))
               (xi (logand xi 255))
               (yi (logand yi 255))
               (u (int::quintic-curve xf))
               (a (+ (aref table xi) yi))
               (b (+ (aref table (1+ xi)) yi))
               (r1 (grad (int::lookup table (aref table a)) xf yf))
               (r2 (grad (int::lookup table (aref table b)) (1- xf) yf))
               (r3 (grad (int::lookup table (aref table (1+ a))) xf (1- yf)))
               (r4 (grad (int::lookup table (aref table (1+ b))) (1- xf) (1- yf))))
      (float (u:lerp (int::quintic-curve yf) (u:lerp u r1 r2) (u:lerp u r3 r4)) 1f0))))
