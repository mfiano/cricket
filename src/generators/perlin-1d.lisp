(in-package #:cl-user)

;;;; 1-dimensional Perlin ("Improved") noise generator

(defpackage #:%cricket.generators.perlin-1d
  (:local-nicknames
   (#:gen #:%cricket.generators)
   (#:int #:%cricket.internal)
   (#:rng #:seedable-rng)
   (#:u #:mfiano-utils))
  (:use #:cl))

(in-package #:%cricket.generators.perlin-1d)

(defstruct (gen:perlin-1d
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (table nil :type (u:ub8a 512)))

(defun gen:perlin-1d (&key seed)
  "Construct a sampler that, when sampled, outputs 1-dimensional Perlin Improved noise values
ranging from -1.0 to 1.0.

`seed`: A string used to seed the random number generator for this sampler, or NIL. If a seed is not
supplied, one will be generated automatically which will negatively affect the reproducibility of
the noise (optional, default: NIL)."
  (let ((rng (int::make-rng seed)))
    (make-perlin-1d :rng rng :table (int::perlin-permute rng))))

(defmethod int:sample ((sampler gen:perlin-1d) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (ignore y z w)
           (optimize speed)
           (int::f50 x y z w))
  (flet ((grad (hash x)
           (let* ((h (logand hash 15))
                  (grad (1+ (logand h 7))))
             (if (zerop (logand h 8))
                 (* grad x)
                 (* (- grad) x)))))
    (declare (inline grad))
    (u:mvlet* ((table (table sampler))
               (xi xf (floor x))
               (xi (logand xi 255))
               (r1 (grad (aref table xi) xf))
               (r2 (grad (aref table (1+ xi)) (1- xf))))
      (float (* (u:lerp (int::quintic-curve xf) r1 r2) 0.25) 1f0))))
