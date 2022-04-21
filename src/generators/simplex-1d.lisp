(in-package #:cl-user)

;;;; 1-dimensional Simplex noise generator

(defpackage #:%cricket.generators.simplex-1d
  (:local-nicknames
   (#:gen #:%cricket.generators)
   (#:int #:%cricket.internal)
   (#:rng #:seedable-rng)
   (#:u #:mfiano-utils))
  (:use #:cl))

(in-package #:%cricket.generators.simplex-1d)

(u:define-constant +scale+ 0.395d0)

(defstruct (gen:simplex-1d
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (table nil :type (u:ub8a 512)))

(defun gen:simplex-1d (&key seed)
  "Construct a sampler that, when sampled, outputs 1-dimensional Simplex noise values ranging from
-1.0 to 1.0.

`seed`: A string used to seed the random number generator for this sampler, or NIL. If a seed is not
supplied, one will be generated automatically which will negatively affect the reproducibility of
the noise (optional, default: NIL)."
  (let ((rng (int::make-rng seed)))
    (make-simplex-1d :rng rng :table (int::perlin-permute rng))))

(defmethod int:sample ((sampler gen:simplex-1d) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (ignore y z w)
           (optimize speed)
           (int::f50 x y z w))
  (flet ((grad (hash x)
           (let* ((s (- 1 (* x x)))
                  (h (logand hash 15))
                  (grad (if (zerop (logand h 8))
                            (* (1+ (logand h 7)) x)
                            (* (- (1+ (logand h 7))) x))))
             (if (plusp s)
                 (* s s s s grad)
                 0d0))))
    (declare (inline grad))
    (u:mvlet* ((table (table sampler))
               (xi xf (floor x))
               (r1 (grad (int::lookup-wrap table xi) xf))
               (r2 (grad (int::lookup-wrap table (1+ xi)) (1- xf))))
      (float (* (+ r1 r2) +scale+) 1f0))))
