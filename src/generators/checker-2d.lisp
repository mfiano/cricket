(in-package #:cl-user)

;;;; 2-dimensional checkered pattern generator

(defpackage #:%cricket.generators.checker-2d
  (:local-nicknames
   (#:gen #:%cricket.generators)
   (#:int #:%cricket.internal)
   (#:rng #:seedable-rng)
   (#:u #:mfiano-utils))
  (:use #:cl))

(in-package #:%cricket.generators.checker-2d)

(defstruct (gen:checker-2d
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil)))

(defun gen:checker-2d (&key seed)
  "Construct a sampler that, when sampled, outputs a 2-dimensional checkered grid pattern, with
values being either -1.0 or 1.0.

`seed`: A string used to seed the random number generator for this sampler, or NIL. If a seed is not
supplied, one will be generated automatically which will negatively affect the reproducibility of
the noise (optional, default: NIL)."
  (make-checker-2d :rng (int::make-rng seed)))

(defmethod int:sample ((sampler gen:checker-2d) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (ignore w)
           (optimize speed)
           (int::f50 x y z w))
  (if (zerop (logxor (logand (floor x) 1)
                     (logand (floor y) 1)
                     (logand (floor z) 1)))
      1.0
      -1.0))
