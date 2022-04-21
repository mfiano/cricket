(in-package #:cl-user)

;;;; Constant value generator

(defpackage #:%cricket.generators.constant
  (:local-nicknames
   (#:gen #:%cricket.generators)
   (#:int #:%cricket.internal)
   (#:rng #:seedable-rng)
   (#:u #:mfiano-utils))
  (:use #:cl))

(in-package #:%cricket.generators.constant)

(defstruct (gen:constant
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (value 0.5 :type u:f32))

(defun gen:constant (value &key seed)
  "Construct a sampler that, when sampled, outputs the constant `value` supplied. This is useful for
debugging and applications where you need to combine a constant value.

`seed`: A string used to seed the random number generator for this sampler, or NIL. If a seed is not
supplied, one will be generated automatically which will negatively affect the reproducibility of
the noise (optional, default: NIL)."
  (unless (realp value)
    (error 'int:invalid-real-argument
           :sampler-type 'constant
           :argument 'value
           :value value))
  (make-constant :rng (int::make-rng seed)
                 :value (float value 1f0)))

(defmethod int:sample ((sampler gen:constant) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (ignore x y z w)
           (optimize speed)
           (int::f50 x y z w))
  (value sampler))
