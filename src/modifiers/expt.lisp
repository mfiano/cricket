(in-package #:cl-user)

;;;; Exponent modifier
;;;; This noise modifier raises the power of the output of its input sampler to the given scalar.

(defpackage #:%cricket.modifiers.expt
  (:local-nicknames
   (#:int #:%cricket.internal)
   (#:mod #:%cricket.modifiers)
   (#:u #:mfiano-utils))
  (:use #:cl)
  (:shadow #:expt))

(in-package #:%cricket.modifiers.expt)

(defstruct (mod:expt
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source nil :type int:sampler)
  (power 1.0 :type u:f32))

(defun mod:expt (source power)
  "Construct a sampler that, when sampled, raises the power of the output of `source` to `power`.

`source`: The input sampler (required).

`power`: The power to raise the output to (required)."
  (unless (typep source 'int:sampler)
    (error 'int:invalid-sampler-argument :sampler-type 'expt :argument 'source :value source))
  (unless (realp power)
    (error 'int:invalid-real-argument :sampler-type 'expt :argument 'power :value power))
  (make-expt :rng (int::sampler-rng source)
             :source source
             :power (float power 1f0)))

(defmethod int:sample ((sampler mod:expt) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (optimize speed))
  (let ((sample (the u:f32 (int:sample (source sampler) x y z w))))
    (1- (* (cl:expt (abs (* (1+ sample) 0.5)) (power sampler)) 2))))
