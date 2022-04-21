(in-package #:cl-user)

;;;; Power modifier
;;;; This noise modifier raises the the output of its first input sampler to the power of the output
;;;; of its second input sampler.

(defpackage #:%cricket.modifiers.power
  (:local-nicknames
   (#:int #:%cricket.internal)
   (#:mod #:%cricket.modifiers)
   (#:u #:mfiano-utils))
  (:use #:cl))

(in-package #:%cricket.modifiers.power)

(defstruct (mod:power
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source1 nil :type int:sampler)
  (source2 nil :type int:sampler))

(defun mod:power (source1 source2)
  "Construct a sampler that, when sampled, raises the output of `source1` to the power of the output
of `source2`.

`source1`: The first input sampler (required).

`source2`: The second input sampler (required)."
  (unless (typep source1 'int:sampler)
    (error 'int:invalid-sampler-argument :sampler-type 'power :argument 'source1 :value source1))
  (unless (typep source2 'int:sampler)
    (error 'int:invalid-sampler-argument :sampler-type 'power :argument 'source2 :value source2))
  (make-power :rng (int::sampler-rng source1) :source1 source1 :source2 source2))

(defmethod int:sample ((sampler mod:power) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (optimize speed))
  (let ((sample1 (abs (* (1+ (the u:f32 (int:sample (source1 sampler) x y z w))) 0.5)))
        (sample2 (abs (* (1+ (the u:f32 (int:sample (source2 sampler) x y z w))) 0.5))))
    (1- (* (expt sample1 sample2) 2))))
