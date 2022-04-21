(in-package #:cl-user)

;;;; Minimum modifier
;;;; This noise modifier takes the minimum output value of its two input samplers.

(defpackage #:%cricket.modifiers.min
  (:local-nicknames
   (#:int #:%cricket.internal)
   (#:mod #:%cricket.modifiers)
   (#:u #:mfiano-utils))
  (:use #:cl)
  (:shadow #:min))

(in-package #:%cricket.modifiers.min)

(defstruct (mod:min
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source1 nil :type int:sampler)
  (source2 nil :type int:sampler))

(defun mod:min (source1 source2)
  "Construct a sampler that, when sampled, outputs the minimum of the outputs of its two input
samplers.

`source1`: The first input sampler (required).

`source2`: The second input sampler (required)."
  (unless (typep source1 'int:sampler)
    (error 'int:invalid-sampler-argument :sampler-type 'min :argument 'source1 :value source1))
  (unless (typep source2 'int:sampler)
    (error 'int:invalid-sampler-argument :sampler-type 'min :argument 'source2 :value source2))
  (make-min :rng (int::sampler-rng source1) :source1 source1 :source2 source2))

(defmethod int:sample ((sampler mod:min) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (optimize speed))
  (cl:min (the u:f32 (int:sample (source1 sampler) x y z w))
          (the u:f32 (int:sample (source2 sampler) x y z w))))
