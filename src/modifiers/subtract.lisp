(in-package #:cl-user)

;;;; Subtraction (-) modifier
;;;; This noise modifier outputs the result of subtracting the output of its second input sampler
;;;; from its first input sampler.

(defpackage #:%cricket.modifiers.subtract
  (:local-nicknames
   (#:int #:%cricket.internal)
   (#:mod #:%cricket.modifiers)
   (#:u #:mfiano-utils))
  (:use #:cl))

(in-package #:%cricket.modifiers.subtract)

(defstruct (mod:-
            (:constructor make-subtract)
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source1 nil :type int:sampler)
  (source2 nil :type int:sampler))

(defun mod:- (source1 source2)
  "Construct a sampler that, when sampled, outputs the result of subtracting the output `source2`
from the output of `source1`.

`source1`: The first input sampler (required).

`source2`: The second input sampler (required)."
  (unless (typep source1 'int:sampler)
    (error 'int:invalid-sampler-argument :sampler-type '- :argument 'source1 :value source1))
  (unless (typep source2 'int:sampler)
    (error 'int:invalid-sampler-argument :sampler-type '- :argument 'source2 :value source2))
  (make-subtract :rng (int::sampler-rng source1) :source1 source1 :source2 source2))

(defmethod int:sample ((sampler mod:-) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (optimize speed))
  (- (the u:f32 (int:sample (source1 sampler) x y z w))
     (the u:f32 (int:sample (source2 sampler) x y z w))))
