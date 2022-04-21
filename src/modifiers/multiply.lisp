(in-package #:cl-user)

;;;; Multiplication (*) modifier
;;;; This noise modifier outputs the result of multiplying the outputs of both of its input
;;;; samplers.

(defpackage #:%cricket.modifiers.multiply
  (:local-nicknames
   (#:int #:%cricket.internal)
   (#:mod #:%cricket.modifiers)
   (#:u #:mfiano-utils))
  (:use #:cl))

(in-package #:%cricket.modifiers.multiply)

(defstruct (mod:*
            (:constructor make-multiply)
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source1 nil :type int:sampler)
  (source2 nil :type int:sampler))

(defun mod:* (source1 source2)
  "Construct a sampler that, when sampled, outputs the result of multiplying the outputs of
`source1` and `source2`.

`source1`: The first input sampler (required).

`source2`: The second input sampler (required)."
  (unless (typep source1 'int:sampler)
    (error 'int:invalid-sampler-argument :sampler-type '* :argument 'source1 :value source1))
  (unless (typep source2 'int:sampler)
    (error 'int:invalid-sampler-argument :sampler-type '* :argument 'source2 :value source2))
  (make-multiply :rng (int::sampler-rng source1) :source1 source1 :source2 source2))

(defmethod int:sample ((sampler mod:*) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (optimize speed))
  (* (the u:f32 (int:sample (source1 sampler) x y z w))
     (the u:f32 (int:sample (source2 sampler) x y z w))))
