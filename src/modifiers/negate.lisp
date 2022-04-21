(in-package #:cl-user)

;;;; Negate modifier
;;;; This noise modifier negates the output of its input sampler.

(defpackage #:%cricket.modifiers.negate
  (:local-nicknames
   (#:int #:%cricket.internal)
   (#:mod #:%cricket.modifiers)
   (#:u #:mfiano-utils))
  (:use #:cl))

(in-package #:%cricket.modifiers.negate)

(defstruct (mod:negate
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source nil :type int:sampler))

(defun mod:negate (source)
  "Construct a sampler that, when sampled, negates the output value of its input sampler.

`source`: The input sampler (required)."
  (unless (typep source 'int:sampler)
    (error 'int:invalid-sampler-argument :sampler-type 'negate :argument 'source :value source))
  (make-negate :rng (int::sampler-rng source) :source source))

(defmethod int:sample ((sampler mod:negate) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (optimize speed))
  (- (the u:f32 (int:sample (source sampler) x y z w))))
