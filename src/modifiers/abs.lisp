(in-package #:cl-user)

;;;; ABS modifier
;;;; This noise modifier outputs the absolute value of its input sampler's output.

(defpackage #:%cricket.modifiers.abs
  (:local-nicknames
   (#:int #:%cricket.internal)
   (#:mod #:%cricket.modifiers)
   (#:u #:mfiano-utils))
  (:use #:cl)
  (:shadow #:abs))

(in-package #:%cricket.modifiers.abs)

(defstruct (mod:abs
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source nil :type int:sampler))

(defun mod:abs (source)
  "Construct a sampler that, when sampled, outputs the absolute value of the output of `source`.

`source`: The input sampler (required)."
  (unless (typep source 'int:sampler)
    (error 'int:invalid-sampler-argument :sampler-type 'abs :argument 'source :value source))
  (make-abs :rng (int::sampler-rng source) :source source))

(defmethod int:sample ((sampler mod:abs) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (optimize speed))
  (cl:abs (the u:f32 (int:sample (source sampler) x y z w))))
