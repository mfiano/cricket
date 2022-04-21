(in-package #:cl-user)

;;;; Clamp modifier
;;;; This noise modifier clamps the output of its input sampler to the specified range.

(defpackage #:%cricket.modifiers.clamp
  (:local-nicknames
   (#:int #:%cricket.internal)
   (#:mod #:%cricket.modifiers)
   (#:u #:mfiano-utils))
  (:use #:cl)
  (:shadow #:min #:max))

(in-package #:%cricket.modifiers.clamp)

(defstruct (mod:clamp
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source nil :type int:sampler)
  (min -1f0 :type u:f32)
  (max 1f0 :type u:f32))

(defun mod:clamp (source min max)
  "Construct a sampler that, when sampled, clamps the output of `source` to be within the range
`min`..`max`. If the output of `source` is less than `min`, the result will be `min`. If the output
of `source` is greater than `max`, the result will be `max`.

`source`: The input sampler to clamp (required).

`min`: A real number denoting the lower bound of the clamping range (required).

`max`: A real number denoting the upper bound of the clamping range (required)."
  (unless (typep source 'int:sampler)
    (error 'int:invalid-sampler-argument :sampler-type 'clamp :argument 'source :value source))
  (unless (realp min)
    (error 'int:invalid-real-argument :sampler-type 'clamp :argument 'min :value min))
  (unless (realp max)
    (error 'int:invalid-real-argument :sampler-type 'clamp :argument 'max :value max))
  (make-clamp :rng (int::sampler-rng source)
              :source source
              :min (float min 1f0)
              :max (float max 1f0)))

(defmethod int:sample ((sampler mod:clamp) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (optimize speed))
  (u:clamp (the u:f32 (int:sample (source sampler) x y z w))
           (min sampler)
           (max sampler)))
