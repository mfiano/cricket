(in-package #:cl-user)

;;;; Strengthen modifier
;;;; This noise modifier applies a scaling factor and a bias to the output of its input sampler.

(defpackage #:%cricket.modifiers.strengthen
  (:local-nicknames
   (#:int #:%cricket.internal)
   (#:mod #:%cricket.modifiers)
   (#:u #:mfiano-utils))
  (:use #:cl))

(in-package #:%cricket.modifiers.strengthen)

(defstruct (mod:strengthen
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source nil :type int:sampler)
  (strength 1.0 :type u:f32)
  (bias 0.0 :type u:f32))

(defun mod:strengthen (source strength &key (bias 0.0))
  "Construct a sampler that, when sampled, modifies the output value of `source` by multiplying it
by `strength`, and optionally adding `bias` afterwards.

`source`: The input sampler (required).

`strength`: A multiplier to apply to the output of `source` (required).

`bias`: An additional bias amount to add to the result (optional, default: 0.0)."
  (unless (typep source 'int:sampler)
    (error 'int:invalid-sampler-argument :sampler-type 'strengthen :argument 'source :value source))
  (unless (realp strength)
    (error 'int:invalid-real-argument
           :sampler-type 'strengthen
           :argument 'strength
           :value strength))
  (unless (realp bias)
    (error 'int:invalid-real-argument :sampler-type 'strengthen :argument :bias :value bias))
  (make-strengthen :rng (int::sampler-rng source)
                   :source source
                   :strength (float strength 1f0)
                   :bias (float bias 1f0)))

(defmethod int:sample ((sampler mod:strengthen) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (optimize speed))
  (+ (* (the u:f32 (int:sample (source sampler) x y z w))
        (strength sampler))
     (bias sampler)))
