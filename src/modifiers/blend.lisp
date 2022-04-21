(in-package #:cl-user)

;;;; Blend modifier
;;;; This noise modifier blends the outputs of its two input samplers together using linear
;;;; interpolation by the output value of its control sampler.

(defpackage #:%cricket.modifiers.blend
  (:local-nicknames
   (#:int #:%cricket.internal)
   (#:mod #:%cricket.modifiers)
   (#:u #:mfiano-utils))
  (:use #:cl))

(in-package #:%cricket.modifiers.blend)

(defstruct (mod:blend
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source1 nil :type int:sampler)
  (source2 nil :type int:sampler)
  (control nil :type int:sampler))

(defun mod:blend (source1 source2 control)
  "Construct a sampler that, when sampled, outputs a blend between the outputs of `source1` and
`source2`, by means of a linear interpolation using the output value of `control` as the
interpolation parameter.

If the output of `control` is negative, the blended output is weighted towards the output of
`source1`. If the output of `control` is positive, the blended output is weighted towards the output
of `source2`.

`source1`: The sampler to weight towards if the output of `control` is negative (required).

`source2`: The sampler to weight towards if the output of `control` is positive (required).

`control`: The sampler that determines the weight of the blending operation (required)."
  (unless (typep source1 'int:sampler)
    (error 'int:invalid-sampler-argument :sampler-type 'blend :argument 'source1 :value source1))
  (unless (typep source2 'int:sampler)
    (error 'int:invalid-sampler-argument :sampler-type 'blend :argument 'source2 :value source2))
  (unless (typep control 'int:sampler)
    (error 'int:invalid-sampler-argument :sampler-type 'blend :argument 'control :value control))
  (make-blend :rng (int::sampler-rng source1) :source1 source1 :source2 source2 :control control))

(defmethod int:sample ((sampler mod:blend) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (optimize speed))
  (u:lerp (the u:f32 (int:sample (control sampler) x y z w))
          (the u:f32 (int:sample (source1 sampler) x y z w))
          (* (1+ (the u:f32 (int:sample (source2 sampler) x y z w))) 0.5)))
