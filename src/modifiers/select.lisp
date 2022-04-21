(in-package #:cl-user)

;;;; Select modifier
;;;; This noise modifier outputs one of its two input samplers, decided based upon the output of a
;;;; control sampler.

(defpackage #:%cricket.modifiers.select
  (:local-nicknames
   (#:int #:%cricket.internal)
   (#:mod #:%cricket.modifiers)
   (#:u #:mfiano-utils))
  (:use #:cl)
  (:shadow #:min #:max))

(in-package #:%cricket.modifiers.select)

(defstruct (mod:select
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source1 nil :type int:sampler)
  (source2 nil :type int:sampler)
  (control nil :type int:sampler)
  (min -1.0 :type u:f32)
  (max 1.0 :type u:f32)
  (falloff 0.0 :type u:f32))

(defun mod:select (source1 source2 control &key (min -1.0) (max 1.0) (falloff 0.0))
  "Construct a sampler that, when sampled, outputs the result of sampling from either `source1` or
`source2`. The input sampler chosen is decided based upon the output of the `control` sampler.

If the output of `control` is within the range denoted by `min` and `max`, `source2` is chosen. If
the output of `control` is outside of this range, `source1` is chosen.

`source1`: The first input sampler (required).

`source2`: The second input sampler (required).

`control`: The sampler that determines the sampler to output (required).

`min`: A real number between -1.0 and 1.0 defining the lower bound of the selection range (optional,
default: -1.0).

`max`: A real number between -1.0 and 1.0 defining the upper bound of the selection range (optional,
default: 1.0).

`falloff`: A real number between 0.0 and 1.0 specifying the smoothness of the transition (optional,
default: 0.0)."
  (unless (typep source1 'int:sampler)
    (error 'int:invalid-sampler-argument :sampler-type 'select :argument 'source1 :value source1))
  (unless (typep source2 'int:sampler)
    (error 'int:invalid-sampler-argument :sampler-type 'select :argument 'source2 :value source2))
  (unless (typep control 'int:sampler)
    (error 'int:invalid-sampler-argument :sampler-type 'select :argument 'control :value control))
  (unless (realp min)
    (error 'int:invalid-real-argument :sampler-type 'select :argument :min :value min))
  (unless (realp max)
    (error 'int:invalid-real-argument :sampler-type 'select :argument :max :value max))
  (unless (realp falloff)
    (error 'int:invalid-real-argument :sampler-type 'select :argument :falloff :value falloff))
  (make-select :rng (int::sampler-rng source1)
               :source1 source1
               :source2 source2
               :control control
               :min (float min 1f0)
               :max (float max 1f0)
               :falloff (float falloff 1f0)))

(defmethod int:sample ((sampler mod:select) x &optional (y 0d0) (z 0d0) (w 0d0))
  (let* ((min (min sampler))
         (max (max sampler))
         (falloff (cl:min (falloff sampler) (* (- max min) 0.5)))
         (sample1 (int:sample (source1 sampler) x y z w))
         (sample2 (int:sample (source2 sampler) x y z w))
         (control (int:sample (control sampler) x y z w)))
    (if (plusp falloff)
        (cond
          ((< control (- min falloff))
           sample1)
          ((< control (+ min falloff))
           (let* ((low (- min falloff))
                  (high (+ min falloff))
                  (alpha (int::cubic-curve (/ (- control low) (- high low)))))
             (u:lerp alpha sample1 sample2)))
          ((< control (- max falloff))
           sample2)
          ((< control (+ max falloff))
           (let* ((low (- max falloff))
                  (high (+ max falloff))
                  (alpha (int::cubic-curve (/ (- control low) (- high low)))))
             (u:lerp alpha sample2 sample1)))
          (t
           sample1))
        (if (or (< control min) (> control max))
            sample1
            sample2))))
