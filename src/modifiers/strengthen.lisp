(in-package #:cl-user)

(defpackage #:coherent-noise.modifiers.strengthen
  (:local-nicknames
   (#:int #:coherent-noise.internal)
   (#:mod #:coherent-noise.modifiers)
   (#:u #:golden-utils))
  (:use #:cl))

(in-package #:coherent-noise.modifiers.strengthen)

(defstruct (strengthen
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source nil :type int:sampler)
  (strength 1.0 :type u:f32)
  (bias 0.0 :type u:f32))

(defun mod:strengthen (source strength &key (bias 0.0))
  (make-strengthen :rng (int::sampler-rng source)
                   :source source
                   :strength (float strength 1f0)
                   :bias (float bias 1f0)))

(defmethod int:sample ((sampler strengthen) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (optimize speed))
  (+ (* (the u:f32 (int:sample (source sampler) x y z w))
        (strength sampler))
     (bias sampler)))
