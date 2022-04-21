(in-package #:cl-user)

;;;; Scale modifier
;;;; This noise modifier scales the input coordinates of its input sampler.

(defpackage #:%cricket.modifiers.scale
  (:local-nicknames
   (#:int #:%cricket.internal)
   (#:mod #:%cricket.modifiers)
   (#:u #:mfiano-utils))
  (:use #:cl))

(in-package #:%cricket.modifiers.scale)

(defstruct (mod:scale
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source nil :type int:sampler)
  (x 1d0 :type u:f64)
  (y 1d0 :type u:f64)
  (z 1d0 :type u:f64)
  (w 1d0 :type u:f64))

(defun mod:scale (source &key (x 1.0) (y 1.0) (z 1.0) (w 1.0))
  "Construct a sampler that, when sampled, scales the input coordinates of `source` down (increases
the wavelength, and decreases the frequency), by the amounts given for each of the `x`, `y`, `z`,
and `w` axes.

`source`: The input sampler (required).

`x`: The amount to scale along the X axis (optional, default: 1.0).

`y`: The amount to scale along the Y axis (optional, default: 1.0).

`z`: The amount to scale along the Z axis (optional, default: 1.0).

`w`: The amount to scale along the W axis (optional, default: 1.0)."
  (unless (typep source 'int:sampler)
    (error 'int:invalid-sampler-argument :sampler-type 'scale :argument 'source :value source))
  (unless (realp x)
    (error 'int:invalid-real-argument :sampler-type 'scale :argument :x :value x))
  (unless (realp y)
    (error 'int:invalid-real-argument :sampler-type 'scale :argument :y :value y))
  (unless (realp z)
    (error 'int:invalid-real-argument :sampler-type 'scale :argument :z :value z))
  (unless (realp w)
    (error 'int:invalid-real-argument :sampler-type 'scale :argument :w :value w))
  (make-scale :rng (int::sampler-rng source)
              :source source
              :x (float x 1d0)
              :y (float y 1d0)
              :z (float z 1d0)
              :w (float w 1d0)))

(defun mod:uniform-scale (source scalar)
  (unless (typep source 'int:sampler)
    (error 'int:invalid-sampler-argument
           :sampler-type 'uniform-scale
           :argument 'source
           :value source))
  (unless (realp scalar)
    (error 'int:invalid-real-argument :sampler-type 'uniform-scale :argument 'scalar :value scalar))
  (let ((scalar (float scalar 1d0)))
    (make-scale :rng (int::sampler-rng source)
                :source source
                :x scalar
                :y scalar
                :z scalar
                :w scalar)))

(defmethod int:sample ((sampler mod:scale) x &optional (y 0d0) (z 0d0) (w 0d0))
  (int:sample (source sampler)
              (/ x (x sampler))
              (/ y (y sampler))
              (/ z (z sampler))
              (/ w (w sampler))))
