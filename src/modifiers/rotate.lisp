(in-package #:cl-user)

;;;; Rotate modifier
;;;; This noise modifier rotates the input coordinates of its input sampler.

(defpackage #:%cricket.modifiers.rotate
  (:local-nicknames
   (#:int #:%cricket.internal)
   (#:mod #:%cricket.modifiers)
   (#:u #:mfiano-utils))
  (:use #:cl))

(in-package #:%cricket.modifiers.rotate)

(defstruct (mod:rotate
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source nil :type int:sampler)
  (rx1 0d0 :type u:f64)
  (rx2 0d0 :type u:f64)
  (rx3 0d0 :type u:f64)
  (ry1 0d0 :type u:f64)
  (ry2 0d0 :type u:f64)
  (ry3 0d0 :type u:f64)
  (rz1 0d0 :type u:f64)
  (rz2 0d0 :type u:f64)
  (rz3 0d0 :type u:f64))

(defun mod:rotate (source &key (x 0.0) (y 0.0) (z 0.0))
  "Construct a sampler that, when sampled, rotates the input coordinates around the origin before
sampling from it. The coordinate system is assumed to be left-handed. The angle of rotation around
each of the `x`, `y`, and `z` axes can be set individually.

`source`: The input sampler (required).

`x`: The angle in radians to rotate around the X axis (optional, default: 0.0).

`y`: The angle in radians to rotate around the Y axis (optional, default: 0.0).

`z`: The angle in radians to rotate around the Z axis (optional, default: 0.0)."
  (unless (typep source 'int:sampler)
    (error 'int:invalid-sampler-argument :sampler-type 'rotate :argument 'source :value source))
  (unless (realp x)
    (error 'int:invalid-real-argument :sampler-type 'rotate :argument :x :value x))
  (unless (realp y)
    (error 'int:invalid-real-argument :sampler-type 'rotate :argument :y :value y))
  (unless (realp z)
    (error 'int:invalid-real-argument :sampler-type 'rotate :argument :z :value z))
  (let ((cx (float (cos x) 1d0))
        (cy (float (cos y) 1d0))
        (cz (float (cos z) 1d0))
        (sx (float (sin x) 1d0))
        (sy (float (sin y) 1d0))
        (sz (float (sin z) 1d0)))
    (make-rotate :rng (int::sampler-rng source)
                 :source source
                 :rx1 (+ (* sy sx sz) (* cy cz))
                 :rx2 (- (* sy sx cz) (* cy sz))
                 :rx3 (* (- sy) cx)
                 :ry1 (* cx sz)
                 :ry2 (* cx cz)
                 :ry3 sx
                 :rz1 (- (* sy cz) (* cy sx sz))
                 :rz2 (- (* (- cy) sx cz) (* sy sz))
                 :rz3 (* cy cx))))

(defmethod int:sample ((sampler mod:rotate) x &optional (y 0d0) (z 0d0) (w 0d0))
  (let ((x (+ (* x (rx1 sampler)) (* y (ry1 sampler)) (* z (rz1 sampler))))
        (y (+ (* x (rx2 sampler)) (* y (ry2 sampler)) (* z (rz2 sampler))))
        (z (+ (* x (rx3 sampler)) (* y (ry3 sampler)) (* z (rz3 sampler)))))
    (int:sample (source sampler) x y z w)))
