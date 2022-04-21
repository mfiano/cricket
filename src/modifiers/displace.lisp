(in-package #:cl-user)

;;;; Displace modifier
;;;; This noise modifier modifies the input coordinates of its input sampler using up to four
;;;; displacement samplers corresponding to each axis of the source that should be offset.

(defpackage #:%cricket.modifiers.displace
  (:local-nicknames
   (#:int #:%cricket.internal)
   (#:mod #:%cricket.modifiers)
   (#:u #:mfiano-utils))
  (:use #:cl))

(in-package #:%cricket.modifiers.displace)

(defstruct (mod:displace
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source nil :type int:sampler)
  (x nil :type (or int:sampler null))
  (y nil :type (or int:sampler null))
  (z nil :type (or int:sampler null))
  (w nil :type (or int:sampler null)))

(defun mod:displace (source &key x y z w)
  "Construct a sampler that, when sampled, modifies the input coordinates of its input sampler
`source`, before sampling from it. `x`, `y`, `z`, and `w` are samplers, where the result of sampling
from them are added to the input coordinate of the corresponding axis. After modifying of the input
coordinates, `source` is then sampled with the new input coordinate set. Any displacement samplers
that are not specified will not displace the corresponding axis of the input sampler.

`source`: The input sampler to displace (required).

`x`: A sampler that is evaluated and added to the X axis input coordinate of `source` (optional).

`y`: A sampler that is evaluated and added to the Y axis input coordinate of `source` (optional).

`z`: A sampler that is evaluated and added to the Z axis input coordinate of `source` (optional).

`w`: A sampler that is evaluated and added to the W axis input coordinate of `source` (optional)."
  (unless (typep source 'int:sampler)
    (error 'int:invalid-sampler-argument :sampler-type 'displace :argument 'source :value source))
  (unless (typep x '(or int:sampler null))
    (error 'int:invalid-sampler-argument :sampler-type 'displace :argument :x :value x))
  (unless (typep y '(or int:sampler null))
    (error 'int:invalid-sampler-argument :sampler-type 'displace :argument :y :value y))
  (unless (typep z '(or int:sampler null))
    (error 'int:invalid-sampler-argument :sampler-type 'displace :argument :z :value z))
  (unless (typep w '(or int:sampler null))
    (error 'int:invalid-sampler-argument :sampler-type 'displace :argument :w :value w))
  (make-displace :rng (int::sampler-rng source) :source source :x x :y y :z z :w w))

(defmethod int:sample ((sampler mod:displace) x &optional (y 0d0) (z 0d0) (w 0d0))
  (let* ((dx (if (x sampler) (int:sample (x sampler) x y z w) 0.0))
         (dy (if (y sampler) (int:sample (y sampler) x y z w) 0.0))
         (dz (if (z sampler) (int:sample (z sampler) x y z w) 0.0))
         (dw (if (w sampler) (int:sample (w sampler) x y z w) 0.0)))
    (int:sample (source sampler) (+ x dx) (+ y dy) (+ z dz) (+ w dw))))
