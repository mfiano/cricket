(in-package #:cl-user)

;;;; Terrace modifier
;;;; This noise modifier maps the output of its input sampler onto a terrace-forming curve.

(defpackage #:%cricket.modifiers.terrace
  (:local-nicknames
   (#:int #:%cricket.internal)
   (#:mod #:%cricket.modifiers)
   (#:u #:mfiano-utils))
  (:use #:cl))

(in-package #:%cricket.modifiers.terrace)

(defstruct (mod:terrace
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source nil :type int:sampler)
  (control-points (u:make-f32-array 0) :type u:f32a)
  (invert-p nil :type boolean))

(defun make-points (points)
  (unless (and (listp points)
               (>= (length points) 2))
    (error "A terrace modifier requires at least 2 control points."))
  (unless (every #'realp points)
    (error "A terrace modifier control point must be a real number."))
  (unless (= (length (remove-duplicates points :test #'=))
             (length points))
    (error "A terrace modifier's control points must be unique."))
  (loop :with result = (u:make-f32-array (length points))
        :for p :in points
        :for i :from 0
        :do (setf (aref result i) (float p 1f0))
        :finally (return (sort result #'<))))

(defun mod:terrace (source &key points invert-p)
  "Construct a sampler that, when sampled, maps the output of its input sampler onto a
terrace-forming curve, defined by the list of `points`. `points` is a list of at least 2 real
numbers, each with a unique value.

`source`: The input sampler (required).

`points`: A list of at least two control points that define the curve (required).

`invert-p`: Whether the curve is inverted between the control points (optional, default: nil)."
  (unless (typep source 'int:sampler)
    (error 'int:invalid-sampler-argument :sampler-type 'terrace :argument 'source :value source))
  (make-terrace :rng (int::sampler-rng source)
                :source source
                :control-points (make-points points)
                :invert-p invert-p))

(defmethod int:sample ((sampler mod:terrace) x &optional (y 0d0) (z 0d0) (w 0d0))
  (let* ((control-points (control-points sampler))
         (count (length control-points))
         (count-1 (1- count))
         (sample (int:sample (source sampler) x y z w))
         (index (or (position sample control-points :test #'<=) count))
         (index1 (u:clamp (- index 1) 0 count-1))
         (index2 (u:clamp index 0 count-1)))
    (if (= index1 index2)
        (aref control-points index2)
        (let* ((input1 (aref control-points index1))
               (input2 (aref control-points index2))
               (alpha (/ (- sample input1) (- input2 input1))))
          (when (invert-p sampler)
            (setf alpha (- 1.0 alpha))
            (rotatef input1 input2))
          (u:lerp (* alpha alpha) input1 input2)))))
