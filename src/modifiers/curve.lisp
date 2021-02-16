(in-package #:cl-user)

(defpackage #:coherent-noise.modifiers.curve
  (:local-nicknames
   (#:int #:coherent-noise.internal)
   (#:mod #:coherent-noise.modifiers)
   (#:u #:golden-utils))
  (:use #:cl))

(in-package #:coherent-noise.modifiers.curve)

(defstruct (curve
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source nil :type int:sampler)
  (control-points (vector) :type vector))

(defun make-points (point-pairs)
  (unless (and (listp point-pairs)
               (>= (length point-pairs) 4))
    (error "A curve modifier requires at least 4 control points."))
  (unless (every (lambda (x) (typep x '(cons real real))) point-pairs)
    (error "A curve modifier control point must be a cons of 2 real numbers."))
  (unless (= (length (remove-duplicates point-pairs :key #'car :test #'=))
             (length point-pairs))
    (error "A curve modifier's control points must have unique input values."))
  (loop :for (i . o) :in point-pairs
        :collect (cons (float i 1f0) (float o 1f0)) :into points
        :finally (return (coerce (sort points #'< :key #'car) 'vector))))

(defun mod:curve (source &key points)
  (make-curve :rng (int::sampler-rng source)
              :source source
              :control-points (make-points points)))

(defmethod int:sample ((sampler curve) x &optional (y 0d0) (z 0d0) (w 0d0))
  (let* ((control-points (control-points sampler))
         (count (length control-points))
         (count-1 (1- count))
         (sample (int:sample (source sampler) x y z w))
         (index (u:clamp (or (position sample control-points :test #'< :key #'car) count) 2 count))
         (index1 (u:clamp (- index 2) 0 count-1))
         (index2 (u:clamp (- index 1) 0 count-1))
         (index3 (u:clamp index 0 count-1))
         (index4 (u:clamp (+ index 1) 0 count-1)))
    (if (= index2 index3)
        (cdr (aref control-points index2))
        (let ((input1 (car (aref control-points index2)))
              (input2 (car (aref control-points index3))))
          (int::interpolate-cubic (cdr (aref control-points index1))
                                  (cdr (aref control-points index2))
                                  (cdr (aref control-points index3))
                                  (cdr (aref control-points index4))
                                  (/ (- sample input1) (- input2 input1)))))))