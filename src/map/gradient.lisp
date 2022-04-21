(in-package #:%cricket.map)

(defstruct (gradient-point
            (:predicate nil)
            (:copier nil))
  (position 0.0 :type u:f32)
  (color (rgba 0 0 0 0) :type color))

(defstruct (gradient
            (:constructor %make-gradient)
            (:predicate nil)
            (:copier nil))
  (points (make-array 0) :type simple-vector))

(defun clear-gradient (gradient)
  (setf (gradient-points gradient) (make-array 0)))

(defgeneric build-gradient (gradient type)
  (:method (gradient type)
    (error "Gradient ~s is not defined." type))
  (:method :around (gradient type)
    (clear-gradient gradient)
    (call-next-method)
    gradient))

(defun make-gradient (&optional (type :grayscale))
  (let ((gradient (%make-gradient)))
    (build-gradient gradient type)))

(defun gradient-point-present-p (gradient position)
  (find-if
   (lambda (x)
     (< (abs (- x position)) 1e-7))
   (gradient-points gradient)
   :key #'gradient-point-position))

(defun insert-gradient-point (gradient position color)
  (unless (gradient-point-present-p gradient position)
    (let* ((points (gradient-points gradient))
           (point-count (length points))
           (new-points (make-array (1+ point-count)))
           (gradient-point (make-gradient-point :position (float position 1f0) :color color)))
      (replace new-points points)
      (setf (aref new-points point-count) gradient-point
            (gradient-points gradient) (sort new-points #'< :key #'gradient-point-position)))))

(declaim (inline get-gradient-color))
(defun get-gradient-color (gradient position)
  (let* ((points (gradient-points gradient))
         (point-count (length points)))
    (unless (>= point-count 2)
      (error "Gradient must have at least 2 points."))
    (let* ((clamped (u:clamp position
                             (gradient-point-position (aref points 0))
                             (gradient-point-position (aref points (1- point-count)))))
           (index (or (position-if (lambda (x)
                                     (> x clamped))
                                   points
                                   :key #'gradient-point-position)
                      point-count))
           (index1 (u:clamp (1- index) 0 (1- point-count)))
           (index2 (u:clamp index 0 (1- point-count))))
      (when (= index1 index2)
        (return-from get-gradient-color
          (get-color-channels (gradient-point-color (aref points index1)))))
      (let* ((input1 (gradient-point-position (aref points index1)))
             (input2 (gradient-point-position (aref points index2)))
             (alpha (u:clamp (/ (- position input1) (- input2 input1)) 0.0 1.0)))
        (blend-colors (gradient-point-color (aref points index1))
                      (gradient-point-color (aref points index2))
                      alpha)))))

(defmacro define-gradient ((type) &body body)
  `(defmethod build-gradient ((gradient gradient) (type (eql ',type)))
     ,@(mapcar
        (lambda (x)
          (destructuring-bind (position color) x
            `(insert-gradient-point gradient ,position (rgba ,@color))))
        body)))

(define-gradient (:grayscale)
  (-1 (0 0 0 255))
  (1 (255 255 255 255)))

(define-gradient (:terrain)
  (-1 (0 0 0 255))
  ((/ -256 16384) (6 58 127 255))
  ((/ -1 16384) (14 112 192 255))
  (0 (70 120 60 255))
  ((/ 1024 16384) (110 140 75 255))
  ((/ 2048 16384) (160 140 111 255))
  ((/ 3072 16384) (184 163 141 255))
  ((/ 4096 16384) (128 128 128 255))
  ((/ 5632 16384) (128 128 128 255))
  ((/ 6144 16384) (250 250 250 255))
  (1 (255 255 255 255)))

(define-gradient (:rainbow)
  (-1 (255 0 0 255))
  (-0.7 (255 255 0 255))
  (-0.4 (0 255 0 255))
  (0 (0 255 255 255))
  (0.3 (0 0 255 255))
  (0.6 (255 0 255 255))
  (1 (255 0 0 255)))
