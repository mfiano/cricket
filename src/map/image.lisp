(in-package #:%cricket.map)

(declaim (inline %make-image))
(defstruct (image
            (:constructor %make-image)
            (:predicate nil)
            (:copier nil))
  (width 0 :type u:ub32)
  (height 0 :type u:ub32)
  (data (make-array 0) :type simple-vector))

(declaim (inline make-image))
(defun make-image (width height)
  (let* ((size (* width height))
         (data (make-array size)))
    (dotimes (i size)
      (setf (aref data i) (rgba 0 0 0 0)))
    (%make-image :width width
                 :height height
                 :data data)))

(declaim (inline get-image-pixel))
(defun get-image-pixel (image x y)
  (aref (image-data image) (+ (* y (image-width image)) x)))

(declaim (inline set-image-channels))
(defun set-image-channels (image x y r g b a)
  (let* ((data (image-data image))
         (width (image-width image))
         (color (aref data (+ (* y width) x))))
    (setf (r color) r
          (g color) g
          (b color) b
          (a color) a)))

(defun write-image (image file-path)
  (let* ((width (image-width image))
         (height (image-height image))
         (data (image-data image))
         (image-data (u:make-ub8-array (* width height 4)))
         (png (make-instance 'zpng:png
                             :color-type :truecolor-alpha
                             :width width
                             :height height
                             :image-data image-data)))
    (dotimes (i (length data))
      (let ((color (aref data i)))
        (dotimes (j 4)
          (setf (aref image-data (+ (* i 4) j)) (aref color j)))))
    (zpng:write-png png file-path)))
