(in-package #:cl-user)

;;;; 2-dimensional OpenSimplex2F noise generator

(defpackage #:%cricket.generators.open-simplex2f-2d
  (:local-nicknames
   (#:gen #:%cricket.generators)
   (#:int #:%cricket.internal)
   (#:rng #:seedable-rng)
   (#:u #:mfiano-utils))
  (:use #:cl))

(in-package #:%cricket.generators.open-simplex2f-2d)

(u:define-constant +gradients+
    (let ((gradients #(#(13.031324456287654d0 98.98273633310245d0)
                       #(38.20591014244875d0 92.23722642870753d0)
                       #(60.77682619065379d0 79.20590197241988d0)
                       #(79.20590197241988d0 60.77682619065379d0)
                       #(92.23722642870753d0 38.20591014244875d0)
                       #(98.98273633310245d0 13.031324456287555d0)
                       #(98.98273633310245d0 -13.031324456287555d0)
                       #(92.23722642870753d0 -38.20591014244875d0)
                       #(79.20590197241988d0 -60.776826190653686d0)
                       #(60.77682619065379d0 -79.20590197241988d0)
                       #(38.20591014244875d0 -92.23722642870753d0)
                       #(13.031324456287654d0 -98.98273633310245d0)
                       #(-13.031324456287654d0 -98.98273633310245d0)
                       #(-38.20591014244875d0 -92.23722642870753d0)
                       #(-60.77682619065379d0 -79.20590197241988d0)
                       #(-79.20590197241988d0 -60.77682619065379d0)
                       #(-92.23722642870753d0 -38.20591014244875d0)
                       #(-98.98273633310245d0 -13.031324456287654d0)
                       #(-98.98273633310245d0 13.031324456287555d0)
                       #(-92.23722642870753d0 38.20591014244875d0)
                       #(-79.20590197241988d0 60.77682619065379d0)
                       #(-60.77682619065379d0 79.20590197241988d0)
                       #(-38.20591014244875d0 92.23722642870753d0)
                       #(-13.031324456287654d0 98.98273633310245d0)))
          (table (u:make-f64-array 4096)))
      (dotimes (i 2048)
        (setf (aref table (* i 2)) (aref (aref gradients (mod i 24)) 0)
              (aref table (1+ (* i 2))) (aref (aref gradients (mod i 24)) 1)))
      table)
  :test #'equalp)

(u:define-constant +lookup+
    (let ((data #(-0.788675134594813d0 0.211324865405187d0 0d0 0d0 -0.577350269189626d0
                  -0.577350269189626d0 0.211324865405187d0 -0.788675134594813d0)))
      (make-array 8 :element-type 'u:f64 :initial-contents data))
  :test #'equalp)

(defstruct (gen:open-simplex2f-2d
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (gradients (u:make-f64-array 4096) :type (u:f64a 4096))
  (table (u:make-b16-array 2048) :type (u:b16a 2048))
  (orientation :standard :type (member :standard :x/y)))

(u:fn-> permute (rng:generator) (values (u:f64a 4096) (u:b16a 2048)))
(defun permute (rng)
  (declare (optimize speed))
  (let ((source (u:make-b16-array 2048))
        (table (u:make-b16-array 2048))
        (gradients (u:make-f64-array 4096)))
    (dotimes (i 2048)
      (setf (aref source i) i))
    (loop :for i :from 2047 :downto 0
          :for r = (mod (+ (rng:int rng 0 #.(1- (expt 2 32)) nil) 31) (1+ i))
          :for x = (aref source r)
          :for pgi = (* i 2)
          :for gi = (* x 2)
          :do (setf (aref table i) x
                    (aref gradients pgi) (aref +gradients+ gi)
                    (aref gradients (1+ pgi)) (aref +gradients+ (1+ gi))
                    (aref source r) (aref source i)))
    (values gradients table)))

(declaim (inline orient))
(defun orient (sampler x y)
  (ecase (orientation sampler)
    (:standard
     (let ((s (* (+ x y) 0.366025403784439d0)))
       (values (+ x s) (+ y s))))
    (:x/y
     (let ((x (* x 0.7071067811865476d0))
           (y (* y 1.224744871380249d0)))
       (values (+ x y) (- y x))))))

(defun gen:open-simplex2f-2d (&key seed (orientation :standard))
  "Construct a sampler that, when sampled, outputs 2-dimensional OpenSimplex2F noise values ranging
from -1.0 to 1.0.

`seed`: A string used to seed the random number generator for this sampler, or NIL. If a seed is not
supplied, one will be generated automatically which will negatively affect the reproducibility of
the noise (optional, default: NIL).

`orientation`: One of `:standard` or `:x/y`, denoting the orientation of the lattice. `:x/y` has the
Y axis pointing down the main diagonal, which might be more suitable for a game where Y is
vertical (optional, default: `:standard`)."
  (unless (member orientation '(:standard :x/y))
    (error 'int:invalid-open-simplex2-orientation
           :sampler-type 'open-simplex2f-2d
           :orientation orientation
           :valid-orientations '(:standard :x/y)))
  (u:mvlet* ((rng (int::make-rng seed))
             (gradients table (permute rng)))
    (make-open-simplex2f-2d :rng rng
                            :gradients gradients
                            :table table
                            :orientation orientation)))

(defmethod int:sample ((sampler gen:open-simplex2f-2d) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (ignore z w)
           (optimize speed)
           (int::f50 x y z w))
  (u:mvlet* ((gradients (gradients sampler))
             (table (table sampler))
             (value 0d0)
             (x y (orient sampler x y))
             (xsb xsi (floor x))
             (ysb ysi (floor y))
             (index (truncate (1+ (* (- ysi xsi) 0.5))))
             (ssi (* (+ xsi ysi) -0.211324865405187d0))
             (xi (+ xsi ssi))
             (yi (+ ysi ssi)))
    (declare (u:f64 value))
    (dotimes (i 3 (float value 1f0))
      (block nil
        (let* ((lpx (* (+ index i) 2))
               (lpy (1+ lpx))
               (dx (+ xi (aref +lookup+ lpx)))
               (dy (+ yi (aref +lookup+ lpy)))
               (attn (- 0.5 (* dx dx) (* dy dy))))
          (when (minusp attn)
            (return))
          (let* ((pxm (logand (+ xsb (ldb (byte 1 lpx) #b10110001)) 2047))
                 (pym (logand (+ ysb (ldb (byte 1 lpy) #b10110001)) 2047))
                 (grad-index (* (logxor (aref table pxm) pym) 2))
                 (grad-x (* (aref gradients grad-index) dx))
                 (grad-y (* (aref gradients (1+ grad-index)) dy)))
            (setf attn (* attn attn))
            (incf value (* attn attn (+ grad-x grad-y)))))))))
