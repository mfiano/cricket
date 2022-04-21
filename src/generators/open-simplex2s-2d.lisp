(in-package #:cl-user)

;;;; 2-dimensional OpenSimplex2S noise generator

(defpackage #:%cricket.generators.open-simplex2s-2d
  (:local-nicknames
   (#:gen #:%cricket.generators)
   (#:int #:%cricket.internal)
   (#:rng #:seedable-rng)
   (#:u #:mfiano-utils))
  (:use #:cl))

(in-package #:%cricket.generators.open-simplex2s-2d)

(u:eval-always
  (defstruct (lattice-point
              (:constructor %make-lattice-point)
              (:conc-name nil)
              (:predicate nil)
              (:copier nil))
    (xsv 0 :type u:b32)
    (ysv 0 :type u:b32)
    (dx 0d0 :type u:f64)
    (dy 0d0 :type u:f64))

  (defmethod make-load-form ((object lattice-point) &optional environment)
    (declare (ignore environment))
    (make-load-form-saving-slots object))

  (defun make-lattice-point (xsv ysv)
    (let ((ssv (* (+ xsv ysv) -0.211324865405187d0)))
      (%make-lattice-point :xsv xsv :ysv ysv :dx (- (- xsv) ssv) :dy (- (- ysv) ssv))))

  (defun build-lattice-points ()
    (let ((table (make-array 32)))
      (dotimes (i 8)
        (let ((i1 0)
              (i2 0)
              (j1 0)
              (j2 0))
          (cond
            ((zerop (logand i 1))
             (if (zerop (logand i 2))
                 (setf i1 -1 j1 0)
                 (setf i1 1 j1 0))
             (if (zerop (logand i 4))
                 (setf i2 0 j2 -1)
                 (setf i2 0 j2 1)))
            (t
             (if (/= (logand i 2) 0)
                 (setf i1 2 j1 1)
                 (setf i1 0 j1 1))
             (if (/= (logand i 4) 0)
                 (setf i2 1 j2 2)
                 (setf i2 1 j2 0))))
          (setf (aref table (* i 4)) (make-lattice-point 0 0)
                (aref table (+ (* i 4) 1)) (make-lattice-point 1 1)
                (aref table (+ (* i 4) 2)) (make-lattice-point i1 j1)
                (aref table (+ (* i 4) 3)) (make-lattice-point i2 j2))))
      table)))

(u:define-constant +gradients+
    (let ((gradients #(#(2.3810538312857545d0 18.085899431608684d0)
                       #(6.980896610132626d0 16.853375273706543d0)
                       #(11.105002821476075d0 14.472321442420789d0)
                       #(14.472321442420789d0 11.105002821476075d0)
                       #(16.853375273706543d0 6.980896610132626d0)
                       #(18.085899431608684d0 2.3810538312857363d0)
                       #(18.085899431608684d0 -2.3810538312857363d0)
                       #(16.853375273706543d0 -6.980896610132626d0)
                       #(14.472321442420789d0 -11.105002821476058d0)
                       #(11.105002821476075d0 -14.472321442420789d0)
                       #(6.980896610132626d0 -16.853375273706543d0)
                       #(2.3810538312857545d0 -18.085899431608684d0)
                       #(-2.3810538312857545d0 -18.085899431608684d0)
                       #(-6.980896610132626d0 -16.853375273706543d0)
                       #(-11.105002821476075d0 -14.472321442420789d0)
                       #(-14.472321442420789d0 -11.105002821476075d0)
                       #(-16.853375273706543d0 -6.980896610132626d0)
                       #(-18.085899431608684d0 -2.3810538312857545d0)
                       #(-18.085899431608684d0 2.3810538312857363d0)
                       #(-16.853375273706543d0 6.980896610132626d0)
                       #(-14.472321442420789d0 11.105002821476075d0)
                       #(-11.105002821476075d0 14.472321442420789d0)
                       #(-6.980896610132626d0 16.853375273706543d0)
                       #(-2.3810538312857545d0 18.085899431608684d0)))
          (table (u:make-f64-array 4096)))
      (dotimes (i 2048)
        (setf (aref table (* i 2)) (aref (aref gradients (mod i 24)) 0)
              (aref table (1+ (* i 2))) (aref (aref gradients (mod i 24)) 1)))
      table)
  :test #'equalp)

(u:define-constant +lookup+ (build-lattice-points) :test #'equalp)

(defstruct (gen:open-simplex2s-2d
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (gradients (u:make-f64-array 4096) :type (u:f64a 4096))
  (table (u:make-b16-array 2048) :type (u:b16a 2048))
  (orientation :standard :type (member :standard :x/y)))

(defun permute (rng)
  (let ((source (u:make-b16-array 2048 0))
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

(defun gen:open-simplex2s-2d (&key seed (orientation :standard))
  "Construct a sampler that, when sampled, outputs 2-dimensional OpenSimplex2S noise values ranging
from -1.0 to 1.0.

`seed`: A string used to seed the random number generator for this sampler, or NIL. If a seed is not
supplied, one will be generated automatically which will negatively affect the reproducibility of
the noise (optional, default: NIL).

`orientation`: One of `:standard` or `:x/y`, denoting the orientation of the lattice. `:x/y` has the
Y axis pointing down the main diagonal, which might be more suitable for a game where Y is
vertical (optional, default: `:standard`)."
  (unless (member orientation '(:standard :x/y))
    (error 'int:invalid-open-simplex2-orientation
           :sampler-type 'open-simplex2s-2d
           :orientation orientation
           :valid-orientations '(:standard :x/y)))
  (u:mvlet* ((rng (int::make-rng seed))
             (gradients table (permute rng)))
    (make-open-simplex2s-2d :rng rng
                            :gradients gradients
                            :table table
                            :orientation orientation)))

(defmethod int:sample ((sampler gen:open-simplex2s-2d) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (ignore z w)
           (optimize speed)
           (int::f50 x y z w))
  (u:mvlet* ((gradients (gradients sampler))
             (table (table sampler))
             (value 0d0)
             (x y (orient sampler x y))
             (xsb xsi (floor x))
             (ysb ysi (floor y))
             (xsi+ysi (+ xsi ysi))
             (a (truncate xsi+ysi))
             (index (logior (ash a 2)
                            (ash (truncate (1+ (- xsi (/ ysi 2) (/ a 2d0)))) 3)
                            (ash (truncate (1+ (- ysi (/ xsi 2) (/ a 2d0)))) 4)))
             (ssi (* xsi+ysi -0.211324865405187d0))
             (xi (+ xsi ssi))
             (yi (+ ysi ssi)))
    (declare (u:f64 value))
    (dotimes (i 4 (float value 1f0))
      (block nil
        (let* ((c (aref +lookup+ (+ index i)))
               (dx (+ xi (dx c)))
               (dy (+ yi (dy c)))
               (attn (- #.(/ 2 3) (* dx dx) (* dy dy))))
          (unless (plusp attn)
            (return))
          (let* ((pxm (logand (+ xsb (xsv c)) 2047))
                 (pym (logand (+ ysb (ysv c)) 2047))
                 (grad-index (* (logxor (aref table pxm) pym) 2))
                 (grad-x (* (aref gradients grad-index) dx))
                 (grad-y (* (aref gradients (1+ grad-index)) dy)))
            (setf attn (* attn attn))
            (incf value (* attn attn (+ grad-x grad-y)))))))))
