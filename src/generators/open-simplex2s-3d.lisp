(in-package #:cl-user)

;;;; 3-dimensional OpenSimplex2S noise generator

(defpackage #:%cricket.generators.open-simplex2s-3d
  (:local-nicknames
   (#:gen #:%cricket.generators)
   (#:int #:%cricket.internal)
   (#:rng #:seedable-rng)
   (#:u #:mfiano-utils))
  (:use #:cl))

(in-package #:%cricket.generators.open-simplex2s-3d)

(u:eval-always
  (defstruct (lattice-point
              (:constructor %make-lattice-point)
              (:conc-name nil)
              (:predicate nil)
              (:copier nil))
    (dxr 0d0 :type u:f64)
    (dyr 0d0 :type u:f64)
    (dzr 0d0 :type u:f64)
    (xrv 0 :type u:b32)
    (yrv 0 :type u:b32)
    (zrv 0 :type u:b32)
    (next/fail nil :type (or lattice-point null))
    (next/success nil :type (or lattice-point null)))

  (defmethod make-load-form ((object lattice-point) &optional environment)
    (declare (ignore environment))
    (make-load-form-saving-slots object))

  (defun make-lattice-point (xrv yrv zrv lattice)
    (let ((l1 (* lattice 0.5d0))
          (l2 (* lattice 1024)))
      (%make-lattice-point :dxr (+ (- xrv) l1)
                           :dyr (+ (- yrv) l1)
                           :dzr (+ (- zrv) l1)
                           :xrv (+ xrv l2)
                           :yrv (+ yrv l2)
                           :zrv (+ zrv l2))))

  (defun build-lattice-points ()
    (let ((table (make-array 8)))
      (dotimes (i 8)
        (let* ((i1 (logand i 1))
               (i2 (logxor i1 1))
               (i3 (+ i1 (logxor i2 1)))
               (j1 (logand (ash i -1) 1))
               (j2 (logxor j1 1))
               (j3 (+ j1 (logxor j2 1)))
               (k1 (logand (ash i -2) 1))
               (k2 (logxor k1 1))
               (k3 (+ k1 (logxor k2 1)))
               (i1i2 (+ i1 i2))
               (j1j2 (+ j1 j2))
               (k1k2 (+ k1 k2))
               (c0 (make-lattice-point i1 j1 k1 0))
               (c1 (make-lattice-point i1i2 j1j2 k1k2 1))
               (c2 (make-lattice-point i2 j1 k1 0))
               (c3 (make-lattice-point i1 j2 k2 0))
               (c4 (make-lattice-point i3 j1j2 k1k2 1))
               (c5 (make-lattice-point i1i2 j3 k3 1))
               (c6 (make-lattice-point i1 j2 k1 0))
               (c7 (make-lattice-point i2 j1 k2 0))
               (c8 (make-lattice-point i1i2 j3 k1k2 1))
               (c9 (make-lattice-point i3 j1j2 k3 1))
               (c10 (make-lattice-point i1 j1 k2 0))
               (c11 (make-lattice-point i2 j2 k1 0))
               (c12 (make-lattice-point i1i2 j1j2 k3 1))
               (c13 (make-lattice-point i3 j3 k1k2 1)))
          (setf (next/fail c0) c1
                (next/success c0) c1
                (next/fail c1) c2
                (next/success c1) c2
                (next/fail c2) c3
                (next/success c2) c5
                (next/fail c3) c4
                (next/success c3) c4
                (next/fail c4) c5
                (next/success c4) c6
                (next/fail c5) c6
                (next/success c5) c6
                (next/fail c6) c7
                (next/success c6) c9
                (next/fail c7) c8
                (next/success c7) c8
                (next/fail c8) c9
                (next/success c8) c10
                (next/fail c9) c10
                (next/success c9) c10
                (next/fail c10) c11
                (next/success c10) c13
                (next/fail c11) c12
                (next/success c11) c12
                (next/fail c12) c13
                (aref table i) c0)))
      table)))

(u:define-constant +lookup+ (build-lattice-points) :test #'equalp)

(u:define-constant +gradients+
    (let ((gradients #(#(-7.997138591759381d0 -7.997138591759381d0 -3.5946317686139184d0)
                       #(-7.997138591759381d0 -7.997138591759381d0 3.5946317686139184d0)
                       #(-11.093991495146318d0 -4.213452452462707d0 0d0)
                       #(-4.213452452462707d0 -11.093991495146318d0 0d0)
                       #(-7.997138591759381d0 -3.5946317686139184d0 -7.997138591759381d0)
                       #(-7.997138591759381d0 3.5946317686139184d0 -7.997138591759381d0)
                       #(-4.213452452462707d0 0d0 -11.093991495146318d0)
                       #(-11.093991495146318d0 0d0 -4.213452452462707d0)
                       #(-7.997138591759381d0 -3.5946317686139184d0 7.997138591759381d0)
                       #(-7.997138591759381d0 3.5946317686139184d0 7.997138591759381d0)
                       #(-11.093991495146318d0 0d0 4.213452452462707d0)
                       #(-4.213452452462707d0 0d0 11.093991495146318d0)
                       #(-7.997138591759381d0 7.997138591759381d0 -3.5946317686139184d0)
                       #(-7.997138591759381d0 7.997138591759381d0 3.5946317686139184d0)
                       #(-4.213452452462707d0 11.093991495146318d0 0d0)
                       #(-11.093991495146318d0 4.213452452462707d0 0d0)
                       #(-3.5946317686139184d0 -7.997138591759381d0 -7.997138591759381d0)
                       #(3.5946317686139184d0 -7.997138591759381d0 -7.997138591759381d0)
                       #(0d0 -11.093991495146318d0 -4.213452452462707d0)
                       #(0d0 -4.213452452462707d0 -11.093991495146318d0)
                       #(-3.5946317686139184d0 -7.997138591759381d0 7.997138591759381d0)
                       #(3.5946317686139184d0 -7.997138591759381d0 7.997138591759381d0)
                       #(0d0 -4.213452452462707d0 11.093991495146318d0)
                       #(0d0 -11.093991495146318d0 4.213452452462707d0)
                       #(-3.5946317686139184d0 7.997138591759381d0 -7.997138591759381d0)
                       #(3.5946317686139184d0 7.997138591759381d0 -7.997138591759381d0)
                       #(0d0 4.213452452462707d0 -11.093991495146318d0)
                       #(0d0 11.093991495146318d0 -4.213452452462707d0)
                       #(-3.5946317686139184d0 7.997138591759381d0 7.997138591759381d0)
                       #(3.5946317686139184d0 7.997138591759381d0 7.997138591759381d0)
                       #(0d0 11.093991495146318d0 4.213452452462707d0)
                       #(0d0 4.213452452462707d0 11.093991495146318d0)
                       #(7.997138591759381d0 -7.997138591759381d0 -3.5946317686139184d0)
                       #(7.997138591759381d0 -7.997138591759381d0 3.5946317686139184d0)
                       #(4.213452452462707d0 -11.093991495146318d0 0d0)
                       #(11.093991495146318d0 -4.213452452462707d0 0d0)
                       #(7.997138591759381d0 -3.5946317686139184d0 -7.997138591759381d0)
                       #(7.997138591759381d0 3.5946317686139184d0 -7.997138591759381d0)
                       #(11.093991495146318d0 0d0 -4.213452452462707d0)
                       #(4.213452452462707d0 0d0 -11.093991495146318d0)
                       #(7.997138591759381d0 -3.5946317686139184d0 7.997138591759381d0)
                       #(7.997138591759381d0 3.5946317686139184d0 7.997138591759381d0)
                       #(4.213452452462707d0 0d0 11.093991495146318d0)
                       #(11.093991495146318d0 0d0 4.213452452462707d0)
                       #(7.997138591759381d0 7.997138591759381d0 -3.5946317686139184d0)
                       #(7.997138591759381d0 7.997138591759381d0 3.5946317686139184d0)
                       #(11.093991495146318d0 4.213452452462707d0 0d0)
                       #(4.213452452462707d0 11.093991495146318d0 0d0)))
          (table (u:make-f64-array 6144)))
      (dotimes (i 2048)
        (setf (aref table (* i 3)) (aref (aref gradients (mod i 48)) 0)
              (aref table (+ (* i 3) 1)) (aref (aref gradients (mod i 48)) 1)
              (aref table (+ (* i 3) 2)) (aref (aref gradients (mod i 48)) 2)))
      table)
  :test #'equalp)

(defstruct (gen:open-simplex2s-3d
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (gradients (u:make-f64-array 6144) :type (u:f64a 6144))
  (table (u:make-b16-array 2048) :type (u:b16a 2048))
  (orientation :standard :type (member :standard :xy/z :xz/y)))

(u:fn-> permute (rng:generator) (values (u:f64a 6144) (u:b16a 2048)))
(defun permute (rng)
  (declare (optimize speed))
  (let ((source (u:make-b16-array 2048))
        (table (u:make-b16-array 2048))
        (gradients (u:make-f64-array 6144)))
    (dotimes (i 2048)
      (setf (aref source i) i))
    (loop :for i :from 2047 :downto 0
          :for r = (mod (+ (rng:int rng 0 #.(1- (expt 2 32)) nil) 31) (1+ i))
          :for x = (aref source r)
          :for pgi = (* i 3)
          :for gi = (* x 3)
          :do (setf (aref table i) x
                    (aref gradients pgi) (aref +gradients+ gi)
                    (aref gradients (+ pgi 1)) (aref +gradients+ (+ gi 1))
                    (aref gradients (+ pgi 2)) (aref +gradients+ (+ gi 2))
                    (aref source r) (aref source i)))
    (values gradients table)))

(declaim (inline orient))
(defun orient (sampler x y z)
  (ecase (orientation sampler)
    (:standard
     (let ((r (* (/ 2 3) (+ x y z))))
       (values (- r x) (- r y) (- r z))))
    (:xy/z
     (let* ((xy (+ x y))
            (s2 (* xy -0.211324865405187d0))
            (zz (* z 0.577350269189626d0)))
       (values (- (+ x s2) zz)
               (- (+ y s2) zz)
               (+ (* xy 0.577350269189626d0) zz))))
    (:xz/y
     (let* ((xz (+ x z))
            (s2 (* xz -0.211324865405187d0))
            (yy (* y 0.577350269189626d0)))
       (values (- (+ x s2) yy)
               (+ (* xz 0.577350269189626d0) yy)
               (- (+ z s2) yy))))))

(defun gen:open-simplex2s-3d (&key seed (orientation :standard))
  "Construct a sampler that, when sampled, outputs 3-dimensional OpenSimplex2S noise values ranging
from -1.0 to 1.0.

`seed`: A string used to seed the random number generator for this sampler, or NIL. If a seed is not
supplied, one will be generated automatically which will negatively affect the reproducibility of
the noise (optional, default: NIL).

`orientation`: One of `:standard`, `:xy/z`, or `:xz/y`, denoting the orientation of the lattice.
`:xy/z` has better visual isotropy in XY, and `:xz/y` has better visual isotropy in XZ (optional,
default: `:standard`)."
  (unless (member orientation '(:standard :xy/z :xz/y))
    (error 'int:invalid-open-simplex2-orientation
           :sampler-type 'open-simplex2s-3d
           :orientation orientation
           :valid-orientations '(:standard :xy/z :xz/y)))
  (u:mvlet* ((rng (int::make-rng seed))
             (gradients table (permute rng)))
    (make-open-simplex2s-3d :rng rng
                            :gradients gradients
                            :table table
                            :orientation orientation)))

(defmethod int:sample ((sampler gen:open-simplex2s-3d) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (ignore w)
           (optimize speed)
           (int::f50 x y z w))
  (u:mvlet* ((gradients (gradients sampler))
             (table (table sampler))
             (value 0d0)
             (xr yr zr (orient sampler x y z))
             (xrb xri (floor xr))
             (yrb yri (floor yr))
             (zrb zri (floor zr))
             (xht (truncate (+ xri 0.5)))
             (yht (truncate (+ yri 0.5)))
             (zht (truncate (+ zri 0.5)))
             (index (logior xht (ash yht 1) (ash zht 2)))
             (c (aref +lookup+ index)))
    (declare (u:f64 value))
    (u:while c
      (let* ((dxr (+ xri (dxr c)))
             (dyr (+ yri (dyr c)))
             (dzr (+ zri (dzr c)))
             (attn (- 0.75 (* dxr dxr) (* dyr dyr) (* dzr dzr))))
        (if (minusp attn)
            (setf c (next/fail c))
            (let* ((pxm (logand (+ xrb (xrv c)) 2047))
                   (pym (logand (+ yrb (yrv c)) 2047))
                   (pzm (logand (+ zrb (zrv c)) 2047))
                   (grad-index (* (logxor (aref table (logxor (aref table pxm) pym)) pzm) 3))
                   (grad-x (* (aref gradients grad-index) dxr))
                   (grad-y (* (aref gradients (+ grad-index 1)) dyr))
                   (grad-z (* (aref gradients (+ grad-index 2)) dzr)))
              (setf attn (* attn attn)
                    c (next/success c))
              (incf value (* attn attn (+ grad-x grad-y grad-z)))))))
    (float value 1f0)))
