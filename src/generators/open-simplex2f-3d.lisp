(in-package #:cl-user)

(defpackage #:coherent-noise.generators.open-simplex2f-3d
  (:local-nicknames
   (#:int #:coherent-noise.internal)
   (#:rng #:seedable-rng)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export #:open-simplex2f-3d))

(in-package #:coherent-noise.generators.open-simplex2f-3d)

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
               (i3 (logxor i2 1))
               (j1 (logand (ash i -1) 1))
               (j2 (logxor j1 1))
               (j3 (logxor j2 1))
               (k1 (logand (ash i -2) 1))
               (k2 (logxor k1 1))
               (k3 (logxor k2 1))
               (i1i2 (+ i1 i2))
               (j1j2 (+ j1 j2))
               (k1k2 (+ k1 k2))
               (c0 (make-lattice-point i1 j1 k1 0))
               (c1 (make-lattice-point i1i2 j1j2 k1k2 1))
               (c2 (make-lattice-point i2 j1 k1 0))
               (c3 (make-lattice-point i1 j2 k1 0))
               (c4 (make-lattice-point i1 j1 k2 0))
               (c5 (make-lattice-point (+ i1 i3) j1j2 k1k2 1))
               (c6 (make-lattice-point i1i2 (+ j1 j3) k1k2 1))
               (c7 (make-lattice-point i1i2 j1j2 (+ k1 k3) 1)))
          (setf (next/fail c0) c1
                (next/success c0) c1
                (next/fail c1) c2
                (next/success c1) c2
                (next/fail c2) c3
                (next/success c2) c6
                (next/fail c3) c4
                (next/success c3) c5
                (next/fail c4) c5
                (next/success c4) c5
                (next/fail c5) c6
                (next/fail c6) c7
                (aref table i) c0)))
      table)))

(u:define-constant +lookup+ (build-lattice-points) :test #'equalp)

(u:define-constant +gradients+
    (let ((gradients #(#(-72.97611190577304d0 -72.97611190577304d0 -32.80201376986577d0)
                       #(-72.97611190577304d0 -72.97611190577304d0 32.80201376986577d0)
                       #(-101.23575520696082d0 -38.448924468736266d0 0d0)
                       #(-38.448924468736266d0 -101.23575520696082d0 0d0)
                       #(-72.97611190577304d0 -32.80201376986577d0 -72.97611190577304d0)
                       #(-72.97611190577304d0 32.80201376986577d0 -72.97611190577304d0)
                       #(-38.448924468736266d0 0d0 -101.23575520696082d0)
                       #(-101.23575520696082d0 0d0 -38.448924468736266d0)
                       #(-72.97611190577304d0 -32.80201376986577d0 72.97611190577304d0)
                       #(-72.97611190577304d0 32.80201376986577d0 72.97611190577304d0)
                       #(-101.23575520696082d0 0d0 38.448924468736266d0)
                       #(-38.448924468736266d0 0d0 101.23575520696082d0)
                       #(-72.97611190577304d0 72.97611190577304d0 -32.80201376986577d0)
                       #(-72.97611190577304d0 72.97611190577304d0 32.80201376986577d0)
                       #(-38.448924468736266d0 101.23575520696082d0 0d0)
                       #(-101.23575520696082d0 38.448924468736266d0 0d0)
                       #(-32.80201376986577d0 -72.97611190577304d0 -72.97611190577304d0)
                       #(32.80201376986577d0 -72.97611190577304d0 -72.97611190577304d0)
                       #(0d0 -101.23575520696082d0 -38.448924468736266d0)
                       #(0d0 -38.448924468736266d0 -101.23575520696082d0)
                       #(-32.80201376986577d0 -72.97611190577304d0 72.97611190577304d0)
                       #(32.80201376986577d0 -72.97611190577304d0 72.97611190577304d0)
                       #(0d0 -38.448924468736266d0 101.23575520696082d0)
                       #(0d0 -101.23575520696082d0 38.448924468736266d0)
                       #(-32.80201376986577d0 72.97611190577304d0 -72.97611190577304d0)
                       #(32.80201376986577d0 72.97611190577304d0 -72.97611190577304d0)
                       #(0d0 38.448924468736266d0 -101.23575520696082d0)
                       #(0d0 101.23575520696082d0 -38.448924468736266d0)
                       #(-32.80201376986577d0 72.97611190577304d0 72.97611190577304d0)
                       #(32.80201376986577d0 72.97611190577304d0 72.97611190577304d0)
                       #(0d0 101.23575520696082d0 38.448924468736266d0)
                       #(0d0 38.448924468736266d0 101.23575520696082d0)
                       #(72.97611190577304d0 -72.97611190577304d0 -32.80201376986577d0)
                       #(72.97611190577304d0 -72.97611190577304d0 32.80201376986577d0)
                       #(38.448924468736266d0 -101.23575520696082d0 0d0)
                       #(101.23575520696082d0 -38.448924468736266d0 0d0)
                       #(72.97611190577304d0 -32.80201376986577d0 -72.97611190577304d0)
                       #(72.97611190577304d0 32.80201376986577d0 -72.97611190577304d0)
                       #(101.23575520696082d0 0d0 -38.448924468736266d0)
                       #(38.448924468736266d0 0d0 -101.23575520696082d0)
                       #(72.97611190577304d0 -32.80201376986577d0 72.97611190577304d0)
                       #(72.97611190577304d0 32.80201376986577d0 72.97611190577304d0)
                       #(38.448924468736266d0 0d0 101.23575520696082d0)
                       #(101.23575520696082d0 0d0 38.448924468736266d0)
                       #(72.97611190577304d0 72.97611190577304d0 -32.80201376986577d0)
                       #(72.97611190577304d0 72.97611190577304d0 32.80201376986577d0)
                       #(101.23575520696082d0 38.448924468736266d0 0d0)
                       #(38.448924468736266d0 101.23575520696082d0 0d0)))
          (table (make-array 6144 :element-type 'u:f64)))
      (dotimes (i 2048)
        (setf (aref table (* i 3)) (aref (aref gradients (mod i 48)) 0)
              (aref table (+ (* i 3) 1)) (aref (aref gradients (mod i 48)) 1)
              (aref table (+ (* i 3) 2)) (aref (aref gradients (mod i 48)) 2)))
      table)
  :test #'equalp)

(u:fn-> permute (rng:generator) (values (simple-array u:f64 (6144)) (simple-array u:b16 (2048))))
(defun permute (rng)
  (declare (optimize speed))
  (let ((source (make-array 2048 :element-type 'u:b16 :initial-element 0))
        (table (make-array 2048 :element-type 'u:b16 :initial-element 0))
        (gradients (make-array 6144 :element-type 'u:f64)))
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
(defun orient (orientation x y z)
  (ecase orientation
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

(u:fn-> sample ((simple-array u:f64 (6144))
                (simple-array u:b16 (2048))
                keyword
                int::f50
                int::f50
                int::f50)
        u:f32)
(declaim (inline sample))
(defun sample (gradients table orientation x y z)
  (declare (optimize speed))
  (u:mvlet* ((value 0d0)
             (xr yr zr (orient orientation x y z))
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
             (attn (- 0.5 (expt dxr 2) (expt dyr 2) (expt dzr 2))))
        (if (minusp attn)
            (setf c (next/fail c))
            (let* ((pxm (logand (+ xrb (xrv c)) 2047))
                   (pym (logand (+ yrb (yrv c)) 2047))
                   (pzm (logand (+ zrb (zrv c)) 2047))
                   (grad-index (* (logxor (aref table (logxor (aref table pxm) pym)) pzm) 3))
                   (grad-x (aref gradients grad-index))
                   (grad-y (aref gradients (+ grad-index 1)))
                   (grad-z (aref gradients (+ grad-index 2))))
              (setf attn (expt attn 2)
                    c (next/success c))
              (incf value (* (expt attn 2) (+ (* grad-x dxr) (* grad-y dyr) (* grad-z dzr))))))))
    (float value 1f0)))

(defun open-simplex2f-3d (&key (seed "default") (orientation :standard))
  (unless (member orientation '(:standard :xy/z :xz/y))
    (error 'int:invalid-open-simplex2-orientation
           :sampler-type 'open-simplex2f-3d
           :orientation orientation
           :valid-orientations '(:standard :xy/z :xz/y)))
  (u:mvlet* ((rng (int::make-rng seed))
             (gradients table (permute rng)))
    (lambda (x &optional (y 0d0) (z 0d0) w)
      (declare (ignore w))
      (sample gradients table orientation x y z))))