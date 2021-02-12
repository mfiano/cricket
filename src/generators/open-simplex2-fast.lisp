(in-package #:coherent-noise/internal)

;;; 2D OpenSimplex2F

(u:define-constant +open-simplex2-fast-2d/gradients+
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
          (table (make-array 4096 :element-type 'u:f64)))
      (dotimes (i 2048)
        (setf (aref table (ash i 1)) (aref (aref gradients (mod i 24)) 0)
              (aref table (1+ (ash i 1))) (aref (aref gradients (mod i 24)) 1)))
      table)
  :test #'equalp)

(u:define-constant +open-simplex2-fast-2d/lookup+
    (let ((data #(-0.788675134594813d0 0.211324865405187d0 0d0 0d0 -0.577350269189626d0
                  -0.577350269189626d0 0.211324865405187d0 -0.788675134594813d0)))
      (make-array 8 :element-type 'u:f64 :initial-contents data))
  :test #'equalp)

(u:fn-> open-simplex2-fast-2d/permute (rng:generator)
        (values (simple-array u:f64 (4096))
                (simple-array u:b16 (2048))))
(defun open-simplex2-fast-2d/permute (rng)
  (declare (optimize speed))
  (let ((source (make-array 2048 :element-type 'u:b16 :initial-element 0))
        (table (make-array 2048 :element-type 'u:b16 :initial-element 0))
        (gradients (make-array 4096 :element-type 'u:f64)))
    (dotimes (i 2048)
      (setf (aref source i) i))
    (loop :for i :from 2047 :downto 0
          :for r = (mod (+ (rng:int rng 0 #.(1- (expt 2 32)) nil) 31) (1+ i))
          :for x = (aref source r)
          :for pgi = (ash i 1)
          :for gi = (ash x 1)
          :do (setf (aref table i) x
                    (aref gradients pgi) (aref +open-simplex2-fast-2d/gradients+ gi)
                    (aref gradients (1+ pgi)) (aref +open-simplex2-fast-2d/gradients+ (1+ gi))
                    (aref source r) (aref source i)))
    (values gradients table)))

(u:fn-> %open-simplex2-fast-2d ((simple-array u:f64 (4096)) (simple-array u:b16 (2048)) f50 f50)
        u:f32)
(declaim (inline %open-simplex2-fast-2d))
(defun %open-simplex2-fast-2d (gradients table x y)
  (declare (optimize speed))
  (u:mvlet* ((value 0d0)
             (s (* (+ x y) 0.366025403784439d0))
             (xsb xsi (floor (+ x s)))
             (ysb ysi (floor (+ y s)))
             (index (truncate (1+ (* (- ysi xsi) 0.5))))
             (ssi (* (+ xsi ysi) -0.211324865405187d0))
             (xi (+ xsi ssi))
             (yi (+ ysi ssi)))
    (declare (u:f64 value))
    (dotimes (i 3 (float value 1f0))
      (block nil
        (let* ((lpx (* (+ index i) 2))
               (lpy (1+ lpx))
               (dx (+ xi (aref +open-simplex2-fast-2d/lookup+ lpx)))
               (dy (+ yi (aref +open-simplex2-fast-2d/lookup+ lpy)))
               (attn (- 0.5 (expt dx 2) (expt dy 2))))
          (when (minusp attn)
            (return))
          (let* ((pxm (logand (+ xsb (ldb (byte 1 lpx) #b10110001)) 2047))
                 (pym (logand (+ ysb (ldb (byte 1 lpy) #b10110001)) 2047))
                 (grad-index (ash (logxor (aref table pxm) pym) 1))
                 (grad-x (aref gradients grad-index))
                 (grad-y (aref gradients (1+ grad-index))))
            (setf attn (expt attn 2))
            (incf value (* (expt attn 2) (+ (* grad-x dx) (* grad-y dy))))))))))

;;; 3D OpenSimplex2F

(u:eval-always
  (defstruct (open-simplex2-fast-3d-lattice-point
              (:constructor %make-open-simplex2-fast-3d-lattice-point)
              (:conc-name open-simplex2-fast-3d-lattice-point-)
              (:predicate nil)
              (:copier nil))
    (dxr 0d0 :type u:f64)
    (dyr 0d0 :type u:f64)
    (dzr 0d0 :type u:f64)
    (xrv 0 :type u:ub32)
    (yrv 0 :type u:ub32)
    (zrv 0 :type u:ub32)
    (next/fail nil :type (or open-simplex2-fast-3d-lattice-point null))
    (next/success nil :type (or open-simplex2-fast-3d-lattice-point null)))

  (defun make-open-simplex2-fast-3d-lattice-point (xrv yrv zrv lattice)
    (let ((l1 (* lattice 0.5d0))
          (l2 (* lattice 1024)))
      (%make-open-simplex2-fast-3d-lattice-point :dxr (+ (- xrv) l1)
                                                 :dyr (+ (- yrv) l1)
                                                 :dzr (+ (- zrv) l1)
                                                 :xrv (+ xrv l2)
                                                 :yrv (+ yrv l2)
                                                 :zrv (+ zrv l2))))

  (defun build-open-simplex2-fast-3d-lattice-points ()
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
               (c0 (make-open-simplex2-fast-3d-lattice-point i1 j1 k1 0))
               (c1 (make-open-simplex2-fast-3d-lattice-point i1i2 j1j2 k1k2 1))
               (c2 (make-open-simplex2-fast-3d-lattice-point i2 j1 k1 0))
               (c3 (make-open-simplex2-fast-3d-lattice-point i1 j2 k1 0))
               (c4 (make-open-simplex2-fast-3d-lattice-point i1 j1 k2 0))
               (c5 (make-open-simplex2-fast-3d-lattice-point (+ i1 i3) j1j2 k1k2 1))
               (c6 (make-open-simplex2-fast-3d-lattice-point i1i2 (+ j1 j3) k1k2 1))
               (c7 (make-open-simplex2-fast-3d-lattice-point i1i2 j1j2 (+ k1 k3) 1)))
          (setf (open-simplex2-fast-3d-lattice-point-next/fail c0) c1
                (open-simplex2-fast-3d-lattice-point-next/success c0) c1
                (open-simplex2-fast-3d-lattice-point-next/fail c1) c2
                (open-simplex2-fast-3d-lattice-point-next/success c1) c2
                (open-simplex2-fast-3d-lattice-point-next/fail c2) c3
                (open-simplex2-fast-3d-lattice-point-next/success c2) c6
                (open-simplex2-fast-3d-lattice-point-next/fail c3) c4
                (open-simplex2-fast-3d-lattice-point-next/success c3) c5
                (open-simplex2-fast-3d-lattice-point-next/fail c4) c5
                (open-simplex2-fast-3d-lattice-point-next/success c4) c5
                (open-simplex2-fast-3d-lattice-point-next/fail c5) c6
                (open-simplex2-fast-3d-lattice-point-next/fail c6) c7
                (aref table i) c0)))
      table)))

(u:define-constant +open-simplex2-fast-3d/lookup+ (build-open-simplex2-fast-3d-lattice-points)
  :test #'equalp)

(u:define-constant +open-simplex2-fast-3d/gradients+
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

(u:fn-> open-simplex2-fast-3d/permute (rng:generator)
        (values (simple-array u:f64 (6144))
                (simple-array u:b16 (2048))))
(defun open-simplex2-fast-3d/permute (rng)
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
                    (aref gradients pgi) (aref +open-simplex2-fast-3d/gradients+ gi)
                    (aref gradients (+ pgi 1)) (aref +open-simplex2-fast-3d/gradients+ (+ gi 1))
                    (aref gradients (+ pgi 2)) (aref +open-simplex2-fast-3d/gradients+ (+ gi 2))
                    (aref source r) (aref source i)))
    (values gradients table)))

(u:fn-> %open-simplex2-fast-3d ((simple-array u:f64 (6144)) (simple-array u:b16 (2048)) f50 f50 f50)
        u:f32)
(defun %open-simplex2-fast-3d (gradients table x y z)
  (declare (optimize speed))
  (u:mvlet* ((value 0d0)
             (r (* (/ 2 3) (+ x y z)))
             (xr (- r x))
             (yr (- r y))
             (zr (- r z))
             (xrb xri (floor xr))
             (yrb yri (floor yr))
             (zrb zri (floor zr))
             (xht (truncate (+ xri 0.5)))
             (yht (truncate (+ yri 0.5)))
             (zht (truncate (+ zri 0.5)))
             (index (logior xht (ash yht 1) (ash zht 2)))
             (c (aref +open-simplex2-fast-3d/lookup+ index)))
    (declare (u:f64 value))
    (u:while c
      (let* ((dxr (+ xri (open-simplex2-fast-3d-lattice-point-dxr c)))
             (dyr (+ yri (open-simplex2-fast-3d-lattice-point-dyr c)))
             (dzr (+ zri (open-simplex2-fast-3d-lattice-point-dzr c)))
             (attn (- 0.5 (expt dxr 2) (expt dyr 2) (expt dzr 2))))
        (if (minusp attn)
            (setf c (open-simplex2-fast-3d-lattice-point-next/fail c))
            (let* ((pxm (logand (+ xrb (open-simplex2-fast-3d-lattice-point-xrv c)) 2047))
                   (pym (logand (+ yrb (open-simplex2-fast-3d-lattice-point-yrv c)) 2047))
                   (pzm (logand (+ zrb (open-simplex2-fast-3d-lattice-point-zrv c)) 2047))
                   (grad-index (* (logxor (aref table (logxor (aref table pxm) pym)) pzm) 3))
                   (grad-x (aref gradients grad-index))
                   (grad-y (aref gradients (+ grad-index 1)))
                   (grad-z (aref gradients (+ grad-index 2))))
              (setf attn (expt attn 2)
                    c (open-simplex2-fast-3d-lattice-point-next/success c))
              (incf value (* (expt attn 2) (+ (* grad-x dxr) (* grad-y dyr) (* grad-z dzr))))))))
    (float value 1f0)))
