(in-package #:cl-user)

;;;; 3-dimensional Perlin ("Improved") noise generator

(defpackage #:%cricket.generators.perlin-3d
  (:local-nicknames
   (#:gen #:%cricket.generators)
   (#:int #:%cricket.internal)
   (#:rng #:seedable-rng)
   (#:u #:mfiano-utils))
  (:use #:cl))

(in-package #:%cricket.generators.perlin-3d)

(defstruct (gen:perlin-3d
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (table nil :type (u:ub8a 512)))

(defun gen:perlin-3d (&key seed)
  "Construct a sampler that, when sampled, outputs 3-dimensional Perlin Improved noise values
ranging from -1.0 to 1.0.

`seed`: A string used to seed the random number generator for this sampler, or NIL. If a seed is not
supplied, one will be generated automatically which will negatively affect the reproducibility of
the noise (optional, default: NIL)."
  (let ((rng (int::make-rng seed)))
    (make-perlin-3d :rng rng :table (int::perlin-permute rng))))

(defmethod int:sample ((sampler gen:perlin-3d) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (ignore w)
           (optimize speed)
           (int::f50 x y z w))
  (flet ((grad (hash x y z)
           (let* ((h (logand hash 15))
                  (u (if (< h 8) x y))
                  (v (case h
                       ((0 1 2 3) y)
                       ((12 14) x)
                       (t z))))
             (+ (if (zerop (logand h 1)) u (- u))
                (if (zerop (logand h 2)) v (- v))))))
    (declare (inline grad))
    (u:mvlet* ((table (table sampler))
               (xi xf (floor x))
               (yi yf (floor y))
               (zi zf (floor z))
               (xi (logand xi 255))
               (yi (logand yi 255))
               (zi (logand zi 255))
               (u (int::quintic-curve xf))
               (v (int::quintic-curve yf))
               (w (int::quintic-curve zf))
               (a (+ (aref table xi) yi))
               (b (+ (aref table (1+ xi)) yi))
               (r1 (grad (int::lookup table zi a) xf yf zf))
               (r2 (grad (int::lookup table zi b) (1- xf) yf zf))
               (r3 (grad (int::lookup table zi (1+ a)) xf (1- yf) zf))
               (r4 (grad (int::lookup table zi (1+ b)) (1- xf) (1- yf) zf))
               (r5 (grad (int::lookup table (1+ zi) a) xf yf (1- zf)))
               (r6 (grad (int::lookup table (1+ zi) b) (1- xf) yf (1- zf)))
               (r7 (grad (int::lookup table (1+ zi) (1+ a)) xf (1- yf) (1- zf)))
               (r8 (grad (int::lookup table (1+ zi) (1+ b)) (1- xf) (1- yf) (1- zf)))
               (r9 (u:lerp v (u:lerp u r1 r2) (u:lerp u r3 r4)))
               (r10 (u:lerp v (u:lerp u r5 r6) (u:lerp u r7 r8))))
      (float (* (u:lerp w r9 r10) 0.9649214285521897d0) 1f0))))
