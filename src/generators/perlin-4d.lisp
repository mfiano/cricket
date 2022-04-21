(in-package #:cl-user)

;;;; 4-dimensional Perlin ("Improved") noise generator

(defpackage #:%cricket.generators.perlin-4d
  (:local-nicknames
   (#:gen #:%cricket.generators)
   (#:int #:%cricket.internal)
   (#:rng #:seedable-rng)
   (#:u #:mfiano-utils))
  (:use #:cl))

(in-package #:%cricket.generators.perlin-4d)

(defstruct (gen:perlin-4d
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (table nil :type (u:ub8a 512)))

(defun gen:perlin-4d (&key seed)
  "Construct a sampler that, when sampled, outputs 4-dimensional Perlin Improved noise values
ranging from -1.0 to 1.0.

`seed`: A string used to seed the random number generator for this sampler, or NIL. If a seed is not
supplied, one will be generated automatically which will negatively affect the reproducibility of
the noise (optional, default: NIL)."
  (let ((rng (int::make-rng seed)))
    (make-perlin-4d :rng rng :table (int::perlin-permute rng))))

(defmethod int:sample ((sampler gen:perlin-4d) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (optimize speed)
           (int::f50 x y z w))
  (flet ((grad (hash x y z w)
           (let ((h (logand hash 31))
                 (a y)
                 (b z)
                 (c w))
             (case (ash h -3)
               (1 (setf a w b x c y))
               (2 (setf a z b w c x))
               (3 (setf a y b z c w)))
             (+ (if (zerop (logand h 4)) (- a) a)
                (if (zerop (logand h 2)) (- b) b)
                (if (zerop (logand h 1)) (- c) c)))))
    (declare (inline grad))
    (u:mvlet* ((table (table sampler))
               (xi xf (floor x))
               (yi yf (floor y))
               (zi zf (floor z))
               (wi wf (floor w))
               (xi (logand xi 255))
               (yi (logand yi 255))
               (zi (logand zi 255))
               (wi (logand wi 255))
               (fa (int::quintic-curve xf))
               (fb (int::quintic-curve yf))
               (fc (int::quintic-curve zf))
               (fd (int::quintic-curve wf))
               (a (+ (aref table xi) yi))
               (aa (+ (aref table a) zi))
               (ab (+ (aref table (1+ a)) zi))
               (b (+ (aref table (1+ xi)) yi))
               (ba (+ (aref table b) zi))
               (bb (+ (aref table (1+ b)) zi))
               (aaa (+ (aref table aa) wi))
               (aab (+ (aref table (1+ aa)) wi))
               (aba (+ (aref table ab) wi))
               (abb (+ (aref table (1+ ab)) wi))
               (baa (+ (aref table ba) wi))
               (bab (+ (aref table (1+ ba)) wi))
               (bba (+ (aref table bb) wi))
               (bbb (+ (aref table (1+ bb)) wi))
               (r1 (u:lerp fa
                           (grad (aref table aaa) xf yf zf wf)
                           (grad (aref table baa) (1- xf) yf zf wf)))
               (r2 (u:lerp fa
                           (grad (aref table aba) xf (1- yf) zf wf)
                           (grad (aref table bba) (1- xf) (1- yf) zf wf)))
               (r3 (u:lerp fa
                           (grad (aref table aab) xf yf (1- zf) wf)
                           (grad (aref table bab) (1- xf) yf (1- zf) wf)))
               (r4 (u:lerp fa
                           (grad (aref table abb) xf (1- yf) (1- zf) wf)
                           (grad (aref table bbb) (1- xf) (1- yf) (1- zf) wf)))
               (r5 (u:lerp fa
                           (grad (aref table (1+ aaa)) xf yf zf (1- wf))
                           (grad (aref table (1+ baa)) (1- xf) yf zf (1- wf))))
               (r6 (u:lerp fa
                           (grad (aref table (1+ aba)) xf (1- yf) zf (1- wf))
                           (grad (aref table (1+ bba)) (1- xf) (1- yf) zf (1- wf))))
               (r7 (u:lerp fa
                           (grad (aref table (1+ aab)) xf yf (1- zf) (1- wf))
                           (grad (aref table (1+ bab)) (1- xf) yf (1- zf) (1- wf))))
               (r8 (u:lerp fa
                           (grad (aref table (1+ abb)) xf (1- yf) (1- zf) (1- wf))
                           (grad (aref table (1+ bbb)) (1- xf) (1- yf) (1- zf) (1- wf)))))
      (float (* 0.84 (u:lerp fd
                             (u:lerp fc (u:lerp fb r1 r2) (u:lerp fb r3 r4))
                             (u:lerp fc (u:lerp fb r5 r6) (u:lerp fb r7 r8))))
             1f0))))
