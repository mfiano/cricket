(in-package #:%cricket.internal)

(deftype f50 () '(u:f64 #.(- (expt 2d0 50)) #.(expt 2d0 50)))

(u:define-constant +prime-x+ 501125321)

(u:define-constant +prime-y+ 1136930381)

(u:define-constant +prime-z+ 1720413743)

(defstruct (sampler
            (:constructor nil)
            (:predicate nil)
            (:copier nil))
  (rng nil :type (or rng:generator null)))

(u:define-printer (sampler stream :type nil :identity t)
  (format stream "~a" (class-name (class-of sampler))))

(defun perlin-permute (rng)
  (let* ((table (u:make-ub8-array 512))
         (data (rng:shuffle rng (u:iota 256))))
    (replace table data)
    (replace table data :start1 256)
    table))

(defmacro lookup (table &body (first . rest))
  (if rest
      `(aref ,table (+ ,first (lookup ,table ,@rest)))
      `(aref ,table ,first)))

(defmacro lookup-wrap (table &body (first . rest))
  (if rest
      `(aref ,table (logand (+ ,first (lookup-wrap ,table ,@rest)) 255))
      `(aref ,table (logand ,first 255))))

(declaim (inline interpolate-cubic))
(defun interpolate-cubic (a b c d alpha)
  (declare (optimize speed)
           (u:f32 a b c d alpha))
  (let* ((x (- (- d c) (- a b)))
         (y (- (- a b) x))
         (z (- c a)))
    (+ (* x alpha alpha alpha) (* y alpha alpha) (* z alpha) a)))

(declaim (inline cubic-curve))
(defun cubic-curve (x)
  (* x x (- 3.0 (* x 2.0))))

(declaim (inline quintic-curve))
(defun quintic-curve (x)
  (* x x x (+ (* x (- (* x 6.0) 15.0)) 10.0)))

(defun make-rng (seed)
  (unless (or (stringp seed)
              (null seed))
    (error 'invalid-seed :seed seed))
  (let ((rng (rng:make-generator seed)))
    (values rng (nth-value 1 (rng:get-seed rng)))))

(defun make-fractal-sources (generator rng count)
  (let ((sources (make-array count)))
    (dotimes (i count)
      (let ((rng (rng:make-generator rng)))
        (setf (aref sources i) (funcall generator :seed (rng:get-seed rng)))))
    sources))

(defgeneric sample (sampler x &optional y z w))
