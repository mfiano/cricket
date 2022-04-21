(in-package #:cl-user)

;;;; Cache modifier
;;;; This noise modifier caches the last output of its input sampler.

(defpackage #:%cricket.modifiers.cache
  (:local-nicknames
   (#:int #:%cricket.internal)
   (#:mod #:%cricket.modifiers)
   (#:u #:mfiano-utils))
  (:use #:cl))

(in-package #:%cricket.modifiers.cache)

(defstruct (mod:cache
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (source nil :type int:sampler)
  (cached-p nil :type boolean)
  (value 0f0 :type u:f32)
  (x 0d0 :type u:f64)
  (y 0d0 :type u:f64)
  (z 0d0 :type u:f64)
  (w 0d0 :type u:f64))

(defun mod:cache (source)
  "Construct a sampler that, when sampled, caches the set of input coordinates and the output of its
input sampler. If a set of input coordinates differs from the previous input coordinates, the cache
is invalidated and the new input coordinates and output value is cached.

Caching is useful if a sampler is used as a source for multiple modifiers. Without caching, the
duplicated input sources would redundantly compute the same outputs, which would be expensive,
especially if long modifier chains are shared.

`source`: The sampler to cache (required)."
  (unless (typep source 'int:sampler)
    (error 'int:invalid-sampler-argument :sampler-type 'cache :argument 'source :value source))
  (make-cache :rng (int::sampler-rng source) :source source))

(defmethod int:sample ((sampler mod:cache) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (optimize speed)
           (u:f64 x y z w))
  (unless (and (cached-p sampler)
               (= x (x sampler))
               (= y (y sampler))
               (= z (z sampler))
               (= w (w sampler)))
    (setf (cached-p sampler) t
          (x sampler) x
          (y sampler) y
          (z sampler) z
          (w sampler) w
          (value sampler) (int:sample (source sampler) x y z w)))
  (value sampler))
