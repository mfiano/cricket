(in-package #:cl-user)

;;;; 2-dimensional multifractal fractal noise generator

(defpackage #:%cricket.generators.multifractal-2d
  (:local-nicknames
   (#:gen #:%cricket.generators)
   (#:int #:%cricket.internal)
   (#:rng #:seedable-rng)
   (#:u #:mfiano-utils))
  (:use #:cl))

(in-package #:%cricket.generators.multifractal-2d)

(defstruct (gen:multifractal-2d
            (:include int:sampler)
            (:conc-name "")
            (:predicate nil)
            (:copier nil))
  (sources (vector) :type simple-vector)
  (scale 1.0 :type u:f32)
  (octaves 4 :type (integer 1 32))
  (frequency 1.0 :type u:f32)
  (lacunarity 2.0 :type u:f32)
  (persistence 0.5 :type u:f32))

(defun get-scale-factor (octaves persistence)
  (loop :for i :below octaves
        :for result = 1
          :then (+ result (* result (expt persistence i)))
        :finally (return (float result 1f0))))

(defun gen:multifractal-2d (&key seed (generator #'gen:open-simplex2s-2d) (octaves 4)
                              (frequency 1.0) (lacunarity 2.0) (persistence 0.5))
  "Construct a sampler that, when sampled, outputs the application of multiple octaves of a
2-dimensional multifractal noise, using the supplied `generator` function to construct each octave's
sampler.

`seed`: A string used to seed the random number generator for this sampler, or NIL. If a seed is not
supplied, one will be generated automatically which will negatively affect the reproducibility of
the noise (optional, default: NIL).

`generator`: a function object pointing to one of the built-in 2-dimensional generators that is used
to construct a different sampler, each with a different seed, for each octave (optional, default
`#'open-simplex2s-2d`).

`octaves`: An integer between 1 and 32, denoting the number of octaves to apply (optional, default:
4).

`frequency`: The frequency of the first octave's signal (optional, default: 1.0).

`lacunarity`: A multiplier that determines how quickly the frequency increases for successive
octaves (optional, default: 2.0).

`persistence`: A multiplier that determines how quickly the amplitude diminishes for successive
octaves (optional, default 0.5)."
  (unless (typep octaves '(integer 1 32))
    (error 'int:invalid-fractal-octave-count :sampler-type 'multifractal-2d :value octaves))
  (unless (realp frequency)
    (error 'int:invalid-real-argument
           :sampler-type 'multifractal-2d
           :argument :frequency
           :value frequency))
  (unless (realp lacunarity)
    (error 'int:invalid-real-argument
           :sampler-type 'multifractal-2d
           :argument :lacunarity
           :value lacunarity))
  (unless (realp persistence)
    (error 'int:invalid-real-argument
           :sampler-type 'multifractal-2d
           :argument :persistence
           :value persistence))
  (let ((rng (int::make-rng seed)))
    (make-multifractal-2d :rng rng
                          :sources (int::make-fractal-sources generator rng octaves)
                          :scale (get-scale-factor octaves persistence)
                          :octaves octaves
                          :frequency (float frequency 1f0)
                          :lacunarity (float lacunarity 1f0)
                          :persistence (float persistence 1f0))))

(defmethod int:sample ((sampler gen:multifractal-2d) x &optional (y 0d0) (z 0d0) (w 0d0))
  (declare (ignore z w))
  (loop :with sources = (sources sampler)
        :with frequency = (frequency sampler)
        :with lacunarity = (lacunarity sampler)
        :with persistence = (persistence sampler)
        :for i :below (octaves sampler)
        :for fx = (* x frequency) :then (* fx lacunarity)
        :for fy = (* y frequency) :then (* fy lacunarity)
        :for amplitude = 1 :then (* amplitude persistence)
        :for sample = (int:sample (aref sources i) fx fy)
        :for result = sample :then (+ result (* result sample amplitude))
        :finally (return (/ result (scale sampler)))))
