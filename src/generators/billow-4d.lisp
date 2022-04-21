(in-package #:cl-user)

;;;; 4-dimensional "billow" fractal noise generator

(defpackage #:%cricket.generators.billow-4d
  (:local-nicknames
   (#:gen #:%cricket.generators)
   (#:int #:%cricket.internal)
   (#:rng #:seedable-rng)
   (#:u #:mfiano-utils))
  (:use #:cl))

(in-package #:%cricket.generators.billow-4d)

(defstruct (gen:billow-4d
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
        :sum (expt persistence i) :into result
        :finally (return (float result 1f0))))

(defun gen:billow-4d (&key seed (generator #'gen:open-simplex2s-4d) (octaves 4) (frequency 1.0)
                        (lacunarity 2.0) (persistence 0.5))
  "Construct a sampler that, when sampled, outputs the application of multiple octaves of a
4-dimensional billow fractal noise, using the supplied `generator` function to construct each
octave's sampler.

`seed`: A string used to seed the random number generator for this sampler, or NIL. If a seed is not
supplied, one will be generated automatically which will negatively affect the reproducibility of
the noise (optional, default: NIL).

`generator`: a function object pointing to one of the built-in 4-dimensional generators that is used
to construct a different sampler, each with a different seed, for each octave (optional, default
`#'open-simplex2s-4d`).

`octaves`: An integer between 1 and 32, denoting the number of octaves to apply (optional, default:
4).

`frequency`: The frequency of the first octave's signal (optional, default: 1.0).

`lacunarity`: A multiplier that determines how quickly the frequency increases for successive
octaves (optional, default: 2.0).

`persistence`: A multiplier that determines how quickly the amplitude diminishes for successive
octaves (optional, default 0.5)."
  (unless (typep octaves '(integer 1 32))
    (error 'int:invalid-fractal-octave-count :sampler-type 'billow-4d :value octaves))
  (unless (realp frequency)
    (error 'int:invalid-real-argument
           :sampler-type 'billow-4d
           :argument :frequency
           :value frequency))
  (unless (realp lacunarity)
    (error 'int:invalid-real-argument
           :sampler-type 'billow-4d
           :argument :lacunarity
           :value lacunarity))
  (unless (realp persistence)
    (error 'int:invalid-real-argument
           :sampler-type 'billow-4d
           :argument :persistence
           :value persistence))
  (let ((rng (int::make-rng seed)))
    (make-billow-4d :rng rng
                    :sources (int::make-fractal-sources generator rng octaves)
                    :scale (get-scale-factor octaves persistence)
                    :octaves octaves
                    :frequency (float frequency 1f0)
                    :lacunarity (float lacunarity 1f0)
                    :persistence (float persistence 1f0))))

(defmethod int:sample ((sampler gen:billow-4d) x &optional (y 0d0) (z 0d0) (w 0d0))
  (loop :with sources = (sources sampler)
        :with frequency = (frequency sampler)
        :with lacunarity = (lacunarity sampler)
        :with persistence = (persistence sampler)
        :for i :below (octaves sampler)
        :for amplitude = 1.0 :then (* amplitude persistence)
        :for fx = (* x frequency) :then (* fx lacunarity)
        :for fy = (* y frequency) :then (* fy lacunarity)
        :for fz = (* z frequency) :then (* fz lacunarity)
        :for fw = (* w frequency) :then (* fw lacunarity)
        :for sample = (1- (* (abs (int:sample (aref sources i) fx fy fz fw)) 2))
        :sum (* sample amplitude) :into result
        :finally (return (/ result (scale sampler)))))
