(in-package #:cl-user)

;;;; Fractalize modifier
;;;; This noise modifier outputs a fractal of its input sampler's output.

(defpackage #:%cricket.modifiers.fractalize
  (:local-nicknames
   (#:gen #:%cricket.generators)
   (#:int #:%cricket.internal)
   (#:rng #:seedable-rng)
   (#:mod #:%cricket.modifiers))
  (:use #:cl))

(in-package #:%cricket.modifiers.fractalize)

(defun mod:fractalize (source type &rest args
                       &key (octaves 4) (frequency 1.0) (lacunarity 2.0) (persistence 0.5)
                         (attenuation 2.0))
  "Construct a sampler that, when sampled, outputs the result of fractalizing its input sampler's
output over an arbitrary number of octaves. `type` may be a keyword denoting any of the built-in
generator fractal types:

`:fbm`: Fractional Brownian motion.

`:billow`: Creates billowy cloud or rock-like formations.

`:multi`: Multifractal whose fractal dimensions vary.

`:hybrid-multi`: Multifractal noise that results in valleys having smooth bottoms.

`:ridged-multi`: Creates ridges suitable for terrain generation.

The parameters supplied to this modifier are consistent with the behaviors they provide for the
corresponding fractal generator.

There are a few differences between this modifier and the corresponding generator:

- This modifier fractalizes each dimension of the input sampler; there is no ability to specify the
  dimensionality of the output.

- The default parameter values of this modifier are generic, and may not match the default values of
  the corresponding generator. You should ensure that you are calling this with the desired
  parameter values if leaving any unspecified.

- The input sampler's output is used for each octave of the generated fractal. This is in contrast
  to generator fractals, where their octaves are other generators each being seeded differently.

`source`: The input sampler (required).

`type`: A keyword denoting one of the valid fractal types (required).

`octaves`: The number of octaves to generate (optional, default: 4).

`frequency`: The frequency of the first octave (optional, default: 1.0).

`lacunarity`: A multiplier that determines how quickly the frequency increases for successive
octaves (optional, default: 2.0).

`persistence`: A multiplier that determines how quickly the amplitude diminishes for successive
octaves (optional, default 0.5).

`attenuation`: The attenuation to apply to the weight of each octave - only applicable for the
`:ridged-multi` type (optional, default: 2.0)"
  (unless (typep source 'int:sampler)
    (error 'int:invalid-sampler-argument :sampler-type 'fractalize :argument 'source :value source))
  (unless (typep octaves '(integer 1 32))
    (error 'int:invalid-fractal-octave-count :sampler-type 'fractalize :value octaves))
  (unless (realp frequency)
    (error 'int:invalid-real-argument
           :sampler-type 'fractalize
           :argument :frequency
           :value frequency))
  (unless (realp lacunarity)
    (error 'int:invalid-real-argument
           :sampler-type 'fractalize
           :argument :lacunarity
           :value lacunarity))
  (unless (realp persistence)
    (error 'int:invalid-real-argument
           :sampler-type 'fractalize
           :argument :persistence
           :value persistence))
  (unless (realp attenuation)
    (error 'int:invalid-real-argument
           :sampler-type 'fractalize
           :argument :attenuation
           :value attenuation))
  (let ((seed (rng:get-seed (int::sampler-rng source)))
        (fractal (ecase type
                   (:fbm #'gen:fbm-4d)
                   (:billow #'gen:billow-4d)
                   (:multi #'gen:multifractal-4d)
                   (:hybrid-multi #'gen:hybrid-multifractal-4d)
                   (:ridged-multi #'gen:ridged-multifractal-4d))))
    (apply fractal :seed seed :generator (constantly source) :allow-other-keys t args)))
