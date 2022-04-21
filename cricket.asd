(asdf:defsystem #:cricket
  :description "A library for generating and manipulating coherent noise"
  :author "Michael Fiano <mail@mfiano.net>"
  :license "MIT"
  :homepage "https://git.mfiano.net/mfiano/cricket"
  :version "0.1.0"
  :encoding :utf-8
  :depends-on (#:arrow-macros
               #:cl-cpus
               #:lparallel
               #:seedable-rng
               #:mfiano-utils
               #:uiop
               #:zpng)
  :in-order-to ((asdf:test-op (asdf:test-op #:cricket.test)))
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "conditions")
   (:file "common")
   (:module "generators"
    :serial t
    :components
    ((:file "perlin-1d")
     (:file "perlin-2d")
     (:file "perlin-3d")
     (:file "perlin-4d")
     (:file "simplex-1d")
     (:file "simplex-2d")
     (:file "simplex-3d")
     (:file "simplex-4d")
     (:file "open-simplex-2d")
     (:file "open-simplex-3d")
     (:file "open-simplex-4d")
     (:file "open-simplex2f-2d")
     (:file "open-simplex2f-3d")
     (:file "open-simplex2f-4d")
     (:file "open-simplex2s-2d")
     (:file "open-simplex2s-3d")
     (:file "open-simplex2s-4d")
     (:file "value-2d")
     (:file "value-3d")
     (:file "cellular-2d")
     (:file "cellular-3d")
     (:file "cylinders-3d")
     (:file "spheres-3d")
     (:file "checker-2d")
     (:file "constant")
     (:file "fbm-2d")
     (:file "fbm-3d")
     (:file "fbm-4d")
     (:file "billow-2d")
     (:file "billow-3d")
     (:file "billow-4d")
     (:file "multifractal-2d")
     (:file "multifractal-3d")
     (:file "multifractal-4d")
     (:file "hybrid-multifractal-2d")
     (:file "hybrid-multifractal-3d")
     (:file "hybrid-multifractal-4d")
     (:file "ridged-multifractal-2d")
     (:file "ridged-multifractal-3d")
     (:file "ridged-multifractal-4d")))
   (:module "modifiers"
    :serial t
    :components
    ((:file "abs")
     (:file "add")
     (:file "blend")
     (:file "cache")
     (:file "clamp")
     (:file "curve")
     (:file "displace")
     (:file "divide")
     (:file "expt")
     (:file "fractalize")
     (:file "max")
     (:file "min")
     (:file "negate")
     (:file "multiply")
     (:file "power")
     (:file "rotate")
     (:file "scale")
     (:file "select")
     (:file "strengthen")
     (:file "subtract")
     (:file "terrace")
     (:file "translate")
     (:file "turbulence")))
   (:module "map"
    :serial t
    :components
    ((:file "color")
     (:file "gradient")
     (:file "image")
     (:file "map")))))
