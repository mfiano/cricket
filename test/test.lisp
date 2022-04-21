(in-package #:cricket.test)

(plan 3)

(subtest "generators"
  (plan 25)
  (is-values (compare 'generate-perlin-1d
                      (c:uniform-scale (c:perlin-1d :seed "default") 10))
             '(t t)
             "perlin-1d")
  (is-values (compare 'generate-perlin-2d
                      (c:uniform-scale (c:perlin-2d :seed "default") 10))
             '(t t)
             "perlin-2d")
  (is-values (compare 'generate-perlin-3d
                      (c:uniform-scale (c:perlin-3d :seed "default") 10))
             '(t t)
             "perlin-3d")
  (is-values (compare 'generate-perlin-4d
                      (c:uniform-scale (c:perlin-4d :seed "default") 10))
             '(t t)
             "perlin-4d")
  (is-values (compare 'generate-simplex-1d
                      (c:uniform-scale (c:simplex-1d :seed "default") 10))
             '(t t)
             "simplex-1d")
  (is-values (compare 'generate-simplex-2d
                      (c:uniform-scale (c:simplex-2d :seed "default") 10))
             '(t t)
             "simplex-2d")
  (is-values (compare 'generate-simplex-3d
                      (c:uniform-scale (c:simplex-3d :seed "default") 10))
             '(t t)
             "simplex-3d")
  (is-values (compare 'generate-simplex-4d
                      (c:uniform-scale (c:simplex-4d :seed "default") 10))
             '(t t)
             "simplex-4d")
  (is-values (compare 'generate-open-simplex-2d
                      (c:uniform-scale (c:open-simplex-2d :seed "default") 10))
             '(t t)
             "open-simplex-2d")
  (is-values (compare 'generate-open-simplex-3d
                      (c:uniform-scale (c:open-simplex-3d :seed "default") 10))
             '(t t)
             "open-simplex-3d")
  (is-values (compare 'generate-open-simplex-4d
                      (c:uniform-scale (c:open-simplex-4d :seed "default") 10))
             '(t t)
             "open-simplex-4d")
  (is-values (compare 'generate-open-simplex2f-2d
                      (c:uniform-scale (c:open-simplex2f-2d :seed "default") 10))
             '(t t)
             "open-simplex2f-2d")
  (is-values (compare 'generate-open-simplex2f-3d
                      (c:uniform-scale (c:open-simplex2f-3d :seed "default") 10))
             '(t t)
             "open-simplex2f-3d")
  (is-values (compare 'generate-open-simplex2f-4d
                      (c:uniform-scale (c:open-simplex2f-4d :seed "default") 10))
             '(t t)
             "open-simplex2f-4d")
  (is-values (compare 'generate-open-simplex2s-2d
                      (c:uniform-scale (c:open-simplex2s-2d :seed "default") 10))
             '(t t)
             "open-simplex2s-2d")
  (is-values (compare 'generate-open-simplex2s-3d
                      (c:uniform-scale (c:open-simplex2s-3d :seed "default") 10))
             '(t t)
             "open-simplex2s-3d")
  (is-values (compare 'generate-open-simplex2s-4d
                      (c:uniform-scale (c:open-simplex2s-4d  :seed "default") 10))
             '(t t)
             "open-simplex2s-4d")
  (is-values (compare 'generate-value-2d
                      (c:uniform-scale (c:value-2d :seed "default") 10))
             '(t t)
             "value-2d")
  (is-values (compare 'generate-value-3d
                      (c:uniform-scale (c:value-3d :seed "default") 10))
             '(t t)
             "value-3d")
  (is-values (compare 'generate-cellular-2d
                      (c:uniform-scale (c:cellular-2d :seed "default") 10))
             '(t t)
             "cellular-2d")
  (is-values (compare 'generate-cellular-3d
                      (c:uniform-scale (c:cellular-3d :seed "default") 10))
             '(t t)
             "cellular-3d")
  (is-values (compare 'generate-cylinders-3d
                      (c:uniform-scale (c:cylinders-3d :seed "default") 10))
             '(t t)
             "cylinders-3d")
  (is-values (compare 'generate-spheres-3d
                      (c:uniform-scale (c:spheres-3d :seed "default") 10))
             '(t t)
             "spheres-3d")
  (is-values (compare 'generate-checker-2d
                      (c:uniform-scale (c:checker-2d :seed "default") 10))
             '(t t)
             "checker-2d")
  (is-values (compare 'generate-constant
                      (c:uniform-scale (c:constant 0.5 :seed "default") 10))
             '(t t)
             "constant")
  (finalize))

(subtest "seed"
  (plan 21)
  (is-values (compare 'generate-perlin-1d
                      (c:uniform-scale (c:perlin-1d :seed "new-seed") 10))
             '(nil t)
             "perlin-1d")
  (is-values (compare 'generate-perlin-2d
                      (c:uniform-scale (c:perlin-2d :seed "new-seed") 10))
             '(nil t)
             "perlin-2d")
  (is-values (compare 'generate-perlin-3d
                      (c:uniform-scale (c:perlin-3d :seed "new-seed") 10))
             '(nil t)
             "perlin-3d")
  (is-values (compare 'generate-perlin-4d
                      (c:uniform-scale (c:perlin-4d :seed "new-seed") 10))
             '(nil t)
             "perlin-4d")
  (is-values (compare 'generate-simplex-1d
                      (c:uniform-scale (c:simplex-1d :seed "new-seed") 10))
             '(nil t)
             "simplex-1d")
  (is-values (compare 'generate-simplex-2d
                      (c:uniform-scale (c:simplex-2d :seed "new-seed") 10))
             '(nil t)
             "simplex-2d")
  (is-values (compare 'generate-simplex-3d
                      (c:uniform-scale (c:simplex-3d :seed "new-seed") 10))
             '(nil t)
             "simplex-3d")
  (is-values (compare 'generate-simplex-4d
                      (c:uniform-scale (c:simplex-4d :seed "new-seed") 10))
             '(nil t)
             "simplex-4d")
  (is-values (compare 'generate-open-simplex-2d
                      (c:uniform-scale (c:open-simplex-2d :seed "new-seed") 10))
             '(nil t)
             "open-simplex-2d")
  (is-values (compare 'generate-open-simplex-3d
                      (c:uniform-scale (c:open-simplex-3d :seed "new-seed") 10))
             '(nil t)
             "open-simplex-3d")
  (is-values (compare 'generate-open-simplex-4d
                      (c:uniform-scale (c:open-simplex-4d :seed "new-seed") 10))
             '(nil t)
             "open-simplex-4d")
  (is-values (compare 'generate-open-simplex2f-2d
                      (c:uniform-scale (c:open-simplex2f-2d :seed "new-seed") 10))
             '(nil t)
             "open-simplex2f-2d")
  (is-values (compare 'generate-open-simplex2f-3d
                      (c:uniform-scale (c:open-simplex2f-3d :seed "new-seed") 10))
             '(nil t)
             "open-simplex2f-3d")
  (is-values (compare 'generate-open-simplex2f-4d
                      (c:uniform-scale (c:open-simplex2f-4d :seed "new-seed") 10))
             '(nil t)
             "open-simplex2f-4d")
  (is-values (compare 'generate-open-simplex2s-2d
                      (c:uniform-scale (c:open-simplex2s-2d :seed "new-seed") 10))
             '(nil t)
             "open-simplex2s-2d")
  (is-values (compare 'generate-open-simplex2s-3d
                      (c:uniform-scale (c:open-simplex2s-3d :seed "new-seed") 10))
             '(nil t)
             "open-simplex2s-3d")
  (is-values (compare 'generate-open-simplex2s-4d
                      (c:uniform-scale (c:open-simplex2s-4d :seed "new-seed") 10))
             '(nil t)
             "open-simplex2s-4d")
  (is-values (compare 'generate-value-2d
                      (c:uniform-scale (c:value-2d :seed "new-seed") 10))
             '(nil t)
             "value-2d")
  (is-values (compare 'generate-value-3d
                      (c:uniform-scale (c:value-3d :seed "new-seed") 10))
             '(nil t)
             "value-3d")
  (is-values (compare 'generate-cellular-2d
                      (c:uniform-scale (c:cellular-2d :seed "new-seed") 10))
             '(nil t)
             "cellular-2d")
  (is-values (compare 'generate-cellular-3d
                      (c:uniform-scale (c:cellular-3d :seed "new-seed") 10))
             '(nil t)
             "cellular-3d")
  (finalize))

(subtest "modifiers"
  (plan 24)
  (is-values (compare 'modify-abs
                      (c:abs (c:uniform-scale (c:perlin-3d :seed "default") 10)))
             '(t t)
             "abs")
  (is-values (compare 'modify-blend
                      (c:blend (c:uniform-scale (c:perlin-3d :seed "default") 10)
                               (c:uniform-scale (c:simplex-2d :seed "default") 8)
                               (c:uniform-scale (c:open-simplex-3d :seed "default") 10)))
             '(t t)
             "blend")
  (is-values (compare 'modify-blend
                      (c:cache
                       (c:blend (c:uniform-scale (c:perlin-3d :seed "default") 10)
                                (c:uniform-scale (c:simplex-2d :seed "default") 8)
                                (c:uniform-scale (c:open-simplex-3d :seed "default") 10))))
             '(t t)
             "cache")
  (is-values (compare 'modify-clamp
                      (c:clamp (c:uniform-scale (c:perlin-3d :seed "default") 10) -0.2 0.4))
             '(t t)
             "clamp")
  (is-values (compare 'modify-curve
                      (c:curve (c:abs (c:uniform-scale (c:perlin-3d :seed "default") 50))
                               :points '((0.1 . 0.43) (0.4 . 0.25) (0.6 . 0.4) (0.65 . 0.8))))
             '(t t)
             "curve")
  (is-values (compare 'modify-displace
                      (c:displace (c:uniform-scale (c:perlin-3d :seed "default") 10)
                                  :x (c:uniform-scale (c:simplex-3d :seed "default") 8)
                                  :y (c:uniform-scale (c:open-simplex-2d :seed "default") 9)
                                  :z (c:uniform-scale (c:perlin-2d :seed "default") 7)))
             '(t t)
             "displace")
  (is-values (compare 'modify-expt
                      (c:expt (c:uniform-scale (c:perlin-3d :seed "default") 10) 2))
             '(t t)
             "expt")
  (is-values (compare 'modify-fractalize-billow
                      (c:fractalize (c:uniform-scale (c:perlin-3d :seed "default") 10) :billow))
             '(t t)
             "fractalize-billow")
  (is-values (compare 'modify-fractalize-fbm
                      (c:fractalize (c:uniform-scale (c:perlin-3d :seed "default") 10) :fbm))
             '(t t)
             "fractalize-fbm")
  (is-values (compare 'modify-fractalize-hybrid-multi
                      (c:fractalize (c:uniform-scale (c:perlin-3d :seed "default") 10)
                                    :hybrid-multi))
             '(t t)
             "fractalize-hybrid-multi")
  (is-values (compare 'modify-fractalize-multi
                      (c:fractalize (c:uniform-scale (c:perlin-3d :seed "default") 10)
                                    :multi))
             '(t t)
             "fractalize-multi")
  (is-values (compare 'modify-fractalize-ridged-multi
                      (c:fractalize (c:uniform-scale (c:perlin-3d :seed "default") 10)
                                    :ridged-multi))
             '(t t)
             "fractalize-ridged-multi")
  (is-values (compare 'modify-negate
                      (c:negate (c:uniform-scale (c:perlin-3d :seed "default") 10)))
             '(t t)
             "negate")
  (is-values (compare 'modify-max
                      (c:max (c:uniform-scale (c:perlin-3d :seed "default") 10)
                             (c:uniform-scale (c:simplex-3d :seed "default") 10)))
             '(t t)
             "max")
  (is-values (compare 'modify-min
                      (c:min (c:uniform-scale (c:perlin-3d :seed "default") 10)
                             (c:uniform-scale (c:simplex-3d :seed "default") 10)))
             '(t t)
             "min")
  (is-values (compare 'modify-power
                      (c:power (c:uniform-scale (c:perlin-3d :seed "default") 10)
                               (c:uniform-scale (c:simplex-3d :seed "default") 10)))
             '(t t)
             "power")
  (is-values (compare 'modify-rotate
                      (c:rotate (c:uniform-scale (c:simplex-3d :seed "default") 10) :z (/ pi 4)))
             '(t t)
             "rotate")
  (is-values (compare 'modify-scale
                      (c:scale (c:simplex-3d :seed "default") :x 5 :y 10 :z 15))
             '(t t)
             "scale")
  (is-values (compare 'modify-select
                      (c:select (c:uniform-scale (c:perlin-3d :seed "default") 10)
                                (c:uniform-scale (c:simplex-2d :seed "default") 9)
                                (c:uniform-scale (c:open-simplex-2d :seed "default") 8)
                                :min -0.5
                                :max 0.2
                                :falloff 0.5))
             '(t t)
             "select")
  (is-values (compare 'modify-strengthen
                      (c:strengthen
                       (c:uniform-scale (c:perlin-3d :seed "default") 10)
                       1.5
                       :bias 0.1))
             '(t t)
             "strengthen")
  (is-values (compare 'modify-terrace
                      (c:terrace (c:uniform-scale (c:perlin-3d :seed "default") 50)
                                 :invert-p t
                                 :points '(0.04 0.28 0.4 0.6 0.7 1)))
             '(t t)
             "terrace")
  (is-values (compare 'modify-translate
                      (c:translate (c:uniform-scale (c:perlin-3d :seed "default") 10)
                                   :x 5
                                   :y 10
                                   :z 15))
             '(t t)
             "translate")
  (is-values (compare 'modify-turbulence
                      (c:turbulence (c:uniform-scale (c:perlin-3d :seed "default") 10)
                                    (c:perlin-3d :seed "default")))
             '(t t)
             "turbulence")
  (is-values (compare 'modify-uniform-scale
                      (c:uniform-scale (c:perlin-3d :seed "default") 5))
             '(t t)
             "uniform-scale")
  (finalize))

(finalize)
