(in-package #:%cricket.internal)

(define-condition cricket-error (error)
  ((%sampler-type :reader sampler-type
                  :initarg :sampler-type)))

(define-condition invalid-seed (cricket-error)
  ((%seed :reader seed
          :initarg :seed))
  (:report
   (lambda (condition stream)
     (format stream "Invalid seed: ~s.~%~%Seed must be a string or NIL." (seed condition)))))

(define-condition invalid-sampler-argument (cricket-error)
  ((%argument :reader argument
              :initarg :argument)
   (%value :reader value
           :initarg :value))
  (:report
   (lambda (condition stream)
     (format stream "Argument ~a for sampler type ~s is not a sampler.~%~%Value: ~s."
             (argument condition)
             (sampler-type condition)
             (value condition)))))

(define-condition invalid-fractal-octave-count (cricket-error)
  ((%value :reader value
           :initarg :value))
  (:report
   (lambda (condition stream)
     (format stream "Octave count for sampler ~s must be an integer between 1 and 32 ~
                     inclusive.~%~%Value: ~s."
             (sampler-type condition)
             (value condition)))))

(define-condition invalid-real-argument (cricket-error)
  ((%argument :reader argument
              :initarg :argument)
   (%value :reader value
           :initarg :value))
  (:report
   (lambda (condition stream)
     (format stream "Argument ~s for sampler ~s must be a real number.~%~%Value: ~s."
             (argument condition)
             (sampler-type condition)
             (value condition)))))

(define-condition invalid-open-simplex2-orientation (cricket-error)
  ((%orientation :reader orientation
                 :initarg :orientation)
   (%valid-orientations :reader valid-orientations
                        :initarg :valid-orientations))
  (:report
   (lambda (condition stream)
     (format stream "Invalid orientation ~s for sampler ~s.~%~% ~
                     Valid orientations: ~{~s~^, ~}"
             (orientation condition)
             (sampler-type condition)
             (valid-orientations condition)))))

(define-condition invalid-cellular-distance-method (cricket-error)
  ((%distance-method :reader distance-method
                     :initarg :distance-method)
   (%valid-distance-methods :reader valid-distance-methods
                            :initarg :valid-distance-methods))
  (:report
   (lambda (condition stream)
     (format stream "Invalid distance method ~s for sampler ~s.~%~%Valid types: ~{~s~^, ~}"
             (distance-method condition)
             (sampler-type condition)
             (valid-distance-methods condition)))))

(define-condition invalid-cellular-output-type (cricket-error)
  ((%output-type :reader output-type
                 :initarg :output-type)
   (%valid-output-types :reader valid-output-types
                        :initarg :valid-output-types))
  (:report
   (lambda (condition stream)
     (format stream "Invalid output type ~s for sampler ~s.~%~%Valid types: ~{~s~^, ~}"
             (output-type condition)
             (sampler-type condition)
             (valid-output-types condition)))))
