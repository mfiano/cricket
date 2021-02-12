(in-package #:coherent-noise/internal)

(defun perlin-1d (&key (seed "default"))
  (let* ((rng (make-rng seed))
         (table (rng:shuffle rng +perlin/permutation+)))
    (lambda (x &optional y z w)
      (declare (ignore y z w))
      (%perlin-1d table x))))

(defun perlin-2d (&key (seed "default"))
  (let* ((rng (make-rng seed))
         (table (rng:shuffle rng +perlin/permutation+)))
    (lambda (x &optional (y 0d0) z w)
      (declare (ignore z w))
      (%perlin-2d table x y))))

(defun perlin-3d (&key (seed "default"))
  (let* ((rng (make-rng seed))
         (table (rng:shuffle rng +perlin/permutation+)))
    (lambda (x &optional (y 0d0) (z 0d0) w)
      (declare (ignore w))
      (%perlin-3d table x y z))))

(defun perlin-4d (&key (seed "default"))
  (let* ((rng (make-rng seed))
         (table (rng:shuffle rng +perlin/permutation+)))
    (lambda (x &optional (y 0d0) (z 0d0) (w 0d0))
      (%perlin-4d table x y z w))))

(defun simplex-1d (&key (seed "default"))
  (let* ((rng (make-rng seed))
         (table (rng:shuffle rng +perlin/permutation+)))
    (lambda (x &optional y z w)
      (declare (ignore y z w))
      (%simplex-1d table x))))

(defun simplex-2d (&key (seed "default"))
  (let* ((rng (make-rng seed))
         (table (rng:shuffle rng +perlin/permutation+)))
    (lambda (x &optional (y 0d0) z w)
      (declare (ignore z w))
      (%simplex-2d table x y))))

(defun simplex-3d (&key (seed "default"))
  (let* ((rng (make-rng seed))
         (table (rng:shuffle rng +perlin/permutation+)))
    (lambda (x &optional (y 0d0) (z 0d0) w)
      (declare (ignore w))
      (%simplex-3d table x y z))))

(defun simplex-4d (&key (seed "default"))
  (let* ((rng (make-rng seed))
         (table (rng:shuffle rng +perlin/permutation+)))
    (lambda (x &optional (y 0d0) (z 0d0) (w 0d0))
      (%simplex-4d table x y z w))))

(defun open-simplex-2d (&key (seed "default"))
  (let* ((rng (make-rng seed))
         (table (rng:shuffle rng +perlin/permutation+)))
    (lambda (x &optional (y 0d0) z w)
      (declare (ignore z w))
      (%open-simplex-2d table x y))))

(defun open-simplex-3d (&key (seed "default"))
  (let* ((rng (make-rng seed))
         (table (rng:shuffle rng +open-simplex-3d/permutation+)))
    (lambda (x &optional (y 0d0) (z 0d0) w)
      (declare (ignore w))
      (%open-simplex-3d table x y z))))

(defun open-simplex-4d (&key (seed "default"))
  (let* ((rng (make-rng seed))
         (table (rng:shuffle rng +perlin/permutation+)))
    (lambda (x &optional (y 0d0) (z 0d0) (w 0d0))
      (%open-simplex-4d table x y z w))))

(defun open-simplex2-fast-2d (&key (seed "default"))
  (u:mvlet* ((rng (make-rng seed))
             (perm-grad perm (open-simplex2-fast-2d/permute rng)))
    (lambda (x &optional (y 0d0) z w)
      (declare (ignore z w))
      (%open-simplex2-fast-2d perm-grad perm x y))))

(defun open-simplex2-fast-3d (&key (seed "default"))
  (u:mvlet* ((rng (make-rng seed))
             (perm-grad perm (open-simplex2-fast-3d/permute rng)))
    (lambda (x &optional (y 0d0) (z 0d0) w)
      (declare (ignore w))
      (%open-simplex2-fast-3d perm-grad perm x y z))))

(defun open-simplex2-fast-4d (&key (seed "default"))
  (u:mvlet* ((rng (make-rng seed))
             (perm-grad perm (open-simplex2-fast-4d/permute rng)))
    (lambda (x &optional (y 0d0) (z 0d0) (w 0d0))
      (%open-simplex2-fast-4d perm-grad perm x y z w))))

(defun value-2d (&key (seed "default"))
  (u:mvlet ((rng seed (make-rng seed)))
    (lambda (x &optional (y 0d0) z w)
      (declare (ignore z w))
      (%value-2d seed x y))))

(defun value-3d (&key (seed "default"))
  (u:mvlet ((rng seed (make-rng seed)))
    (lambda (x &optional (y 0d0) (z 0d0) w)
      (declare (ignore w))
      (%value-3d seed x y z))))

(defun cellular-2d (&key (seed "default") (distance-method :euclidean) (output-type :min)
                      (jitter 1.0))
  (check-cellular-distance-method 'cellular-2d distance-method)
  (check-cellular-output-type 'cellular-2d output-type)
  (check-cellular-jitter 'cellular-2d jitter)
  (u:mvlet ((rng seed (make-rng seed))
            (jitter (u:clamp (float jitter 1f0) 0.0 1.0)))
    (lambda (x &optional (y 0d0) z w)
      (declare (ignore z w))
      (%cellular-2d seed distance-method output-type jitter x y))))

(defun cellular-3d (&key (seed "default") (distance-method :euclidean) (output-type :min)
                      (jitter 1.0))
  (check-cellular-distance-method 'cellular-3d distance-method)
  (check-cellular-output-type 'cellular-3d output-type)
  (check-cellular-jitter 'cellular-3d jitter)
  (u:mvlet ((rng seed (make-rng seed))
            (jitter (u:clamp (float jitter 1f0) 0.0 1.0)))
    (lambda (x &optional (y 0d0) (z 0d0) w)
      (declare (ignore w))
      (%cellular-3d seed distance-method output-type jitter x y z))))

(defun cylinders (&key (frequency 1.0))
  (lambda (x &optional y (z 0d0) w)
    (declare (ignore y w))
    (%cylinders frequency x z)))

(defun spheres (&key (frequency 1.0))
  (lambda (x &optional (y 0d0) (z 0d0) w)
    (declare (ignore w))
    (%spheres frequency x y z)))

(defun checkered ()
  (lambda (x &optional (y 0d0) (z 0d0) w)
    (declare (ignore w))
    (%checkered x y z)))

(defun constant (value)
  (constantly (u:lerp (u:clamp value 0 1) -1.0 1.0)))