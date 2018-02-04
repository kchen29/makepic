;;assume P3
;;size - (width height)
;;pixels - a list of pixels for now
;;each pixel is a list '(R G B)
(defun write-ppm (filename size pixels)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (format stream
            "P3 ~a ~a 255 ~{~%~{~a ~a ~a ~}~}"
            (first size) (second size) pixels)))

;;generate the pixels
(defun make-pixels ()
  (let ((pixels '()))
    (dotimes (b 4)
      (dotimes (g 256)
        (dotimes (r 256)
          (push (list r g (* b 64)) pixels))))
    (reverse pixels)))

;;(write-ppm "output.ppm" '(512 512) (make-pixels))


;;pixels - 2D array of pixels
(defun write-ppm2 (filename size pixels)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (format stream
            "P3 ~a ~a 255 ~{~%~{~{~a ~a ~a ~}~}~}"
            (first size) (second size) (2d-array-to-list pixels))))

(defun 2d-array-to-list (array)
  (loop for i below (array-dimension array 0)
        collect (loop for j below (array-dimension array 1)
                      collect (aref array i j))))

;;cube's side length, and assume plane is of form x + y + z = c
;;assume zero-indexed
;;can be optimized
;; (defun cube-plane-intersect (side constant)
;;   (let (inter)
;;     (dotimes (r side)
;;       (dotimes (g side)
;;         (let ((b (- constant r g)))
;;           (if (and (>= b 0) (< b side))
;;               (push (list r g b) inter)))))
;;     inter))

;;pixels-side is side length of pixels
;;for each diagonal line of pixels, populate randomly with cube intersection
;;diag is diagonal constant for x + y = d
(defun intersect-populate-line (pixels pixels-side c-size c-i diag)
  (dotimes (x pixels-side)
    (let ((y (- diag x)))
      (if (and (>= y 0) (< y pixels-side))
          (setf (aref pixels x y) (random-triple c-size c-i))))))

;;return random double with each coord having a maximum of side - 1, and summing to sum
(defun random-double (side sum)
  (let* ((init (max 0 (- sum side -1)))
         (ran-max (- (min (1+ sum) side) init))
         (x (+ init (random ran-max)))
         (y (- sum x)))
    (list x y)))

;;not actually uniformly random...
(defun random-triple (side sum)
  (let* ((init (max 0 (- sum (* 2 (1- side)))))
         (ran-max (- (min (1+ sum) side) init))
         (x (+ init (random ran-max)))
         (yz (random-double side (- sum x))))
    (list x (first yz) (second yz))))

(defun make-pixels2 (a-size c-size multiplicity)
  (let ((pixels (make-array (list a-size a-size))))
    ;;# of cube cross-sections
    (dotimes (c-i (- (* 3 c-size) 2))
      ;;cut cube diagonally (cross-section w/ plane)
      ;;map certain values of intersection to line in plane
      ;;(random for now)
      ;;for every cube-intersect, map to multiplicity plane-intersects
      (dotimes (i multiplicity)
        (intersect-populate-line pixels a-size c-size c-i (+ i (* multiplicity c-i)))))
    pixels))

;;macros are not hygienic
;;iterate over a square 2d array
;;supplies x y and the element
(defmacro iterate-2d-array (var1 var2 el arr arr-size &body body)
  `(dotimes (,var1 ,arr-size)
     (dotimes (,var2 ,arr-size)
       (let ((,el (aref ,arr ,var1 ,var2)))
         ,@body))))

;;iterate over a square 2d pixel array
;;supplies x y and r g b
(defmacro iterate-pixels (x1 x2 var1 var2 var3 pixels pixels-size &body body)
  `(iterate-2d-array ,x1 ,x2 pixel ,pixels ,pixels-size
     (let ((,var1 (first pixel))
           (,var2 (second pixel))
           (,var3 (third pixel)))
       ,@body)))

;;iterate over pixels, create a new array
(defmacro new-pixels-iterate-pixels (x1 x2 var1 var2 var3 new-pixels pixels pixels-size &body body)
  `(let ((,new-pixels (make-array (list ,pixels-size ,pixels-size))))
     (iterate-pixels ,x1 ,x2 ,var1 ,var2 ,var3 ,pixels ,pixels-size
       ,@body)
     ,new-pixels))

(defun red-only (pixels pixels-size)
  (new-pixels-iterate-pixels x y r g b new-pixels pixels pixels-size
    (if (and (> r g) (> r b))
        (setf (aref new-pixels x y) pixel)
        (setf (aref new-pixels x y) '(0 0 0)))))

(defun green-only (pixels pixels-size)
  (new-pixels-iterate-pixels x y r g b new-pixels pixels pixels-size
    (if (and (> g r) (> g b))
        (setf (aref new-pixels x y) pixel)
        (setf (aref new-pixels x y) '(0 0 0)))))

(defun blue-only (pixels pixels-size)
  (new-pixels-iterate-pixels x y r g b new-pixels pixels pixels-size
    (if (and (> b r) (> b g))
        (setf (aref new-pixels x y) pixel)
        (setf (aref new-pixels x y) '(0 0 0)))))

(setf *random-state* (make-random-state t))
(let ((pixels (make-pixels2 766 256 2)))
  (write-ppm2 "output.ppm" '(766 766) pixels)
  (write-ppm2 "output-red.ppm" '(766 766) (red-only pixels 766))
  (write-ppm2 "output-green.ppm" '(766 766) (green-only pixels 766))
  (write-ppm2 "output-blue.ppm" '(766 766) (blue-only pixels 766)))

;;keep the pixels w/ more red than green and blue
;;black otherwise
;; (defun red-only (pixels pixels-size)
;;   (let ((new-pixels (make-array (list pixels-size pixels-size))))
;;     (dotimes (x pixels-size)
;;       (dotimes (y pixels-size)
;;         (let* ((pixel (aref pixels x y))
;;                (r (first pixel))
;;                (g (second pixel))
;;                (b (third pixel)))
;;           (if (and (> r g) (> r b))
;;               (setf (aref new-pixels x y) pixel)
;;               (setf (aref new-pixels x y) '(0 0 0))))))
;;     new-pixels))

