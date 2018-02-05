;;pixels - 2D array of pixels
(defun write-ppm (filename size pixels)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (format stream
            "P3 ~a ~a 255 ~{~%~{~{~a ~a ~a ~}~}~}"
            (first size) (second size) (2d-array-to-list pixels))))

(defun 2d-array-to-list (array)
  (loop for i below (array-dimension array 0)
        collect (loop for j below (array-dimension array 1)
                      collect (aref array i j))))

;; cube's side length, and assume plane is of form x + y + z = c
;; can be optimized
(defun cube-plane-intersect (side constant)
  (let (inter)
    (dotimes (r side)
      (dotimes (g side)
        (let ((b (- constant r g)))
          (if (and (>= b 0) (< b side))
              (push (list r g b) inter)))))
    inter))

;;pixels-side is side length of pixels
;;for each diagonal line of pixels, populate randomly with cube intersection
;;diag is diagonal constant for x + y = d
(defun intersect-populate-line (pixels pixels-side inter inter-size diag)
  (dotimes (x pixels-side)
    (let ((y (- diag x)))
      (if (and (>= y 0) (< y pixels-side))
          (setf (aref pixels x y) (nth (random inter-size) inter))))))

(defun make-pixels (a-size c-size)
  (let ((pixels (make-array (list a-size a-size))))
    ;;# of cube cross-sections
    (dotimes (c-i (- (* 3 c-size) 2))
      ;;cut cube diagonally (cross-section w/ plane)
      (let* ((inter (cube-plane-intersect c-size c-i))
            (inter-size (list-length inter)))
        ;;map certain values of intersection to line in plane
        ;;(random for now)
        ;;for every cube-intersect, map to 1  plane-intersects
        (intersect-populate-line pixels a-size inter inter-size c-i)))
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

;;iterate over pixels, create a new array where the pixels are kept only if they satisfy test
(defmacro pixels-keep-test-only (var1 var2 var3 pixels pixels-size &body test)
  `(new-pixels-iterate-pixels x y ,var1 ,var2 ,var3 new-pixels ,pixels ,pixels-size
     (if ,@test
         (setf (aref new-pixels x y) pixel)
         (setf (aref new-pixels x y) '(0 0 0)))))

;;color-only: keep that color only if it is greater or equal than the others
(defun red-only (pixels pixels-size)
  (pixels-keep-test-only r g b pixels pixels-size
    (and (>= r g) (>= r b))))
              
(defun green-only (pixels pixels-size)
  (pixels-keep-test-only r g b pixels pixels-size
    (and (>= g r) (>= g b))))

(defun blue-only (pixels pixels-size)
  (pixels-keep-test-only r g b pixels pixels-size
    (and (>= b r) (>= b g))))

;;copies source into dest starting at start in dest. Does not check bounds
(defun copy-array (dest start source source-size)
  (let ((start-x (first start))
        (start-y (second start)))
    (iterate-2d-array i j pixel source source-size
      (setf (aref dest (+ start-x i) (+ start-y j)) pixel))))
  
(defun main (a-size c-size)
  (setf *random-state* (make-random-state t))
  (let* ((pixels (make-pixels a-size c-size))
         (pixels-r (red-only pixels a-size))
         (pixels-g (green-only pixels a-size))
         (pixels-b (blue-only pixels a-size))
         (output-dimensions (list (* 2 a-size) (* 2 a-size)))
         (output (make-array output-dimensions)))
    (copy-array output '(0 0) pixels a-size)
    (copy-array output (list a-size 0) pixels-r a-size)
    (copy-array output (list 0 a-size) pixels-g a-size)
    (copy-array output (list a-size a-size) pixels-b a-size)
    (write-ppm "output.ppm" output-dimensions output)))

;;(main 190 128)
(main 382 256)
