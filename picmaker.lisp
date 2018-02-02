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

(write-ppm "output.ppm" '(512 512) (make-pixels))
