;;assume P3
;;size - (width height)
;;pixels - arrays of arrays of pixels
;;actually just a list of pixels for now
;;each pixel is a list '(R G B)
(defparameter *lst* '())

(defun write-ppm (filename size pixels)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (format stream
            "P3 ~a ~a 255 ~{~%~{~a ~a ~a ~}~}"
            (first size) (second size) pixels)))

;;populate lst
(dotimes (b 4)
  (dotimes (g 256)
    (dotimes (r 256)
      (setf *lst* (cons (list r g (* b 64)) *lst*)))))
(setf *lst* (reverse *lst*))
;;(print *lst*)

(write-ppm "output.ppm" '(512 512)
           *lst*)
