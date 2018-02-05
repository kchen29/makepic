all:
	sbcl --script picmaker.lisp

clean:
	rm -f *.ppm
	rm -f *.png
	rm -f *~
	rm -f *.fasl
