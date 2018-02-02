all: picmaker.lisp
	sbcl --script picmaker.lisp

clean:
	rm -f output.ppm
	rm -f *~
	rm -f *.fasl
