bsfast.so: bsfast.pyx setup.py
	python setup.py build_ext --inplace

clean:
	python setup.py clean
