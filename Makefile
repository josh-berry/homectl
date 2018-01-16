#
# Developer makefile for testing purposes; you can ignore this unless you want
# to run tests yourself.
#

PY := venv/bin/python
PIP := venv/bin/pip
COV := venv/bin/coverage

test check: test-py3 test-py2
.PHONY: test check

test-py3: $(PY) $(COV)
	$(COV) run --branch ./test.py
	$(COV) html
.PHONY: test-py3

test-py2:
	python2.7 ./test.py
.PHONY: test-py2

$(COV): $(PY) $(PIP)
	$(PIP) install coverage

$(PIP) $(PY):
	python3 -m venv venv

clean:
	tr '\n' '\0' < .gitignore |xargs -t0 rm -rf
.PHONY: clean
