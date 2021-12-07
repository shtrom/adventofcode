PYTHON=python3

DAYS=$(patsubst %.in,%,$(wildcard day*.in))

run: $(addprefix run-,$(DAYS))
test: test-utils $(addprefix test-,$(DAYS))
clean:
	rm -rf *.pyc __pycache__
	rm -f $(addsuffix, %.py,$(DAYS))

test-utils: test-AoCUtils


# Python
run-day%: day%.py day%.in
	$(PYTHON) $(<) < day$(*).in
test-%: %.py
	grep -qv '>>>' $(^) \
		|| $(PYTHON) -m doctest $(^)

# Haskell
sample-day%: day%.hs sample%.in
	runghc $(<) < sample$(*).in
run-day%: day%.hs day%.in
	runghc $(<) < day$(*).in
# cabal install --overwrite-policy=always doctest
test-%: %.hs
	doctest $(<)

.PHONY: run run-day* test
