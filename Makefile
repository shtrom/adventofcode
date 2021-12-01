PYTHON=python3

DAYS=$(patsubst %.in,%,$(wildcard *.in))

run: $(addprefix run-,$(DAYS))
test: $(addprefix test-,$(DAYS))
clean:
	rm -rf *.pyc __pycache__
	rm -f $(addsuffix, %.py,$(DAYS))

# Python
run-day%: day%.py day%.in
	$(PYTHON) $(^) < day$(*).in
test-day%: day%.py
	grep -qv '>>>' $(^) \
		|| $(PYTHON) -m doctest $(^)

# Haskell
run-day%: day%.hs day%.in
	runghc $(^) < day$(*).in
# cabal install --overwrite-policy=always doctest
test-day%: day%.hs
	doctest $(^)

.PHONY: run run-day* test
