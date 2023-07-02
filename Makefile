
.DEFAULT_GOAL := test
CMD = emacs -batch -f package-initialize -L . -f buttercup-run-discover

test:
	$(CMD)

test-coverage:
	rm -rf coverage
	mkdir -p coverage
	UNDERCOVER_FORCE=true $(CMD)
	@./tests/coverage_report.rb
