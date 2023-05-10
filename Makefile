test:
	emacs -batch -f package-initialize -L . -f buttercup-run-discover

test-coverage:
	rm -rf coverage
	mkdir -p coverage
	UNDERCOVER_FORCE=true emacs -batch -L . -f package-initialize -f buttercup-run-discover
	@./tests/coverage_report.rb
