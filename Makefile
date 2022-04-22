.PHONY: doc build clean

doc:
	@echo "Building the documentation"
	@dune build @doc
	@rm -rf docs/*
	@cp -r _build/default/_doc/_html/* docs/
	@echo "Done."


build:
	@echo "Building the sources"
	@dune build
	@echo "Done."

clean:
	@echo "Cleaning"
	@dune clean
	@echo "Done."