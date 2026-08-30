test:
	pytest

run:
	@if [ -z "$(INPUT)" ]; then \
		echo "variable INPUT was not set!"; \
		exit 1; \
	fi
	@if [ -z "$(OUTPUTS)" ]; then \
		echo "variable OUTPUTS was not set!"; \
		exit 1; \
	fi
	python3 -m analysis $(INPUT) $(OUTPUTS)
