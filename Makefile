TARGET := flp21-log
ARCHIVE := flp-log-xpolok03
SRC := spanning_tree.pl

.PHONY: build
build: $(TARGET)

$(TARGET): spanning_tree.pl
	swipl --goal=main --stand_alone=true -o $(TARGET) -c $(SRC)
	

.PHONY: arch
arch: $(ARCHIVE).zip

$(ARCHIVE).zip: Makefile README.md  $(SRC) tests
	zip -r $@ $^

.PHONY: clean
clean:
	rm -f $(ARCHIVE).zip $(TARGET)

.PHONY: run
run: $(TARGET)
	./$(TARGET) < tests/ref.in

.PHONY: test
test: $(TARGET)
	python3 tests/run_tests.py $(TARGET) tests