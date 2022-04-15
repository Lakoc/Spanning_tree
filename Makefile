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
	./$(TARGET) < tests/input1.txt > tests/output1.txt
	# ./$(TARGET) < tests/input2.txt > tests/output2.txt	
	# ./$(TARGET) < tests/input3.txt > tests/output3.txt		