proj2 : picture.scm 
	@if stk -no-tk -file ~cs61a/lib/proj2/run-load.scm > /dev/null;\
	then \
	echo "Your code loaded without any errors.";\
	else \
	echo "Your code generated an error while loading."; \
	echo "Please check it to make sure it loads corectly."; \
	echo "\n"; \
	exit 1;\
	fi
	@if stk -no-tk -file ~cs61a/lib/proj2/run-check.scm > /dev/null;\
	then \
	echo "All required definitions present.";\
	else \
	echo "The following were not defined: \n"; \
	cat not-defined; \
	echo "\n"; \
	exit 1;\
	fi
	@if stk -no-tk -file ~cs61a/lib/proj2/run-test.scm > /dev/null;\
	then \
	echo "All tests were passed."; \
	exit 0;\
	else \
	echo "One or more of the following tests failed: \n"; \
	cat tests-failed; \
	exit 1; \
	fi 

OK :
	@echo "It worked!!!"
	@echo " "
	@echo "Your project was sucsesfully loaded and passed our sanity check."

NOT-OK : 
	@echo "PROJECT FAILED!"
	@echo " "
	@echo "Your project did not load, or did not pass one of our sanity checks."
	@echo " "
	@echo "Please try the following:"
	@echo " "
	@echo " * Make sure that your picture.scm file loads sucessfully from a fresh launch of STk."
	@echo " * Make sure that all of your soultions can be run from this fresh load."
	@echo " * Test your solutions to make sure that they work for many different types of input. Think about typical cases as well as boundry cases."
	@echo " * If you do not see any tests listed, then the calling of"; \
	@echo "   one of your procedures may have resulted in an error, rather";\
	@echo "   than an incorrect result. Load your code from scratch and test";\
	@echo "   all of your functions.";\













