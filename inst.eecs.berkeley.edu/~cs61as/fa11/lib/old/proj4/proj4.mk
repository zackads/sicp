proj4 : logo.scm logo-meta.scm
	@if stk -no-tk -file ~cs61a/lib/proj4/run-load.scm > /dev/null;\
	then \
	echo "Your code loaded without any errors.";\
	rm -f loading-errors ;\
echo "\n"; \
	else \
	echo "Your code generated an error while loading."; \
	echo "Please check it to make sure it loads corectly."; \
	echo "All errors generated are in the file loading-errors."; \
	echo "\n"; \
	exit 1;\
	fi
	@if stk -no-tk -file ~cs61a/lib/proj4/run-test.scm > /dev/null ;\
	then \
	echo "" ;\
	else \
	echo "Time limit exceeded while running the tests."; \
	echo "Your code was terminated, most likely due to an infinite loop."; \
	exit 1; \
	fi
	@if diff test-errors /dev/null > /dev/null;\
	then \
	echo "Your code ran all of the tests without any errors.";\
	rm -f test-errors ;\
	echo "\n"; \
	else \
	echo "Your code generated an error while running the tests."; \
	echo "Please further test your project to locate any errors."; \
	echo "All errors generated are in the file test-errors."; \
	echo "\n"; \
	exit 1;\
	fi
	@if diff -i -w -C 5 test-results test-results.ref ;\
	then \
        echo "Your code passed all of the tests."; \
	exit 0; \
	else \
	echo "Your code failed one of the tests."; \
	echo "The above output is a diff of the files test-results and "; \
	echo " test-results.ref. See the README for more info."; \
	exit 1; \
        fi
OK :
	@echo "It worked!!!"
	@echo " "
	@echo "Your submission was sucsesfully loaded and passed our sanity check."
	@echo "Your reader will examin your project soon and will email comments to you."

NOT-OK : 
	@echo "SUBMISSION FAILED!"
	@echo " "
	@echo "Your submission did not load, or did not pass one of our sanity checks."
	@echo " "
	@echo "Please try the following:"
	@echo " "
	@echo " * Make sure that your code.scm file loads sucessfully from a fresh launch of STk."
	@echo " * Make sure that all of your soultions can be run from this fresh load."
	@echo " * Test your solutions to make sure that they work for many different types of input. Think about typical cases as well as boundry cases."
	@echo " * If you do not see any tests listed, then the calling of"; \
	@echo "   one of your procedures may have resulted in an error, rather";\
	@echo "   than an incorrect result. Load your code from scratch and test";\
	@echo "   all of your functions.";\























