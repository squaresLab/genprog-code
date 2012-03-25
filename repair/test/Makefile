#gcd-test:
#	cd ../gcd-test && ../repair configuration

test: *.test
	for test in *.test ;do \
		./$$test &> /dev/null ; \
		if [[ "$$?" -eq "0" ]];then result=PASS; \
		else result=FAIL;fi; \
		echo "=============== $$result $$test"; \
	done
