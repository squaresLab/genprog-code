

all:	LineNumbers.class test/Test.class

LineNumbers.class: LineNumbers.java LineNumberTransformer.java
	javac -cp lib/sootclasses-2.4.0.jar LineNumbers.java LineNumberTransformer.java

test/Test.class: test/Test.java
	javac test/Test.java

run:
	java -cp ./:lib/sootclasses-2.4.0.jar LineNumbers test

clean:
	rm -f *.class test/*.class
