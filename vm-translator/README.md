First part of the vm translator - translates arithmetic / logical and memory commands into asm.

As usual, we can assume good inputs, so most bad inputs will result in runtime errors (or possibly weird behaviour).

Really struggled working out how to submit this in scala - many many failed attempts - for posterity, steps I took were:

* Add sbt assembly to plugins
* Add some configuration to build sbt to name jar file
* Run `sbt assembly` to generate the jar file from the source code
* Find generated jar file in target 
* Add empty makefile
* Add VM translator script to run the jar file via `java -jar name-of-jar-file.jar $arg0`

