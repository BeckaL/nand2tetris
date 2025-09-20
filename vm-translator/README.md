First part of the vm translator - translates arithmetic / logical and memory commands into asm.

As usual, we can assume good inputs, so most bad inputs will result in runtime errors (or possibly weird behaviour).

Really struggled working out how to submit this in scala - many many failed attempts - for posterity, steps I took were:

* Add sbt assembly to plugins
* Add some configuration to build sbt to name jar file
* Run `sbt assembly` (note assembl*y* not assmebl*e*) to generate the jar file from the source code
* Find generated jar file in target (target/your-scala-version/submission.jar)
* Add makefile consisting of the following `.DEFAULT none: ;`
* Add VM translator script to run the jar file via `java -jar name-of-jar-file.jar $arg0`
* Add executable permission to the script you just created

To create the zip file:
Zip the folder containing the script, the makefile and the jar, but not the directory itself, with the following
`cd my-folder
zip -r ../intended-name-of-zip.zip .
`

Debugging:
* Unzip the file into a folder, and ensure the three files are top-level
* Ensure the VM translator script has execute permissions
* Run the submission.jar with `java -jar path-to-submission/submission.jar <whatever-command-you-need>` 
