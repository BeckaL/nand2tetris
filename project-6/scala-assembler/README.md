This implements an assembler for the hack computer, as part of the [Nand2Tetris course](https://www.nand2tetris.org/), in scala 3 built using sbt.


### Usage

[Install scala 3 and sbt](https://www.scala-lang.org/download/) and set up a directory, containing 'in' and 'out' subdirectories.

Put input asm files in the 'in' directory, and use the programme to output hack binary code files to the 'out' directory.

Use as follows:

`sbt run $relative_path_to_directory, $name_of_asm_file_without_extension`

for example:

`sbt run "./src/resources" "Rect"`

to assemble the following file: `./src/resources/in/Rect.asm` and store the output in `./src/resources/out/Rect.hack`

As per the course, we assume the inputs are correct, so there's minimal error handling, and most syntax errors in the 
asm file will result in runtime errors.

### Running the tests and refactoring

To run the tests:

`sbt test`

An integration test ensures that the test files (the same asm files as given in the course) still 
output the correct result (as determined by submitting the expected files and passing the week). So when refactoring / modifying, 
this test should be run via

`sbt testOnly *IntegrationTest`

to ensure the programme still outputs the correct binary code.