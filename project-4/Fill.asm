// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

// Put your code here.

@8192
D = A
@column_max
M = D

//THIS IS PRETTY INEFFICIENT, RECALCULATES EVERY TIME IT LOOPS
//REFACTOR WHITE AND BLACK LOOPS TO BE THE SAME

(START)
    @SCREEN  // get address of screen and store in D
    D = A
    @address //set address to address of screen to start with
    M = D
    //JUMP TO FILL SCREEN WHITE IF KEYBOARD INPUT IS 0 (NO KEY PRESSED)
    @KBD
    D = M
    @WHITE
    D; JEQ

    //FILL SCREEN BLACK
    @0
    D = A
    @column_counter //initialize a counter to 0
    M = D

    (LOOP)
    // if column counter is at 16, finish
    @column_counter
    D = M
    @column_max
    D = D - M
    @START // Go back to start if column counter is at column max
    D; JEQ

    @address // Set address held in variable to -1 (all black pixels)
    A = M
    M = -1

    @address //Increment address by 1
    M = M + 1

    @column_counter // increment column counter
    M = M + 1
    @LOOP // go back to loop start
    0; JMP

    (WHITE)
    @0
    D = A
    @column_counter //initialize a counter to 0
    M = D
    @SCREEN  // get address of screen and store in D
    D = A
    @address //set address to address of screen to start with
    M = D

    (LOOP_WHITE)
    // if column counter is at 16, finish
    @column_counter
    D = M
    @column_max
    D = D - M
    @START // Go back to start if column counter is at column max
    D; JEQ

    @address // Set address held in variable to 0 (all white pixels)
    A = M
    M = 0

    @address //Increment address by 1
    M = M + 1

    @column_counter // increment column counter
    M = M + 1
    @LOOP_WHITE // go back to loop start
    0; JMP


