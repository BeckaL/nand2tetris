(Sys.init)
//push CONSTANT 4000
@4000
D=A
@SP
A=M
M=D
@SP
M=M+1
//pop POINTER 0
@3
D=A
@R13
M=D
@SP
M=M-1
A=M
D=M
@R13
A=M
M=D
//push CONSTANT 5000
@5000
D=A
@SP
A=M
M=D
@SP
M=M+1
//pop POINTER 1
@4
D=A
@R13
M=D
@SP
M=M-1
A=M
D=M
@R13
A=M
M=D
//push NestedCallreturn1
@NestedCallreturn1
D=A
@SP
A=M
M=D
@SP
M=M+1
//push LCL
@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1
//push ARG
@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1
//push THIS
@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1
//push THAT
@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1
//repositions arg
@SP
A=M
A=A-1
A=A-1
A=A-1
A=A-1
A=A-1
D=A
@ARG
M=D
//repositions LCL
@SP
D=M
@LCL
M=D
//goTo function Sys.main
@Sys.main
0;JMP
(NestedCallreturn1)
//pop TEMP 1
@5
D=A
@1
D=D+A
@R13
M=D
@SP
M=M-1
A=M
D=M
@R13
A=M
M=D
(LOOP)
@LOOP
0;JMP
(Sys.main)
//push CONSTANT 0
@0
D=A
@SP
A=M
M=D
@SP
M=M+1
//push CONSTANT 0
@0
D=A
@SP
A=M
M=D
@SP
M=M+1
//push CONSTANT 0
@0
D=A
@SP
A=M
M=D
@SP
M=M+1
//push CONSTANT 0
@0
D=A
@SP
A=M
M=D
@SP
M=M+1
//push CONSTANT 0
@0
D=A
@SP
A=M
M=D
@SP
M=M+1
//push CONSTANT 4001
@4001
D=A
@SP
A=M
M=D
@SP
M=M+1
//pop POINTER 0
@3
D=A
@R13
M=D
@SP
M=M-1
A=M
D=M
@R13
A=M
M=D
//push CONSTANT 5001
@5001
D=A
@SP
A=M
M=D
@SP
M=M+1
//pop POINTER 1
@4
D=A
@R13
M=D
@SP
M=M-1
A=M
D=M
@R13
A=M
M=D
//push CONSTANT 200
@200
D=A
@SP
A=M
M=D
@SP
M=M+1
//pop LCL 1
@LCL
D=M
@1
D=D+A
@R13
M=D
@SP
M=M-1
A=M
D=M
@R13
A=M
M=D
//push CONSTANT 40
@40
D=A
@SP
A=M
M=D
@SP
M=M+1
//pop LCL 2
@LCL
D=M
@2
D=D+A
@R13
M=D
@SP
M=M-1
A=M
D=M
@R13
A=M
M=D
//push CONSTANT 6
@6
D=A
@SP
A=M
M=D
@SP
M=M+1
//pop LCL 3
@LCL
D=M
@3
D=D+A
@R13
M=D
@SP
M=M-1
A=M
D=M
@R13
A=M
M=D
//push CONSTANT 123
@123
D=A
@SP
A=M
M=D
@SP
M=M+1
//push NestedCallreturn2
@NestedCallreturn2
D=A
@SP
A=M
M=D
@SP
M=M+1
//push LCL
@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1
//push ARG
@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1
//push THIS
@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1
//push THAT
@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1
//repositions arg
@SP
A=M
A=A-1
A=A-1
A=A-1
A=A-1
A=A-1
A=A-1
D=A
@ARG
M=D
//repositions LCL
@SP
D=M
@LCL
M=D
//goTo function Sys.add12
@Sys.add12
0;JMP
(NestedCallreturn2)
//pop TEMP 0
@5
D=A
@0
D=D+A
@R13
M=D
@SP
M=M-1
A=M
D=M
@R13
A=M
M=D
//push LCL 0
@LCL
D=M
@0
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1
//push LCL 1
@LCL
D=M
@1
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1
//push LCL 2
@LCL
D=M
@2
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1
//push LCL 3
@LCL
D=M
@3
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1
//push LCL 4
@LCL
D=M
@4
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1
//add
@SP
A=M-1
D=M
A=A-1
M=D+M
D=A
@SP
M=D+1
//add
@SP
A=M-1
D=M
A=A-1
M=D+M
D=A
@SP
M=D+1
//add
@SP
A=M-1
D=M
A=A-1
M=D+M
D=A
@SP
M=D+1
//add
@SP
A=M-1
D=M
A=A-1
M=D+M
D=A
@SP
M=D+1
//set frame to LCL
@LCL
D=M
@frame
M=D
//set retAddr to frame minus 5
@frame
A=M
A=A-1
A=A-1
A=A-1
A=A-1
A=A-1
D=M
@retAddr
M=D
//reposition return value for caller
@SP
M=M-1
A=M
D=M
@ARG
A=M
M=D
//repositionSpForCaller
@ARG
D=M
D=D+1
@SP
M=D
//set THAT to frame minus 1
@frame
A=M
A=A-1
D=M
@THAT
M=D
//set THIS to frame minus 2
@frame
A=M
A=A-1
A=A-1
D=M
@THIS
M=D
//set ARG to frame minus 3
@frame
A=M
A=A-1
A=A-1
A=A-1
D=M
@ARG
M=D
//set LCL to frame minus 4
@frame
A=M
A=A-1
A=A-1
A=A-1
A=A-1
D=M
@LCL
M=D
//goToRetAddr
@retAddr
A=M
0;JMP
(Sys.add12)
//push CONSTANT 4002
@4002
D=A
@SP
A=M
M=D
@SP
M=M+1
//pop POINTER 0
@3
D=A
@R13
M=D
@SP
M=M-1
A=M
D=M
@R13
A=M
M=D
//push CONSTANT 5002
@5002
D=A
@SP
A=M
M=D
@SP
M=M+1
//pop POINTER 1
@4
D=A
@R13
M=D
@SP
M=M-1
A=M
D=M
@R13
A=M
M=D
//push ARG 0
@ARG
D=M
@0
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1
//push CONSTANT 12
@12
D=A
@SP
A=M
M=D
@SP
M=M+1
//add
@SP
A=M-1
D=M
A=A-1
M=D+M
D=A
@SP
M=D+1
//set frame to LCL
@LCL
D=M
@frame
M=D
//set retAddr to frame minus 5
@frame
A=M
A=A-1
A=A-1
A=A-1
A=A-1
A=A-1
D=M
@retAddr
M=D
//reposition return value for caller
@SP
M=M-1
A=M
D=M
@ARG
A=M
M=D
//repositionSpForCaller
@ARG
D=M
D=D+1
@SP
M=D
//set THAT to frame minus 1
@frame
A=M
A=A-1
D=M
@THAT
M=D
//set THIS to frame minus 2
@frame
A=M
A=A-1
A=A-1
D=M
@THIS
M=D
//set ARG to frame minus 3
@frame
A=M
A=A-1
A=A-1
A=A-1
D=M
@ARG
M=D
//set LCL to frame minus 4
@frame
A=M
A=A-1
A=A-1
A=A-1
A=A-1
D=M
@LCL
M=D
//goToRetAddr
@retAddr
A=M
0;JMP