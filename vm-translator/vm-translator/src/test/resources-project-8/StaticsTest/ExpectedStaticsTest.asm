@256
D=A
@SP
M=D
//push bootstrapreturn0
@bootstrapreturn0
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
//goTo function Sys.init
@Sys.init
0;JMP
(bootstrapreturn0)
(Class1.set)
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
//pop STATIC 0
@SP
M=M-1
A=M
D=M
@Class1.0
M=D
//push ARG 1
@ARG
D=M
@1
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1
//pop STATIC 1
@SP
M=M-1
A=M
D=M
@Class1.1
M=D
//push CONSTANT 0
@0
D=A
@SP
A=M
M=D
@SP
M=M+1
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
(Class1.get)
//push STATIC 0
@Class1.0
D=M
@SP
A=M
M=D
@SP
M=M+1
//push STATIC 1
@Class1.1
D=M
@SP
A=M
M=D
@SP
M=M+1
//sub
@SP
A=M-1
D=M
A=A-1
M=M-D
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
(Sys.init)
//push CONSTANT 6
@6
D=A
@SP
A=M
M=D
@SP
M=M+1
//push CONSTANT 8
@8
D=A
@SP
A=M
M=D
@SP
M=M+1
//push Sysreturn1
@Sysreturn1
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
A=A-1
D=A
@ARG
M=D
//repositions LCL
@SP
D=M
@LCL
M=D
//goTo function Class1.set
@Class1.set
0;JMP
(Sysreturn1)
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
//push CONSTANT 23
@23
D=A
@SP
A=M
M=D
@SP
M=M+1
//push CONSTANT 15
@15
D=A
@SP
A=M
M=D
@SP
M=M+1
//push Sysreturn2
@Sysreturn2
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
A=A-1
D=A
@ARG
M=D
//repositions LCL
@SP
D=M
@LCL
M=D
//goTo function Class2.set
@Class2.set
0;JMP
(Sysreturn2)
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
//push Sysreturn3
@Sysreturn3
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
//goTo function Class1.get
@Class1.get
0;JMP
(Sysreturn3)
//push Sysreturn4
@Sysreturn4
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
//goTo function Class2.get
@Class2.get
0;JMP
(Sysreturn4)
(WHILE)
@WHILE
0;JMP
(Class2.set)
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
//pop STATIC 0
@SP
M=M-1
A=M
D=M
@Class2.0
M=D
//push ARG 1
@ARG
D=M
@1
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1
//pop STATIC 1
@SP
M=M-1
A=M
D=M
@Class2.1
M=D
//push CONSTANT 0
@0
D=A
@SP
A=M
M=D
@SP
M=M+1
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
(Class2.get)
//push STATIC 0
@Class2.0
D=M
@SP
A=M
M=D
@SP
M=M+1
//push STATIC 1
@Class2.1
D=M
@SP
A=M
M=D
@SP
M=M+1
//sub
@SP
A=M-1
D=M
A=A-1
M=M-D
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
