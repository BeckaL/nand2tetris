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
(Main.fibonacci)
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
//push CONSTANT 2
@2
D=A
@SP
A=M
M=D
@SP
M=M+1
//lt
@SP
A=M-1
D=M
A=A-1
D=M-D
@SP
M=M-1
M=M-1
A=M
M=0
@SET_M_TO_TRUE.0
D;JLT
@AFTER_LOGICAL_COMPARISON.0
0;JMP
(SET_M_TO_TRUE.0)
@SP
A=M
M=-1
(AFTER_LOGICAL_COMPARISON.0)
@SP
M=M+1
@SP
M=M-1
A=M
D=M
@IF_TRUE
D;JNE
@IF_FALSE
0;JMP
(IF_TRUE)
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
(IF_FALSE)
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
//push CONSTANT 2
@2
D=A
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
//push Mainreturn1
@Mainreturn1
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
//goTo function Main.fibonacci
@Main.fibonacci
0;JMP
(Mainreturn1)
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
//push CONSTANT 1
@1
D=A
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
//push Mainreturn2
@Mainreturn2
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
//goTo function Main.fibonacci
@Main.fibonacci
0;JMP
(Mainreturn2)
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
(Sys.init)
//push CONSTANT 4
@4
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
D=A
@ARG
M=D
//repositions LCL
@SP
D=M
@LCL
M=D
//goTo function Main.fibonacci
@Main.fibonacci
0;JMP
(Sysreturn1)
(WHILE)
@WHILE
0;JMP
