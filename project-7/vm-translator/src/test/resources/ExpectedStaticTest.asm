//push CONSTANT 111
@111
D=A
@SP
A=M
M=D
@SP
M=M+1
//push CONSTANT 333
@333
D=A
@SP
A=M
M=D
@SP
M=M+1
//push CONSTANT 888
@888
D=A
@SP
A=M
M=D
@SP
M=M+1
//pop STATIC 8
@SP
M=M-1
A=M
D=M
@StaticTest.8
M=D
//pop STATIC 3
@SP
M=M-1
A=M
D=M
@StaticTest.3
M=D
//pop STATIC 1
@SP
M=M-1
A=M
D=M
@StaticTest.1
M=D
//push STATIC 3
@StaticTest.3
D=M
@SP
A=M
M=D
@SP
M=M+1
//push STATIC 1
@StaticTest.1
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
//push STATIC 8
@StaticTest.8
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
(END)
@END
0;JMP