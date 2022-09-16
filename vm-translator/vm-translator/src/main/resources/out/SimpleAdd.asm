//push CONSTANT 7
@7
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
//add
@SP
A=M-1
D=M
A=A-1
M=D+M
D=A
@SP
M=D+1