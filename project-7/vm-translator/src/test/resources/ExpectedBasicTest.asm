//push CONSTANT 10
@10
D=A
@SP
A=M
M=D
@SP
M=M+1
//pop LCL 0
@LCL
D=M
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
//push CONSTANT 21
@21
D=A
@SP
A=M
M=D
@SP
M=M+1
//push CONSTANT 22
@22
D=A
@SP
A=M
M=D
@SP
M=M+1
//pop ARG 2
@ARG
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
//pop ARG 1
@ARG
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
//push CONSTANT 36
@36
D=A
@SP
A=M
M=D
@SP
M=M+1
//pop THIS 6
@THIS
D=M
@6
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
//push CONSTANT 42
@42
D=A
@SP
A=M
M=D
@SP
M=M+1
//push CONSTANT 45
@45
D=A
@SP
A=M
M=D
@SP
M=M+1
//pop THAT 5
@THAT
D=M
@5
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
//pop THAT 2
@THAT
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
//push CONSTANT 510
@510
D=A
@SP
A=M
M=D
@SP
M=M+1
//pop TEMP 6
@5
D=A
@6
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
//push THAT 5
@THAT
D=M
@5
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
//sub
@SP
A=M-1
D=M
A=A-1
M=M-D
D=A
@SP
M=D+1
//push THIS 6
@THIS
D=M
@6
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1
//push THIS 6
@THIS
D=M
@6
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
//sub
@SP
A=M-1
D=M
A=A-1
M=M-D
D=A
@SP
M=D+1
//push TEMP 6
@5
D=A
@6
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
(END)
@END
0;JMP