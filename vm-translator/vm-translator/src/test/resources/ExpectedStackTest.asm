//push CONSTANT 17
@17
D=A
@SP
A=M
M=D
@SP
M=M+1
//push CONSTANT 17
@17
D=A
@SP
A=M
M=D
@SP
M=M+1
//eq
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
D;JEQ
@AFTER_LOGICAL_COMPARISON.0
0;JMP
(SET_M_TO_TRUE.0)
@SP
A=M
M=-1
(AFTER_LOGICAL_COMPARISON.0)
@SP
M=M+1
//push CONSTANT 17
@17
D=A
@SP
A=M
M=D
@SP
M=M+1
//push CONSTANT 16
@16
D=A
@SP
A=M
M=D
@SP
M=M+1
//eq
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
@SET_M_TO_TRUE.1
D;JEQ
@AFTER_LOGICAL_COMPARISON.1
0;JMP
(SET_M_TO_TRUE.1)
@SP
A=M
M=-1
(AFTER_LOGICAL_COMPARISON.1)
@SP
M=M+1
//push CONSTANT 16
@16
D=A
@SP
A=M
M=D
@SP
M=M+1
//push CONSTANT 17
@17
D=A
@SP
A=M
M=D
@SP
M=M+1
//eq
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
@SET_M_TO_TRUE.2
D;JEQ
@AFTER_LOGICAL_COMPARISON.2
0;JMP
(SET_M_TO_TRUE.2)
@SP
A=M
M=-1
(AFTER_LOGICAL_COMPARISON.2)
@SP
M=M+1
//push CONSTANT 892
@892
D=A
@SP
A=M
M=D
@SP
M=M+1
//push CONSTANT 891
@891
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
@SET_M_TO_TRUE.3
D;JLT
@AFTER_LOGICAL_COMPARISON.3
0;JMP
(SET_M_TO_TRUE.3)
@SP
A=M
M=-1
(AFTER_LOGICAL_COMPARISON.3)
@SP
M=M+1
//push CONSTANT 891
@891
D=A
@SP
A=M
M=D
@SP
M=M+1
//push CONSTANT 892
@892
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
@SET_M_TO_TRUE.4
D;JLT
@AFTER_LOGICAL_COMPARISON.4
0;JMP
(SET_M_TO_TRUE.4)
@SP
A=M
M=-1
(AFTER_LOGICAL_COMPARISON.4)
@SP
M=M+1
//push CONSTANT 891
@891
D=A
@SP
A=M
M=D
@SP
M=M+1
//push CONSTANT 891
@891
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
@SET_M_TO_TRUE.5
D;JLT
@AFTER_LOGICAL_COMPARISON.5
0;JMP
(SET_M_TO_TRUE.5)
@SP
A=M
M=-1
(AFTER_LOGICAL_COMPARISON.5)
@SP
M=M+1
//push CONSTANT 32767
@32767
D=A
@SP
A=M
M=D
@SP
M=M+1
//push CONSTANT 32766
@32766
D=A
@SP
A=M
M=D
@SP
M=M+1
//gt
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
@SET_M_TO_TRUE.6
D;JGT
@AFTER_LOGICAL_COMPARISON.6
0;JMP
(SET_M_TO_TRUE.6)
@SP
A=M
M=-1
(AFTER_LOGICAL_COMPARISON.6)
@SP
M=M+1
//push CONSTANT 32766
@32766
D=A
@SP
A=M
M=D
@SP
M=M+1
//push CONSTANT 32767
@32767
D=A
@SP
A=M
M=D
@SP
M=M+1
//gt
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
@SET_M_TO_TRUE.7
D;JGT
@AFTER_LOGICAL_COMPARISON.7
0;JMP
(SET_M_TO_TRUE.7)
@SP
A=M
M=-1
(AFTER_LOGICAL_COMPARISON.7)
@SP
M=M+1
//push CONSTANT 32766
@32766
D=A
@SP
A=M
M=D
@SP
M=M+1
//push CONSTANT 32766
@32766
D=A
@SP
A=M
M=D
@SP
M=M+1
//gt
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
@SET_M_TO_TRUE.8
D;JGT
@AFTER_LOGICAL_COMPARISON.8
0;JMP
(SET_M_TO_TRUE.8)
@SP
A=M
M=-1
(AFTER_LOGICAL_COMPARISON.8)
@SP
M=M+1
//push CONSTANT 57
@57
D=A
@SP
A=M
M=D
@SP
M=M+1
//push CONSTANT 31
@31
D=A
@SP
A=M
M=D
@SP
M=M+1
//push CONSTANT 53
@53
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
//push CONSTANT 112
@112
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
//neg
@SP
A=M-1
M=-M
D=A
@SP
M=D+1
//and
@SP
A=M-1
D=M
A=A-1
M=M&D
D=A
@SP
M=D+1
//push CONSTANT 82
@82
D=A
@SP
A=M
M=D
@SP
M=M+1
//or
@SP
A=M-1
D=M
A=A-1
M=D|M
D=A
@SP
M=D+1
//not
@SP
A=M-1
M=!M
D=A
@SP
M=D+1
(END)
@END
0;JMP