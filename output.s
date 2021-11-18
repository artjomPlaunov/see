main:
  pushq %rbp
  movq %rsp, %rbp
  subq $36, %rsp
  movl $5 -12(%rbp)
  movl $6 -8(%rbp)
  movl $7 -4(%rbp)
  movl $1 -16(%rbp)
  movl $-1 -20(%rbp)
  movl -16(%rbp) %eax
  movl %eax -24(%rbp)
  movl -12(%rbp), %eax
  cmpl 0, %eax
  jg .L0
  jmp .L1
.L0:
.L1:
  movl $0, -32(%rbp)
  jmp L2
L3:
  movl -32(%rbp), %eax
  addl 5, %eax
  movl %eax, -28(%rbp)
  movl -32(%rbp), %eax
  addl 1, %eax
  movl %eax, -32(%rbp)
L2:
  cmpl $2, -32(%rbp)
  jle L3
  movl $0, %eax
  leave
  ret

