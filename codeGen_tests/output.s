test:
  pushq %rbp
  movq %rsp, %rbp
  subq $12, %rsp
  movl %edi, -4(%rbp)
  movl %esi, -8(%rbp)
  movl $0 -12(%rbp)

main:
  pushq %rbp
  movq %rsp, %rbp
  subq $48, %rsp
  movl $1 -20(%rbp)
  movl $2 -16(%rbp)
  movl $3 -12(%rbp)
  movl $4 -8(%rbp)
  movl $5 -4(%rbp)
  movl $4234 -48(%rbp)
  movl $42342 -44(%rbp)
  movl $423423 -40(%rbp)
  movl $423423 -36(%rbp)
  movl $423423 -32(%rbp)
  movl $423423 -28(%rbp)
  movl $42343 -24(%rbp)

