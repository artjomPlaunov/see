test:
  pushq %rbp
  movq %rsp, %rbp
  subq $12, %rsp
  movl %edi, -4(%rbp)
  movl %esi, -8(%rbp)
  movl $0 -12(%rbp)
  movl -4(%rbp), %eax
  subl -8(%rbp), %eax
  movl %eax, -12(%rbp)
  movl -4(%rbp), %eax
  addl -8(%rbp), %eax
  movl %eax, -12(%rbp)
  movl -4(%rbp), %eax
  imull -8(%rbp), %eax
  movl %eax, -12(%rbp)
  movl -12(%rbp), %eax
  leave
  ret

main:
  pushq %rbp
  movq %rsp, %rbp
  subq $20, %rsp
  movl $1 -20(%rbp)
  movl $2 -16(%rbp)
  movl $3 -12(%rbp)
  movl $4 -8(%rbp)
  movl $5 -4(%rbp)
  movl -20(%rbp) edi
  movl $100 esi
  call test
  movl $0, %eax
  leave
  ret

