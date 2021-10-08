test:
  pushq %rbp
  movq %rsp, %rbp
  subq $12, %rsp
  movl %edi, -4(%rbp)
  movl %esi, -8(%rbp)

main:
  pushq %rbp
  movq %rsp, %rbp
  subq $20, %rsp

