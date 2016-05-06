.cstring
LC0:
    .ascii "Hello world!\12\0"
    .text
    .align 4,0x90
.globl _main
_main:
    pushl   %ebp
    movl    %esp, %ebp
    subl    $24, %esp
    movl    $LC0, (%esp)
    call    _puts
    xorl    %eax, %eax
    leave
    ret
    .subsections_via_symbols
