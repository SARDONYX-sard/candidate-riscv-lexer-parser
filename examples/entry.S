#include "sample.h"
        .section .text
        .global _start
        # For _start comments 1
        # For _start comments 2
_start:
        la sp, sample0
        li a0, 1024-1
        li a0,(1024-1)
        csrr a1, sscrach
        addi a1, a1, 1
        ecall
        mul a0, a0, a1
        # instraction inner comment
        add sp, sp, a0
        fence.vma
        add fence.vma, a0, 1
        add _entry_e.vma_, a0, 1

spin:
        j spin

string:
        .asciiz "Hello!"
