#include "sample.h"
.section .text
.global _start
_start:
        # comments
        # comments
        la sp, sample0
        li a0, 1024-1
        csrr a1, sscrach
        addi a1, a1, 1
        ecall
        mul a0, a0, a1
        add sp, sp, a0
        # comments

spin:
        j spin
