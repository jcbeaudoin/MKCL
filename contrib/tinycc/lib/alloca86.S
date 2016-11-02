/* ---------------------------------------------- */
/* alloca86.S */

.globl __mkcc_alloca

__mkcc_alloca:
    pop     %edx
    pop     %eax
    add     $3,%eax
    and     $-4,%eax
    jz      3f

#ifdef MKCC_TARGET_PE
1:
    cmp     $4096,%eax
    jbe     2f
    test    %eax,-4096(%esp)
    sub     $4096,%esp
    sub     $4096,%eax
    jmp 1b
2:
#endif

    sub     %eax,%esp
    mov     %esp,%eax
3:
    push    %edx
    push    %edx
    ret

/* mark stack as nonexecutable */
#if defined __ELF__ && defined __linux__
    .section    .note.GNU-stack,"",@progbits
#endif
/* ---------------------------------------------- */
