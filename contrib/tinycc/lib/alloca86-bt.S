/* ---------------------------------------------- */
/* alloca86-bt.S */

.globl __mkcc_bound_alloca

__mkcc_bound_alloca:
    pop     %edx
    pop     %eax
    mov     %eax, %ecx
    add     $3,%eax
    and     $-4,%eax
    jz      6f

#ifdef MKCC_TARGET_PE
4:
    cmp     $4096,%eax
    jbe     5f
    test    %eax,-4096(%esp)
    sub     $4096,%esp
    sub     $4096,%eax
    jmp 4b

5:
#endif

    sub     %eax,%esp
    mov     %esp,%eax

    push    %edx
    push    %eax
    push    %ecx
    push    %eax
#if 0
    call   __mkcc_bound_new_region
#endif
    add    $8, %esp
    pop     %eax
    pop     %edx

6:
    push    %edx
    push    %edx
    ret

/* mark stack as nonexecutable */
#if defined __ELF__ && defined __linux__
    .section    .note.GNU-stack,"",@progbits
#endif
/* ---------------------------------------------- */
