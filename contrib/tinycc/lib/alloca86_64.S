/* ---------------------------------------------- */
/* alloca86_64.S */

.globl __mkcc_alloca

__mkcc_alloca:
    pop     %rdx
#ifdef MKCC_TARGET_PE
    mov     %rcx,%rax
#else
    mov     %rdi,%rax
#endif
    add     $15,%rax
    and     $-16,%rax
    jz      3f

#ifdef MKCC_TARGET_PE
1:
    cmp     $4096,%rax
    jbe     2f
    test    %rax,-4096(%rsp)
    sub     $4096,%rsp
    sub     $4096,%rax
    jmp 1b
2:
#endif

    sub     %rax,%rsp
    mov     %rsp,%rax
#ifdef MKCC_TARGET_PE
    add     $32,%rax
#endif

3:
    push    %rdx
    ret

/* mark stack as nonexecutable */
#if defined __ELF__ && defined __linux__
    .section    .note.GNU-stack,"",@progbits
#endif
/* ---------------------------------------------- */
