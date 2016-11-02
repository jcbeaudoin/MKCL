/* ---------------------------------------------- */
/* alloca86_64.S */

.globl __mkcc_bound_alloca
__mkcc_bound_alloca:

#ifdef MKCC_TARGET_PE
    # bound checking is not implemented
    pop     %rdx
    mov     %rcx,%rax
    add     $15,%rax
    and     $-16,%rax
    jz      3f

1:
    cmp     $4096,%rax
    jbe     2f
    test    %rax,-4096(%rsp)
    sub     $4096,%rsp
    sub     $4096,%rax
    jmp 1b
2:

    sub     %rax,%rsp
    mov     %rsp,%rax
    add     $32,%rax

3:
    push    %rdx
    ret
#else
    pop     %rdx
    mov     %rdi,%rax
    movl    %rax,%rsi	# size, a second parm to the __mkcc_bound_new_region

    add     $15,%rax
    and     $-16,%rax
    jz      3f


    sub     %rax,%rsp
    mov     %rsp,%rdi	# pointer, a first parm to the __mkcc_bound_new_region
    mov     %rsp,%rax

    push    %rdx
    push    %rax
#if 0
        call   __mkcc_bound_new_region
#endif
    pop     %rax
    pop     %rdx

3:
    push    %rdx
    ret
#endif

/* mark stack as nonexecutable */
#if defined __ELF__ && defined __linux__
    .section    .note.GNU-stack,"",@progbits
#endif
/* ---------------------------------------------- */
