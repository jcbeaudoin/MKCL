        .text
        .align 16
        .global MK_GC_save_regs_in_stack
        .proc MK_GC_save_regs_in_stack
MK_GC_save_regs_in_stack:
        .body
        flushrs
        ;;
        mov r8=ar.bsp
        br.ret.sptk.few rp
        .endp MK_GC_save_regs_in_stack
