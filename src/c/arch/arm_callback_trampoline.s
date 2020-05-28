	.arch armv7-a
	.file "arm_callback_trampoline.s"


	.text
	.align 1
	.p2align 2,,3
	.arch armv7-a
	.syntax unified
	.thumb
	.thumb_func
	.fpu vfpv3-d16
	.type trampoline, %function
trampoline:
	mov	r3, pc
	add	r3, #24
	push	{r4, lr}
	ldr	r2, [r3]
	ldr	r0, [r3, #4]
	mov	r1, sp
	blx	r2
	pop	{r4, pc}
	nop
	nop
	.align	2
	.Lptr:
	.word -1 @ mkcl_dynamic_callback_execute
	.word -2 @ cbk_info
	.size	trampoline, .-trampoline
	.align	2

	
