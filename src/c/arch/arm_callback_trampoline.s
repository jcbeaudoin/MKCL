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
	push	{r4, lr}
	mov	r4, sp
	push    {r4}            @ push stack_marker
	ldr	r4, =0xdeadbeef @ get cbk_info
	push    {r4}            @ push cbk_info
	ldr	r4, =0xbeefdead @ get pointer to mkcl_XXX_dynamic_callback_execute
	blx	r4              @ call through r4
	add	sp, sp, #8      @ remove pushed args
	pop	{r4, pc}
	.align	2
	.Lptr:
	@ .word -1 @ mkcl_dynamic_callback_execute
	@ .word -2 @ cbk_info
	.size	trampoline, .-trampoline
	.align	2

	
