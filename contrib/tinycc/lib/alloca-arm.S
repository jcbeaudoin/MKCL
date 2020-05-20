	.text
	.align	2
	.global	__mkcc_alloca
	.type	__mkcc_alloca, %function
__mkcc_alloca:
	rsb	sp, r0, sp
	bic	sp, sp, #7
	mov	r0, sp
	mov	pc, lr
	.size	__mkcc_alloca, .-__mkcc_alloca
	.section	.note.GNU-stack,"",%progbits
