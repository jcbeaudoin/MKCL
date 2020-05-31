	//
	//   Copyright (c) 2020, Jean-Claude Beaudoin.
	//
	//   This is the AARCH64 assembly code, in GAS syntax,
	//   for the MKCL C to Lisp callback trampoline.
	//
	//   The machine code it produces when processed with:
	//      as -a aarch64_callback_trampoline.s
	//   is incorporated into ffi_aarch64.d after having
	//   been extracted from the assembler listing.
	//
	.arch armv8-a
	.file "aarch64_callback_trampoline.s"
	.text
	.align 2
	.p2align 3,,7
	.type trampoline, %function
trampoline:
	.cfi_startproc
	stp	x29, x30, [sp, -16]!  // x29 is FP (Frame Pointer), x30 is LR (Link Register)
	.cfi_def_cfa_offset 16
	.cfi_offset 29, -16
	.cfi_offset 30, -8
	mov     x29, sp
	str	x29, [sp, -16]!  // push stack_marker
	ldr	x8, =0xdeadbeef12345678  // get cbk_info
	str	x8, [sp, -16]!  // push cbk_info
	ldr	x8, =0xbeefdead87654321 // mkcl_dynamic_callback_execute
	blr	x8
	ldp	x29, x30, [sp], 16
	.cfi_restore 30
	.cfi_restore 29
	.cfi_def_cfa_offset 0
	ret
	.cfi_endproc
	.align	2
	.p2align 3,,7
	.Lptr:
	//.dword -1 // cbk_info
	//.dword -2 // mkcl_dynamic_callback_execute
	.size	trampoline, .-trampoline
	.align	2

	
