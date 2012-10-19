	.file	"gcd.c"
	.section	.rodata
.LC1:
	.string	"%g\n"
	.text
	.globl	main
	.type	main, @function
main:
.LFB0:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$48, %rsp
	movl	%edi, -20(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	addq	$8, %rax
	movq	(%rax), %rax
	movq	%rax, %rdi
	movl	$0, %eax
	call	atoi
	cvtsi2sd	%eax, %xmm0
	movsd	%xmm0, -8(%rbp)
	movq	-32(%rbp), %rax
	addq	$16, %rax
	movq	(%rax), %rax
	movq	%rax, %rdi
	movl	$0, %eax
	call	atoi
	cvtsi2sd	%eax, %xmm0
	movsd	%xmm0, -16(%rbp)
	xorpd	%xmm0, %xmm0
	ucomisd	-8(%rbp), %xmm0
	jp	.L4
	xorpd	%xmm0, %xmm0
	ucomisd	-8(%rbp), %xmm0
	jne	.L11
.L9:
	movq	-16(%rbp), %rax
	movq	%rax, -40(%rbp)
	movsd	-40(%rbp), %xmm0
	movl	$.LC1, %edi
	movl	$1, %eax
	call	printf
	jmp	.L4
.L11:
	jmp	.L4
.L7:
	movsd	-8(%rbp), %xmm0
	ucomisd	-16(%rbp), %xmm0
	jbe	.L12
.L10:
	movsd	-8(%rbp), %xmm0
	subsd	-16(%rbp), %xmm0
	movsd	%xmm0, -8(%rbp)
	jmp	.L4
.L12:
	movsd	-16(%rbp), %xmm0
	subsd	-8(%rbp), %xmm0
	movsd	%xmm0, -16(%rbp)
.L4:
	xorpd	%xmm0, %xmm0
	ucomisd	-16(%rbp), %xmm0
	jp	.L7
	xorpd	%xmm0, %xmm0
	ucomisd	-16(%rbp), %xmm0
	jne	.L7
	movq	-8(%rbp), %rax
	movq	%rax, -40(%rbp)
	movsd	-40(%rbp), %xmm0
	movl	$.LC1, %edi
	movl	$1, %eax
	call	printf
	movl	$0, %eax
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE0:
	.size	main, .-main
	.ident	"GCC: (GNU) 4.7.0 20120414 (prerelease)"
	.section	.note.GNU-stack,"",@progbits
