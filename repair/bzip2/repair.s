	.file	"repair.sanity.c"
	.text
	.p2align 4,,15
.globl BZ2_hbAssignCodes
	.type	BZ2_hbAssignCodes, @function
BZ2_hbAssignCodes:
.LFB15:
	xorl	%r11d, %r11d
	cmpl	%ecx, %edx
	jg	.L14
	.p2align 4,,7
.L4:
	xorl	%r10d, %r10d
	xorl	%r9d, %r9d
	testl	%r8d, %r8d
	jg	.L6
	jmp	.L9
	.p2align 4,,7
.L7:
	addl	$1, %r10d
	addq	$1, %r9
	cmpl	%r8d, %r10d
	je	.L9
.L6:
	movzbl	(%r9,%rsi), %eax
	cmpl	%edx, %eax
	jne	.L7
	addl	$1, %r10d
	movl	%r11d, (%rdi,%r9,4)
	addl	$1, %r11d
	addq	$1, %r9
	cmpl	%r8d, %r10d
	jne	.L6
	.p2align 4,,7
.L9:
	addl	$1, %edx
	cmpl	%edx, %ecx
	jl	.L11
	addl	%r11d, %r11d
	jmp	.L4
.L11:
	rep ; ret
.L14:
	rep ; ret
.LFE15:
	.size	BZ2_hbAssignCodes, .-BZ2_hbAssignCodes
	.p2align 4,,15
.globl BZ2_hbCreateDecodeTables
	.type	BZ2_hbCreateDecodeTables, @function
BZ2_hbCreateDecodeTables:
.LFB16:
	pushq	%r12
.LCFI0:
	movq	%rcx, %r12
	xorl	%ecx, %ecx
	cmpl	%r9d, %r8d
	pushq	%rbp
.LCFI1:
	movq	%rdi, %rbp
	movq	%rsi, %rdi
	pushq	%rbx
.LCFI2:
	movq	%rdx, %rbx
	movl	32(%rsp), %esi
	movl	%r8d, %edx
	jg	.L16
	.p2align 4,,7
.L18:
	xorl	%r10d, %r10d
	testl	%esi, %esi
	movq	%r12, %r11
	jg	.L19
	jmp	.L22
	.p2align 4,,7
.L20:
	addl	$1, %r10d
	addq	$1, %r11
	cmpl	%esi, %r10d
	je	.L22
.L19:
	movzbl	(%r11), %eax
	cmpl	%edx, %eax
	jne	.L20
	movslq	%ecx,%rax
	addq	$1, %r11
	addl	$1, %ecx
	movl	%r10d, (%rbx,%rax,4)
	addl	$1, %r10d
	cmpl	%esi, %r10d
	jne	.L19
	.p2align 4,,7
.L22:
	addl	$1, %edx
	cmpl	%edx, %r9d
	jge	.L18
.L16:
	xorl	%eax, %eax
	.p2align 4,,7
.L24:
	movl	$0, (%rdi,%rax,4)
	addq	$1, %rax
	cmpq	$23, %rax
	jne	.L24
	testl	%esi, %esi
	leaq	4(%rdi), %r10
	jle	.L28
	leaq	4(%rdi), %r10
	movq	%r12, %rcx
	xorl	%edx, %edx
	.p2align 4,,7
.L29:
	movzbl	(%rcx), %eax
	addl	$1, %edx
	addq	$1, %rcx
	addl	$1, (%r10,%rax,4)
	cmpl	%esi, %edx
	jne	.L29
.L28:
	xorl	%edx, %edx
	.p2align 4,,7
.L30:
	movl	(%rdi,%rdx,4), %eax
	addl	%eax, 4(%rdi,%rdx,4)
	addq	$1, %rdx
	cmpq	$22, %rdx
	jne	.L30
	xorl	%eax, %eax
	.p2align 4,,7
.L32:
	movl	$0, (%rbp,%rax,4)
	addq	$1, %rax
	cmpq	$23, %rax
	jne	.L32
	cmpl	%r9d, %r8d
	jg	.L34
	movslq	%r8d,%rax
	movl	%r8d, %ecx
	xorl	%edx, %edx
	salq	$2, %rax
	leaq	(%r10,%rax), %rsi
	leaq	(%rdi,%rax), %r11
	leaq	(%rbp,%rax), %r10
	.p2align 4,,7
.L36:
	movl	(%rsi), %eax
	subl	(%r11), %eax
	addl	$1, %ecx
	addq	$4, %rsi
	addq	$4, %r11
	addl	%edx, %eax
	leal	-1(%rax), %edx
	movl	%edx, (%r10)
	addq	$4, %r10
	cmpl	%ecx, %r9d
	leal	(%rax,%rax), %edx
	jge	.L36
.L34:
	addl	$1, %r8d
	cmpl	%r8d, %r9d
	jl	.L40
	movslq	%r8d,%rax
	salq	$2, %rax
	leaq	(%rax,%rbp), %rcx
	leaq	(%rdi,%rax), %rdx
	.p2align 4,,7
.L39:
	movl	-4(%rcx), %eax
	addl	$1, %r8d
	addq	$4, %rcx
	addl	$1, %eax
	addl	%eax, %eax
	subl	(%rdx), %eax
	movl	%eax, (%rdx)
	addq	$4, %rdx
	cmpl	%r8d, %r9d
	jge	.L39
.L40:
	popq	%rbx
	popq	%rbp
	popq	%r12
	ret
.LFE16:
	.size	BZ2_hbCreateDecodeTables, .-BZ2_hbCreateDecodeTables
	.p2align 4,,15
.globl BZ2_bsInitWrite
	.type	BZ2_bsInitWrite, @function
BZ2_bsInitWrite:
.LFB17:
	movl	$0, 644(%rdi)
	movl	$0, 640(%rdi)
	ret
.LFE17:
	.size	BZ2_bsInitWrite, .-BZ2_bsInitWrite
	.p2align 4,,15
	.type	bz_config_ok, @function
bz_config_ok:
.LFB29:
	movl	$1, %eax
	ret
.LFE29:
	.size	bz_config_ok, .-bz_config_ok
	.p2align 4,,15
	.type	prepare_new_block, @function
prepare_new_block:
.LFB32:
	movl	$0, 108(%rdi)
	movl	$0, 116(%rdi)
	xorl	%eax, %eax
	movl	$0, 120(%rdi)
	movl	$-1, 648(%rdi)
	.p2align 4,,7
.L56:
	movb	$0, 128(%rax,%rdi)
	addq	$1, %rax
	cmpq	$256, %rax
	jne	.L56
	addl	$1, 660(%rdi)
	ret
.LFE32:
	.size	prepare_new_block, .-prepare_new_block
	.p2align 4,,15
	.type	init_RL, @function
init_RL:
.LFB33:
	movl	$256, 92(%rdi)
	movl	$0, 96(%rdi)
	ret
.LFE33:
	.size	init_RL, .-init_RL
	.p2align 4,,15
	.type	isempty_RL, @function
isempty_RL:
.LFB34:
	cmpl	$255, 92(%rdi)
	movl	$1, %eax
	ja	.L66
	xorl	%eax, %eax
	cmpl	$0, 96(%rdi)
	setle	%al
.L66:
	rep ; ret
.LFE34:
	.size	isempty_RL, .-isempty_RL
	.p2align 4,,15
.globl BZ2_bzCompressInit
	.type	BZ2_bzCompressInit, @function
BZ2_bzCompressInit:
.LFB35:
	movq	%rbx, -48(%rsp)
.LCFI3:
	movq	%rbp, -40(%rsp)
.LCFI4:
	movq	%rdi, %rbx
	movq	%r12, -32(%rsp)
.LCFI5:
	movq	%r13, -24(%rsp)
.LCFI6:
	movl	%esi, %ebp
	movq	%r14, -16(%rsp)
.LCFI7:
	movq	%r15, -8(%rsp)
.LCFI8:
	subq	$56, %rsp
.LCFI9:
	movl	%edx, %r13d
	movl	%ecx, %r12d
	call	bz_config_ok
	testl	%eax, %eax
	movl	$-9, %edx
	jne	.L96
.L71:
	movq	8(%rsp), %rbx
	movq	16(%rsp), %rbp
	movl	%edx, %eax
	movq	24(%rsp), %r12
	movq	32(%rsp), %r13
	movq	40(%rsp), %r14
	movq	48(%rsp), %r15
	addq	$56, %rsp
	ret
.L96:
	testq	%rbx, %rbx
	jne	.L97
.L72:
	movl	$-2, %edx
	jmp	.L71
.L97:
	testl	%ebp, %ebp
	.p2align 4,,5
	jle	.L72
	cmpl	$9, %ebp
	.p2align 4,,5
	jg	.L72
	testl	%r12d, %r12d
	.p2align 4,,5
	js	.L72
	cmpl	$250, %r12d
	.p2align 4,,5
	jg	.L72
	testl	%r12d, %r12d
	movl	$30, %eax
	cmove	%eax, %r12d
	cmpq	$0, 56(%rbx)
	jne	.L80
	movq	$default_bzalloc, 56(%rbx)
.L80:
	cmpq	$0, 64(%rbx)
	jne	.L82
	movq	$default_bzfree, 64(%rbx)
.L82:
	movl	$1, %edx
	movq	72(%rbx), %rdi
	movl	$55768, %esi
	call	*56(%rbx)
	testq	%rax, %rax
	movq	%rax, %r14
	movl	$-3, %edx
	je	.L71
	imull	$100000, %ebp, %r15d
	movq	72(%rbx), %rdi
	movl	$1, %edx
	movq	%rbx, (%rax)
	movq	$0, 24(%rax)
	movq	$0, 32(%rax)
	movq	$0, 40(%rax)
	movslq	%r15d,%rsi
	salq	$2, %rsi
	call	*56(%rbx)
	leal	34(%r15), %esi
	movq	72(%rbx), %rdi
	movl	$1, %edx
	movq	%rax, 24(%r14)
	movslq	%esi,%rsi
	salq	$2, %rsi
	call	*56(%rbx)
	movl	$262148, %esi
	movq	%rax, 32(%r14)
	movq	72(%rbx), %rdi
	movl	$1, %edx
	call	*56(%rbx)
	movq	24(%r14), %rsi
	movq	%rax, 40(%r14)
	testq	%rsi, %rsi
	je	.L86
	cmpq	$0, 32(%r14)
	je	.L88
	testq	%rax, %rax
	je	.L88
	leal	-19(%r15), %eax
	movl	$0, 660(%r14)
	movl	$2, 12(%r14)
	movl	$2, 8(%r14)
	movl	$0, 652(%r14)
	movq	%r14, %rdi
	movl	%eax, 112(%r14)
	movq	32(%r14), %rax
	movl	%ebp, 664(%r14)
	movl	%r13d, 656(%r14)
	movl	%r12d, 88(%r14)
	movq	%rsi, 72(%r14)
	movq	%rax, 64(%r14)
	movq	%rsi, 56(%r14)
	movq	$0, 80(%r14)
	movq	%r14, 48(%rbx)
	movl	$0, 12(%rbx)
	movl	$0, 16(%rbx)
	movl	$0, 36(%rbx)
	movl	$0, 40(%rbx)
	call	init_RL
	movq	%r14, %rdi
	call	prepare_new_block
	xorl	%edx, %edx
	jmp	.L71
.L88:
	movq	72(%rbx), %rdi
	.p2align 4,,2
	call	*64(%rbx)
.L86:
	movq	32(%r14), %rsi
	testq	%rsi, %rsi
	.p2align 4,,3
	je	.L91
	movq	72(%rbx), %rdi
	.p2align 4,,3
	call	*64(%rbx)
.L91:
	movq	40(%r14), %rsi
	testq	%rsi, %rsi
	je	.L93
	movq	72(%rbx), %rdi
	.p2align 4,,3
	call	*64(%rbx)
.L93:
	movq	72(%rbx), %rdi
	movq	%r14, %rsi
	call	*64(%rbx)
	movl	$-3, %edx
	.p2align 4,,3
	jmp	.L71
.LFE35:
	.size	BZ2_bzCompressInit, .-BZ2_bzCompressInit
	.p2align 4,,15
	.type	add_pair_to_block, @function
add_pair_to_block:
.LFB36:
	movl	96(%rdi), %eax
	movzbl	92(%rdi), %r9d
	testl	%eax, %eax
	jle	.L99
	movl	648(%rdi), %eax
	movl	96(%rdi), %esi
	movzbl	%r9b, %r8d
	xorl	%ecx, %ecx
	.p2align 4,,7
.L101:
	movl	%eax, %edx
	sall	$8, %eax
	addl	$1, %ecx
	shrl	$24, %edx
	xorl	%r8d, %edx
	mov	%edx, %edx
	xorl	BZ2_crc32Table(,%rdx,4), %eax
	cmpl	%ecx, %esi
	movl	%eax, 648(%rdi)
	jg	.L101
.L99:
	mov	92(%rdi), %eax
	movb	$1, 128(%rax,%rdi)
	movl	96(%rdi), %eax
	cmpl	$2, %eax
	je	.L104
	cmpl	$3, %eax
	je	.L105
	cmpl	$1, %eax
	je	.L110
	subl	$4, %eax
	movslq	108(%rdi),%rdx
	cltq
	movb	$1, 128(%rax,%rdi)
	movq	64(%rdi), %rax
	movb	%r9b, (%rax,%rdx)
	movl	108(%rdi), %eax
	movq	64(%rdi), %rdx
	addl	$1, %eax
	movl	%eax, 108(%rdi)
	cltq
	movb	%r9b, (%rdx,%rax)
	movl	108(%rdi), %eax
	movq	64(%rdi), %rdx
	addl	$1, %eax
	movl	%eax, 108(%rdi)
	cltq
	movb	%r9b, (%rdx,%rax)
	movl	108(%rdi), %eax
	movq	64(%rdi), %rdx
	addl	$1, %eax
	movl	%eax, 108(%rdi)
	cltq
	movb	%r9b, (%rdx,%rax)
	movl	108(%rdi), %eax
	movzbl	96(%rdi), %edx
	movq	64(%rdi), %rcx
	addl	$1, %eax
	movl	%eax, 108(%rdi)
	cltq
	subl	$4, %edx
	movb	%dl, (%rcx,%rax)
	addl	$1, 108(%rdi)
	ret
.L104:
	movslq	108(%rdi),%rdx
	movq	64(%rdi), %rax
.L109:
	movb	%r9b, (%rdx,%rax)
	movl	108(%rdi), %eax
	movq	64(%rdi), %rdx
	addl	$1, %eax
	movl	%eax, 108(%rdi)
	cltq
	movb	%r9b, (%rdx,%rax)
	addl	$1, 108(%rdi)
	ret
.L105:
	movslq	108(%rdi),%rdx
	movq	64(%rdi), %rax
	movb	%r9b, (%rax,%rdx)
	movl	108(%rdi), %eax
	movq	64(%rdi), %rdx
	addl	$1, %eax
	movl	%eax, 108(%rdi)
	cltq
	jmp	.L109
.L110:
	movslq	108(%rdi),%rdx
	movq	64(%rdi), %rax
	movb	%r9b, (%rax,%rdx)
	addl	$1, 108(%rdi)
	ret
.LFE36:
	.size	add_pair_to_block, .-add_pair_to_block
	.p2align 4,,15
.globl BZ2_bzCompressEnd
	.type	BZ2_bzCompressEnd, @function
BZ2_bzCompressEnd:
.LFB42:
	movq	%rbx, -16(%rsp)
.LCFI10:
	movq	%rbp, -8(%rsp)
.LCFI11:
	subq	$24, %rsp
.LCFI12:
	testq	%rdi, %rdi
	movq	%rdi, %rbx
	jne	.L124
.L112:
	movl	$-2, %eax
.L122:
	movq	8(%rsp), %rbx
	movq	16(%rsp), %rbp
	addq	$24, %rsp
	ret
	.p2align 4,,7
.L124:
	movq	48(%rdi), %rbp
	testq	%rbp, %rbp
	je	.L112
	cmpq	%rdi, (%rbp)
	jne	.L112
	movq	24(%rbp), %rsi
	testq	%rsi, %rsi
	je	.L116
	movq	72(%rdi), %rdi
	.p2align 4,,3
	call	*64(%rbx)
.L116:
	movq	32(%rbp), %rsi
	testq	%rsi, %rsi
	je	.L118
	movq	72(%rbx), %rdi
	.p2align 4,,3
	call	*64(%rbx)
.L118:
	movq	40(%rbp), %rsi
	testq	%rsi, %rsi
	je	.L120
	movq	72(%rbx), %rdi
	.p2align 4,,3
	call	*64(%rbx)
.L120:
	movq	48(%rbx), %rsi
	movq	72(%rbx), %rdi
	call	*64(%rbx)
	xorl	%eax, %eax
	movq	$0, 48(%rbx)
	jmp	.L122
.LFE42:
	.size	BZ2_bzCompressEnd, .-BZ2_bzCompressEnd
	.p2align 4,,15
.globl BZ2_bzDecompressInit
	.type	BZ2_bzDecompressInit, @function
BZ2_bzDecompressInit:
.LFB43:
	movq	%rbx, -24(%rsp)
.LCFI13:
	movq	%rbp, -16(%rsp)
.LCFI14:
	movq	%rdi, %rbx
	movq	%r12, -8(%rsp)
.LCFI15:
	subq	$24, %rsp
.LCFI16:
	movl	%edx, %ebp
	movl	%esi, %r12d
	call	bz_config_ok
	testl	%eax, %eax
	movl	$-9, %edx
	jne	.L142
.L128:
	movq	(%rsp), %rbx
	movq	8(%rsp), %rbp
	movl	%edx, %eax
	movq	16(%rsp), %r12
	addq	$24, %rsp
	ret
.L142:
	testq	%rbx, %rbx
	je	.L129
	testl	%ebp, %ebp
	je	.L131
	cmpl	$1, %ebp
	.p2align 4,,5
	je	.L131
.L129:
	movl	$-2, %edx
	.p2align 4,,5
	jmp	.L128
.L131:
	testl	%r12d, %r12d
	.p2align 4,,5
	js	.L129
	cmpl	$4, %r12d
	.p2align 4,,5
	jg	.L129
	cmpq	$0, 56(%rbx)
	.p2align 4,,5
	je	.L143
.L135:
	cmpq	$0, 64(%rbx)
	.p2align 4,,5
	je	.L144
.L137:
	movl	$1, %edx
	movq	72(%rbx), %rdi
	movl	$64144, %esi
	call	*56(%rbx)
	testq	%rax, %rax
	movl	$-3, %edx
	je	.L128
	xorl	%edx, %edx
	movl	$10, 8(%rax)
	movl	$0, 36(%rax)
	movl	$0, 32(%rax)
	movl	$0, 3188(%rax)
	movq	%rax, 48(%rbx)
	movl	$0, 12(%rbx)
	movl	$0, 16(%rbx)
	movl	$0, 36(%rbx)
	movl	$0, 40(%rbx)
	movq	%rbx, (%rax)
	movb	%bpl, 44(%rax)
	movq	$0, 3168(%rax)
	movq	$0, 3160(%rax)
	movq	$0, 3152(%rax)
	movl	$0, 48(%rax)
	movl	%r12d, 52(%rax)
	jmp	.L128
.L143:
	movq	$default_bzalloc, 56(%rbx)
	jmp	.L135
.L144:
	movq	$default_bzfree, 64(%rbx)
	jmp	.L137
.LFE43:
	.size	BZ2_bzDecompressInit, .-BZ2_bzDecompressInit
	.p2align 4,,15
.globl BZ2_indexIntoF
	.type	BZ2_indexIntoF, @function
BZ2_indexIntoF:
.LFB45:
	xorl	%r8d, %r8d
	movl	$256, %ecx
	jmp	.L146
	.p2align 4,,7
.L155:
	movl	%edx, %r8d
	movl	%ecx, %eax
	subl	%r8d, %eax
	subl	$1, %eax
	je	.L154
.L146:
	leal	(%rcx,%r8), %edx
	sarl	%edx
	movslq	%edx,%rax
	cmpl	%edi, (%rsi,%rax,4)
	jle	.L155
	movl	%edx, %ecx
	movl	%ecx, %eax
	subl	%r8d, %eax
	subl	$1, %eax
	jne	.L146
.L154:
	movl	%r8d, %eax
	ret
.LFE45:
	.size	BZ2_indexIntoF, .-BZ2_indexIntoF
	.p2align 4,,15
.globl BZ2_bzDecompressEnd
	.type	BZ2_bzDecompressEnd, @function
BZ2_bzDecompressEnd:
.LFB48:
	movq	%rbx, -16(%rsp)
.LCFI17:
	movq	%rbp, -8(%rsp)
.LCFI18:
	subq	$24, %rsp
.LCFI19:
	testq	%rdi, %rdi
	movq	%rdi, %rbx
	jne	.L169
.L157:
	movl	$-2, %eax
.L167:
	movq	8(%rsp), %rbx
	movq	16(%rsp), %rbp
	addq	$24, %rsp
	ret
	.p2align 4,,7
.L169:
	movq	48(%rdi), %rbp
	testq	%rbp, %rbp
	je	.L157
	cmpq	%rdi, (%rbp)
	jne	.L157
	movq	3152(%rbp), %rsi
	testq	%rsi, %rsi
	je	.L161
	movq	72(%rdi), %rdi
	call	*64(%rbx)
.L161:
	movq	3160(%rbp), %rsi
	testq	%rsi, %rsi
	je	.L163
	movq	72(%rbx), %rdi
	call	*64(%rbx)
.L163:
	movq	3168(%rbp), %rsi
	testq	%rsi, %rsi
	je	.L165
	movq	72(%rbx), %rdi
	call	*64(%rbx)
.L165:
	movq	48(%rbx), %rsi
	movq	72(%rbx), %rdi
	call	*64(%rbx)
	xorl	%eax, %eax
	movq	$0, 48(%rbx)
	jmp	.L167
.LFE48:
	.size	BZ2_bzDecompressEnd, .-BZ2_bzDecompressEnd
	.p2align 4,,15
.globl BZ2_bzReadGetUnused
	.type	BZ2_bzReadGetUnused, @function
BZ2_bzReadGetUnused:
.LFB57:
	testq	%rsi, %rsi
	je	.L187
	cmpl	$4, 5096(%rsi)
	je	.L175
	testq	%rdi, %rdi
	je	.L177
	movl	$-1, (%rdi)
.L177:
	movl	$-1, 5096(%rsi)
	ret
	.p2align 4,,7
.L175:
	testq	%rdx, %rdx
	je	.L179
	testq	%rcx, %rcx
	je	.L179
	testq	%rdi, %rdi
	.p2align 4,,5
	je	.L184
	movl	$0, (%rdi)
.L184:
	movl	5024(%rsi), %eax
	movl	$0, 5096(%rsi)
	movl	%eax, (%rcx)
	movq	5016(%rsi), %rax
	movq	%rax, (%rdx)
.L186:
	rep ; ret
.L179:
	testq	%rdi, %rdi
	je	.L182
	movl	$-2, (%rdi)
.L182:
	movl	$-2, 5096(%rsi)
	ret
	.p2align 4,,7
.L187:
	testq	%rdi, %rdi
	je	.L186
	movl	$-2, (%rdi)
	.p2align 4,,1
	ret
.LFE57:
	.size	BZ2_bzReadGetUnused, .-BZ2_bzReadGetUnused
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC0:
	.string	"1.0.2, 30-Dec-2001"
	.text
	.p2align 4,,15
.globl BZ2_bzlibVersion
	.type	BZ2_bzlibVersion, @function
BZ2_bzlibVersion:
.LFB60:
	movl	$.LC0, %eax
	ret
.LFE60:
	.size	BZ2_bzlibVersion, .-BZ2_bzlibVersion
	.p2align 4,,15
.globl BZ2_bzflush
	.type	BZ2_bzflush, @function
BZ2_bzflush:
.LFB66:
	xorl	%eax, %eax
	ret
.LFE66:
	.size	BZ2_bzflush, .-BZ2_bzflush
	.p2align 4,,15
.globl BZ2_bzerror
	.type	BZ2_bzerror, @function
BZ2_bzerror:
.LFB68:
	movl	5096(%rdi), %edx
	xorl	%eax, %eax
	testl	%edx, %edx
	cmovle	%edx, %eax
	movl	%eax, (%rsi)
	negl	%eax
	cltq
	movq	bzerrorstrings(,%rax,8), %rax
	ret
.LFE68:
	.size	BZ2_bzerror, .-BZ2_bzerror
	.p2align 4,,15
	.type	uInt64_from_UInt32s, @function
uInt64_from_UInt32s:
.LFB73:
	movl	%edx, %eax
	movb	%dl, 4(%rdi)
	movb	%sil, (%rdi)
	shrl	$24, %eax
	movb	%al, 7(%rdi)
	movl	%edx, %eax
	shrl	$16, %eax
	movb	%al, 6(%rdi)
	movl	%edx, %eax
	shrl	$8, %eax
	movb	%al, 5(%rdi)
	movl	%esi, %eax
	shrl	$24, %eax
	movb	%al, 3(%rdi)
	movl	%esi, %eax
	shrl	$16, %eax
	movb	%al, 2(%rdi)
	movl	%esi, %eax
	shrl	$8, %eax
	movb	%al, 1(%rdi)
	ret
.LFE73:
	.size	uInt64_from_UInt32s, .-uInt64_from_UInt32s
	.section	.rodata.cst8,"aM",@progbits,8
	.align 8
.LC1:
	.long	0
	.long	1072693248
	.align 8
.LC3:
	.long	0
	.long	1081081856
	.text
	.p2align 4,,15
	.type	uInt64_to_double, @function
uInt64_to_double:
.LFB74:
	movsd	.LC1(%rip), %xmm1
	xorl	%edx, %edx
	xorpd	%xmm2, %xmm2
	movsd	.LC3(%rip), %xmm3
	.p2align 4,,7
.L197:
	movzbl	(%rdx,%rdi), %eax
	addq	$1, %rdx
	cmpq	$8, %rdx
	cvtsi2sd	%eax, %xmm0
	mulsd	%xmm1, %xmm0
	mulsd	%xmm3, %xmm1
	addsd	%xmm0, %xmm2
	jne	.L197
	movapd	%xmm2, %xmm0
	ret
.LFE74:
	.size	uInt64_to_double, .-uInt64_to_double
	.p2align 4,,15
	.type	uInt64_toAscii, @function
uInt64_toAscii:
.LFB77:
	pushq	%rbx
.LCFI20:
	movzbl	3(%rsi), %r8d
	movq	%rdi, %rbx
	movzbl	1(%rsi), %r10d
	movzbl	4(%rsi), %edi
	movzbl	2(%rsi), %r9d
	movzbl	6(%rsi), %edx
	movzbl	5(%rsi), %ecx
	movzbl	(%rsi), %r11d
	movzbl	7(%rsi), %eax
	movb	%r8b, -13(%rsp)
	leaq	-16(%rsp), %r8
	movb	%r10b, -15(%rsp)
	leaq	-48(%rsp), %r10
	movb	%dil, -12(%rsp)
	movb	%r9b, -14(%rsp)
	movb	%dl, -10(%rsp)
	xorl	%r9d, %r9d
	movb	%cl, -11(%rsp)
	movb	%r11b, -16(%rsp)
	movl	$-858993459, %edi
	movb	%al, -9(%rsp)
.L203:
	xorl	%edx, %edx
	movl	$7, %esi
	.p2align 4,,7
.L204:
	movslq	%esi,%rcx
	sall	$8, %edx
	subl	$1, %esi
	movzbl	-16(%rsp,%rcx), %eax
	leal	(%rax,%rdx), %r11d
	movl	%r11d, %eax
	mull	%edi
	shrl	$3, %edx
	movb	%dl, -16(%rsp,%rcx)
	leal	(%rdx,%rdx,4), %edx
	addl	%edx, %edx
	subl	%edx, %r11d
	cmpl	$-1, %esi
	movl	%r11d, %edx
	jne	.L204
	leal	48(%rdx), %eax
	leal	1(%r9), %edx
	movb	%al, (%r9,%r10)
	xorl	%eax, %eax
	.p2align 4,,7
.L206:
	cmpb	$0, (%rax,%r8)
	jne	.L219
	addq	$1, %rax
	cmpq	$8, %rax
	jne	.L206
	movslq	%edx,%rax
	testl	%edx, %edx
	movb	$0, (%rax,%rbx)
	jle	.L213
	subl	$1, %edx
	movq	%rbx, %rdi
	.p2align 4,,7
.L210:
	movslq	%edx,%rax
	subl	$1, %edx
	movzbl	-48(%rsp,%rax), %eax
	movb	%al, (%rdi)
	addq	$1, %rdi
	cmpl	$-1, %edx
	jne	.L210
.L213:
	popq	%rbx
	ret
.L219:
	addq	$1, %r9
	jmp	.L203
.LFE77:
	.size	uInt64_toAscii, .-uInt64_toAscii
	.p2align 4,,15
	.type	setExit, @function
setExit:
.LFB81:
	cmpl	%edi, exitValue(%rip)
	jge	.L223
	movl	%edi, exitValue(%rip)
.L223:
	rep ; ret
.LFE81:
	.size	setExit, .-setExit
	.p2align 4,,15
	.type	containsDubiousChars, @function
containsDubiousChars:
.LFB101:
	xorl	%eax, %eax
	ret
.LFE101:
	.size	containsDubiousChars, .-containsDubiousChars
	.p2align 4,,15
	.type	hasSuffix, @function
hasSuffix:
.LFB102:
	movq	%rbx, -24(%rsp)
.LCFI21:
	movq	%rbp, -16(%rsp)
.LCFI22:
	movq	%rdi, %rbp
	movq	%r12, -8(%rsp)
.LCFI23:
	subq	$24, %rsp
.LCFI24:
	movq	%rsi, %r12
	call	strlen
	movq	%r12, %rdi
	movl	%eax, %ebx
	call	strlen
	xorl	%edx, %edx
	cmpl	%eax, %ebx
	jl	.L229
	movslq	%ebx,%rdi
	cltq
	movq	%r12, %rsi
	addq	%rbp, %rdi
	subq	%rax, %rdi
	call	strcmp
	xorl	%edx, %edx
	testl	%eax, %eax
	sete	%dl
.L229:
	movq	(%rsp), %rbx
	movq	8(%rsp), %rbp
	movl	%edx, %eax
	movq	16(%rsp), %r12
	addq	$24, %rsp
	ret
.LFE102:
	.size	hasSuffix, .-hasSuffix
	.section	.rodata.str1.8,"aMS",@progbits,1
	.align 8
.LC4:
	.string	"%s: %s is redundant in versions 0.9.5 and above\n"
	.text
	.p2align 4,,15
	.type	redundant, @function
redundant:
.LFB109:
	movq	%rdi, %rcx
	movq	progName(%rip), %rdx
	movq	stderr(%rip), %rdi
	movl	$.LC4, %esi
	xorl	%eax, %eax
	jmp	fprintf
.LFE109:
	.size	redundant, .-redundant
	.section	.rodata.str1.8
	.align 8
.LC5:
	.ascii	"bzip2, a block-sorting file compressor.  Version %s.\n\n   u"
	.ascii	"sage: %s [flags and input files in any order]\n\n   -h --hel"
	.ascii	"p           print this message\n   -d --decompress     force"
	.ascii	" decompression\n   -z --compress       force compression\n  "
	.ascii	" -k --keep           keep (don't delete) input files\n   -f "
	.ascii	"--force          overwrite existing output files\n   -t --te"
	.ascii	"st           test compressed file integrity\n   -c --stdout "
	.ascii	"        output to standard out\n   -q --quiet          suppr"
	.ascii	"ess noncritical error messages\n   -v --verbose        be ve"
	.ascii	"rbose (a 2nd -v gives more)\n   -L --license        display "
	.ascii	"software version & license\n   -V --version        display s"
	.ascii	"oftware version & license\n   -s --small          use less m"
	.ascii	"emory (at most 2500k)\n   -1 .. -9            set block size"
	.ascii	" to 100k .. 900k\n   --fast              alias for -1\n   --"
	.ascii	"best              alias for -9\n\n   If invoked as `bzip2', "
	.ascii	"default action is to compress.\n              as `bunzip2', "
	.ascii	" default action is to decompress.\n"
	.string	"              as `bzcat', default action is to decompress to stdout.\n\n   If no file names are given, bzip2 compresses or decompresses\n   from standard input to standard output.  You can combine\n   short flags, so `-v -4' means the same as -v4 or -4v, &c.\n\n"
	.text
	.p2align 4,,15
	.type	usage, @function
usage:
.LFB108:
	pushq	%rbx
.LCFI25:
	movq	%rdi, %rbx
	call	BZ2_bzlibVersion
	movq	%rbx, %rcx
	movq	stderr(%rip), %rdi
	movq	%rax, %rdx
	popq	%rbx
	movl	$.LC5, %esi
	xorl	%eax, %eax
	jmp	fprintf
.LFE108:
	.size	usage, .-usage
	.section	.rodata.str1.8
	.align 8
.LC6:
	.ascii	"bzip2, a block-sorting file compressor.  Version %s.\n   \n "
	.ascii	"  Copyright (C) 1996-2002 by Julian Seward.\n   \n   This pr"
	.ascii	"ogram is free software; you can redistribute it and/or modif"
	.ascii	"y\n   it under the terms set out in the LICENSE file, which "
	.ascii	"is included\n   in the bzip2-1.0 sourc"
	.string	"e distribution.\n   \n   This program is distributed in the hope that it will be useful,\n   but WITHOUT ANY WARRANTY; without even the implied warranty of\n   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n   LICENSE file for more details.\n   \n"
	.text
	.p2align 4,,15
	.type	license, @function
license:
.LFB107:
	subq	$8, %rsp
.LCFI26:
	call	BZ2_bzlibVersion
	movq	stderr(%rip), %rdi
	movq	%rax, %rdx
	movl	$.LC6, %esi
	xorl	%eax, %eax
	addq	$8, %rsp
	jmp	fprintf
.LFE107:
	.size	license, .-license
	.section	.rodata.str1.8
	.align 8
.LC7:
	.string	"\tInput file = %s, output file = %s\n"
	.text
	.p2align 4,,15
	.type	showFileNames, @function
showFileNames:
.LFB83:
	cmpb	$0, noisy(%rip)
	jne	.L241
	rep ; ret
	.p2align 4,,7
.L241:
	movq	stderr(%rip), %rdi
	movl	$outName, %ecx
	movl	$inName, %edx
	movl	$.LC7, %esi
	xorl	%eax, %eax
	jmp	fprintf
.LFE83:
	.size	showFileNames, .-showFileNames
	.section	.rodata.str1.8
	.align 8
.LC8:
	.string	"bzip2: I'm not configured correctly for this platform!\n\tI require Int32, Int16 and Char to have sizes\n\tof 4, 2 and 1 bytes to run properly, and they don't.\n\tProbably you can fix this by defining them correctly,\n\tand recompiling.  Bye!\n"
	.text
	.p2align 4,,15
	.type	configError, @function
configError:
.LFB92:
	subq	$8, %rsp
.LCFI27:
	movq	stderr(%rip), %rcx
	movl	$235, %edx
	movl	$1, %esi
	movl	$.LC8, %edi
	call	fwrite
	movl	$3, %edi
	call	setExit
	movl	exitValue(%rip), %edi
	call	exit
.LFE92:
	.size	configError, .-configError
	.section	.rodata.str1.8
	.align 8
.LC9:
	.string	"\nIt is possible that the compressed file(s) have become corrupted.\nYou can use the -tvv option to test integrity of such files.\n\nYou can use the `bzip2recover' program to attempt to recover\ndata from undamaged sections of corrupted files.\n\n"
	.text
	.p2align 4,,15
	.type	cadvise, @function
cadvise:
.LFB82:
	cmpb	$0, noisy(%rip)
	jne	.L248
	rep ; ret
	.p2align 4,,7
.L248:
	movq	stderr(%rip), %rcx
	movl	$240, %edx
	movl	$1, %esi
	movl	$.LC9, %edi
	jmp	fwrite
.LFE82:
	.size	cadvise, .-cadvise
	.section	.rodata.str1.8
	.align 8
.LC10:
	.ascii	"\n\nbzip2/libbzip2: internal error number %d.\nThis is a bug"
	.ascii	" in bzip2/libbzip2, %s.\nPlease report it to me at: jseward@"
	.ascii	"acm.org.  If this happened\nwhen you were using some program"
	.ascii	" which use"
	.string	"s libbzip2 as a\ncomponent, you should also report this bug to the author(s)\nof that program.  Please make an effort to report this bug;\ntimely and accurate bug reports eventually lead to higher\nquality software.  Thanks.  Julian Seward, 30 December 2001.\n\n"
	.align 8
.LC11:
	.ascii	"\n*** A special note about internal error number 1007 ***\n\n"
	.ascii	"Experience suggests that a common cause of i.e. 1007\nis unr"
	.ascii	"eliable memory or other hardware.  The 1007 assertion\njust "
	.ascii	"happens to cross-check the results of huge numbers of\nmemor"
	.ascii	"y reads/writes, and so acts (unintendedly) as a stress\ntest"
	.ascii	" of your memory system.\n\nI suggest the following: try comp"
	.ascii	"ressing the file again,\npossibly monitoring progress in det"
	.ascii	"ail with the -vv flag.\n\n* If the error cannot be reproduce"
	.ascii	"d, and/or happens at different\n  points in compression, you"
	.ascii	" may have a flaky memory system.\n  Try a memory-test progra"
	.ascii	"m.  I have used Memtest86\n  (www.memtest86.com).  At the ti"
	.ascii	"me of writing it is free (GPLd).\n  Memtest86 tests memory m"
	.ascii	"uch more thorougly than your BIOSs\n  power-on test, and may"
	.ascii	" find failures that the BIOS doesn't"
	.string	".\n\n* If the error can be repeatably reproduced, this is a bug in\n  bzip2, and I would very much like to hear about it.  Please\n  let me know, and, ideally, save a copy of the file causing the\n  problem -- without which I will be unable to investigate it.\n\n"
	.text
	.p2align 4,,15
.globl BZ2_bz__AssertH__fail
	.type	BZ2_bz__AssertH__fail, @function
BZ2_bz__AssertH__fail:
.LFB28:
	pushq	%rbx
.LCFI28:
	movl	%edi, %ebx
	call	BZ2_bzlibVersion
	movq	stderr(%rip), %rdi
	movq	%rax, %rcx
	movl	%ebx, %edx
	xorl	%eax, %eax
	movl	$.LC10, %esi
	call	fprintf
	cmpl	$1007, %ebx
	je	.L253
.L250:
	movl	$3, %edi
	call	exit
	.p2align 4,,7
.L253:
	movq	stderr(%rip), %rcx
	movl	$1056, %edx
	movl	$1, %esi
	movl	$.LC11, %edi
	call	fwrite
	jmp	.L250
.LFE28:
	.size	BZ2_bz__AssertH__fail, .-BZ2_bz__AssertH__fail
	.p2align 4,,15
.globl BZ2_hbMakeCodeLengths
	.type	BZ2_hbMakeCodeLengths, @function
BZ2_hbMakeCodeLengths:
.LFB14:
	pushq	%r15
.LCFI29:
	pushq	%r14
.LCFI30:
	pushq	%r13
.LCFI31:
	pushq	%r12
.LCFI32:
	pushq	%rbp
.LCFI33:
	pushq	%rbx
.LCFI34:
	subq	$5192, %rsp
.LCFI35:
	testl	%edx, %edx
	movq	%rdi, 8(%rsp)
	movl	%edx, 4(%rsp)
	movl	%ecx, (%rsp)
	jle	.L332
	leaq	2080(%rsp), %rbp
	xorl	%r8d, %r8d
	xorl	%edi, %edi
.L257:
	movl	(%rsi,%rdi,4), %eax
	movl	$256, %edx
	movl	%eax, %ecx
	sall	$8, %ecx
	testl	%eax, %eax
	cmovne	%ecx, %edx
	addl	$1, %r8d
	movl	%edx, 4(%rbp,%rdi,4)
	addq	$1, %rdi
	cmpl	4(%rsp), %r8d
	jne	.L257
.L332:
	movl	4(%rsp), %ecx
	movl	$0, 4144(%rsp)
	movl	$0, 2080(%rsp)
	movl	$-2, 16(%rsp)
	testl	%ecx, %ecx
	jle	.L261
	movl	4(%rsp), %r10d
	leaq	2080(%rsp), %rbp
	leaq	16(%rsp), %rbx
	leaq	4144(%rsp), %r11
	movl	$1, %r8d
	xorl	%r9d, %r9d
	addl	$1, %r10d
	.p2align 4,,7
.L263:
	movl	%r8d, %esi
	movl	%r8d, 4(%r11,%r9,4)
	movl	4(%rbp,%r9,4), %edi
	sarl	%esi
	movl	$-1, 4(%rbx,%r9,4)
	movslq	%esi,%rax
	movl	4144(%rsp,%rax,4), %edx
	movslq	%edx,%rax
	cmpl	2080(%rsp,%rax,4), %edi
	jge	.L264
	movl	%r8d, %eax
	jmp	.L266
	.p2align 4,,7
.L333:
	movl	%esi, %eax
	movl	%ecx, %esi
.L266:
	movl	%esi, %ecx
	cltq
	sarl	%ecx
	movl	%edx, 4144(%rsp,%rax,4)
	movslq	%ecx,%rax
	movl	4144(%rsp,%rax,4), %edx
	movslq	%edx,%rax
	cmpl	2080(%rsp,%rax,4), %edi
	jl	.L333
.L267:
	movslq	%esi,%rax
	addq	$1, %r9
	movl	%r8d, 4144(%rsp,%rax,4)
	addl	$1, %r8d
	cmpl	%r10d, %r8d
	jne	.L263
	cmpl	$259, 4(%rsp)
	jg	.L270
	cmpl	$1, 4(%rsp)
	jle	.L261
.L272:
	movl	4(%rsp), %r12d
	movl	4(%rsp), %r11d
	movl	4(%rsp), %ebx
	subl	$1, %r11d
	movl	%r12d, %r13d
	movslq	%r12d,%r8
	subl	$2, %ebx
	.p2align 4,,7
.L273:
	movl	4144(%rsp,%r8,4), %ebp
	cmpl	$1, %r11d
	movl	4148(%rsp), %r15d
	movl	$1, %r10d
	movl	%ebp, 4148(%rsp)
	jle	.L276
	movslq	%ebp,%rax
	movl	$2, %esi
	movl	2080(%rsp,%rax,4), %r9d
	jmp	.L277
	.p2align 4,,7
.L334:
	movslq	%r10d,%rax
	movl	%esi, %r10d
	movl	%edx, 4144(%rsp,%rax,4)
	leal	(%rsi,%rsi), %eax
	cmpl	%eax, %r11d
	jl	.L282
	movl	%eax, %esi
.L277:
	cmpl	%esi, %r11d
	movslq	%esi,%rcx
	jle	.L278
	leal	1(%rsi), %edi
	movslq	4144(%rsp,%rcx,4),%rdx
	movslq	%edi,%r8
	movslq	4144(%rsp,%r8,4),%rax
	movl	2080(%rsp,%rax,4), %eax
	cmpl	2080(%rsp,%rdx,4), %eax
	jge	.L278
	movl	%edi, %esi
	movq	%r8, %rcx
	.p2align 4,,7
.L278:
	movl	4144(%rsp,%rcx,4), %edx
	movslq	%edx,%rax
	cmpl	2080(%rsp,%rax,4), %r9d
	jge	.L334
.L276:
	movslq	%r10d,%rax
	cmpl	$1, %ebx
	movl	$1, %r10d
	movl	%ebp, 4144(%rsp,%rax,4)
	movslq	%r11d,%rax
	movl	4148(%rsp), %r14d
	movl	4144(%rsp,%rax,4), %ebp
	movl	%ebp, 4148(%rsp)
	jle	.L286
	movslq	%ebp,%rax
	movl	$2, %esi
	movl	2080(%rsp,%rax,4), %r9d
	jmp	.L287
	.p2align 4,,7
.L335:
	movslq	%r10d,%rax
	movl	%esi, %r10d
	movl	%edx, 4144(%rsp,%rax,4)
	leal	(%rsi,%rsi), %eax
	cmpl	%eax, %ebx
	jl	.L292
	movl	%eax, %esi
.L287:
	cmpl	%esi, %ebx
	movslq	%esi,%rcx
	jle	.L288
	leal	1(%rsi), %edi
	movslq	4144(%rsp,%rcx,4),%rdx
	movslq	%edi,%r8
	movslq	4144(%rsp,%r8,4),%rax
	movl	2080(%rsp,%rax,4), %eax
	cmpl	2080(%rsp,%rdx,4), %eax
	jge	.L288
	movl	%edi, %esi
	movq	%r8, %rcx
	.p2align 4,,7
.L288:
	movl	4144(%rsp,%rcx,4), %edx
	movslq	%edx,%rax
	cmpl	2080(%rsp,%rax,4), %r9d
	jge	.L335
.L286:
	movslq	%r10d,%rax
	addl	$1, %r12d
	movslq	%r14d,%rdx
	movl	%ebp, 4144(%rsp,%rax,4)
	movslq	%r15d,%rax
	movl	%r12d, 16(%rsp,%rdx,4)
	movl	2080(%rsp,%rax,4), %esi
	movl	2080(%rsp,%rdx,4), %edx
	movl	%r12d, 16(%rsp,%rax,4)
	movzbl	%sil,%ecx
	movzbl	%dl,%eax
	cmpl	%ecx, %eax
	cmovl	%ecx, %eax
	andb	$0, %sil
	subl	$1, %r13d
	leal	1(%rax), %edi
	xorb	%dl, %dl
	movslq	%r12d,%rax
	addl	%esi, %edx
	movl	%r13d, %esi
	movl	$-1, 16(%rsp,%rax,4)
	orl	%edx, %edi
	sarl	%esi
	movslq	%r13d,%r8
	movl	%edi, 2080(%rsp,%rax,4)
	movslq	%esi,%rax
	movl	%r12d, 4144(%rsp,%r8,4)
	movl	4144(%rsp,%rax,4), %edx
	movslq	%edx,%rax
	cmpl	2080(%rsp,%rax,4), %edi
	jge	.L294
	movl	%r13d, %eax
	jmp	.L296
	.p2align 4,,7
.L336:
	movl	%esi, %eax
	movl	%ecx, %esi
.L296:
	movl	%esi, %ecx
	cltq
	sarl	%ecx
	movl	%edx, 4144(%rsp,%rax,4)
	movslq	%ecx,%rax
	movl	4144(%rsp,%rax,4), %edx
	movslq	%edx,%rax
	cmpl	2080(%rsp,%rax,4), %edi
	jl	.L336
.L297:
	movslq	%esi,%rax
	subl	$1, %r11d
	subl	$1, %ebx
	cmpl	$1, %r13d
	movl	%r12d, 4144(%rsp,%rax,4)
	jg	.L273
.L299:
	cmpl	$515, %r12d
	jg	.L337
	movl	4(%rsp), %edx
	testl	%edx, %edx
	jle	.L316
.L338:
	movl	4(%rsp), %r9d
	movq	8(%rsp), %rsi
	movl	$1, %eax
	movl	$1, %edi
	xorl	%r8d, %r8d
	addl	$1, %r9d
	.p2align 4,,7
.L304:
	cltq
	xorl	%edx, %edx
	xorl	%ecx, %ecx
	movl	16(%rsp,%rax,4), %eax
	testl	%eax, %eax
	js	.L308
	xorl	%edx, %edx
	.p2align 4,,7
.L306:
	cltq
	addl	$1, %edx
	movl	16(%rsp,%rax,4), %eax
	testl	%eax, %eax
	jns	.L306
	movl	%edx, %ecx
.L308:
	movb	%cl, (%rsi)
	cmpl	%edx, (%rsp)
	movl	$1, %eax
	cmovl	%eax, %r8d
	addl	$1, %edi
	addq	$1, %rsi
	cmpl	%r9d, %edi
	movl	%edi, %eax
	jne	.L304
	testb	%r8b, %r8b
	je	.L316
	cmpl	$1, 4(%rsp)
	jle	.L332
	leaq	2080(%rsp), %rcx
	movl	$1, %esi
.L315:
	movl	4(%rcx), %edx
	addl	$1, %esi
	sarl	$8, %edx
	movl	%edx, %eax
	shrl	$31, %eax
	addl	%edx, %eax
	sarl	%eax
	addl	$1, %eax
	sall	$8, %eax
	movl	%eax, 4(%rcx)
	addq	$4, %rcx
	cmpl	4(%rsp), %esi
	je	.L332
	jmp	.L315
	.p2align 4,,7
.L292:
	movl	%esi, %r10d
	jmp	.L286
.L282:
	movl	%esi, %r10d
	.p2align 4,,7
	jmp	.L276
.L294:
	movl	%r13d, %esi
	.p2align 4,,5
	jmp	.L297
.L264:
	movl	%r8d, %esi
	.p2align 4,,5
	jmp	.L267
.L270:
	movl	$2001, %edi
	.p2align 4,,5
	call	BZ2_bz__AssertH__fail
	.p2align 4,,4
	jmp	.L272
.L337:
	movl	$2002, %edi
	call	BZ2_bz__AssertH__fail
	movl	4(%rsp), %edx
	testl	%edx, %edx
	jg	.L338
.L316:
	addq	$5192, %rsp
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
.L261:
	movl	4(%rsp), %r12d
	jmp	.L299
.LFE14:
	.size	BZ2_hbMakeCodeLengths, .-BZ2_hbMakeCodeLengths
	.section	.rodata.str1.1
.LC12:
	.string	"\n    [%d: huff+mtf "
.LC13:
	.string	"rt+rld"
	.text
	.p2align 4,,15
.globl BZ2_decompress
	.type	BZ2_decompress, @function
BZ2_decompress:
.LFB27:
	pushq	%r15
.LCFI36:
	pushq	%r14
.LCFI37:
	pushq	%r13
.LCFI38:
	pushq	%r12
.LCFI39:
	pushq	%rbp
.LCFI40:
	pushq	%rbx
.LCFI41:
	movq	%rdi, %rbx
	subq	$152, %rsp
.LCFI42:
	cmpl	$10, 8(%rdi)
	movq	(%rdi), %rbp
	je	.L1110
.L340:
	movl	64040(%rbx), %eax
	movl	64044(%rbx), %edx
	movl	64048(%rbx), %ecx
	movl	64052(%rbx), %esi
	movl	64036(%rbx), %r12d
	movl	64100(%rbx), %r14d
	movl	%eax, 124(%rsp)
	movl	%edx, 36(%rsp)
	movl	64056(%rbx), %eax
	movl	64060(%rbx), %edx
	movl	%ecx, 40(%rsp)
	movl	%esi, 44(%rsp)
	movl	64064(%rbx), %ecx
	movl	64068(%rbx), %esi
	movl	%eax, 48(%rsp)
	movl	%edx, 52(%rsp)
	movl	64072(%rbx), %eax
	movl	64076(%rbx), %edx
	movl	%ecx, 56(%rsp)
	movl	%esi, 60(%rsp)
	movl	64080(%rbx), %ecx
	movl	64084(%rbx), %esi
	movl	%eax, 64(%rsp)
	movl	%edx, 68(%rsp)
	movl	64088(%rbx), %eax
	movl	64092(%rbx), %edx
	movl	%ecx, 72(%rsp)
	movl	%esi, 76(%rsp)
	movl	64096(%rbx), %ecx
	movl	64112(%rbx), %esi
	movl	%eax, 120(%rsp)
	movl	%edx, 80(%rsp)
	movl	64104(%rbx), %r13d
	movl	64108(%rbx), %r15d
	movl	%ecx, 84(%rsp)
	movl	%esi, 88(%rsp)
	movl	64116(%rbx), %eax
	movq	64120(%rbx), %rdx
	movq	64128(%rbx), %rcx
	movq	64136(%rbx), %rsi
	movl	%eax, 92(%rsp)
	movl	8(%rbx), %eax
	movq	%rdx, 96(%rsp)
	movq	%rcx, 104(%rsp)
	movq	%rsi, 112(%rsp)
	subl	$10, %eax
	cmpl	$40, %eax
	ja	.L342
	mov	%eax, %eax
	jmp	*.L384(,%rax,8)
	.section	.rodata
	.align 8
	.align 4
.L384:
	.quad	.L343
	.quad	.L344
	.quad	.L345
	.quad	.L346
	.quad	.L347
	.quad	.L348
	.quad	.L349
	.quad	.L350
	.quad	.L351
	.quad	.L352
	.quad	.L353
	.quad	.L354
	.quad	.L355
	.quad	.L356
	.quad	.L357
	.quad	.L358
	.quad	.L359
	.quad	.L360
	.quad	.L937
	.quad	.L938
	.quad	.L363
	.quad	.L364
	.quad	.L993
	.quad	.L366
	.quad	.L367
	.quad	.L368
	.quad	.L369
	.quad	.L370
	.quad	.L371
	.quad	.L372
	.quad	.L373
	.quad	.L374
	.quad	.L375
	.quad	.L376
	.quad	.L377
	.quad	.L378
	.quad	.L379
	.quad	.L380
	.quad	.L381
	.quad	.L382
	.quad	.L383
	.text
	.p2align 4,,7
.L342:
	movl	$4001, %edi
	call	BZ2_bz__AssertH__fail
	movl	$4002, %edi
	call	BZ2_bz__AssertH__fail
	xorl	%eax, %eax
.L593:
	movl	124(%rsp), %edx
	movl	36(%rsp), %ecx
	movl	40(%rsp), %esi
	movl	%r12d, 64036(%rbx)
	movl	%r14d, 64100(%rbx)
	movl	%r13d, 64104(%rbx)
	movl	%edx, 64040(%rbx)
	movl	%ecx, 64044(%rbx)
	movl	44(%rsp), %edx
	movl	48(%rsp), %ecx
	movl	%esi, 64048(%rbx)
	movl	52(%rsp), %esi
	movl	%edx, 64052(%rbx)
	movl	%ecx, 64056(%rbx)
	movl	56(%rsp), %edx
	movl	60(%rsp), %ecx
	movl	%esi, 64060(%rbx)
	movl	64(%rsp), %esi
	movl	%edx, 64064(%rbx)
	movl	%ecx, 64068(%rbx)
	movl	68(%rsp), %edx
	movl	72(%rsp), %ecx
	movl	%esi, 64072(%rbx)
	movl	76(%rsp), %esi
	movl	%edx, 64076(%rbx)
	movl	%ecx, 64080(%rbx)
	movl	120(%rsp), %edx
	movl	80(%rsp), %ecx
	movl	%esi, 64084(%rbx)
	movl	84(%rsp), %esi
	movl	%edx, 64088(%rbx)
	movl	%ecx, 64092(%rbx)
	movl	%esi, 64096(%rbx)
	movl	%r15d, 64108(%rbx)
	movl	88(%rsp), %edx
	movl	92(%rsp), %ecx
	movq	96(%rsp), %rsi
	movl	%edx, 64112(%rbx)
	movl	%ecx, 64116(%rbx)
	movq	104(%rsp), %rdx
	movq	112(%rsp), %rcx
	movq	%rsi, 64120(%rbx)
	movq	%rdx, 64128(%rbx)
	movq	%rcx, 64136(%rbx)
	addq	$152, %rsp
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
	.p2align 4,,7
.L1110:
	movl	$0, 64036(%rdi)
	movl	$0, 64040(%rdi)
	movl	$0, 64044(%rdi)
	movl	$0, 64048(%rdi)
	movl	$0, 64052(%rdi)
	movl	$0, 64056(%rdi)
	movl	$0, 64060(%rdi)
	movl	$0, 64064(%rdi)
	movl	$0, 64068(%rdi)
	movl	$0, 64072(%rdi)
	movl	$0, 64076(%rdi)
	movl	$0, 64080(%rdi)
	movl	$0, 64084(%rdi)
	movl	$0, 64088(%rdi)
	movl	$0, 64092(%rdi)
	movl	$0, 64096(%rdi)
	movl	$0, 64100(%rdi)
	movl	$0, 64104(%rdi)
	movl	$0, 64108(%rdi)
	movl	$0, 64112(%rdi)
	movl	$0, 64116(%rdi)
	movq	$0, 64120(%rdi)
	movq	$0, 64128(%rdi)
	movq	$0, 64136(%rdi)
	jmp	.L340
.L1159:
	testq	%rax, %rax
	je	.L411
.L347:
	movl	$14, 8(%rbx)
.L1073:
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jg	.L1111
.L414:
	movq	(%rbx), %rcx
	movl	8(%rcx), %r11d
	testl	%r11d, %r11d
	je	.L1066
	movq	(%rcx), %rax
	movzbl	(%rax), %edx
	movl	32(%rbx), %eax
	addl	$8, 36(%rbx)
	addq	$1, (%rcx)
	sall	$8, %eax
	orl	%eax, %edx
	movl	%edx, 32(%rbx)
	addl	$1, 12(%rcx)
	movl	12(%rcx), %r10d
	subl	$1, 8(%rcx)
	testl	%r10d, %r10d
	jne	.L1073
	addl	$1, 16(%rcx)
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jle	.L414
.L1111:
	leal	-8(%rax), %ecx
	movl	32(%rbx), %eax
	movl	%ecx, 36(%rbx)
	shrl	%cl, %eax
	cmpb	$23, %al
	je	.L375
	cmpb	$49, %al
	jne	.L1068
.L348:
	movl	$15, 8(%rbx)
.L1074:
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jg	.L1112
.L421:
	movq	(%rbx), %rcx
	movl	8(%rcx), %r9d
	testl	%r9d, %r9d
	je	.L1066
	movq	(%rcx), %rax
	movzbl	(%rax), %edx
	movl	32(%rbx), %eax
	addl	$8, 36(%rbx)
	addq	$1, (%rcx)
	sall	$8, %eax
	orl	%eax, %edx
	movl	%edx, 32(%rbx)
	addl	$1, 12(%rcx)
	movl	12(%rcx), %r8d
	subl	$1, 8(%rcx)
	testl	%r8d, %r8d
	jne	.L1074
	addl	$1, 16(%rcx)
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jle	.L421
.L1112:
	leal	-8(%rax), %ecx
	movl	32(%rbx), %eax
	movl	%ecx, 36(%rbx)
	shrl	%cl, %eax
	cmpb	$65, %al
	jne	.L1068
.L349:
	movl	$16, 8(%rbx)
.L1075:
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jg	.L1113
.L426:
	movq	(%rbx), %rcx
	movl	8(%rcx), %edi
	testl	%edi, %edi
	je	.L1066
	movq	(%rcx), %rax
	movzbl	(%rax), %edx
	movl	32(%rbx), %eax
	addl	$8, 36(%rbx)
	addq	$1, (%rcx)
	sall	$8, %eax
	orl	%eax, %edx
	movl	%edx, 32(%rbx)
	addl	$1, 12(%rcx)
	movl	12(%rcx), %esi
	subl	$1, 8(%rcx)
	testl	%esi, %esi
	jne	.L1075
	addl	$1, 16(%rcx)
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jle	.L426
.L1113:
	leal	-8(%rax), %ecx
	movl	32(%rbx), %eax
	movl	%ecx, 36(%rbx)
	shrl	%cl, %eax
	cmpb	$89, %al
	jne	.L1068
.L350:
	movl	$17, 8(%rbx)
.L1076:
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jg	.L1114
.L431:
	movq	(%rbx), %rcx
	movl	8(%rcx), %edx
	testl	%edx, %edx
	je	.L1066
	movq	(%rcx), %rax
	movzbl	(%rax), %edx
	movl	32(%rbx), %eax
	addl	$8, 36(%rbx)
	addq	$1, (%rcx)
	sall	$8, %eax
	orl	%eax, %edx
	movl	%edx, 32(%rbx)
	addl	$1, 12(%rcx)
	movl	12(%rcx), %eax
	subl	$1, 8(%rcx)
	testl	%eax, %eax
	jne	.L1076
	addl	$1, 16(%rcx)
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jle	.L431
.L1114:
	leal	-8(%rax), %ecx
	movl	32(%rbx), %eax
	movl	%ecx, 36(%rbx)
	shrl	%cl, %eax
	cmpb	$38, %al
	jne	.L1068
.L351:
	movl	$18, 8(%rbx)
.L1077:
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jg	.L1115
.L436:
	movq	(%rbx), %rcx
	movl	8(%rcx), %eax
	testl	%eax, %eax
	je	.L1066
	movq	(%rcx), %rax
	movzbl	(%rax), %edx
	movl	32(%rbx), %eax
	addl	$8, 36(%rbx)
	addq	$1, (%rcx)
	sall	$8, %eax
	orl	%eax, %edx
	movl	%edx, 32(%rbx)
	addl	$1, 12(%rcx)
	movl	12(%rcx), %eax
	subl	$1, 8(%rcx)
	testl	%eax, %eax
	jne	.L1077
	addl	$1, 16(%rcx)
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jle	.L436
.L1115:
	leal	-8(%rax), %ecx
	movl	32(%rbx), %eax
	movl	%ecx, 36(%rbx)
	shrl	%cl, %eax
	cmpb	$83, %al
	jne	.L1068
.L352:
	movl	$19, 8(%rbx)
.L1078:
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jg	.L1116
.L441:
	movq	(%rbx), %rcx
	movl	8(%rcx), %eax
	testl	%eax, %eax
	je	.L1066
	movq	(%rcx), %rax
	movzbl	(%rax), %edx
	movl	32(%rbx), %eax
	addl	$8, 36(%rbx)
	addq	$1, (%rcx)
	sall	$8, %eax
	orl	%eax, %edx
	movl	%edx, 32(%rbx)
	addl	$1, 12(%rcx)
	movl	12(%rcx), %eax
	subl	$1, 8(%rcx)
	testl	%eax, %eax
	jne	.L1078
	addl	$1, 16(%rcx)
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jle	.L441
.L1116:
	leal	-8(%rax), %ecx
	movl	32(%rbx), %eax
	movl	%ecx, 36(%rbx)
	shrl	%cl, %eax
	cmpb	$89, %al
	jne	.L1068
	movl	48(%rbx), %edx
	addl	$1, %edx
	cmpl	$1, 52(%rbx)
	movl	%edx, 48(%rbx)
	jle	.L446
	movq	stderr(%rip), %rdi
	movl	$.LC12, %esi
	xorl	%eax, %eax
	call	fprintf
.L446:
	movl	$0, 3176(%rbx)
.L353:
	movl	$20, 8(%rbx)
.L1079:
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jg	.L830
.L1117:
	movq	(%rbx), %rcx
	movl	8(%rcx), %eax
	testl	%eax, %eax
	je	.L1066
	movq	(%rcx), %rax
	movzbl	(%rax), %edx
	movl	32(%rbx), %eax
	addl	$8, 36(%rbx)
	addq	$1, (%rcx)
	sall	$8, %eax
	orl	%eax, %edx
	movl	%edx, 32(%rbx)
	addl	$1, 12(%rcx)
	movl	12(%rcx), %ebp
	subl	$1, 8(%rcx)
	testl	%ebp, %ebp
	jne	.L1079
	addl	$1, 16(%rcx)
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jle	.L1117
.L830:
	movl	32(%rbx), %edx
	leal	-8(%rax), %ecx
	movl	3176(%rbx), %eax
	movl	%ecx, 36(%rbx)
	shrl	%cl, %edx
	sall	$8, %eax
	movzbl	%dl, %edx
	orl	%eax, %edx
	movl	%edx, 3176(%rbx)
.L354:
	movl	$21, 8(%rbx)
	.p2align 4,,7
.L1080:
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jg	.L831
.L1118:
	movq	(%rbx), %rcx
	movl	8(%rcx), %r11d
	testl	%r11d, %r11d
	je	.L1066
	movq	(%rcx), %rax
	movzbl	(%rax), %edx
	movl	32(%rbx), %eax
	addl	$8, 36(%rbx)
	addq	$1, (%rcx)
	sall	$8, %eax
	orl	%eax, %edx
	movl	%edx, 32(%rbx)
	addl	$1, 12(%rcx)
	movl	12(%rcx), %r10d
	subl	$1, 8(%rcx)
	testl	%r10d, %r10d
	jne	.L1080
	addl	$1, 16(%rcx)
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jle	.L1118
.L831:
	movl	32(%rbx), %edx
	leal	-8(%rax), %ecx
	movl	3176(%rbx), %eax
	movl	%ecx, 36(%rbx)
	shrl	%cl, %edx
	sall	$8, %eax
	movzbl	%dl, %edx
	orl	%eax, %edx
	movl	%edx, 3176(%rbx)
.L355:
	movl	$22, 8(%rbx)
	.p2align 4,,7
.L1081:
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jg	.L832
.L1119:
	movq	(%rbx), %rcx
	movl	8(%rcx), %r9d
	testl	%r9d, %r9d
	je	.L1066
	movq	(%rcx), %rax
	movzbl	(%rax), %edx
	movl	32(%rbx), %eax
	addl	$8, 36(%rbx)
	addq	$1, (%rcx)
	sall	$8, %eax
	orl	%eax, %edx
	movl	%edx, 32(%rbx)
	addl	$1, 12(%rcx)
	movl	12(%rcx), %r8d
	subl	$1, 8(%rcx)
	testl	%r8d, %r8d
	jne	.L1081
	addl	$1, 16(%rcx)
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jle	.L1119
.L832:
	movl	32(%rbx), %edx
	leal	-8(%rax), %ecx
	movl	3176(%rbx), %eax
	movl	%ecx, 36(%rbx)
	shrl	%cl, %edx
	sall	$8, %eax
	movzbl	%dl, %edx
	orl	%eax, %edx
	movl	%edx, 3176(%rbx)
.L356:
	movl	$23, 8(%rbx)
	.p2align 4,,7
.L1082:
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jg	.L833
.L1120:
	movq	(%rbx), %rcx
	movl	8(%rcx), %edi
	testl	%edi, %edi
	je	.L1066
	movq	(%rcx), %rax
	movzbl	(%rax), %edx
	movl	32(%rbx), %eax
	addl	$8, 36(%rbx)
	addq	$1, (%rcx)
	sall	$8, %eax
	orl	%eax, %edx
	movl	%edx, 32(%rbx)
	addl	$1, 12(%rcx)
	movl	12(%rcx), %esi
	subl	$1, 8(%rcx)
	testl	%esi, %esi
	jne	.L1082
	addl	$1, 16(%rcx)
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jle	.L1120
.L833:
	movl	32(%rbx), %edx
	leal	-8(%rax), %ecx
	movl	3176(%rbx), %eax
	movl	%ecx, 36(%rbx)
	shrl	%cl, %edx
	sall	$8, %eax
	movzbl	%dl, %edx
	orl	%eax, %edx
	movl	%edx, 3176(%rbx)
.L357:
	movl	$24, 8(%rbx)
	.p2align 4,,7
.L1083:
	movl	36(%rbx), %eax
	testl	%eax, %eax
	jg	.L834
.L1121:
	movq	(%rbx), %rcx
	movl	8(%rcx), %edx
	testl	%edx, %edx
	je	.L1066
	movq	(%rcx), %rax
	movzbl	(%rax), %edx
	movl	32(%rbx), %eax
	addl	$8, 36(%rbx)
	addq	$1, (%rcx)
	sall	$8, %eax
	orl	%eax, %edx
	movl	%edx, 32(%rbx)
	addl	$1, 12(%rcx)
	movl	12(%rcx), %eax
	subl	$1, 8(%rcx)
	testl	%eax, %eax
	jne	.L1083
	addl	$1, 16(%rcx)
	movl	36(%rbx), %eax
	testl	%eax, %eax
	jle	.L1121
.L834:
	leal	-1(%rax), %ecx
	movl	32(%rbx), %eax
	movl	$0, 56(%rbx)
	movl	%ecx, 36(%rbx)
	shrl	%cl, %eax
	andl	$1, %eax
	movb	%al, 20(%rbx)
.L358:
	movl	$25, 8(%rbx)
	.p2align 4,,7
.L1084:
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jg	.L835
.L1122:
	movq	(%rbx), %rcx
	movl	8(%rcx), %eax
	testl	%eax, %eax
	je	.L1066
	movq	(%rcx), %rax
	movzbl	(%rax), %edx
	movl	32(%rbx), %eax
	addl	$8, 36(%rbx)
	addq	$1, (%rcx)
	sall	$8, %eax
	orl	%eax, %edx
	movl	%edx, 32(%rbx)
	addl	$1, 12(%rcx)
	movl	12(%rcx), %eax
	subl	$1, 8(%rcx)
	testl	%eax, %eax
	jne	.L1084
	addl	$1, 16(%rcx)
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jle	.L1122
.L835:
	movl	32(%rbx), %edx
	leal	-8(%rax), %ecx
	movl	56(%rbx), %eax
	movl	%ecx, 36(%rbx)
	shrl	%cl, %edx
	sall	$8, %eax
	movzbl	%dl, %edx
	orl	%eax, %edx
	movl	%edx, 56(%rbx)
.L359:
	movl	$26, 8(%rbx)
	.p2align 4,,7
.L1085:
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jg	.L836
.L1123:
	movq	(%rbx), %rcx
	movl	8(%rcx), %eax
	testl	%eax, %eax
	je	.L1066
	movq	(%rcx), %rax
	movzbl	(%rax), %edx
	movl	32(%rbx), %eax
	addl	$8, 36(%rbx)
	addq	$1, (%rcx)
	sall	$8, %eax
	orl	%eax, %edx
	movl	%edx, 32(%rbx)
	addl	$1, 12(%rcx)
	movl	12(%rcx), %eax
	subl	$1, 8(%rcx)
	testl	%eax, %eax
	jne	.L1085
	addl	$1, 16(%rcx)
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jle	.L1123
.L836:
	movl	32(%rbx), %edx
	leal	-8(%rax), %ecx
	movl	56(%rbx), %eax
	movl	%ecx, 36(%rbx)
	shrl	%cl, %edx
	sall	$8, %eax
	movzbl	%dl, %edx
	orl	%eax, %edx
	movl	%edx, 56(%rbx)
.L360:
	movl	$27, 8(%rbx)
	.p2align 4,,7
.L1086:
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jg	.L1124
.L484:
	movq	(%rbx), %rcx
	movl	8(%rcx), %eax
	testl	%eax, %eax
	je	.L1066
	movq	(%rcx), %rax
	movzbl	(%rax), %edx
	movl	32(%rbx), %eax
	addl	$8, 36(%rbx)
	addq	$1, (%rcx)
	sall	$8, %eax
	orl	%eax, %edx
	movl	%edx, 32(%rbx)
	addl	$1, 12(%rcx)
	movl	12(%rcx), %ebp
	subl	$1, 8(%rcx)
	testl	%ebp, %ebp
	jne	.L1086
	addl	$1, 16(%rcx)
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jle	.L484
.L1124:
	leal	-8(%rax), %ecx
	movl	32(%rbx), %eax
	movl	%ecx, 36(%rbx)
	shrl	%cl, %eax
	movzbl	%al, %edx
	movl	56(%rbx), %eax
	sall	$8, %eax
	orl	%eax, %edx
	testl	%edx, %edx
	movl	%edx, 56(%rbx)
	jns	.L1125
.L1068:
	movl	$-4, %eax
	jmp	.L593
.L375:
	movl	$42, 8(%rbx)
	.p2align 4,,7
.L1101:
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jg	.L1126
.L743:
	movq	(%rbx), %rcx
	movl	8(%rcx), %r9d
	testl	%r9d, %r9d
	je	.L1066
	movq	(%rcx), %rax
	movzbl	(%rax), %edx
	movl	32(%rbx), %eax
	addl	$8, 36(%rbx)
	addq	$1, (%rcx)
	sall	$8, %eax
	orl	%eax, %edx
	movl	%edx, 32(%rbx)
	addl	$1, 12(%rcx)
	movl	12(%rcx), %r8d
	subl	$1, 8(%rcx)
	testl	%r8d, %r8d
	jne	.L1101
	addl	$1, 16(%rcx)
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jle	.L743
.L1126:
	leal	-8(%rax), %ecx
	movl	32(%rbx), %eax
	movl	%ecx, 36(%rbx)
	shrl	%cl, %eax
	cmpb	$114, %al
	jne	.L1068
.L376:
	movl	$43, 8(%rbx)
.L1102:
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jg	.L1127
.L748:
	movq	(%rbx), %rcx
	movl	8(%rcx), %edi
	testl	%edi, %edi
	je	.L1066
	movq	(%rcx), %rax
	movzbl	(%rax), %edx
	movl	32(%rbx), %eax
	addl	$8, 36(%rbx)
	addq	$1, (%rcx)
	sall	$8, %eax
	orl	%eax, %edx
	movl	%edx, 32(%rbx)
	addl	$1, 12(%rcx)
	movl	12(%rcx), %esi
	subl	$1, 8(%rcx)
	testl	%esi, %esi
	jne	.L1102
	addl	$1, 16(%rcx)
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jle	.L748
.L1127:
	leal	-8(%rax), %ecx
	movl	32(%rbx), %eax
	movl	%ecx, 36(%rbx)
	shrl	%cl, %eax
	cmpb	$69, %al
	jne	.L1068
.L377:
	movl	$44, 8(%rbx)
.L1103:
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jg	.L1128
.L753:
	movq	(%rbx), %rcx
	movl	8(%rcx), %edx
	testl	%edx, %edx
	je	.L1066
	movq	(%rcx), %rax
	movzbl	(%rax), %edx
	movl	32(%rbx), %eax
	addl	$8, 36(%rbx)
	addq	$1, (%rcx)
	sall	$8, %eax
	orl	%eax, %edx
	movl	%edx, 32(%rbx)
	addl	$1, 12(%rcx)
	movl	12(%rcx), %eax
	subl	$1, 8(%rcx)
	testl	%eax, %eax
	jne	.L1103
	addl	$1, 16(%rcx)
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jle	.L753
.L1128:
	leal	-8(%rax), %ecx
	movl	32(%rbx), %eax
	movl	%ecx, 36(%rbx)
	shrl	%cl, %eax
	cmpb	$56, %al
	jne	.L1068
.L378:
	movl	$45, 8(%rbx)
.L1104:
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jg	.L1129
.L758:
	movq	(%rbx), %rcx
	movl	8(%rcx), %eax
	testl	%eax, %eax
	je	.L1066
	movq	(%rcx), %rax
	movzbl	(%rax), %edx
	movl	32(%rbx), %eax
	addl	$8, 36(%rbx)
	addq	$1, (%rcx)
	sall	$8, %eax
	orl	%eax, %edx
	movl	%edx, 32(%rbx)
	addl	$1, 12(%rcx)
	movl	12(%rcx), %eax
	subl	$1, 8(%rcx)
	testl	%eax, %eax
	jne	.L1104
	addl	$1, 16(%rcx)
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jle	.L758
.L1129:
	leal	-8(%rax), %ecx
	movl	32(%rbx), %eax
	movl	%ecx, 36(%rbx)
	shrl	%cl, %eax
	cmpb	$80, %al
	jne	.L1068
.L379:
	movl	$46, 8(%rbx)
.L1105:
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jg	.L1130
.L763:
	movq	(%rbx), %rcx
	movl	8(%rcx), %eax
	testl	%eax, %eax
	je	.L1066
	movq	(%rcx), %rax
	movzbl	(%rax), %edx
	movl	32(%rbx), %eax
	addl	$8, 36(%rbx)
	addq	$1, (%rcx)
	sall	$8, %eax
	orl	%eax, %edx
	movl	%edx, 32(%rbx)
	addl	$1, 12(%rcx)
	movl	12(%rcx), %eax
	subl	$1, 8(%rcx)
	testl	%eax, %eax
	jne	.L1105
	addl	$1, 16(%rcx)
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jle	.L763
.L1130:
	leal	-8(%rax), %ecx
	movl	32(%rbx), %eax
	movl	%ecx, 36(%rbx)
	shrl	%cl, %eax
	cmpb	$-112, %al
	jne	.L1068
	movl	$0, 3180(%rbx)
.L380:
	movl	$47, 8(%rbx)
.L1106:
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jg	.L849
.L1131:
	movq	(%rbx), %rcx
	movl	8(%rcx), %eax
	testl	%eax, %eax
	je	.L1066
	movq	(%rcx), %rax
	movzbl	(%rax), %edx
	movl	32(%rbx), %eax
	addl	$8, 36(%rbx)
	addq	$1, (%rcx)
	sall	$8, %eax
	orl	%eax, %edx
	movl	%edx, 32(%rbx)
	addl	$1, 12(%rcx)
	movl	12(%rcx), %ebp
	subl	$1, 8(%rcx)
	testl	%ebp, %ebp
	jne	.L1106
	addl	$1, 16(%rcx)
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jle	.L1131
.L849:
	movl	32(%rbx), %edx
	leal	-8(%rax), %ecx
	movl	3180(%rbx), %eax
	movl	%ecx, 36(%rbx)
	shrl	%cl, %edx
	sall	$8, %eax
	movzbl	%dl, %edx
	orl	%eax, %edx
	movl	%edx, 3180(%rbx)
.L381:
	movl	$48, 8(%rbx)
	.p2align 4,,7
.L1107:
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jg	.L850
.L1132:
	movq	(%rbx), %rcx
	movl	8(%rcx), %r11d
	testl	%r11d, %r11d
	je	.L1066
	movq	(%rcx), %rax
	movzbl	(%rax), %edx
	movl	32(%rbx), %eax
	addl	$8, 36(%rbx)
	addq	$1, (%rcx)
	sall	$8, %eax
	orl	%eax, %edx
	movl	%edx, 32(%rbx)
	addl	$1, 12(%rcx)
	movl	12(%rcx), %r10d
	subl	$1, 8(%rcx)
	testl	%r10d, %r10d
	jne	.L1107
	addl	$1, 16(%rcx)
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jle	.L1132
.L850:
	movl	32(%rbx), %edx
	leal	-8(%rax), %ecx
	movl	3180(%rbx), %eax
	movl	%ecx, 36(%rbx)
	shrl	%cl, %edx
	sall	$8, %eax
	movzbl	%dl, %edx
	orl	%eax, %edx
	movl	%edx, 3180(%rbx)
.L382:
	movl	$49, 8(%rbx)
	.p2align 4,,7
.L1108:
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jg	.L851
.L1133:
	movq	(%rbx), %rcx
	movl	8(%rcx), %r9d
	testl	%r9d, %r9d
	je	.L1066
	movq	(%rcx), %rax
	movzbl	(%rax), %edx
	movl	32(%rbx), %eax
	addl	$8, 36(%rbx)
	addq	$1, (%rcx)
	sall	$8, %eax
	orl	%eax, %edx
	movl	%edx, 32(%rbx)
	addl	$1, 12(%rcx)
	movl	12(%rcx), %r8d
	subl	$1, 8(%rcx)
	testl	%r8d, %r8d
	jne	.L1108
	addl	$1, 16(%rcx)
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jle	.L1133
.L851:
	movl	32(%rbx), %edx
	leal	-8(%rax), %ecx
	movl	3180(%rbx), %eax
	movl	%ecx, 36(%rbx)
	shrl	%cl, %edx
	sall	$8, %eax
	movzbl	%dl, %edx
	orl	%eax, %edx
	movl	%edx, 3180(%rbx)
.L383:
	movl	$50, 8(%rbx)
	.p2align 4,,7
.L1109:
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jg	.L852
.L1134:
	movq	(%rbx), %rcx
	movl	8(%rcx), %edi
	testl	%edi, %edi
	je	.L1066
	movq	(%rcx), %rax
	movzbl	(%rax), %edx
	movl	32(%rbx), %eax
	addl	$8, 36(%rbx)
	addq	$1, (%rcx)
	sall	$8, %eax
	orl	%eax, %edx
	movl	%edx, 32(%rbx)
	addl	$1, 12(%rcx)
	movl	12(%rcx), %esi
	subl	$1, 8(%rcx)
	testl	%esi, %esi
	jne	.L1109
	addl	$1, 16(%rcx)
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jle	.L1134
.L852:
	movl	32(%rbx), %edx
	leal	-8(%rax), %ecx
	movl	3180(%rbx), %eax
	movl	$1, 8(%rbx)
	movl	%ecx, 36(%rbx)
	shrl	%cl, %edx
	sall	$8, %eax
	movzbl	%dl, %edx
	orl	%eax, %edx
	movl	$4, %eax
	movl	%edx, 3180(%rbx)
	jmp	.L593
.L533:
	movl	$0, 124(%rsp)
.L993:
	movl	$32, 8(%rbx)
	.p2align 4,,7
.L1091:
	movl	36(%rbx), %eax
	testl	%eax, %eax
	jg	.L1135
.L535:
	movq	(%rbx), %rcx
	movl	8(%rcx), %eax
	testl	%eax, %eax
	je	.L1066
	movq	(%rcx), %rax
	movzbl	(%rax), %edx
	movl	32(%rbx), %eax
	addl	$8, 36(%rbx)
	addq	$1, (%rcx)
	sall	$8, %eax
	orl	%eax, %edx
	movl	%edx, 32(%rbx)
	addl	$1, 12(%rcx)
	movl	12(%rcx), %eax
	subl	$1, 8(%rcx)
	testl	%eax, %eax
	jne	.L1091
	addl	$1, 16(%rcx)
	movl	36(%rbx), %eax
	testl	%eax, %eax
	jle	.L535
.L1135:
	leal	-1(%rax), %ecx
	movl	32(%rbx), %eax
	movl	%ecx, 36(%rbx)
	shrl	%cl, %eax
	testb	$1, %al
	je	.L1136
	addl	$1, 124(%rsp)
	movl	44(%rsp), %eax
	cmpl	%eax, 124(%rsp)
	jl	.L993
	jmp	.L1068
.L1125:
	movl	40(%rbx), %r11d
	imull	$100000, %r11d, %eax
	addl	$10, %eax
	cmpl	%eax, %edx
	jg	.L1068
	xorl	%r12d, %r12d
.L937:
	movl	$28, 8(%rbx)
	.p2align 4,,7
.L1087:
	movl	36(%rbx), %eax
	testl	%eax, %eax
	jg	.L1137
.L492:
	movq	(%rbx), %rcx
	movl	8(%rcx), %r10d
	testl	%r10d, %r10d
	je	.L1066
	movq	(%rcx), %rax
	movzbl	(%rax), %edx
	movl	32(%rbx), %eax
	addl	$8, 36(%rbx)
	addq	$1, (%rcx)
	sall	$8, %eax
	orl	%eax, %edx
	movl	%edx, 32(%rbx)
	addl	$1, 12(%rcx)
	movl	12(%rcx), %r9d
	subl	$1, 8(%rcx)
	testl	%r9d, %r9d
	jne	.L1087
	addl	$1, 16(%rcx)
	movl	36(%rbx), %eax
	testl	%eax, %eax
	jle	.L492
.L1137:
	leal	-1(%rax), %ecx
	movl	32(%rbx), %eax
	movl	%ecx, 36(%rbx)
	shrl	%cl, %eax
	testb	$1, %al
	jne	.L1138
	movslq	%r12d,%rax
	movb	$0, 3452(%rax,%rbx)
.L498:
	addl	$1, %r12d
	cmpl	$15, %r12d
	jle	.L937
	xorl	%eax, %eax
	.p2align 4,,7
.L500:
	movb	$0, 3196(%rax,%rbx)
	addq	$1, %rax
	cmpq	$256, %rax
	jne	.L500
	xorl	%r12d, %r12d
.L503:
	movslq	%r12d,%rax
	cmpb	$0, 3452(%rax,%rbx)
	je	.L504
	movl	$0, 124(%rsp)
.L938:
	movl	$29, 8(%rbx)
.L1088:
	movl	36(%rbx), %eax
	testl	%eax, %eax
	jg	.L1139
.L508:
	movq	(%rbx), %rcx
	movl	8(%rcx), %r8d
	testl	%r8d, %r8d
	je	.L1066
	movq	(%rcx), %rax
	movzbl	(%rax), %edx
	movl	32(%rbx), %eax
	addl	$8, 36(%rbx)
	addq	$1, (%rcx)
	sall	$8, %eax
	orl	%eax, %edx
	movl	%edx, 32(%rbx)
	addl	$1, 12(%rcx)
	movl	12(%rcx), %edi
	subl	$1, 8(%rcx)
	testl	%edi, %edi
	jne	.L1088
	addl	$1, 16(%rcx)
	movl	36(%rbx), %eax
	testl	%eax, %eax
	jle	.L508
.L1139:
	leal	-1(%rax), %ecx
	movl	32(%rbx), %eax
	movl	%ecx, 36(%rbx)
	shrl	%cl, %eax
	testb	$1, %al
	jne	.L1140
.L511:
	addl	$1, 124(%rsp)
	cmpl	$15, 124(%rsp)
	jle	.L938
.L504:
	addl	$1, %r12d
	cmpl	$15, %r12d
	jle	.L503
	movl	$0, 3192(%rbx)
	xorl	%edx, %edx
.L515:
	cmpb	$0, 3196(%rdx,%rbx)
	je	.L516
	movslq	3192(%rbx),%rax
	addl	$1, 3192(%rbx)
	movb	%dl, 3468(%rax,%rbx)
.L516:
	addq	$1, %rdx
	cmpq	$256, %rdx
	jne	.L515
	movl	3192(%rbx), %eax
	testl	%eax, %eax
	je	.L1068
	addl	$2, %eax
	movl	%eax, 40(%rsp)
.L363:
	movl	$30, 8(%rbx)
.L1089:
	movl	36(%rbx), %eax
	cmpl	$2, %eax
	jg	.L1141
.L521:
	movq	(%rbx), %rcx
	movl	8(%rcx), %esi
	testl	%esi, %esi
	je	.L1066
	movq	(%rcx), %rax
	movzbl	(%rax), %edx
	movl	32(%rbx), %eax
	addl	$8, 36(%rbx)
	addq	$1, (%rcx)
	sall	$8, %eax
	orl	%eax, %edx
	movl	%edx, 32(%rbx)
	addl	$1, 12(%rcx)
	movl	12(%rcx), %edx
	subl	$1, 8(%rcx)
	testl	%edx, %edx
	jne	.L1089
	addl	$1, 16(%rcx)
	movl	36(%rbx), %eax
	cmpl	$2, %eax
	jle	.L521
.L1141:
	leal	-3(%rax), %ecx
	movl	32(%rbx), %eax
	movl	%ecx, 36(%rbx)
	shrl	%cl, %eax
	andl	$7, %eax
	cmpl	$1, %eax
	movl	%eax, 44(%rsp)
	jle	.L1068
	cmpl	$6, %eax
	jg	.L1068
.L364:
	movl	$31, 8(%rbx)
.L1090:
	movl	36(%rbx), %eax
	cmpl	$14, %eax
	jg	.L1142
.L527:
	movq	(%rbx), %rcx
	movl	8(%rcx), %eax
	testl	%eax, %eax
	je	.L1066
	movq	(%rcx), %rax
	movzbl	(%rax), %edx
	movl	32(%rbx), %eax
	addl	$8, 36(%rbx)
	addq	$1, (%rcx)
	sall	$8, %eax
	orl	%eax, %edx
	movl	%edx, 32(%rbx)
	addl	$1, 12(%rcx)
	movl	12(%rcx), %eax
	subl	$1, 8(%rcx)
	testl	%eax, %eax
	jne	.L1090
	addl	$1, 16(%rcx)
	movl	36(%rbx), %eax
	cmpl	$14, %eax
	jle	.L527
.L1142:
	leal	-15(%rax), %ecx
	movl	32(%rbx), %eax
	movl	%ecx, 36(%rbx)
	shrl	%cl, %eax
	andl	$32767, %eax
	movl	%eax, 48(%rsp)
	jle	.L1068
	xorl	%r12d, %r12d
.L530:
	cmpl	48(%rsp), %r12d
	jl	.L533
	movl	44(%rsp), %eax
	testl	%eax, %eax
	jle	.L542
	xorl	%edx, %edx
	xorl	%eax, %eax
.L544:
	cltq
	movb	%dl, 128(%rsp,%rax)
	addl	$1, %edx
	movzbl	%dl, %eax
	cmpl	%eax, 44(%rsp)
	jg	.L544
.L542:
	movl	48(%rsp), %eax
	xorl	%r12d, %r12d
	movl	$0, 36(%rsp)
	testl	%eax, %eax
	jle	.L547
	movq	%rbx, %rsi
	xorl	%r8d, %r8d
.L548:
	movzbl	25886(%rsi), %ecx
	movzbl	%cl, %eax
	testb	%cl, %cl
	movzbl	128(%rsp,%rax), %edi
	je	.L549
	.p2align 4,,7
.L939:
	movzbl	%cl, %eax
	movslq	%eax,%rdx
	subl	$1, %eax
	subb	$1, %cl
	cltq
	movzbl	128(%rsp,%rax), %eax
	movb	%al, 128(%rsp,%rdx)
	jne	.L939
.L549:
	movb	%dil, 7884(%rsi)
	addl	$1, %r8d
	addq	$1, %rsi
	cmpl	48(%rsp), %r8d
	je	.L837
	movb	%dil, 128(%rsp)
	jmp	.L548
.L366:
	movl	$33, 8(%rbx)
.L1092:
	movl	36(%rbx), %eax
	cmpl	$4, %eax
	jg	.L838
.L1143:
	movq	(%rbx), %rcx
	movl	8(%rcx), %ebp
	testl	%ebp, %ebp
	je	.L1066
	movq	(%rcx), %rax
	movzbl	(%rax), %edx
	movl	32(%rbx), %eax
	addl	$8, 36(%rbx)
	addq	$1, (%rcx)
	sall	$8, %eax
	orl	%eax, %edx
	movl	%edx, 32(%rbx)
	addl	$1, 12(%rcx)
	movl	12(%rcx), %r11d
	subl	$1, 8(%rcx)
	testl	%r11d, %r11d
	jne	.L1092
	addl	$1, 16(%rcx)
	movl	36(%rbx), %eax
	cmpl	$4, %eax
	jle	.L1143
.L838:
	movl	32(%rbx), %esi
	leal	-5(%rax), %ecx
	xorl	%r12d, %r12d
	movl	%ecx, 36(%rbx)
	shrl	%cl, %esi
	andl	$31, %esi
	movl	%esi, 80(%rsp)
.L556:
	cmpl	40(%rsp), %r12d
	jl	.L559
	addl	$1, 36(%rsp)
.L547:
	movl	44(%rsp), %esi
	cmpl	%esi, 36(%rsp)
	jl	.L366
	xorl	%r10d, %r10d
	xorl	%r12d, %r12d
	testl	%esi, %esi
	movl	$0, 36(%rsp)
	jle	.L578
.L579:
	movl	40(%rsp), %edx
	xorl	%r9d, %r9d
	movl	$32, %ebp
	testl	%edx, %edx
	jle	.L581
	movslq	%r10d,%rax
	movq	%rax, %rdx
	salq	$8, %rdx
	leaq	43888(%rdx,%rax,2), %rax
	xorl	%edx, %edx
	leaq	(%rbx,%rax), %rcx
.L580:
	movzbl	(%rcx), %eax
	cmpl	%eax, %r9d
	cmovl	%eax, %r9d
	cmpl	%eax, %ebp
	cmovg	%eax, %ebp
	addl	$1, %edx
	addq	$1, %rcx
	cmpl	40(%rsp), %edx
	jne	.L580
.L581:
	movq	%r12, %rax
	movl	%ebp, %r8d
	movl	%r10d, 16(%rsp)
	salq	$8, %rax
	leaq	43888(%rax,%r12,2), %rcx
	movq	%r12, %rax
	salq	$10, %rax
	leaq	(%rax,%r12,8), %rdi
	movl	40(%rsp), %eax
	leaq	(%rbx,%rcx), %rcx
	leaq	57820(%rdi,%rbx), %rdx
	leaq	51628(%rdi,%rbx), %rsi
	leaq	45436(%rdi,%rbx), %rdi
	movl	%eax, (%rsp)
	call	BZ2_hbCreateDecodeTables
	movl	16(%rsp), %r10d
	movl	%ebp, 64012(%rbx,%r12,4)
	addq	$1, %r12
	addl	$1, %r10d
	cmpl	44(%rsp), %r10d
	jne	.L579
	movl	44(%rsp), %edx
	movl	%edx, 36(%rsp)
.L578:
	movl	40(%rbx), %eax
	movl	3192(%rbx), %ecx
	imull	$100000, %eax, %esi
	addl	$1, %ecx
	xorl	%eax, %eax
	movl	%ecx, 52(%rsp)
	movl	%esi, 68(%rsp)
.L585:
	movl	$0, 68(%rbx,%rax,4)
	addq	$1, %rax
	cmpq	$256, %rax
	jne	.L585
	movl	$15, %r8d
	movl	$4080, %edi
.L589:
	leal	15(%rdi), %ecx
	movl	$15, %esi
.L587:
	leal	(%rsi,%rdi), %eax
	subl	$1, %esi
	movslq	%ecx,%rdx
	subl	$1, %ecx
	cmpl	$-1, %esi
	movb	%al, 3724(%rdx,%rbx)
	jne	.L587
	movslq	%r8d,%rax
	subl	$1, %r8d
	movl	%edi, 7820(%rbx,%rax,4)
	subl	$16, %edi
	cmpl	%esi, %r8d
	jne	.L589
	movl	48(%rsp), %eax
	testl	%eax, %eax
	jle	.L839
	movzbl	7884(%rbx), %eax
	movl	$256, %r12d
	movl	$0, 56(%rsp)
	movl	$49, 60(%rsp)
	movl	$0, 72(%rsp)
	movl	%eax, 88(%rsp)
	cltq
	movl	64012(%rbx,%rax,4), %edx
	movl	%edx, 92(%rsp)
	movq	%rax, %rdx
	movl	92(%rsp), %r14d
	salq	$10, %rdx
	leaq	(%rdx,%rax,8), %rax
	leaq	45436(%rax,%rbx), %rcx
	leaq	57820(%rax,%rbx), %rsi
	leaq	51628(%rax,%rbx), %rax
	movq	%rcx, 96(%rsp)
	movq	%rsi, 112(%rsp)
	movq	%rax, 104(%rsp)
.L369:
	movl	$36, 8(%rbx)
.L1095:
	movl	36(%rbx), %eax
	cmpl	%eax, %r14d
	jle	.L840
.L1144:
	movq	(%rbx), %rcx
	movl	8(%rcx), %eax
	testl	%eax, %eax
	je	.L1066
	movq	(%rcx), %rax
	movzbl	(%rax), %edx
	movl	32(%rbx), %eax
	addl	$8, 36(%rbx)
	addq	$1, (%rcx)
	sall	$8, %eax
	orl	%eax, %edx
	movl	%edx, 32(%rbx)
	addl	$1, 12(%rcx)
	movl	12(%rcx), %eax
	subl	$1, 8(%rcx)
	testl	%eax, %eax
	jne	.L1095
	addl	$1, 16(%rcx)
	movl	36(%rbx), %eax
	cmpl	%eax, %r14d
	jg	.L1144
.L840:
	movl	%eax, %edx
	movl	32(%rbx), %r13d
	movl	$1, %eax
	subl	%r14d, %edx
	movl	%edx, %ecx
	movl	%edx, 36(%rbx)
	shrl	%cl, %r13d
	movl	%r14d, %ecx
	sall	%cl, %eax
	subl	$1, %eax
	andl	%eax, %r13d
.L597:
	cmpl	$20, %r14d
	jg	.L1068
	movq	96(%rsp), %rcx
	movslq	%r14d,%rax
	leaq	0(,%rax,4), %rdx
	cmpl	(%rcx,%rax,4), %r13d
	jle	.L692
	addl	$1, %r14d
.L370:
	movl	$37, 8(%rbx)
.L1096:
	movl	36(%rbx), %eax
	testl	%eax, %eax
	jg	.L841
.L1145:
	movq	(%rbx), %rcx
	movl	8(%rcx), %eax
	testl	%eax, %eax
	je	.L1066
	movq	(%rcx), %rax
	movzbl	(%rax), %edx
	movl	32(%rbx), %eax
	addl	$8, 36(%rbx)
	addq	$1, (%rcx)
	sall	$8, %eax
	orl	%eax, %edx
	movl	%edx, 32(%rbx)
	addl	$1, 12(%rcx)
	movl	12(%rcx), %eax
	subl	$1, 8(%rcx)
	testl	%eax, %eax
	jne	.L1096
	addl	$1, 16(%rcx)
	movl	36(%rbx), %eax
	testl	%eax, %eax
	jle	.L1145
.L841:
	movl	32(%rbx), %r15d
	leal	-1(%rax), %ecx
	addl	%r13d, %r13d
	movl	%ecx, 36(%rbx)
	shrl	%cl, %r15d
	andl	$1, %r15d
	orl	%r15d, %r13d
	jmp	.L597
.L843:
	movl	32(%rbx), %r15d
	leal	-1(%rax), %ecx
	addl	%r13d, %r13d
	movl	%ecx, 36(%rbx)
	shrl	%cl, %r15d
	andl	$1, %r15d
	orl	%r15d, %r13d
.L627:
	cmpl	$20, %r14d
	jg	.L1068
	movq	96(%rsp), %rcx
	movslq	%r14d,%rax
	leaq	0(,%rax,4), %rdx
	cmpl	(%rcx,%rax,4), %r13d
	jle	.L631
	addl	$1, %r14d
.L372:
	movl	$39, 8(%rbx)
.L1098:
	movl	36(%rbx), %eax
	testl	%eax, %eax
	jg	.L843
.L1146:
	movq	(%rbx), %rcx
	movl	8(%rcx), %edi
	testl	%edi, %edi
	je	.L1066
	movq	(%rcx), %rax
	movzbl	(%rax), %edx
	movl	32(%rbx), %eax
	addl	$8, 36(%rbx)
	addq	$1, (%rcx)
	sall	$8, %eax
	orl	%eax, %edx
	movl	%edx, 32(%rbx)
	addl	$1, 12(%rcx)
	movl	12(%rcx), %esi
	subl	$1, 8(%rcx)
	testl	%esi, %esi
	jne	.L1098
	addl	$1, 16(%rcx)
	movl	36(%rbx), %eax
	testl	%eax, %eax
	jle	.L1146
	jmp	.L843
.L343:
	movl	$10, 8(%rbx)
.L1069:
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jg	.L1147
.L386:
	movl	8(%rbp), %esi
	testl	%esi, %esi
	je	.L1066
	movq	(%rbp), %rax
	movzbl	(%rax), %edx
	movl	32(%rbx), %eax
	addl	$8, 36(%rbx)
	addq	$1, (%rbp)
	sall	$8, %eax
	orl	%eax, %edx
	movl	%edx, 32(%rbx)
	addl	$1, 12(%rbp)
	movl	12(%rbp), %ecx
	subl	$1, 8(%rbp)
	testl	%ecx, %ecx
	jne	.L1069
	addl	$1, 16(%rbp)
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jle	.L386
.L1147:
	leal	-8(%rax), %ecx
	movl	32(%rbx), %eax
	movl	%ecx, 36(%rbx)
	shrl	%cl, %eax
	cmpb	$66, %al
	jne	.L388
.L344:
	movl	$11, 8(%rbx)
.L1070:
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jg	.L1148
.L393:
	movl	8(%rbp), %edx
	testl	%edx, %edx
	je	.L1066
	movq	(%rbp), %rax
	movzbl	(%rax), %edx
	movl	32(%rbx), %eax
	addl	$8, 36(%rbx)
	addq	$1, (%rbp)
	sall	$8, %eax
	orl	%eax, %edx
	movl	%edx, 32(%rbx)
	addl	$1, 12(%rbp)
	movl	12(%rbp), %eax
	subl	$1, 8(%rbp)
	testl	%eax, %eax
	jne	.L1070
	addl	$1, 16(%rbp)
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jle	.L393
.L1148:
	leal	-8(%rax), %ecx
	movl	32(%rbx), %eax
	movl	%ecx, 36(%rbx)
	shrl	%cl, %eax
	cmpb	$90, %al
	jne	.L388
.L345:
	movl	$12, 8(%rbx)
.L1071:
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jg	.L1149
.L398:
	movl	8(%rbp), %eax
	testl	%eax, %eax
	je	.L1066
	movq	(%rbp), %rax
	movzbl	(%rax), %edx
	movl	32(%rbx), %eax
	addl	$8, 36(%rbx)
	addq	$1, (%rbp)
	sall	$8, %eax
	orl	%eax, %edx
	movl	%edx, 32(%rbx)
	addl	$1, 12(%rbp)
	movl	12(%rbp), %eax
	subl	$1, 8(%rbp)
	testl	%eax, %eax
	jne	.L1071
	addl	$1, 16(%rbp)
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jle	.L398
.L1149:
	leal	-8(%rax), %ecx
	movl	32(%rbx), %eax
	movl	%ecx, 36(%rbx)
	shrl	%cl, %eax
	cmpb	$104, %al
	jne	.L388
.L346:
	movl	$13, 8(%rbx)
.L1072:
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jg	.L1150
.L403:
	movl	8(%rbp), %eax
	testl	%eax, %eax
	je	.L1066
	movq	(%rbp), %rax
	movzbl	(%rax), %edx
	movl	32(%rbx), %eax
	addl	$8, 36(%rbx)
	addq	$1, (%rbp)
	sall	$8, %eax
	orl	%eax, %edx
	movl	%edx, 32(%rbx)
	addl	$1, 12(%rbp)
	movl	12(%rbp), %eax
	subl	$1, 8(%rbp)
	testl	%eax, %eax
	jne	.L1072
	addl	$1, 16(%rbp)
	movl	36(%rbx), %eax
	cmpl	$7, %eax
	jle	.L403
.L1150:
	leal	-8(%rax), %ecx
	movl	32(%rbx), %eax
	movl	%ecx, 36(%rbx)
	shrl	%cl, %eax
	andl	$255, %eax
	cmpl	$48, %eax
	movl	%eax, 40(%rbx)
	jg	.L1151
.L389:
.L388:
	movl	$-5, %eax
	jmp	.L593
.L368:
	movl	$35, 8(%rbx)
.L1094:
	movl	36(%rbx), %eax
	testl	%eax, %eax
	jg	.L1152
.L568:
	movq	(%rbx), %rcx
	movl	8(%rcx), %edi
	testl	%edi, %edi
	je	.L1066
	movq	(%rcx), %rax
	movzbl	(%rax), %edx
	movl	32(%rbx), %eax
	addl	$8, 36(%rbx)
	addq	$1, (%rcx)
	sall	$8, %eax
	orl	%eax, %edx
	movl	%edx, 32(%rbx)
	addl	$1, 12(%rcx)
	movl	12(%rcx), %esi
	subl	$1, 8(%rcx)
	testl	%esi, %esi
	jne	.L1094
	addl	$1, 16(%rcx)
	movl	36(%rbx), %eax
	testl	%eax, %eax
	jle	.L568
.L1152:
	leal	-1(%rax), %ecx
	movl	32(%rbx), %eax
	movl	%ecx, 36(%rbx)
	shrl	%cl, %eax
	testb	$1, %al
	jne	.L1153
	addl	$1, 80(%rsp)
.L559:
	movl	80(%rsp), %r10d
	testl	%r10d, %r10d
	jle	.L1068
	cmpl	$20, 80(%rsp)
	jg	.L1068
.L367:
	movl	$34, 8(%rbx)
.L1093:
	movl	36(%rbx), %eax
	testl	%eax, %eax
	jg	.L1154
.L562:
	movq	(%rbx), %rcx
	movl	8(%rcx), %r9d
	testl	%r9d, %r9d
	je	.L1066
	movq	(%rcx), %rax
	movzbl	(%rax), %edx
	movl	32(%rbx), %eax
	addl	$8, 36(%rbx)
	addq	$1, (%rcx)
	sall	$8, %eax
	orl	%eax, %edx
	movl	%edx, 32(%rbx)
	addl	$1, 12(%rcx)
	movl	12(%rcx), %r8d
	subl	$1, 8(%rcx)
	testl	%r8d, %r8d
	jne	.L1093
	addl	$1, 16(%rcx)
	movl	36(%rbx), %eax
	testl	%eax, %eax
	jle	.L562
.L1154:
	leal	-1(%rax), %ecx
	movl	32(%rbx), %eax
	movl	%ecx, 36(%rbx)
	shrl	%cl, %eax
	testb	$1, %al
	jne	.L368
	movslq	36(%rsp),%rdx
	movslq	%r12d,%rax
	addl	$1, %r12d
	movq	%rdx, %rcx
	salq	$8, %rcx
	leaq	(%rcx,%rdx,2), %rdx
	movzbl	80(%rsp), %ecx
	addq	%rbx, %rdx
	movb	%cl, 43888(%rdx,%rax)
	jmp	.L556
.L664:
	movslq	%r8d,%rax
	movb	%r10b, 3724(%rax,%rbx)
.L668:
	movzbl	%r10b, %esi
	movzbl	3468(%rsi,%rbx), %edx
	movslq	%edx,%rax
	addl	$1, 68(%rbx,%rax,4)
	cmpb	$0, 44(%rbx)
	je	.L678
	movslq	72(%rsp),%rdx
	movzbw	3468(%rsi,%rbx), %ax
	movq	3160(%rbx), %rcx
	movw	%ax, (%rcx,%rdx,2)
.L680:
	movl	60(%rsp), %eax
	addl	$1, 72(%rsp)
	testl	%eax, %eax
	je	.L681
	subl	$1, 60(%rsp)
.L683:
	movl	92(%rsp), %r14d
.L373:
	movl	$40, 8(%rbx)
.L1099:
	movl	36(%rbx), %eax
	cmpl	%eax, %r14d
	jle	.L846
.L1155:
	movq	(%rbx), %rcx
	movl	8(%rcx), %eax
	testl	%eax, %eax
	je	.L1066
	movq	(%rcx), %rax
	movzbl	(%rax), %edx
	movl	32(%rbx), %eax
	addl	$8, 36(%rbx)
	addq	$1, (%rcx)
	sall	$8, %eax
	orl	%eax, %edx
	movl	%edx, 32(%rbx)
	addl	$1, 12(%rcx)
	movl	12(%rcx), %eax
	subl	$1, 8(%rcx)
	testl	%eax, %eax
	jne	.L1099
	addl	$1, 16(%rcx)
	movl	36(%rbx), %eax
	cmpl	%eax, %r14d
	jg	.L1155
.L846:
	movl	%eax, %edx
	movl	32(%rbx), %r13d
	movl	$1, %eax
	subl	%r14d, %edx
	movl	%edx, %ecx
	movl	%edx, 36(%rbx)
	shrl	%cl, %r13d
	movl	%r14d, %ecx
	sall	%cl, %eax
	subl	$1, %eax
	andl	%eax, %r13d
.L688:
	cmpl	$20, %r14d
	jg	.L1068
	movq	96(%rsp), %rcx
	movslq	%r14d,%rax
	leaq	0(,%rax,4), %rdx
	cmpl	(%rcx,%rax,4), %r13d
	jle	.L692
	addl	$1, %r14d
.L374:
	movl	$41, 8(%rbx)
.L1100:
	movl	36(%rbx), %eax
	testl	%eax, %eax
	jg	.L847
.L1156:
	movq	(%rbx), %rcx
	movl	8(%rcx), %eax
	testl	%eax, %eax
	je	.L1066
	movq	(%rcx), %rax
	movzbl	(%rax), %edx
	movl	32(%rbx), %eax
	addl	$8, 36(%rbx)
	addq	$1, (%rcx)
	sall	$8, %eax
	orl	%eax, %edx
	movl	%edx, 32(%rbx)
	addl	$1, 12(%rcx)
	movl	12(%rcx), %eax
	subl	$1, 8(%rcx)
	testl	%eax, %eax
	jne	.L1100
	addl	$1, 16(%rcx)
	movl	36(%rbx), %eax
	testl	%eax, %eax
	jle	.L1156
.L847:
	movl	32(%rbx), %r15d
	leal	-1(%rax), %ecx
	addl	%r13d, %r13d
	movl	%ecx, 36(%rbx)
	shrl	%cl, %r15d
	andl	$1, %r15d
	orl	%r15d, %r13d
	jmp	.L688
.L692:
	movq	104(%rsp), %rsi
	movl	%r13d, %eax
	subl	(%rsi,%rdx), %eax
	js	.L1068
	cmpl	$257, %eax
	jg	.L1068
	movq	112(%rsp), %rdx
	cltq
	movl	(%rdx,%rax,4), %eax
	movl	%eax, 64(%rsp)
.L610:
	movl	52(%rsp), %ecx
	cmpl	%ecx, 64(%rsp)
	je	.L1157
	movl	64(%rsp), %ebp
	testl	%ebp, %ebp
	je	.L612
	cmpl	$1, 64(%rsp)
	jne	.L614
.L612:
	movl	64(%rsp), %r11d
	movl	$-1, 76(%rsp)
	movl	$1, %eax
	movl	$2, 120(%rsp)
	testl	%r11d, %r11d
	jne	.L616
.L853:
	addl	%eax, 76(%rsp)
.L618:
	movl	60(%rsp), %r10d
	testl	%r10d, %r10d
	je	.L620
	subl	$1, 60(%rsp)
.L622:
	movl	92(%rsp), %r14d
.L371:
	movl	$38, 8(%rbx)
.L1097:
	movl	36(%rbx), %eax
	cmpl	%eax, %r14d
	jle	.L842
.L1158:
	movq	(%rbx), %rcx
	movl	8(%rcx), %r9d
	testl	%r9d, %r9d
	je	.L1066
	movq	(%rcx), %rax
	movzbl	(%rax), %edx
	movl	32(%rbx), %eax
	addl	$8, 36(%rbx)
	addq	$1, (%rcx)
	sall	$8, %eax
	orl	%eax, %edx
	movl	%edx, 32(%rbx)
	addl	$1, 12(%rcx)
	movl	12(%rcx), %r8d
	subl	$1, 8(%rcx)
	testl	%r8d, %r8d
	jne	.L1097
	addl	$1, 16(%rcx)
	movl	36(%rbx), %eax
	cmpl	%eax, %r14d
	jg	.L1158
.L842:
	movl	%eax, %edx
	movl	32(%rbx), %r13d
	movl	$1, %eax
	subl	%r14d, %edx
	movl	%edx, %ecx
	movl	%edx, 36(%rbx)
	shrl	%cl, %r13d
	movl	%r14d, %ecx
	sall	%cl, %eax
	subl	$1, %eax
	andl	%eax, %r13d
	jmp	.L627
.L848:
	mov	60(%rbx), %eax
	movl	$0, 24(%rbx)
	movl	$0, 28(%rbx)
	movl	(%rsi,%rax,4), %eax
	movl	$1, 1092(%rbx)
	movl	64(%rsp), %esi
	movl	%eax, 60(%rbx)
	movzbl	%al, %eax
	shrl	$8, 60(%rbx)
	movl	%eax, 64(%rbx)
	movl	BZ2_rNums(%rip), %eax
	movl	%esi, 52(%rsp)
	movl	$1, 28(%rbx)
	subl	$1, %eax
	movl	%eax, 24(%rbx)
	subl	$1, %eax
	sete	%al
	movzbl	%al, %eax
	xorl	%eax, 64(%rbx)
	.p2align 4,,7
.L1066:
	xorl	%eax, %eax
	jmp	.L593
.L1153:
	subl	$1, 80(%rsp)
	jmp	.L559
.L837:
	movl	48(%rsp), %r12d
	movl	$0, 36(%rsp)
	jmp	.L547
.L1138:
	movslq	%r12d,%rax
	movb	$1, 3452(%rax,%rbx)
	jmp	.L498
.L1140:
	movl	%r12d, %eax
	sall	$4, %eax
	addl	124(%rsp), %eax
	cltq
	movb	$1, 3196(%rax,%rbx)
	jmp	.L511
.L1136:
	movzbl	124(%rsp), %edx
	movslq	%r12d,%rax
	addl	$1, %r12d
	movb	%dl, 25886(%rax,%rbx)
	jmp	.L530
.L1151:
	cmpl	$57, %eax
	jg	.L388
	subl	$48, %eax
	cmpb	$0, 44(%rbx)
	movl	%eax, 40(%rbx)
	je	.L409
	movslq	%eax,%rsi
	movq	72(%rbp), %rdi
	movl	$1, %edx
	imulq	$200000, %rsi, %rsi
	call	*56(%rbp)
	movq	%rax, 3160(%rbx)
	movl	40(%rbx), %eax
	movl	$1, %edx
	movq	72(%rbp), %rdi
	imull	$100000, %eax, %esi
	sarl	%esi
	call	*56(%rbp)
	cmpq	$0, 3160(%rbx)
	movq	%rax, 3168(%rbx)
	jne	.L1159
.L411:
	movl	$-3, %eax
	jmp	.L593
.L631:
	movq	104(%rsp), %rsi
	movl	%r13d, %eax
	subl	(%rsi,%rdx), %eax
	js	.L1068
	cmpl	$257, %eax
	jg	.L1068
	movq	112(%rsp), %rdx
	cltq
	movl	(%rdx,%rax,4), %eax
	testl	%eax, %eax
	movl	%eax, 64(%rsp)
	jne	.L640
	movl	120(%rsp), %eax
	sall	120(%rsp)
	jmp	.L853
	.p2align 4,,7
.L409:
	movslq	%eax,%rsi
	movq	72(%rbp), %rdi
	movl	$1, %edx
	imulq	$400000, %rsi, %rsi
	call	*56(%rbp)
	testq	%rax, %rax
	movq	%rax, 3152(%rbx)
	jne	.L347
	jmp	.L411
.L839:
	movl	$-4, %eax
	movl	$256, %r12d
	movl	$0, 56(%rsp)
	movl	$0, 60(%rsp)
	movl	$0, 72(%rsp)
	jmp	.L593
.L1157:
	movl	56(%rbx), %eax
	testl	%eax, %eax
	jns	.L1160
.L702:
	movl	64(%rsp), %ecx
	movl	%ecx, 52(%rsp)
	jmp	.L1068
.L614:
	movl	72(%rsp), %ecx
	cmpl	%ecx, 68(%rsp)
	jle	.L1068
	movl	64(%rsp), %esi
	subl	$1, %esi
	cmpl	$15, %esi
	ja	.L659
	movl	7820(%rbx), %r8d
	cmpl	$3, %esi
	leal	(%rsi,%r8), %eax
	movzbl	3724(%rax,%rbx), %r10d
	jbe	.L661
	movl	64(%rsp), %eax
	leal	-2(%r8,%rax), %edi
.L663:
	movslq	%edi,%rcx
	leal	1(%rdi), %eax
	subl	$4, %esi
	movzbl	3724(%rcx,%rbx), %edx
	cltq
	movb	%dl, 3724(%rax,%rbx)
	leal	-1(%rdi), %edx
	movslq	%edx,%rdx
	movzbl	3724(%rdx,%rbx), %eax
	movb	%al, 3724(%rcx,%rbx)
	leal	-2(%rdi), %ecx
	movslq	%ecx,%rcx
	movzbl	3724(%rcx,%rbx), %eax
	movb	%al, 3724(%rdx,%rbx)
	leal	-3(%rdi), %eax
	subl	$4, %edi
	cmpl	$3, %esi
	cltq
	movzbl	3724(%rax,%rbx), %eax
	movb	%al, 3724(%rcx,%rbx)
	ja	.L663
.L661:
	testl	%esi, %esi
	je	.L664
	leal	(%rsi,%r8), %eax
.L666:
	leal	-1(%rax), %ecx
	subl	$1, %esi
	mov	%eax, %eax
	mov	%ecx, %edx
	movzbl	3724(%rdx,%rbx), %edx
	movb	%dl, 3724(%rax,%rbx)
	je	.L664
	movl	%ecx, %eax
	jmp	.L666
.L1160:
	cmpl	%eax, 72(%rsp)
	jle	.L702
	cmpl	$1, 52(%rbx)
	movl	$0, 16(%rbx)
	movb	$0, 12(%rbx)
	movl	$-1, 3184(%rbx)
	movl	$2, 8(%rbx)
	jle	.L705
	movq	stderr(%rip), %rcx
	movl	$6, %edx
	movl	$1, %esi
	movl	$.LC13, %edi
	call	fwrite
.L705:
	movl	$0, 1096(%rbx)
	xorl	%edx, %edx
.L707:
	movl	68(%rbx,%rdx,4), %eax
	movl	%eax, 1100(%rbx,%rdx,4)
	addq	$1, %rdx
	cmpq	$256, %rdx
	jne	.L707
	xorw	%dx, %dx
.L709:
	movl	1096(%rbx,%rdx,4), %eax
	addl	%eax, 1100(%rbx,%rdx,4)
	addq	$1, %rdx
	cmpq	$256, %rdx
	jne	.L709
	xorw	%dx, %dx
	cmpb	$0, 44(%rbx)
	jne	.L716
	movl	72(%rsp), %ebp
	testl	%ebp, %ebp
	jle	.L1161
	movq	3152(%rbx), %rsi
	xorl	%edi, %edi
	xorl	%r8d, %r8d
.L735:
	movzbl	(%rsi,%r8), %eax
	movl	%edi, %edx
	addq	$4, %r8
	sall	$8, %edx
	addl	$1, %edi
	movslq	1096(%rbx,%rax,4),%rcx
	orl	%edx, (%rsi,%rcx,4)
	addl	$1, 1096(%rbx,%rax,4)
	cmpl	72(%rsp), %edi
	jne	.L735
	movl	72(%rsp), %r12d
.L715:
	movslq	56(%rbx),%rax
	movl	(%rsi,%rax,4), %eax
	movl	$0, 1092(%rbx)
	shrl	$8, %eax
	cmpb	$0, 20(%rbx)
	movl	%eax, 60(%rbx)
	jne	.L848
	mov	%eax, %eax
	movl	64(%rsp), %edx
	movl	(%rsi,%rax,4), %eax
	movl	$1, 1092(%rbx)
	movl	%edx, 52(%rsp)
	movl	%eax, 60(%rbx)
	shrl	$8, 60(%rbx)
	movzbl	%al, %eax
	movl	%eax, 64(%rbx)
	xorl	%eax, %eax
	jmp	.L593
.L659:
	movl	%esi, %r8d
	andl	$15, %esi
	shrl	$4, %r8d
	movslq	%r8d,%r9
	movl	7820(%rbx,%r9,4), %edi
	leal	(%rdi,%rsi), %eax
	movslq	%eax,%rcx
	cmpl	%eax, %edi
	movzbl	3724(%rcx,%rbx), %r10d
	jge	.L669
.L670:
	leal	-1(%rax), %edx
	movslq	%edx,%rsi
	cmpl	%edx, %edi
	movzbl	3724(%rsi,%rbx), %eax
	movb	%al, 3724(%rcx,%rbx)
	jge	.L669
	movl	%edx, %eax
	movq	%rsi, %rcx
	jmp	.L670
.L669:
	leal	1(%rdi), %eax
	testl	%r8d, %r8d
	movl	%eax, 7820(%rbx,%r9,4)
	jle	.L672
.L940:
	movl	7820(%rbx,%r9,4), %eax
	subl	$1, %r8d
	subl	$1, %eax
	movl	%eax, 7820(%rbx,%r9,4)
	movslq	%r8d,%r9
	cltq
	movl	7820(%rbx,%r9,4), %edx
	addl	$15, %edx
	testl	%r8d, %r8d
	movslq	%edx,%rdx
	movzbl	3724(%rdx,%rbx), %edx
	movb	%dl, 3724(%rax,%rbx)
	jne	.L940
.L672:
	movl	7820(%rbx), %eax
	movl	$4095, %edi
	movl	$15, %r8d
	subl	$1, %eax
	movl	%eax, 7820(%rbx)
	cltq
	movb	%r10b, 3724(%rax,%rbx)
	movl	7820(%rbx), %eax
	testl	%eax, %eax
	jne	.L668
.L677:
	movl	%edi, %esi
	movl	$15, %ecx
	movslq	%r8d,%r9
.L675:
	movl	%ecx, %edx
	addl	7820(%rbx,%r9,4), %edx
	subl	$1, %ecx
	movslq	%esi,%rax
	subl	$1, %esi
	cmpl	$-1, %ecx
	movslq	%edx,%rdx
	movzbl	3724(%rdx,%rbx), %edx
	movb	%dl, 3724(%rax,%rbx)
	jne	.L675
	subl	$16, %edi
	subl	$1, %r8d
	leal	1(%rdi), %eax
	cmpl	%ecx, %r8d
	movl	%eax, 7820(%rbx,%r9,4)
	jne	.L677
	jmp	.L668
.L1161:
	movq	3152(%rbx), %rsi
	xorl	%r12d, %r12d
	jmp	.L715
.L716:
	movl	1096(%rbx,%rdx,4), %eax
	movl	%eax, 2124(%rbx,%rdx,4)
	addq	$1, %rdx
	cmpq	$257, %rdx
	jne	.L716
	movl	72(%rsp), %r11d
	testl	%r11d, %r11d
	jle	.L718
	xorl	%edi, %edi
	xorl	%r8d, %r8d
.L720:
	movq	%r8, %rax
	addq	3160(%rbx), %rax
	testb	$1, %dil
	movzbl	(%rax), %esi
	movl	2124(%rbx,%rsi,4), %ecx
	movw	%cx, (%rax)
	jne	.L721
	movl	%edi, %edx
	sarl	$16, %ecx
	sarl	%edx
	movslq	%edx,%rdx
	addq	3168(%rbx), %rdx
	movzbl	(%rdx), %eax
	andl	$-16, %eax
	orl	%ecx, %eax
	movb	%al, (%rdx)
.L723:
	addl	$1, 2124(%rbx,%rsi,4)
	addl	$1, %edi
	addq	$2, %r8
	cmpl	72(%rsp), %edi
	jne	.L720
.L718:
	movl	56(%rbx), %esi
	movq	3168(%rbx), %r8
	movl	%esi, %eax
	leal	0(,%rsi,4), %ecx
	movslq	%esi,%rdx
	sarl	%eax
	cltq
	andl	$4, %ecx
	movzbl	(%r8,%rax), %ebp
	movq	3160(%rbx), %rax
	movzwl	(%rax,%rdx,2), %eax
	shrl	%cl, %ebp
	andl	$15, %ebp
	sall	$16, %ebp
	orl	%eax, %ebp
.L724:
	movl	%ebp, %eax
	movslq	%ebp,%rdx
	leal	0(,%rbp,4), %ecx
	sarl	%eax
	addq	%rdx, %rdx
	addq	3160(%rbx), %rdx
	movslq	%eax,%rdi
	andl	$4, %ecx
	movzbl	(%r8,%rdi), %r8d
	movzwl	(%rdx), %eax
	movw	%si, (%rdx)
	shrl	%cl, %r8d
	andl	$15, %r8d
	sall	$16, %r8d
	orl	%eax, %r8d
	testb	$1, %bpl
	jne	.L725
	movq	%rdi, %rdx
	addq	3168(%rbx), %rdx
	sarl	$16, %esi
	movzbl	(%rdx), %eax
	andl	$-16, %eax
	orl	%esi, %eax
	movb	%al, (%rdx)
.L727:
	cmpl	56(%rbx), %ebp
	movl	%ebp, %esi
	je	.L728
	movl	%r8d, %ebp
	movq	3168(%rbx), %r8
	jmp	.L724
.L681:
	addl	$1, 56(%rsp)
	movl	56(%rsp), %esi
	cmpl	%esi, 48(%rsp)
	jle	.L1068
	movslq	%esi,%rax
	movzbl	7884(%rax,%rbx), %eax
	movl	$49, 60(%rsp)
	movl	%eax, 88(%rsp)
	cltq
	movl	64012(%rbx,%rax,4), %edx
	movl	%edx, 92(%rsp)
	movq	%rax, %rdx
	salq	$10, %rdx
	leaq	(%rdx,%rax,8), %rax
	leaq	45436(%rax,%rbx), %rcx
	leaq	57820(%rax,%rbx), %rsi
	leaq	51628(%rax,%rbx), %rax
	movq	%rcx, 96(%rsp)
	movq	%rsi, 112(%rsp)
	movq	%rax, 104(%rsp)
	jmp	.L683
.L678:
	movslq	72(%rsp),%rax
	movq	3152(%rbx), %rsi
	movl	%edx, (%rsi,%rax,4)
	jmp	.L680
.L620:
	addl	$1, 56(%rsp)
	movl	56(%rsp), %esi
	cmpl	%esi, 48(%rsp)
	jle	.L1068
	movslq	%esi,%rax
	movzbl	7884(%rax,%rbx), %eax
	movl	$49, 60(%rsp)
	movl	%eax, 88(%rsp)
	cltq
	movl	64012(%rbx,%rax,4), %edx
	movl	%edx, 92(%rsp)
	movq	%rax, %rdx
	salq	$10, %rdx
	leaq	(%rdx,%rax,8), %rax
	leaq	45436(%rax,%rbx), %rcx
	leaq	57820(%rax,%rbx), %rsi
	leaq	51628(%rax,%rbx), %rax
	movq	%rcx, 96(%rsp)
	movq	%rsi, 112(%rsp)
	movq	%rax, 104(%rsp)
	jmp	.L622
.L616:
	cmpl	$1, 64(%rsp)
	jne	.L618
.L854:
	movl	120(%rsp), %esi
	addl	%esi, 76(%rsp)
	jmp	.L618
.L640:
	cmpl	$1, 64(%rsp)
	je	.L1162
	movslq	7820(%rbx),%rax
	addl	$1, 76(%rsp)
	movl	76(%rsp), %esi
	movzbl	3724(%rax,%rbx), %eax
	movzbl	3468(%rax,%rbx), %edx
	movzbl	%dl, %ecx
	movslq	%ecx,%rax
	addl	%esi, 68(%rbx,%rax,4)
	cmpb	$0, 44(%rbx)
	je	.L644
	testl	%esi, %esi
	jle	.L610
	movl	72(%rsp), %eax
	cmpl	%eax, 68(%rsp)
	jle	.L1068
	movzbw	%dl, %si
	movq	3160(%rbx), %rcx
	movl	72(%rsp), %edx
	cltq
	addq	%rax, %rax
.L648:
	addl	$1, %edx
	subl	$1, 76(%rsp)
	movw	%si, (%rcx,%rax)
	je	.L1064
	addq	$2, %rax
	cmpl	68(%rsp), %edx
	jne	.L648
.L845:
	movl	%edx, 72(%rsp)
	jmp	.L1068
.L721:
	movl	%edi, %edx
	sarl	$16, %ecx
	sarl	%edx
	sall	$4, %ecx
	movslq	%edx,%rdx
	addq	3168(%rbx), %rdx
	movzbl	(%rdx), %eax
	andl	$15, %eax
	orl	%ecx, %eax
	movb	%al, (%rdx)
	jmp	.L723
.L1064:
	movl	%edx, 72(%rsp)
	jmp	.L610
.L644:
	movl	76(%rsp), %edx
	testl	%edx, %edx
	jle	.L610
	movl	72(%rsp), %edx
	cmpl	%edx, 68(%rsp)
	jle	.L1068
	movq	3152(%rbx), %rsi
	movslq	%edx,%rax
	salq	$2, %rax
.L651:
	addl	$1, %edx
	subl	$1, 76(%rsp)
	movl	%ecx, (%rsi,%rax)
	je	.L1064
	addq	$4, %rax
	cmpl	68(%rsp), %edx
	jne	.L651
	jmp	.L845
.L1162:
	sall	120(%rsp)
	.p2align 4,,3
	jmp	.L854
.L728:
	cmpb	$0, 20(%rbx)
	movl	%ebp, 60(%rbx)
	movl	$0, 1092(%rbx)
	je	.L730
	leaq	1096(%rbx), %rsi
	movl	$0, 24(%rbx)
	movl	$0, 28(%rbx)
	movl	%ebp, %edi
	movl	%r8d, 24(%rsp)
	call	BZ2_indexIntoF
	mov	60(%rbx), %esi
	movl	%eax, 64(%rbx)
	movq	3168(%rbx), %rdx
	movl	24(%rbx), %r10d
	movl	24(%rsp), %r8d
	movl	%esi, %eax
	leal	0(,%rsi,4), %ecx
	shrl	%eax
	mov	%eax, %eax
	andl	$4, %ecx
	movzbl	(%rdx,%rax), %eax
	movq	3160(%rbx), %rdx
	addl	$1, 1092(%rbx)
	movzwl	(%rdx,%rsi,2), %edx
	shrl	%cl, %eax
	andl	$15, %eax
	sall	$16, %eax
	orl	%edx, %eax
	testl	%r10d, %r10d
	movl	%eax, 60(%rbx)
	jne	.L732
	movl	28(%rbx), %edx
	movslq	%edx,%rax
	addl	$1, %edx
	movl	BZ2_rNums(,%rax,4), %eax
	movl	%eax, 24(%rbx)
	xorl	%eax, %eax
	cmpl	$512, %edx
	cmovne	%edx, %eax
	movl	%eax, 28(%rbx)
.L732:
	movl	24(%rbx), %eax
	movl	64(%rsp), %esi
	movl	%ebp, %r12d
	movl	%r8d, 124(%rsp)
	subl	$1, %eax
	movl	%esi, 52(%rsp)
	movl	%eax, 24(%rbx)
	subl	$1, %eax
	sete	%al
	movzbl	%al, %eax
	xorl	%eax, 64(%rbx)
	xorl	%eax, %eax
	jmp	.L593
.L730:
	leaq	1096(%rbx), %rsi
	movl	%ebp, %edi
	movl	%r8d, 24(%rsp)
	movl	%ebp, %r12d
	call	BZ2_indexIntoF
	mov	60(%rbx), %esi
	movl	%eax, 64(%rbx)
	movq	3168(%rbx), %rdx
	movl	24(%rsp), %r8d
	movl	%esi, %eax
	leal	0(,%rsi,4), %ecx
	shrl	%eax
	mov	%eax, %eax
	andl	$4, %ecx
	movzbl	(%rdx,%rax), %eax
	movq	3160(%rbx), %rdx
	addl	$1, 1092(%rbx)
	movl	%r8d, 124(%rsp)
	movzwl	(%rdx,%rsi,2), %edx
	shrl	%cl, %eax
	andl	$15, %eax
	sall	$16, %eax
	orl	%edx, %eax
	movl	%eax, 60(%rbx)
	movl	64(%rsp), %eax
	movl	%eax, 52(%rsp)
	xorl	%eax, %eax
	jmp	.L593
.L725:
	movq	%rdi, %rdx
	addq	3168(%rbx), %rdx
	sarl	$16, %esi
	sall	$4, %esi
	movzbl	(%rdx), %eax
	andl	$15, %eax
	orl	%esi, %eax
	movb	%al, (%rdx)
	jmp	.L727
.LFE27:
	.size	BZ2_decompress, .-BZ2_decompress
	.section	.rodata.str1.1
.LC14:
	.string	"        bucket sorting ...\n"
.LC15:
	.string	"        depth %6d has "
.LC16:
	.string	"%6d unresolved strings\n"
	.section	.rodata.str1.8
	.align 8
.LC17:
	.string	"        reconstructing block ...\n"
	.text
	.p2align 4,,15
	.type	fallbackSort, @function
fallbackSort:
.LFB7:
	pushq	%r15
.LCFI43:
	pushq	%r14
.LCFI44:
	pushq	%r13
.LCFI45:
	movq	%rsi, %r13
	pushq	%r12
.LCFI46:
	pushq	%rbp
.LCFI47:
	pushq	%rbx
.LCFI48:
	subq	$2968, %rsp
.LCFI49:
	cmpl	$3, %r8d
	movq	%rdi, 56(%rsp)
	movq	%rdx, 48(%rsp)
	movl	%ecx, 44(%rsp)
	movl	%r8d, 40(%rsp)
	jg	.L1339
.L1164:
	leaq	96(%rsp), %rdi
	xorl	%eax, %eax
.L1166:
	movl	$0, (%rdi,%rax,4)
	addq	$1, %rax
	cmpq	$257, %rax
	jne	.L1166
	movl	44(%rsp), %r14d
	testl	%r14d, %r14d
	jle	.L1168
	movq	%r13, %rdx
	xorl	%ecx, %ecx
.L1170:
	movzbl	(%rdx), %eax
	addl	$1, %ecx
	addq	$1, %rdx
	addl	$1, 96(%rsp,%rax,4)
	cmpl	44(%rsp), %ecx
	jne	.L1170
.L1168:
	leaq	1136(%rsp), %rcx
	xorl	%edx, %edx
.L1171:
	movl	(%rdi,%rdx,4), %eax
	movl	%eax, (%rcx,%rdx,4)
	addq	$1, %rdx
	cmpq	$256, %rdx
	jne	.L1171
	leaq	1024(%rdi), %rcx
	movq	%rdi, %rdx
.L1173:
	movl	(%rdx), %eax
	addl	%eax, 4(%rdx)
	addq	$4, %rdx
	cmpq	%rcx, %rdx
	jne	.L1173
	movl	44(%rsp), %r12d
	testl	%r12d, %r12d
	jle	.L1175
	xorl	%esi, %esi
	xorl	%ecx, %ecx
.L1177:
	movzbl	(%rcx,%r13), %edx
	addl	$1, %esi
	movl	96(%rsp,%rdx,4), %eax
	subl	$1, %eax
	movl	%eax, 96(%rsp,%rdx,4)
	movq	56(%rsp), %rdx
	cltq
	movl	%ecx, (%rdx,%rax,4)
	addq	$1, %rcx
	cmpl	44(%rsp), %esi
	jne	.L1177
.L1175:
	movl	44(%rsp), %eax
	movl	44(%rsp), %ebp
	movl	44(%rsp), %edx
	addl	$31, %eax
	testl	%ebp, %ebp
	cmovs	%eax, %edx
	sarl	$5, %edx
	leal	2(%rdx), %eax
	testl	%eax, %eax
	jle	.L1178
	leal	1(%rdx), %eax
	xorl	%ecx, %ecx
	addq	$1, %rax
.L1180:
	movq	48(%rsp), %rbx
	movl	$0, (%rbx,%rcx,4)
	addq	$1, %rcx
	cmpq	%rax, %rcx
	jne	.L1180
.L1178:
	xorl	%edx, %edx
	movl	$1, %esi
.L1181:
	movl	(%rdi,%rdx,4), %ecx
	movl	%esi, %ebx
	addq	$1, %rdx
	movl	%ecx, %eax
	andl	$31, %ecx
	sall	%cl, %ebx
	sarl	$5, %eax
	movl	%ebx, %ecx
	movq	48(%rsp), %rbx
	cltq
	orl	%ecx, (%rbx,%rax,4)
	cmpq	$256, %rdx
	jne	.L1181
	movl	44(%rsp), %edx
	xorl	%esi, %esi
	movl	$1, %edi
.L1183:
	movl	%edx, %ecx
	movl	%edi, %ebx
	movl	%edx, %eax
	andl	$31, %ecx
	sarl	$5, %eax
	addl	$1, %esi
	sall	%cl, %ebx
	cltq
	movl	%ebx, %ecx
	movq	48(%rsp), %rbx
	orl	%ecx, (%rbx,%rax,4)
	leal	1(%rdx), %ecx
	movl	%edi, %ebx
	addl	$2, %edx
	movl	%ecx, %eax
	andl	$31, %ecx
	sall	%cl, %ebx
	sarl	$5, %eax
	movl	%ebx, %ecx
	movq	48(%rsp), %rbx
	cltq
	notl	%ecx
	andl	%ecx, (%rbx,%rax,4)
	cmpl	$32, %esi
	jne	.L1183
	movl	$1, 64(%rsp)
.L1185:
	cmpl	$3, 40(%rsp)
	jg	.L1340
.L1186:
	movl	44(%rsp), %ebx
	testl	%ebx, %ebx
	jle	.L1188
	movq	56(%rsp), %rdi
	xorl	%esi, %esi
	xorl	%r8d, %r8d
	movl	$1, %r9d
.L1190:
	movl	%esi, %eax
	movq	48(%rsp), %rbx
	movl	%esi, %ecx
	sarl	$5, %eax
	andl	$31, %ecx
	movl	%r9d, %edx
	cltq
	sall	%cl, %edx
	movl	44(%rsp), %ecx
	testl	%edx, (%rbx,%rax,4)
	movl	(%rdi), %eax
	cmovne	%esi, %r8d
	subl	64(%rsp), %eax
	leal	(%rax,%rcx), %edx
	cmpl	$-1, %eax
	cmovle	%edx, %eax
	addl	$1, %esi
	addq	$4, %rdi
	cltq
	cmpl	%ecx, %esi
	movl	%r8d, (%r13,%rax,4)
	jne	.L1190
.L1188:
	movq	56(%rsp), %rbx
	movq	56(%rsp), %rsi
	movl	$-1, 72(%rsp)
	movl	$0, 76(%rsp)
	subq	$16, %rbx
	subq	$4, %rsi
	movq	%rbx, 16(%rsp)
	movq	%rsi, 8(%rsp)
.L1336:
	movl	72(%rsp), %edx
	movq	48(%rsp), %rcx
	addl	$1, %edx
	movl	%edx, %eax
	sarl	$5, %eax
	cltq
	leaq	(%rcx,%rax,4), %rsi
	movl	%edx, %ecx
	movl	$1, %eax
	andl	$31, %ecx
	sall	%cl, %eax
	testl	%eax, (%rsi)
	je	.L1197
	testl	%ecx, %ecx
	movl	$1, %edi
	jne	.L1200
	jmp	.L1333
.L1201:
	testl	%ecx, %ecx
	.p2align 4,,5
	je	.L1333
.L1200:
	addl	$1, %edx
	movq	48(%rsp), %rbx
	movl	%edx, %eax
	movl	%edx, %ecx
	sarl	$5, %eax
	andl	$31, %ecx
	cltq
	leaq	(%rbx,%rax,4), %rsi
	movl	%edi, %eax
	sall	%cl, %eax
	testl	%eax, (%rsi)
	jne	.L1201
.L1197:
	leal	-1(%rdx), %esi
	cmpl	%esi, 44(%rsp)
	movl	%esi, 68(%rsp)
	jle	.L1205
.L1342:
	movq	48(%rsp), %rcx
	movl	%edx, %eax
	sarl	$5, %eax
	cltq
	leaq	(%rcx,%rax,4), %rsi
	movl	%edx, %ecx
	movl	$1, %eax
	andl	$31, %ecx
	sall	%cl, %eax
	testl	%eax, (%rsi)
	jne	.L1196
	testl	%ecx, %ecx
	movl	$1, %edi
	jne	.L1209
	jmp	.L1334
.L1210:
	testl	%ecx, %ecx
	.p2align 4,,5
	je	.L1334
.L1209:
	addl	$1, %edx
	movq	48(%rsp), %rbx
	movl	%edx, %eax
	movl	%edx, %ecx
	sarl	$5, %eax
	andl	$31, %ecx
	cltq
	leaq	(%rbx,%rax,4), %rsi
	movl	%edi, %eax
	sall	%cl, %eax
	testl	%eax, (%rsi)
	je	.L1210
.L1211:
	subl	$1, %edx
	cmpl	%edx, 44(%rsp)
	movl	%edx, 72(%rsp)
	jle	.L1205
	cmpl	%edx, 68(%rsp)
	jge	.L1336
	movl	68(%rsp), %esi
	movl	%edx, 2160(%rsp)
	movl	$1, 84(%rsp)
	movl	$0, 80(%rsp)
	movl	%esi, 2560(%rsp)
.L1217:
	cmpl	$99, 84(%rsp)
	jg	.L1341
.L1218:
	subl	$1, 84(%rsp)
	movslq	84(%rsp),%r12
	movl	2560(%rsp,%r12,4), %eax
	movl	2160(%rsp,%r12,4), %r11d
	movl	%eax, 92(%rsp)
	movl	%r11d, %eax
	subl	92(%rsp), %eax
	cmpl	$9, %eax
	jg	.L1220
	cmpl	%r11d, 92(%rsp)
	je	.L1222
	cmpl	$3, %eax
	jle	.L1224
	leal	-4(%r11), %eax
	cmpl	%eax, 92(%rsp)
	jg	.L1224
	movq	56(%rsp), %rdx
	cltq
	movl	%r11d, %r10d
	leaq	(%rdx,%rax,4), %r15
	movslq	%r11d,%rax
	movq	%rdx, %r14
	leaq	0(,%rax,4), %rbx
	leal	4(%r11), %eax
	movq	%rdx, %r12
	cltq
	addq	%rbx, %r14
	salq	$2, %rax
	addq	%rax, %r12
	movq	%rax, %rbp
.L1227:
	movl	(%r15), %ecx
	cmpl	%r10d, %r11d
	movl	%r10d, %edi
	movq	%rbx, %rsi
	movslq	%ecx,%rax
	movl	%ecx, 88(%rsp)
	movl	(%r13,%rax,4), %r9d
	jl	.L1230
	movl	(%r14), %edx
	movq	%rbp, %r8
	movq	%r12, %rcx
	mov	%edx, %eax
	cmpl	(%r13,%rax,4), %r9d
	ja	.L1232
	jmp	.L1230
	.p2align 4,,7
.L1233:
	movl	(%rcx), %edx
	addq	$16, %rcx
	movq	%r8, %rsi
	leaq	16(%r8), %r8
	mov	%edx, %eax
	cmpl	(%r13,%rax,4), %r9d
	jbe	.L1230
.L1232:
	movq	16(%rsp), %rax
	addl	$4, %edi
	cmpl	%edi, %r11d
	movl	%edx, (%rax,%rsi)
	jge	.L1233
	movslq	%edi,%rax
	leaq	0(,%rax,4), %rsi
.L1230:
	subl	$1, %r10d
	subq	$4, %r15
	subq	$4, %rbx
	subq	$4, %r14
	subq	$4, %r12
	subq	$4, %rbp
	leal	-4(%r10), %eax
	cmpl	%eax, 92(%rsp)
	movl	88(%rsp), %ecx
	movq	16(%rsp), %rdx
	movl	%ecx, (%rsi,%rdx)
	jle	.L1227
.L1224:
	leal	-1(%r11), %ecx
	cmpl	%ecx, 92(%rsp)
	jg	.L1222
	leal	1(%r11), %edx
	movq	56(%rsp), %rsi
	movslq	%r11d,%rax
	leaq	0(,%rax,4), %rbp
	movslq	%ecx,%rax
	movl	%r11d, %ebx
	movslq	%edx,%rdx
	leaq	0(,%rax,4), %r15
	xorl	%r12d, %r12d
	leaq	0(,%rdx,4), %r14
	movq	%rbp, 32(%rsp)
	addq	%r14, %rsi
	movq	%rsi, 24(%rsp)
	.p2align 4,,7
.L1236:
	movq	56(%rsp), %rcx
	leaq	(%r15,%r12), %rax
	cmpl	%ebx, %r11d
	movl	%ebx, %r8d
	movq	%rbp, %rdi
	movl	(%rax,%rcx), %edx
	movslq	%edx,%rax
	movl	(%r13,%rax,4), %r10d
	jl	.L1239
	movq	32(%rsp), %rax
	addq	%r12, %rax
	movl	(%rax,%rcx), %ecx
	mov	%ecx, %eax
	cmpl	(%r13,%rax,4), %r10d
	jbe	.L1239
	movq	24(%rsp), %rax
	leaq	(%r12,%r14), %r9
	leaq	(%r12,%rax), %rsi
	jmp	.L1241
	.p2align 4,,7
.L1242:
	movl	(%rsi), %ecx
	addq	$4, %rsi
	movq	%r9, %rdi
	leaq	4(%r9), %r9
	mov	%ecx, %eax
	cmpl	(%r13,%rax,4), %r10d
	jbe	.L1239
.L1241:
	movq	8(%rsp), %rax
	addl	$1, %r8d
	cmpl	%r8d, %r11d
	movl	%ecx, (%rax,%rdi)
	jge	.L1242
	movslq	%r8d,%rax
	leaq	0(,%rax,4), %rdi
.L1239:
	subl	$1, %ebx
	subq	$4, %r12
	subq	$4, %rbp
	leal	-1(%rbx), %eax
	cmpl	%eax, 92(%rsp)
	movq	8(%rsp), %rcx
	movl	%edx, (%rdi,%rcx)
	jle	.L1236
.L1222:
	movl	84(%rsp), %r9d
	testl	%r9d, %r9d
	jg	.L1217
.L1345:
	movslq	68(%rsp),%rax
	movq	56(%rsp), %rcx
	movl	$1, %r8d
	movl	68(%rsp), %edx
	leaq	(%rcx,%rax,4), %rsi
	movl	$-1, %ecx
.L1271:
	mov	(%rsi), %eax
	movl	(%r13,%rax,4), %edi
	cmpl	%edi, %ecx
	je	.L1272
	movl	%edx, %ecx
	movl	%r8d, %ebx
	movl	%edx, %eax
	andl	$31, %ecx
	sarl	$5, %eax
	sall	%cl, %ebx
	cltq
	movl	%ebx, %ecx
	movq	48(%rsp), %rbx
	orl	%ecx, (%rbx,%rax,4)
.L1272:
	addl	$1, %edx
	addq	$4, %rsi
	cmpl	%edx, 72(%rsp)
	movl	%edi, %ecx
	jge	.L1271
	movl	76(%rsp), %eax
	addl	$1, %eax
	subl	68(%rsp), %eax
	addl	72(%rsp), %eax
	movl	%eax, 76(%rsp)
	jmp	.L1336
.L1296:
	addl	$32, %edx
	movq	48(%rsp), %rcx
	movl	%edx, %eax
	sarl	$5, %eax
	cltq
	leaq	(%rcx,%rax,4), %rsi
.L1333:
	cmpl	$-1, (%rsi)
	je	.L1296
	movl	%edx, %ecx
	movl	$1, %ebx
	andl	$31, %ecx
	sall	%cl, %ebx
	testl	%ebx, (%rsi)
	je	.L1197
	movl	$1, %esi
.L1204:
	addl	$1, %edx
	movl	%esi, %ebx
	movl	%edx, %ecx
	movl	%edx, %eax
	andl	$31, %ecx
	sarl	$5, %eax
	sall	%cl, %ebx
	cltq
	movl	%ebx, %ecx
	movq	48(%rsp), %rbx
	testl	%ecx, (%rbx,%rax,4)
	jne	.L1204
	leal	-1(%rdx), %esi
	cmpl	%esi, 44(%rsp)
	movl	%esi, 68(%rsp)
	jg	.L1342
.L1205:
	cmpl	$3, 40(%rsp)
	jg	.L1343
.L1275:
	sall	64(%rsp)
	movl	64(%rsp), %esi
	cmpl	%esi, 44(%rsp)
	jl	.L1277
	movl	76(%rsp), %r8d
	testl	%r8d, %r8d
	jne	.L1185
.L1277:
	cmpl	$3, 40(%rsp)
	jg	.L1344
.L1279:
	movl	44(%rsp), %edi
	testl	%edi, %edi
	jle	.L1289
	movq	56(%rsp), %rdi
	xorl	%ecx, %ecx
	xorl	%r8d, %r8d
	xorl	%esi, %esi
	jmp	.L1338
.L1299:
	addl	$1, %ecx
	movslq	%ecx,%rsi
.L1338:
	movl	1136(%rsp,%rsi,4), %edx
	testl	%edx, %edx
	je	.L1299
	mov	(%rdi), %eax
	addl	$1, %r8d
	addq	$4, %rdi
	movb	%cl, (%rax,%r13)
	cmpl	44(%rsp), %r8d
	je	.L1286
	leal	-1(%rdx), %eax
	movl	%eax, 1136(%rsp,%rsi,4)
	jmp	.L1338
.L1297:
	addl	$32, %edx
	movq	48(%rsp), %rcx
	movl	%edx, %eax
	sarl	$5, %eax
	cltq
	leaq	(%rcx,%rax,4), %rsi
.L1334:
	movl	(%rsi), %r11d
	testl	%r11d, %r11d
	je	.L1297
	movl	%edx, %ecx
	movl	$1, %ebx
	andl	$31, %ecx
	sall	%cl, %ebx
	testl	%ebx, (%rsi)
	jne	.L1211
	movl	$1, %esi
.L1214:
	addl	$1, %edx
	movl	%esi, %ebx
	movl	%edx, %ecx
	movl	%edx, %eax
	andl	$31, %ecx
	sarl	$5, %eax
	sall	%cl, %ebx
	cltq
	movl	%ebx, %ecx
	movq	48(%rsp), %rbx
	testl	%ecx, (%rbx,%rax,4)
	je	.L1214
	jmp	.L1211
.L1220:
	movl	80(%rsp), %r10d
	movl	$-1431655765, %edx
	imull	$7621, %r10d, %eax
	addl	$1, %eax
	andl	$32767, %eax
	movl	%eax, 80(%rsp)
	mull	%edx
	movl	80(%rsp), %eax
	shrl	%edx
	leal	(%rdx,%rdx,2), %edx
	subl	%edx, %eax
	jne	.L1244
	movslq	92(%rsp),%rax
	movq	56(%rsp), %rbx
	mov	(%rbx,%rax,4), %eax
	movl	(%r13,%rax,4), %edi
.L1246:
	movl	92(%rsp), %r9d
	movl	%r11d, %r8d
	movl	%r11d, %r10d
	movl	%r9d, %ebx
.L1337:
	cmpl	%r8d, %r9d
	jg	.L1255
.L1250:
	movq	56(%rsp), %rsi
	movslq	%r9d,%rax
	leaq	(%rsi,%rax,4), %rbp
	movl	(%rbp), %ecx
	mov	%ecx, %eax
	movl	(%r13,%rax,4), %eax
	subl	%edi, %eax
	cmpl	$0, %eax
	jne	.L1251
	movslq	%ebx,%rax
	addl	$1, %r9d
	addl	$1, %ebx
	leaq	(%rsi,%rax,4), %rax
	cmpl	%r8d, %r9d
	movl	(%rax), %edx
	movl	%edx, (%rbp)
	movl	%ecx, (%rax)
	jle	.L1250
.L1255:
	cmpl	%ebx, %r10d
	jl	.L1222
	movl	%ebx, %eax
	subl	92(%rsp), %eax
	movl	%r9d, %edi
	subl	%ebx, %edi
	cmpl	%eax, %edi
	cmovg	%eax, %edi
	testl	%edi, %edi
	jle	.L1262
	movslq	92(%rsp),%rax
	movq	56(%rsp), %rcx
	leaq	(%rcx,%rax,4), %rsi
	movl	%r9d, %eax
	subl	%edi, %eax
	cltq
	leaq	(%rcx,%rax,4), %rcx
.L1264:
	movl	(%rsi), %edx
	movl	(%rcx), %eax
	movl	%eax, (%rsi)
	movl	%edx, (%rcx)
	addq	$4, %rsi
	addq	$4, %rcx
	subl	$1, %edi
	jne	.L1264
.L1262:
	movl	%r10d, %esi
	movl	%r11d, %eax
	subl	%r8d, %esi
	subl	%r10d, %eax
	leal	1(%r11), %r10d
	cmpl	%eax, %esi
	movl	%eax, %edi
	movl	%esi, %r8d
	cmovle	%esi, %edi
	testl	%edi, %edi
	jle	.L1265
	movq	56(%rsp), %rdx
	movslq	%r9d,%rax
	leaq	(%rdx,%rax,4), %rsi
	movl	%r10d, %eax
	subl	%edi, %eax
	cltq
	leaq	(%rdx,%rax,4), %rcx
.L1267:
	movl	(%rsi), %edx
	movl	(%rcx), %eax
	movl	%eax, (%rsi)
	movl	%edx, (%rcx)
	addq	$4, %rsi
	addq	$4, %rcx
	subl	$1, %edi
	jne	.L1267
.L1265:
	movl	92(%rsp), %esi
	movl	%r11d, %eax
	leal	-1(%rsi,%r9), %ecx
	movl	%r10d, %esi
	subl	%r8d, %esi
	subl	%ebx, %ecx
	subl	%esi, %eax
	movl	%ecx, %edx
	subl	92(%rsp), %edx
	cmpl	%eax, %edx
	jle	.L1268
	movl	92(%rsp), %eax
	movl	%ecx, 2160(%rsp,%r12,4)
	movl	%eax, 2560(%rsp,%r12,4)
	movl	84(%rsp), %eax
	addl	$2, 84(%rsp)
	movl	84(%rsp), %r9d
	addl	$1, %eax
	cltq
	testl	%r9d, %r9d
	movl	%esi, 2560(%rsp,%rax,4)
	movl	%r11d, 2160(%rsp,%rax,4)
	jg	.L1217
	jmp	.L1345
.L1341:
	movl	$1004, %edi
	call	BZ2_bz__AssertH__fail
	.p2align 4,,6
	jmp	.L1218
.L1244:
	subl	$1, %eax
	.p2align 4,,4
	je	.L1346
	movq	56(%rsp), %rcx
	movslq	%r11d,%rax
	mov	(%rcx,%rax,4), %eax
	movl	(%r13,%rax,4), %edi
	jmp	.L1246
.L1251:
	jg	.L1298
	.p2align 4,,2
	jmp	.L1350
.L1348:
	movslq	%r10d,%rax
	subl	$1, %r8d
	subl	$1, %r10d
	leaq	(%rdx,%rax,4), %rax
	movl	(%rax), %edx
	movl	%edx, (%rsi)
	movl	%ecx, (%rax)
.L1258:
	cmpl	%r8d, %r9d
	jg	.L1255
.L1298:
	movq	56(%rsp), %rdx
	movslq	%r8d,%rax
	leaq	(%rdx,%rax,4), %rsi
	movl	(%rsi), %ecx
	mov	%ecx, %eax
	movl	(%r13,%rax,4), %eax
	subl	%edi, %eax
	cmpl	$0, %eax
	je	.L1348
	jl	.L1259
	subl	$1, %r8d
	jmp	.L1258
.L1268:
	movl	84(%rsp), %eax
	addl	$2, 84(%rsp)
	movl	84(%rsp), %r9d
	movl	92(%rsp), %edx
	movl	%esi, 2560(%rsp,%r12,4)
	movl	%r11d, 2160(%rsp,%r12,4)
	addl	$1, %eax
	cltq
	testl	%r9d, %r9d
	movl	%edx, 2560(%rsp,%rax,4)
	movl	%ecx, 2160(%rsp,%rax,4)
	jg	.L1217
	jmp	.L1345
.L1346:
	movl	92(%rsp), %esi
	movq	56(%rsp), %rdx
	leal	(%r11,%rsi), %eax
	sarl	%eax
	cltq
	mov	(%rdx,%rax,4), %eax
	movl	(%r13,%rax,4), %edi
	jmp	.L1246
.L1259:
	movl	(%rbp), %eax
	addl	$1, %r9d
	subl	$1, %r8d
	movl	%ecx, (%rbp)
	movl	%eax, (%rsi)
	jmp	.L1337
.L1350:
	addl	$1, %r9d
	jmp	.L1337
.L1196:
	movl	68(%rsp), %eax
	movl	%eax, 72(%rsp)
	jmp	.L1336
.L1340:
	movl	64(%rsp), %edx
	movq	stderr(%rip), %rdi
	movl	$.LC15, %esi
	xorl	%eax, %eax
	call	fprintf
	jmp	.L1186
.L1343:
	movl	76(%rsp), %edx
	movq	stderr(%rip), %rdi
	movl	$.LC16, %esi
	xorl	%eax, %eax
	call	fprintf
	jmp	.L1275
.L1286:
	cmpl	$255, %ecx
	jg	.L1349
.L1289:
	addq	$2968, %rsp
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
.L1349:
	addq	$2968, %rsp
	movl	$1005, %edi
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	BZ2_bz__AssertH__fail
.L1339:
	movq	stderr(%rip), %rcx
	movl	$27, %edx
	movl	$1, %esi
	movl	$.LC14, %edi
	call	fwrite
	jmp	.L1164
.L1344:
	movq	stderr(%rip), %rcx
	movl	$33, %edx
	movl	$1, %esi
	movl	$.LC17, %edi
	call	fwrite
	jmp	.L1279
.LFE7:
	.size	fallbackSort, .-fallbackSort
	.p2align 4,,15
.globl BZ2_bzReadClose
	.type	BZ2_bzReadClose, @function
BZ2_bzReadClose:
.LFB55:
	testq	%rdi, %rdi
	pushq	%rbx
.LCFI50:
	movq	%rsi, %rbx
	je	.L1352
	movl	$0, (%rdi)
.L1352:
	testq	%rbx, %rbx
	je	.L1354
	cmpb	$0, 5012(%rbx)
	movl	$0, 5096(%rbx)
	je	.L1366
	testq	%rdi, %rdi
	je	.L1360
	movl	$-1, (%rdi)
.L1360:
	movl	$-1, 5096(%rbx)
.L1364:
	popq	%rbx
	ret
	.p2align 4,,7
.L1366:
	cmpb	$0, 5100(%rbx)
	jne	.L1367
	movq	%rbx, %rdi
	popq	%rbx
	jmp	free
	.p2align 4,,7
.L1354:
	testq	%rdi, %rdi
	je	.L1364
	movl	$0, (%rdi)
	popq	%rbx
	ret
	.p2align 4,,7
.L1367:
	leaq	5016(%rbx), %rdi
	call	BZ2_bzDecompressEnd
	movq	%rbx, %rdi
	popq	%rbx
	jmp	free
.LFE55:
	.size	BZ2_bzReadClose, .-BZ2_bzReadClose
	.p2align 4,,15
	.type	default_bzfree, @function
default_bzfree:
.LFB31:
	testq	%rsi, %rsi
	je	.L1371
	movq	%rsi, %rdi
	jmp	free
	.p2align 4,,7
.L1371:
	rep ; ret
.LFE31:
	.size	default_bzfree, .-default_bzfree
	.section	.rodata.str1.8
	.align 8
.LC18:
	.string	"bzip2: file name\n`%s'\nis suspiciously (more than %d chars) long.\nTry using a reasonable file name instead.  Sorry! :-)\n"
	.text
	.p2align 4,,15
	.type	copyFileName, @function
copyFileName:
.LFB94:
	movq	%rbx, -16(%rsp)
.LCFI51:
	movq	%rbp, -8(%rsp)
.LCFI52:
	subq	$24, %rsp
.LCFI53:
	movq	%rdi, %rbp
	movq	%rsi, %rdi
	movq	%rsi, %rbx
	call	strlen
	cmpq	$1024, %rax
	ja	.L1376
	movq	%rbx, %rsi
	movq	%rbp, %rdi
	movl	$1024, %edx
	call	strncpy
	movb	$0, 1024(%rbp)
	movq	8(%rsp), %rbx
	movq	16(%rsp), %rbp
	addq	$24, %rsp
	ret
.L1376:
	movq	stderr(%rip), %rdi
	movl	$1024, %ecx
	movq	%rbx, %rdx
	movl	$.LC18, %esi
	xorl	%eax, %eax
	call	fprintf
	movl	$1, %edi
	call	setExit
	movl	exitValue(%rip), %edi
	call	exit
.LFE94:
	.size	copyFileName, .-copyFileName
	.p2align 4,,15
	.type	default_bzalloc, @function
default_bzalloc:
.LFB30:
	imull	%esi, %edx
	movslq	%edx,%rdi
	jmp	malloc
.LFE30:
	.size	default_bzalloc, .-default_bzalloc
	.section	.rodata.str1.8
	.align 8
.LC19:
	.string	"%s: Deleting output file %s, if it exists.\n"
	.align 8
.LC20:
	.string	"%s: WARNING: deletion of output file (apparently) failed.\n"
	.align 8
.LC21:
	.string	"%s: WARNING: deletion of output file suppressed\n"
	.align 8
.LC22:
	.string	"%s:    since input file no longer exists.  Output file\n"
	.align 8
.LC23:
	.string	"%s:    `%s' may be incomplete.\n"
	.align 8
.LC24:
	.string	"%s:    I suggest doing an integrity test (bzip2 -tv) of it.\n"
	.align 8
.LC25:
	.string	"%s: WARNING: some files have not been processed:\n%s:    %d specified on command line, %d not processed yet.\n\n"
	.text
	.p2align 4,,15
	.type	cleanUpAndFail, @function
cleanUpAndFail:
.LFB84:
	pushq	%rbx
.LCFI54:
	movl	%edi, %ebx
	subq	$144, %rsp
.LCFI55:
	cmpl	$3, srcMode(%rip)
	je	.L1396
.L1380:
	cmpb	$0, noisy(%rip)
	je	.L1391
	movl	numFileNames(%rip), %eax
	testl	%eax, %eax
	jle	.L1391
	movl	numFilesProcessed(%rip), %ecx
	cmpl	%ecx, %eax
	jg	.L1397
	.p2align 4,,7
.L1391:
	movl	%ebx, %edi
	call	setExit
	movl	exitValue(%rip), %edi
	call	exit
	.p2align 4,,7
.L1396:
	cmpl	$3, opMode(%rip)
	je	.L1380
	cmpb	$0, deleteOutputOnInterrupt(%rip)
	je	.L1380
	movq	%rsp, %rdx
	movl	$inName, %esi
	movl	$1, %edi
	call	__xstat
	testl	%eax, %eax
	jne	.L1384
	cmpb	$0, noisy(%rip)
	jne	.L1398
.L1386:
	movq	outputHandleJustInCase(%rip), %rdi
	testq	%rdi, %rdi
	je	.L1388
	call	fclose
.L1388:
	movl	$outName, %edi
	call	remove
	testl	%eax, %eax
	je	.L1380
	movq	progName(%rip), %rdx
	movq	stderr(%rip), %rdi
	movl	$.LC20, %esi
	xorl	%eax, %eax
	call	fprintf
	jmp	.L1380
	.p2align 4,,7
.L1397:
	movq	progName(%rip), %rdx
	movq	stderr(%rip), %rdi
	movl	%eax, %r9d
	subl	%ecx, %r9d
	movl	%eax, %r8d
	movl	$.LC25, %esi
	xorl	%eax, %eax
	movq	%rdx, %rcx
	call	fprintf
	jmp	.L1391
.L1384:
	movq	progName(%rip), %rdx
	movq	stderr(%rip), %rdi
	movl	$.LC21, %esi
	xorl	%eax, %eax
	call	fprintf
	movq	progName(%rip), %rdx
	movq	stderr(%rip), %rdi
	movl	$.LC22, %esi
	xorl	%eax, %eax
	call	fprintf
	movq	progName(%rip), %rdx
	movq	stderr(%rip), %rdi
	movl	$outName, %ecx
	movl	$.LC23, %esi
	xorl	%eax, %eax
	call	fprintf
	movq	progName(%rip), %rdx
	movq	stderr(%rip), %rdi
	movl	$.LC24, %esi
	xorl	%eax, %eax
	call	fprintf
	jmp	.L1380
.L1398:
	movq	progName(%rip), %rdx
	movq	stderr(%rip), %rdi
	movl	$outName, %ecx
	movl	$.LC19, %esi
	call	fprintf
	jmp	.L1386
.LFE84:
	.size	cleanUpAndFail, .-cleanUpAndFail
	.section	.rodata.str1.8
	.align 8
.LC26:
	.string	"\n%s: couldn't allocate enough memory\n"
	.text
	.p2align 4,,15
	.type	outOfMemory, @function
outOfMemory:
.LFB91:
	subq	$8, %rsp
.LCFI56:
	movq	stderr(%rip), %rdi
	movq	progName(%rip), %rdx
	movl	$.LC26, %esi
	xorl	%eax, %eax
	call	fprintf
	call	showFileNames
	movl	$1, %edi
	call	cleanUpAndFail
.LFE91:
	.size	outOfMemory, .-outOfMemory
	.p2align 4,,15
	.type	myMalloc, @function
myMalloc:
.LFB110:
	subq	$8, %rsp
.LCFI57:
	movslq	%edi,%rdi
	call	malloc
	testq	%rax, %rax
	je	.L1405
	addq	$8, %rsp
	ret
.L1405:
	.p2align 4,,8
	call	outOfMemory
.LFE110:
	.size	myMalloc, .-myMalloc
	.p2align 4,,15
	.type	snocString, @function
snocString:
.LFB112:
	pushq	%r12
.LCFI58:
	testq	%rdi, %rdi
	movq	%rsi, %r12
	pushq	%rbp
.LCFI59:
	movq	%rdi, %rbp
	pushq	%rbx
.LCFI60:
	movq	%rdi, %rbx
	jne	.L1410
	jmp	.L1416
	.p2align 4,,7
.L1415:
	movq	%rax, %rbx
.L1410:
	movq	8(%rbx), %rax
	testq	%rax, %rax
	jne	.L1415
	movq	%r12, %rsi
	xorl	%edi, %edi
	call	snocString
	movq	%rax, 8(%rbx)
	movq	%rbp, %rax
.L1409:
	popq	%rbx
	popq	%rbp
	popq	%r12
	ret
.L1416:
	movl	$16, %edi
	call	myMalloc
	movq	%r12, %rdi
	movq	$0, (%rax)
	movq	$0, 8(%rax)
	movq	%rax, %rbx
	call	strlen
	leal	5(%rax), %edi
	call	myMalloc
	movq	%r12, %rsi
	movq	%rax, %rdi
	movq	%rax, (%rbx)
	call	strcpy
	movq	%rbx, %rax
	jmp	.L1409
.LFE112:
	.size	snocString, .-snocString
	.p2align 4,,15
	.type	addFlagsFromEnvVar, @function
addFlagsFromEnvVar:
.LFB113:
	pushq	%r13
.LCFI61:
	pushq	%r12
.LCFI62:
	movq	%rdi, %r12
	movq	%rsi, %rdi
	pushq	%rbp
.LCFI63:
	pushq	%rbx
.LCFI64:
	subq	$8, %rsp
.LCFI65:
	call	getenv
	testq	%rax, %rax
	je	.L1435
	movq	%rax, %rbx
	xorl	%ebp, %ebp
	movl	$1024, %r13d
.L1443:
	movslq	%ebp,%rax
	addq	%rax, %rbx
	cmpb	$0, (%rbx)
	je	.L1435
.L1421:
	call	__ctype_b_loc
	movq	(%rax), %rsi
	jmp	.L1422
	.p2align 4,,7
.L1423:
	addq	$1, %rbx
.L1422:
	movzbl	(%rbx), %ecx
	movsbq	%cl,%rax
	testb	$32, 1(%rsi,%rax,2)
	jne	.L1423
	xorl	%ebp, %ebp
	testb	%cl, %cl
	movq	%rbx, %rdx
	jne	.L1428
	jmp	.L1445
	.p2align 4,,7
.L1429:
	movsbq	%al,%rax
	addq	$1, %rdx
	testb	$32, 1(%rsi,%rax,2)
	jne	.L1430
.L1428:
	movzbl	1(%rdx), %eax
	addl	$1, %ebp
	testb	%al, %al
	jne	.L1429
.L1430:
	testl	%ebp, %ebp
	jle	.L1443
	cmpl	$1024, %ebp
	movl	%r13d, %esi
	cmovle	%ebp, %esi
	testl	%esi, %esi
	jle	.L1432
	xorl	%ecx, %ecx
	xorl	%edx, %edx
	.p2align 4,,7
.L1434:
	movzbl	(%rdx,%rbx), %eax
	addl	$1, %ecx
	movb	%al, tmpName(%rdx)
	addq	$1, %rdx
	cmpl	%esi, %ecx
	jne	.L1434
.L1432:
	movslq	%esi,%rax
	movl	$tmpName, %esi
	movb	$0, tmpName(%rax)
	movq	(%r12), %rdi
	call	snocString
	movq	%rax, (%r12)
	movslq	%ebp,%rax
	addq	%rax, %rbx
	cmpb	$0, (%rbx)
	jne	.L1421
.L1435:
	addq	$8, %rsp
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	ret
.L1445:
	xorl	%ebp, %ebp
	jmp	.L1443
.LFE113:
	.size	addFlagsFromEnvVar, .-addFlagsFromEnvVar
	.section	.rodata.str1.8
	.align 8
.LC27:
	.ascii	"\n%s: Caught a SIGSEGV or SIGBUS whilst compressing.\n\n   P"
	.ascii	"ossible causes are (most likely first):\n   (1) This compute"
	.ascii	"r has unreliable memory or cache hardware\n       (a surpris"
	.ascii	"ingly common problem; try a different machine.)\n   (2) A bu"
	.ascii	"g in the compiler used to create this executable\n       (un"
	.ascii	"likely, if you didn't compile bzip2 yourself.)\n   (3) A rea"
	.ascii	"l bug in bzip2 -- I hope this should never be the case.\n   "
	.ascii	"The user's manual, Section 4.3, has more info on (1) and (2)"
	.ascii	".\n   \n   If you suspect this is a bug in bzip2, or are uns"
	.ascii	"ure about (1)\n   or (2), feel free to report it to me at: j"
	.ascii	"seward@acm.org.\n   Sect"
	.string	"ion 4.3 of the user's manual describes the info a useful\n   bug report should have.  If the manual is available on your\n   system, please try and read it before mailing me.  If you don't\n   have the manual or can't be bothered to read it, mail me anyway.\n\n"
	.align 8
.LC28:
	.ascii	"\n%s: Caught a SIGSEGV or SIGBUS whilst decompressing.\n\n  "
	.ascii	" Possible causes are (most likely first):\n   (1) The compre"
	.ascii	"ssed data is corrupted, and bzip2's usual checks\n       fai"
	.ascii	"led to detect this.  Try bzip2 -tvv my_file.bz2.\n   (2) Thi"
	.ascii	"s computer has unreliable memory or cache hardware\n       ("
	.ascii	"a surprisingly common problem; try a different machine.)\n  "
	.ascii	" (3) A bug in the compiler used to create this executable\n "
	.ascii	"      (unlikely, if you didn't compile bzip2 yourself.)\n   "
	.ascii	"(4) A real bug in bzip2 -- I hope this should never be the c"
	.ascii	"ase.\n   The user's manual, Section 4.3, has more info on (2"
	.ascii	") and (3).\n   \n   If you suspect this is a bug in bzip2, o"
	.ascii	"r are unsure about (2)\n   or (3), feel free to report it to"
	.ascii	" me at: jseward@acm.org.\n   Sect"
	.string	"ion 4.3 of the user's manual describes the info a useful\n   bug report should have.  If the manual is available on your\n   system, please try and read it before mailing me.  If you don't\n   have the manual or can't be bothered to read it, mail me anyway.\n\n"
	.text
	.p2align 4,,15
	.type	mySIGSEGVorSIGBUScatcher, @function
mySIGSEGVorSIGBUScatcher:
.LFB90:
	subq	$8, %rsp
.LCFI66:
	cmpl	$1, opMode(%rip)
	je	.L1453
	movq	progName(%rip), %rdx
	movq	stderr(%rip), %rdi
	movl	$.LC28, %esi
	xorl	%eax, %eax
	call	fprintf
.L1449:
	call	showFileNames
	cmpl	$1, opMode(%rip)
	jne	.L1450
	movl	$3, %edi
	call	cleanUpAndFail
	.p2align 4,,7
.L1453:
	movq	progName(%rip), %rdx
	movq	stderr(%rip), %rdi
	movl	$.LC27, %esi
	xorl	%eax, %eax
	call	fprintf
	jmp	.L1449
.L1450:
	call	cadvise
	movl	$2, %edi
	.p2align 4,,6
	call	cleanUpAndFail
.LFE90:
	.size	mySIGSEGVorSIGBUScatcher, .-mySIGSEGVorSIGBUScatcher
	.section	.rodata.str1.8
	.align 8
.LC29:
	.string	"\n%s: Control-C or similar caught, quitting.\n"
	.text
	.p2align 4,,15
	.type	mySignalCatcher, @function
mySignalCatcher:
.LFB89:
	subq	$8, %rsp
.LCFI67:
	movq	stderr(%rip), %rdi
	movq	progName(%rip), %rdx
	movl	$.LC29, %esi
	xorl	%eax, %eax
	call	fprintf
	movl	$1, %edi
	call	cleanUpAndFail
.LFE89:
	.size	mySignalCatcher, .-mySignalCatcher
	.section	.rodata.str1.8
	.align 8
.LC30:
	.string	"\n%s: PANIC -- internal consistency error:\n\t%s\n\tThis is a BUG.  Please report it to me at:\n\tjseward@acm.org\n"
	.text
	.p2align 4,,15
	.type	panic, @function
panic:
.LFB85:
	subq	$8, %rsp
.LCFI68:
	movq	%rdi, %rcx
	movq	progName(%rip), %rdx
	movq	stderr(%rip), %rdi
	movl	$.LC30, %esi
	xorl	%eax, %eax
	call	fprintf
	call	showFileNames
	movl	$3, %edi
	call	cleanUpAndFail
.LFE85:
	.size	panic, .-panic
	.section	.rodata.str1.1
.LC31:
	.string	"rb"
	.text
	.p2align 4,,15
	.type	fileExists, @function
fileExists:
.LFB95:
	pushq	%rbx
.LCFI69:
	movl	$.LC31, %esi
	call	fopen
	testq	%rax, %rax
	movq	%rax, %rbx
	je	.L1459
	movq	%rax, %rdi
	call	fclose
.L1459:
	xorl	%eax, %eax
	testq	%rbx, %rbx
	popq	%rbx
	setne	%al
	ret
.LFE95:
	.size	fileExists, .-fileExists
	.p2align 4,,15
	.type	countHardLinks, @function
countHardLinks:
.LFB98:
	subq	$152, %rsp
.LCFI70:
	movq	%rdi, %rsi
	movl	$1, %edi
	movq	%rsp, %rdx
	call	__lxstat
	xorl	%edx, %edx
	testl	%eax, %eax
	jne	.L1465
	movl	16(%rsp), %edx
	subl	$1, %edx
.L1465:
	movl	%edx, %eax
	addq	$152, %rsp
	ret
.LFE98:
	.size	countHardLinks, .-countHardLinks
	.p2align 4,,15
	.type	notAStandardFile, @function
notAStandardFile:
.LFB97:
	subq	$152, %rsp
.LCFI71:
	movq	%rdi, %rsi
	movl	$1, %edi
	movq	%rsp, %rdx
	call	__lxstat
	testl	%eax, %eax
	movl	$1, %edx
	jne	.L1470
	movl	24(%rsp), %eax
	xorl	%edx, %edx
	andl	$61440, %eax
	cmpl	$32768, %eax
	setne	%dl
.L1470:
	movl	%edx, %eax
	addq	$152, %rsp
	ret
.LFE97:
	.size	notAStandardFile, .-notAStandardFile
	.p2align 4,,15
	.type	pad, @function
pad:
.LFB93:
	pushq	%rbp
.LCFI72:
	movq	%rdi, %rbp
	pushq	%rbx
.LCFI73:
	subq	$8, %rsp
.LCFI74:
	call	strlen
	cmpl	%eax, longestFileName(%rip)
	jle	.L1477
	movl	$1, %ebx
	jmp	.L1475
	.p2align 4,,7
.L1478:
	movq	stderr(%rip), %rsi
	movl	$32, %edi
	addl	$1, %ebx
	call	fputc
.L1475:
	movq	%rbp, %rdi
	call	strlen
	movl	longestFileName(%rip), %edx
	subl	%eax, %edx
	cmpl	%edx, %ebx
	jle	.L1478
.L1477:
	addq	$8, %rsp
	popq	%rbx
	popq	%rbp
	ret
.LFE93:
	.size	pad, .-pad
	.section	.rodata.str1.1
.LC32:
	.string	" {0x%x, 0x%x}"
	.section	.rodata.str1.8
	.align 8
.LC33:
	.string	"\n    combined CRCs: stored = 0x%x, computed = 0x%x"
	.text
	.p2align 4,,15
.globl BZ2_bzDecompress
	.type	BZ2_bzDecompress, @function
BZ2_bzDecompress:
.LFB47:
	movq	%rbx, -40(%rsp)
.LCFI75:
	movq	%rbp, -32(%rsp)
.LCFI76:
	movq	%r12, -24(%rsp)
.LCFI77:
	movq	%r13, -16(%rsp)
.LCFI78:
	movq	%r14, -8(%rsp)
.LCFI79:
	subq	$40, %rsp
.LCFI80:
	testq	%rdi, %rdi
	jne	.L1656
.L1480:
	movl	$-2, %edx
.L1486:
	movq	(%rsp), %rbx
	movq	8(%rsp), %rbp
	movl	%edx, %eax
	movq	16(%rsp), %r12
	movq	24(%rsp), %r13
	movq	32(%rsp), %r14
	addq	$40, %rsp
	ret
	.p2align 4,,7
.L1656:
	movq	48(%rdi), %rbx
	testq	%rbx, %rbx
	je	.L1480
	cmpq	%rdi, (%rbx)
	jne	.L1480
	movl	8(%rbx), %eax
.L1650:
	cmpl	$1, %eax
	je	.L1626
.L1660:
	cmpl	$2, %eax
	.p2align 4,,3
	je	.L1657
	cmpl	$9, %eax
	.p2align 4,,3
	jle	.L1650
.L1605:
	movq	%rbx, %rdi
	.p2align 4,,5
	call	BZ2_decompress
	cmpl	$4, %eax
	.p2align 4,,2
	je	.L1658
	cmpl	$2, 8(%rbx)
	.p2align 4,,2
	jne	.L1659
	movl	$2, %eax
	cmpl	$1, %eax
	.p2align 4,,3
	jne	.L1660
.L1626:
	movl	$-1, %edx
	.p2align 4,,3
	jmp	.L1486
	.p2align 4,,7
.L1657:
	cmpb	$0, 44(%rbx)
	.p2align 4,,3
	je	.L1489
	cmpb	$0, 20(%rbx)
	.p2align 4,,5
	je	.L1653
.L1651:
	movq	(%rbx), %rcx
.L1652:
	movl	32(%rcx), %eax
	testl	%eax, %eax
	je	.L1493
.L1661:
	movl	16(%rbx), %eax
	testl	%eax, %eax
	je	.L1495
	movzbl	12(%rbx), %eax
	movq	24(%rcx), %rdx
	movb	%al, (%rdx)
	movl	3184(%rbx), %edx
	movq	(%rbx), %rcx
	movl	%edx, %eax
	sall	$8, %edx
	shrl	$24, %eax
	xorb	12(%rbx), %al
	addq	$1, 24(%rcx)
	movzbl	%al, %eax
	xorl	BZ2_crc32Table(,%rax,4), %edx
	subl	$1, 16(%rbx)
	movl	%edx, 3184(%rbx)
	addl	$1, 36(%rcx)
	subl	$1, 32(%rcx)
	movl	36(%rcx), %eax
	testl	%eax, %eax
	jne	.L1652
	movl	32(%rcx), %eax
	addl	$1, 40(%rcx)
	testl	%eax, %eax
	jne	.L1661
.L1493:
	movl	64080(%rbx), %eax
	addl	$1, %eax
	cmpl	%eax, 1092(%rbx)
	je	.L1498
.L1597:
	xorl	%edx, %edx
	jmp	.L1486
	.p2align 4,,7
.L1653:
	movq	(%rbx), %rcx
.L1654:
	movl	32(%rcx), %r9d
	testl	%r9d, %r9d
	je	.L1493
.L1662:
	movl	16(%rbx), %r8d
	testl	%r8d, %r8d
	je	.L1525
	movzbl	12(%rbx), %eax
	movq	24(%rcx), %rdx
	movb	%al, (%rdx)
	movl	3184(%rbx), %edx
	movq	(%rbx), %rcx
	movl	%edx, %eax
	sall	$8, %edx
	shrl	$24, %eax
	xorb	12(%rbx), %al
	addq	$1, 24(%rcx)
	movzbl	%al, %eax
	xorl	BZ2_crc32Table(,%rax,4), %edx
	subl	$1, 16(%rbx)
	movl	%edx, 3184(%rbx)
	addl	$1, 36(%rcx)
	subl	$1, 32(%rcx)
	movl	36(%rcx), %edi
	testl	%edi, %edi
	jne	.L1654
	movl	32(%rcx), %r9d
	addl	$1, 40(%rcx)
	testl	%r9d, %r9d
	jne	.L1662
	jmp	.L1493
	.p2align 4,,7
.L1495:
	movl	64080(%rbx), %eax
	addl	$1, %eax
	cmpl	%eax, 1092(%rbx)
	je	.L1498
	movl	64(%rbx), %eax
	movl	60(%rbx), %edi
	leaq	1096(%rbx), %rbp
	movl	$1, 16(%rbx)
	movq	%rbp, %rsi
	movb	%al, 12(%rbx)
	call	BZ2_indexIntoF
	mov	60(%rbx), %esi
	movq	3168(%rbx), %rdx
	movl	%eax, %edi
	movl	24(%rbx), %r14d
	movl	%esi, %eax
	leal	0(,%rsi,4), %ecx
	shrl	%eax
	mov	%eax, %eax
	andl	$4, %ecx
	movzbl	(%rdx,%rax), %eax
	movq	3160(%rbx), %rdx
	movzwl	(%rdx,%rsi,2), %edx
	shrl	%cl, %eax
	andl	$15, %eax
	sall	$16, %eax
	orl	%edx, %eax
	testl	%r14d, %r14d
	movl	%eax, 60(%rbx)
	jne	.L1500
	movl	28(%rbx), %edx
	movslq	%edx,%rax
	addl	$1, %edx
	movl	BZ2_rNums(,%rax,4), %eax
	movl	%eax, 24(%rbx)
	xorl	%eax, %eax
	cmpl	$512, %edx
	cmovne	%edx, %eax
	movl	%eax, 28(%rbx)
.L1500:
	movl	1092(%rbx), %edx
	movl	64080(%rbx), %eax
	movl	24(%rbx), %ecx
	addl	$1, %edx
	addl	$1, %eax
	subl	$1, %ecx
	cmpl	%eax, %edx
	movl	%edx, 1092(%rbx)
	movl	%ecx, 24(%rbx)
	je	.L1651
	subl	$1, %ecx
	sete	%al
	xorl	%edi, %eax
	movzbl	%al, %eax
	cmpl	64(%rbx), %eax
	je	.L1663
.L1644:
	movq	(%rbx), %rcx
	movl	%eax, 64(%rbx)
	jmp	.L1652
.L1525:
	movl	64080(%rbx), %eax
	addl	$1, %eax
	cmpl	%eax, 1092(%rbx)
	je	.L1498
	movl	64(%rbx), %eax
	movl	60(%rbx), %edi
	leaq	1096(%rbx), %rbp
	movl	$1, 16(%rbx)
	movq	%rbp, %rsi
	movb	%al, 12(%rbx)
	call	BZ2_indexIntoF
	mov	60(%rbx), %esi
	movq	3168(%rbx), %rdx
	movl	%eax, %r8d
	movl	%esi, %eax
	leal	0(,%rsi,4), %ecx
	shrl	%eax
	mov	%eax, %eax
	andl	$4, %ecx
	movzbl	(%rdx,%rax), %edi
	movq	3160(%rbx), %rax
	movl	1092(%rbx), %edx
	movzwl	(%rax,%rsi,2), %eax
	shrl	%cl, %edi
	addl	$1, %edx
	andl	$15, %edi
	movl	%edx, 1092(%rbx)
	sall	$16, %edi
	orl	%eax, %edi
	movl	64080(%rbx), %eax
	movl	%edi, 60(%rbx)
	addl	$1, %eax
	cmpl	%eax, %edx
	je	.L1653
	movzbl	%r8b, %eax
	cmpl	64(%rbx), %eax
	je	.L1664
.L1645:
	movq	(%rbx), %rcx
	movl	%eax, 64(%rbx)
	jmp	.L1654
.L1498:
	movl	16(%rbx), %eax
	testl	%eax, %eax
	jne	.L1597
	movl	3184(%rbx), %ecx
	cmpl	$2, 52(%rbx)
	notl	%ecx
	movl	%ecx, 3184(%rbx)
	jle	.L1599
	movl	3176(%rbx), %edx
	movq	stderr(%rip), %rdi
	movl	$.LC32, %esi
	xorl	%eax, %eax
	call	fprintf
.L1599:
	cmpl	$1, 52(%rbx)
	jle	.L1601
	movq	stderr(%rip), %rsi
	movl	$93, %edi
	call	fputc
.L1601:
	movl	3184(%rbx), %eax
	cmpl	3176(%rbx), %eax
	jne	.L1603
	movl	3188(%rbx), %eax
	movl	$14, 8(%rbx)
	rorl	$31, %eax
	xorl	3184(%rbx), %eax
	movl	%eax, 3188(%rbx)
	jmp	.L1605
.L1489:
	cmpb	$0, 20(%rbx)
	je	.L1538
	movq	(%rbx), %rcx
.L1655:
	movl	32(%rcx), %esi
	testl	%esi, %esi
	je	.L1493
.L1665:
	movl	16(%rbx), %edx
	testl	%edx, %edx
	je	.L1541
	movzbl	12(%rbx), %eax
	movq	24(%rcx), %rdx
	movb	%al, (%rdx)
	movl	3184(%rbx), %edx
	movq	(%rbx), %rcx
	movl	%edx, %eax
	sall	$8, %edx
	shrl	$24, %eax
	xorb	12(%rbx), %al
	addq	$1, 24(%rcx)
	movzbl	%al, %eax
	xorl	BZ2_crc32Table(,%rax,4), %edx
	subl	$1, 16(%rbx)
	movl	%edx, 3184(%rbx)
	addl	$1, 36(%rcx)
	subl	$1, 32(%rcx)
	movl	36(%rcx), %eax
	testl	%eax, %eax
	jne	.L1655
	movl	32(%rcx), %esi
	addl	$1, 40(%rcx)
	testl	%esi, %esi
	jne	.L1665
	jmp	.L1493
	.p2align 4,,7
.L1541:
	movl	64080(%rbx), %eax
	addl	$1, %eax
	cmpl	%eax, 1092(%rbx)
	je	.L1498
	movl	64(%rbx), %eax
	movq	3152(%rbx), %r8
	movl	$1, 16(%rbx)
	movb	%al, 12(%rbx)
	mov	60(%rbx), %eax
	movl	(%r8,%rax,4), %edi
	movl	%edi, %eax
	shrl	$8, %eax
	movl	%eax, 60(%rbx)
	movl	24(%rbx), %eax
	testl	%eax, %eax
	jne	.L1545
	movl	28(%rbx), %edx
	movslq	%edx,%rax
	addl	$1, %edx
	movl	BZ2_rNums(,%rax,4), %eax
	movl	%eax, 24(%rbx)
	xorl	%eax, %eax
	cmpl	$512, %edx
	cmovne	%edx, %eax
	movl	%eax, 28(%rbx)
.L1545:
	movl	1092(%rbx), %edx
	movl	64080(%rbx), %eax
	movl	24(%rbx), %esi
	addl	$1, %edx
	addl	$1, %eax
	subl	$1, %esi
	cmpl	%eax, %edx
	movl	%edx, 1092(%rbx)
	movl	%esi, 24(%rbx)
	je	.L1655
	cmpl	$1, %esi
	sete	%al
	xorl	%edi, %eax
	movzbl	%al, %eax
	cmpl	64(%rbx), %eax
	je	.L1666
.L1647:
	movl	%eax, 64(%rbx)
	jmp	.L1655
.L1538:
	movq	(%rbx), %rax
	movl	64080(%rbx), %ecx
	movzbl	12(%rbx), %r10d
	movl	3184(%rbx), %edi
	movl	16(%rbx), %esi
	movl	1092(%rbx), %r8d
	movl	32(%rax), %r14d
	movl	64(%rbx), %r9d
	addl	$1, %ecx
	movq	3152(%rbx), %r13
	movl	60(%rbx), %r11d
	movq	24(%rax), %rbp
	movl	%r14d, %r12d
.L1617:
	testl	%esi, %esi
	movl	%r9d, %edx
	jle	.L1573
.L1571:
	testl	%r12d, %r12d
	je	.L1574
	cmpl	$1, %esi
	movzbl	%r10b, %edx
	jne	.L1578
	.p2align 4,,5
	jmp	.L1649
	.p2align 4,,7
.L1639:
	cmpl	$1, %esi
	.p2align 4,,5
	je	.L1580
.L1578:
	movl	%edi, %eax
	movb	%r10b, (%rbp)
	sall	$8, %edi
	shrl	$24, %eax
	subl	$1, %esi
	addq	$1, %rbp
	xorl	%edx, %eax
	mov	%eax, %eax
	xorl	BZ2_crc32Table(,%rax,4), %edi
	subl	$1, %r12d
	jne	.L1639
.L1574:
	movq	(%rbx), %rcx
	movl	36(%rcx), %edx
	leal	(%rdx,%r14), %eax
	subl	%r12d, %eax
	cmpl	%eax, %edx
	movl	%eax, 36(%rcx)
	jbe	.L1595
	addl	$1, 40(%rcx)
.L1595:
	movb	%r10b, 12(%rbx)
	movl	%edi, 3184(%rbx)
	movl	%esi, 16(%rbx)
	movl	%r8d, 1092(%rbx)
	movl	%r9d, 64(%rbx)
	movl	%r11d, 60(%rbx)
	movq	%r13, 3152(%rbx)
	movq	%rbp, 24(%rcx)
	movl	%r12d, 32(%rcx)
	jmp	.L1493
.L1584:
	mov	%r11d, %eax
	movl	%r9d, %r10d
	addl	$1, %r8d
	movl	(%r13,%rax,4), %eax
	movl	%eax, %r11d
	movzbl	%al, %r9d
	shrl	$8, %r11d
	cmpl	%edx, %r9d
	je	.L1667
.L1581:
	testl	%r12d, %r12d
	je	.L1582
.L1649:
	movzbl	%r10b, %edx
.L1580:
	movl	%edi, %eax
	movb	%r10b, (%rbp)
	sall	$8, %edi
	shrl	$24, %eax
	addq	$1, %rbp
	subl	$1, %r12d
	xorl	%edx, %eax
	movl	%r9d, %edx
	mov	%eax, %eax
	xorl	BZ2_crc32Table(,%rax,4), %edi
.L1573:
	cmpl	%ecx, %r8d
	jne	.L1584
	xorl	%esi, %esi
	jmp	.L1574
	.p2align 4,,7
.L1666:
	mov	60(%rbx), %eax
	movl	$2, 16(%rbx)
	movl	(%r8,%rax,4), %edi
	movl	%edi, %eax
	shrl	$8, %eax
	movl	%eax, 60(%rbx)
	movl	24(%rbx), %eax
	testl	%eax, %eax
	jne	.L1551
	movl	28(%rbx), %eax
	movslq	%eax,%rdx
	addl	$1, %eax
	movl	BZ2_rNums(,%rdx,4), %edx
	cmpl	$512, %eax
	cmovne	%eax, %esi
	movl	%esi, 28(%rbx)
	movl	%edx, 24(%rbx)
.L1551:
	movl	1092(%rbx), %edx
	movl	64080(%rbx), %eax
	movl	24(%rbx), %esi
	addl	$1, %edx
	addl	$1, %eax
	subl	$1, %esi
	cmpl	%eax, %edx
	movl	%edx, 1092(%rbx)
	movl	%esi, 24(%rbx)
	je	.L1655
	cmpl	$1, %esi
	sete	%al
	xorl	%edi, %eax
	movzbl	%al, %eax
	cmpl	64(%rbx), %eax
	jne	.L1647
	mov	60(%rbx), %eax
	movl	$3, 16(%rbx)
	movl	(%r8,%rax,4), %edi
	movl	%edi, %eax
	shrl	$8, %eax
	movl	%eax, 60(%rbx)
	movl	24(%rbx), %eax
	testl	%eax, %eax
	jne	.L1557
	movl	28(%rbx), %eax
	movslq	%eax,%rdx
	addl	$1, %eax
	movl	BZ2_rNums(,%rdx,4), %edx
	cmpl	$512, %eax
	cmovne	%eax, %esi
	movl	%esi, 28(%rbx)
	movl	%edx, 24(%rbx)
.L1557:
	movl	1092(%rbx), %edx
	movl	64080(%rbx), %eax
	movl	24(%rbx), %esi
	addl	$1, %edx
	addl	$1, %eax
	subl	$1, %esi
	cmpl	%eax, %edx
	movl	%edx, 1092(%rbx)
	movl	%esi, 24(%rbx)
	je	.L1655
	cmpl	$1, %esi
	sete	%al
	xorl	%edi, %eax
	movzbl	%al, %eax
	cmpl	64(%rbx), %eax
	jne	.L1647
	mov	60(%rbx), %eax
	movl	(%r8,%rax,4), %edi
	movl	%edi, %eax
	shrl	$8, %eax
	movl	%eax, 60(%rbx)
	movl	24(%rbx), %eax
	testl	%eax, %eax
	jne	.L1563
	movl	28(%rbx), %eax
	movslq	%eax,%rdx
	addl	$1, %eax
	movl	BZ2_rNums(,%rdx,4), %edx
	cmpl	$512, %eax
	cmovne	%eax, %esi
	movl	%esi, 28(%rbx)
	movl	%edx, 24(%rbx)
.L1563:
	movl	24(%rbx), %esi
	addl	$1, 1092(%rbx)
	subl	$1, %esi
	cmpl	$1, %esi
	movl	%esi, 24(%rbx)
	sete	%al
	xorl	%edi, %eax
	movzbl	%al, %eax
	addl	$4, %eax
	movl	%eax, 16(%rbx)
	mov	60(%rbx), %eax
	movl	(%r8,%rax,4), %eax
	movl	%eax, 60(%rbx)
	movzbl	%al, %eax
	shrl	$8, 60(%rbx)
	testl	%esi, %esi
	movl	%eax, 64(%rbx)
	jne	.L1566
	movl	28(%rbx), %eax
	movslq	%eax,%rdx
	addl	$1, %eax
	movl	BZ2_rNums(,%rdx,4), %edx
	cmpl	$512, %eax
	cmovne	%eax, %esi
	movl	%esi, 28(%rbx)
	movl	%edx, 24(%rbx)
.L1566:
	movl	24(%rbx), %eax
	subl	$1, %eax
	movl	%eax, 24(%rbx)
	subl	$1, %eax
	sete	%al
	addl	$1, 1092(%rbx)
	movzbl	%al, %eax
	xorl	%eax, 64(%rbx)
	jmp	.L1655
	.p2align 4,,7
.L1663:
	movl	60(%rbx), %edi
	movq	%rbp, %rsi
	movl	$2, 16(%rbx)
	call	BZ2_indexIntoF
	mov	60(%rbx), %esi
	movq	3168(%rbx), %rdx
	movl	%eax, %edi
	movl	24(%rbx), %r13d
	movl	%esi, %eax
	leal	0(,%rsi,4), %ecx
	shrl	%eax
	mov	%eax, %eax
	andl	$4, %ecx
	movzbl	(%rdx,%rax), %eax
	movq	3160(%rbx), %rdx
	movzwl	(%rdx,%rsi,2), %edx
	shrl	%cl, %eax
	andl	$15, %eax
	sall	$16, %eax
	orl	%edx, %eax
	testl	%r13d, %r13d
	movl	%eax, 60(%rbx)
	jne	.L1506
	movl	28(%rbx), %edx
	movslq	%edx,%rax
	addl	$1, %edx
	movl	BZ2_rNums(,%rax,4), %eax
	movl	%eax, 24(%rbx)
	xorl	%eax, %eax
	cmpl	$512, %edx
	cmovne	%edx, %eax
	movl	%eax, 28(%rbx)
.L1506:
	movl	1092(%rbx), %edx
	movl	64080(%rbx), %eax
	movl	24(%rbx), %ecx
	addl	$1, %edx
	addl	$1, %eax
	subl	$1, %ecx
	cmpl	%eax, %edx
	movl	%edx, 1092(%rbx)
	movl	%ecx, 24(%rbx)
	je	.L1651
	subl	$1, %ecx
	sete	%al
	xorl	%edi, %eax
	movzbl	%al, %eax
	cmpl	64(%rbx), %eax
	jne	.L1644
	movl	60(%rbx), %edi
	movq	%rbp, %rsi
	movl	$3, 16(%rbx)
	call	BZ2_indexIntoF
	mov	60(%rbx), %esi
	movq	3168(%rbx), %rdx
	movl	%eax, %edi
	movl	24(%rbx), %r12d
	movl	%esi, %eax
	leal	0(,%rsi,4), %ecx
	shrl	%eax
	mov	%eax, %eax
	andl	$4, %ecx
	movzbl	(%rdx,%rax), %eax
	movq	3160(%rbx), %rdx
	movzwl	(%rdx,%rsi,2), %edx
	shrl	%cl, %eax
	andl	$15, %eax
	sall	$16, %eax
	orl	%edx, %eax
	testl	%r12d, %r12d
	movl	%eax, 60(%rbx)
	jne	.L1512
	movl	28(%rbx), %edx
	movslq	%edx,%rax
	addl	$1, %edx
	movl	BZ2_rNums(,%rax,4), %eax
	movl	%eax, 24(%rbx)
	xorl	%eax, %eax
	cmpl	$512, %edx
	cmovne	%edx, %eax
	movl	%eax, 28(%rbx)
.L1512:
	movl	1092(%rbx), %edx
	movl	64080(%rbx), %eax
	movl	24(%rbx), %ecx
	addl	$1, %edx
	addl	$1, %eax
	subl	$1, %ecx
	cmpl	%eax, %edx
	movl	%edx, 1092(%rbx)
	movl	%ecx, 24(%rbx)
	je	.L1651
	subl	$1, %ecx
	sete	%al
	xorl	%edi, %eax
	movzbl	%al, %eax
	cmpl	64(%rbx), %eax
	jne	.L1644
	movl	60(%rbx), %edi
	movq	%rbp, %rsi
	call	BZ2_indexIntoF
	mov	60(%rbx), %esi
	movq	3168(%rbx), %rdx
	movl	%eax, %edi
	movl	24(%rbx), %r11d
	movl	%esi, %eax
	leal	0(,%rsi,4), %ecx
	shrl	%eax
	mov	%eax, %eax
	andl	$4, %ecx
	movzbl	(%rdx,%rax), %eax
	movq	3160(%rbx), %rdx
	movzwl	(%rdx,%rsi,2), %edx
	shrl	%cl, %eax
	andl	$15, %eax
	sall	$16, %eax
	orl	%edx, %eax
	testl	%r11d, %r11d
	movl	%eax, 60(%rbx)
	jne	.L1518
	movl	28(%rbx), %edx
	movslq	%edx,%rax
	addl	$1, %edx
	movl	BZ2_rNums(,%rax,4), %eax
	movl	%eax, 24(%rbx)
	xorl	%eax, %eax
	cmpl	$512, %edx
	cmovne	%edx, %eax
	movl	%eax, 28(%rbx)
.L1518:
	movl	24(%rbx), %eax
	addl	$1, 1092(%rbx)
	movq	%rbp, %rsi
	subl	$1, %eax
	movl	%eax, 24(%rbx)
	subl	$1, %eax
	sete	%al
	xorl	%edi, %eax
	movl	60(%rbx), %edi
	movzbl	%al, %eax
	addl	$4, %eax
	movl	%eax, 16(%rbx)
	call	BZ2_indexIntoF
	mov	60(%rbx), %esi
	movl	%eax, 64(%rbx)
	movq	3168(%rbx), %rdx
	movl	24(%rbx), %r10d
	movl	%esi, %eax
	leal	0(,%rsi,4), %ecx
	shrl	%eax
	mov	%eax, %eax
	andl	$4, %ecx
	movzbl	(%rdx,%rax), %eax
	movq	3160(%rbx), %rdx
	movzwl	(%rdx,%rsi,2), %edx
	shrl	%cl, %eax
	andl	$15, %eax
	sall	$16, %eax
	orl	%edx, %eax
	testl	%r10d, %r10d
	movl	%eax, 60(%rbx)
	jne	.L1521
	movl	28(%rbx), %edx
	movslq	%edx,%rax
	addl	$1, %edx
	movl	BZ2_rNums(,%rax,4), %eax
	movl	%eax, 24(%rbx)
	xorl	%eax, %eax
	cmpl	$512, %edx
	cmovne	%edx, %eax
	movl	%eax, 28(%rbx)
.L1521:
	movl	24(%rbx), %eax
	movq	(%rbx), %rcx
	subl	$1, %eax
	movl	%eax, 24(%rbx)
	subl	$1, %eax
	sete	%al
	addl	$1, 1092(%rbx)
	movzbl	%al, %eax
	xorl	%eax, 64(%rbx)
	jmp	.L1652
	.p2align 4,,7
.L1664:
	movq	%rbp, %rsi
	movl	$2, 16(%rbx)
	call	BZ2_indexIntoF
	mov	60(%rbx), %esi
	movq	3168(%rbx), %rdx
	movl	%eax, %r8d
	movl	%esi, %eax
	leal	0(,%rsi,4), %ecx
	shrl	%eax
	mov	%eax, %eax
	andl	$4, %ecx
	movzbl	(%rdx,%rax), %edi
	movq	3160(%rbx), %rax
	movl	1092(%rbx), %edx
	movzwl	(%rax,%rsi,2), %eax
	shrl	%cl, %edi
	addl	$1, %edx
	andl	$15, %edi
	movl	%edx, 1092(%rbx)
	sall	$16, %edi
	orl	%eax, %edi
	movl	64080(%rbx), %eax
	movl	%edi, 60(%rbx)
	addl	$1, %eax
	cmpl	%eax, %edx
	je	.L1653
	movzbl	%r8b, %eax
	cmpl	64(%rbx), %eax
	jne	.L1645
	movq	%rbp, %rsi
	movl	$3, 16(%rbx)
	call	BZ2_indexIntoF
	mov	60(%rbx), %esi
	movq	3168(%rbx), %rdx
	movl	%eax, %r8d
	movl	%esi, %eax
	leal	0(,%rsi,4), %ecx
	shrl	%eax
	mov	%eax, %eax
	andl	$4, %ecx
	movzbl	(%rdx,%rax), %edi
	movq	3160(%rbx), %rax
	movl	1092(%rbx), %edx
	movzwl	(%rax,%rsi,2), %eax
	shrl	%cl, %edi
	addl	$1, %edx
	andl	$15, %edi
	movl	%edx, 1092(%rbx)
	sall	$16, %edi
	orl	%eax, %edi
	movl	64080(%rbx), %eax
	movl	%edi, 60(%rbx)
	addl	$1, %eax
	cmpl	%eax, %edx
	je	.L1653
	movzbl	%r8b, %eax
	cmpl	64(%rbx), %eax
	jne	.L1645
	movq	%rbp, %rsi
	call	BZ2_indexIntoF
	mov	60(%rbx), %esi
	movq	3168(%rbx), %rcx
	movzbl	%al, %eax
	addl	$4, %eax
	movl	%esi, %edx
	shrl	%edx
	mov	%edx, %edx
	movzbl	(%rcx,%rdx), %edi
	movq	3160(%rbx), %rdx
	leal	0(,%rsi,4), %ecx
	addl	$1, 1092(%rbx)
	movl	%eax, 16(%rbx)
	andl	$4, %ecx
	movzwl	(%rdx,%rsi,2), %edx
	movq	%rbp, %rsi
	shrl	%cl, %edi
	andl	$15, %edi
	sall	$16, %edi
	orl	%edx, %edi
	movl	%edi, 60(%rbx)
	call	BZ2_indexIntoF
	mov	60(%rbx), %esi
	movl	%eax, 64(%rbx)
	movq	3168(%rbx), %rdx
	movl	%esi, %eax
	leal	0(,%rsi,4), %ecx
	shrl	%eax
	mov	%eax, %eax
	andl	$4, %ecx
	movzbl	(%rdx,%rax), %eax
	movq	3160(%rbx), %rdx
	addl	$1, 1092(%rbx)
	movzwl	(%rdx,%rsi,2), %edx
	shrl	%cl, %eax
	movq	(%rbx), %rcx
	andl	$15, %eax
	sall	$16, %eax
	orl	%edx, %eax
	movl	%eax, 60(%rbx)
	jmp	.L1654
.L1659:
	movl	%eax, %edx
	jmp	.L1486
.L1658:
	cmpl	$2, 52(%rbx)
	jle	.L1608
	movl	3188(%rbx), %ecx
	movl	3180(%rbx), %edx
	movl	$.LC33, %esi
	movq	stderr(%rip), %rdi
	xorl	%eax, %eax
	call	fprintf
.L1608:
	movl	3188(%rbx), %eax
	cmpl	3180(%rbx), %eax
	movl	$4, %edx
	je	.L1486
.L1603:
	movl	$-4, %edx
	jmp	.L1486
.L1582:
	movl	$1, %esi
	jmp	.L1574
.L1667:
	cmpl	%ecx, %r8d
	.p2align 4,,5
	je	.L1581
	mov	%r11d, %eax
	addl	$1, %r8d
	movl	(%r13,%rax,4), %eax
	movl	%eax, %r11d
	shrl	$8, %r11d
	cmpl	%ecx, %r8d
	je	.L1648
	movzbl	%al, %eax
	cmpl	%eax, %r9d
	je	.L1589
	movl	%eax, %r9d
.L1648:
	movl	$2, %esi
	jmp	.L1571
.L1589:
	mov	%r11d, %eax
	addl	$1, %r8d
	movl	$3, %esi
	movl	(%r13,%rax,4), %eax
	movl	%eax, %r11d
	shrl	$8, %r11d
	cmpl	%ecx, %r8d
	je	.L1571
	movzbl	%al, %eax
	cmpl	%eax, %r9d
	je	.L1593
	movl	%eax, %r9d
	jmp	.L1571
.L1593:
	mov	%r11d, %eax
	addl	$2, %r8d
	movl	(%r13,%rax,4), %eax
	movzbl	%al, %edx
	shrl	$8, %eax
	mov	%eax, %eax
	leal	4(%rdx), %esi
	movl	(%r13,%rax,4), %eax
	movl	%eax, %r11d
	movzbl	%al, %r9d
	shrl	$8, %r11d
	jmp	.L1617
.LFE47:
	.size	BZ2_bzDecompress, .-BZ2_bzDecompress
	.p2align 4,,15
.globl BZ2_bzBuffToBuffDecompress
	.type	BZ2_bzBuffToBuffDecompress, @function
BZ2_bzBuffToBuffDecompress:
.LFB59:
	movq	%rbp, -40(%rsp)
.LCFI81:
	movq	%r12, -32(%rsp)
.LCFI82:
	movq	%rdi, %rbp
	movq	%r13, -24(%rsp)
.LCFI83:
	movq	%r14, -16(%rsp)
.LCFI84:
	movq	%rsi, %r12
	movq	%rbx, -48(%rsp)
.LCFI85:
	movq	%r15, -8(%rsp)
.LCFI86:
	subq	$136, %rsp
.LCFI87:
	testq	%rdi, %rdi
	movq	%rdx, %r13
	movl	%ecx, %r14d
	movl	%r8d, %edx
	movl	%r9d, %esi
	jne	.L1686
.L1669:
	movl	$-2, %ebx
.L1677:
	movl	%ebx, %eax
	movq	96(%rsp), %rbp
	movq	88(%rsp), %rbx
	movq	104(%rsp), %r12
	movq	112(%rsp), %r13
	movq	120(%rsp), %r14
	movq	128(%rsp), %r15
	addq	$136, %rsp
	ret
	.p2align 4,,7
.L1686:
	testq	%r12, %r12
	je	.L1669
	testq	%r13, %r13
	je	.L1669
	testl	%r8d, %r8d
	.p2align 4,,5
	je	.L1673
	cmpl	$1, %r8d
	.p2align 4,,5
	jne	.L1669
.L1673:
	testl	%esi, %esi
	.p2align 4,,5
	js	.L1669
	cmpl	$4, %esi
	.p2align 4,,5
	jg	.L1669
	movq	%rsp, %rdi
	movq	$0, 56(%rsp)
	movq	$0, 64(%rsp)
	movq	$0, 72(%rsp)
	call	BZ2_bzDecompressInit
	testl	%eax, %eax
	movl	%eax, %ebx
	jne	.L1677
	movl	(%r12), %eax
	movq	%rsp, %rdi
	movq	%rbp, 24(%rsp)
	movq	%r13, (%rsp)
	movl	%r14d, 8(%rsp)
	movl	%eax, 32(%rsp)
	call	BZ2_bzDecompress
	testl	%eax, %eax
	movl	%eax, %ebp
	je	.L1679
	cmpl	$4, %eax
	je	.L1687
.L1681:
	movq	%rsp, %rdi
	movl	%ebp, %ebx
	call	BZ2_bzDecompressEnd
	.p2align 4,,2
	jmp	.L1677
.L1679:
	movl	32(%rsp), %eax
	testl	%eax, %eax
	je	.L1683
	movq	%rsp, %rdi
	movl	$-7, %ebx
	call	BZ2_bzDecompressEnd
	jmp	.L1677
.L1687:
	movl	32(%rsp), %eax
	subl	%eax, (%r12)
	movq	%rsp, %rdi
	call	BZ2_bzDecompressEnd
	jmp	.L1677
.L1683:
	movq	%rsp, %rdi
	movl	$-8, %ebx
	call	BZ2_bzDecompressEnd
	.p2align 4,,4
	jmp	.L1677
.LFE59:
	.size	BZ2_bzBuffToBuffDecompress, .-BZ2_bzBuffToBuffDecompress
	.p2align 4,,15
.globl BZ2_bzReadOpen
	.type	BZ2_bzReadOpen, @function
BZ2_bzReadOpen:
.LFB54:
	movq	%rbx, -48(%rsp)
.LCFI88:
	movq	%r12, -32(%rsp)
.LCFI89:
	movl	%r9d, %ebx
	movq	%r13, -24(%rsp)
.LCFI90:
	movq	%r14, -16(%rsp)
.LCFI91:
	movq	%rdi, %r12
	movq	%r15, -8(%rsp)
.LCFI92:
	movq	%rbp, -40(%rsp)
.LCFI93:
	subq	$56, %rsp
.LCFI94:
	testq	%rdi, %rdi
	movq	%rsi, %r14
	movl	%edx, 4(%rsp)
	movl	%ecx, %r15d
	movq	%r8, %r13
	je	.L1689
	movl	$0, (%rdi)
.L1689:
	testq	%r14, %r14
	je	.L1701
	testl	%r15d, %r15d
	jne	.L1723
.L1693:
	movl	4(%rsp), %eax
	testl	%eax, %eax
	js	.L1701
	cmpl	$4, 4(%rsp)
	jg	.L1701
	testq	%r13, %r13
	je	.L1724
	testl	%ebx, %ebx
	.p2align 4,,4
	js	.L1701
	cmpl	$5000, %ebx
	.p2align 4,,5
	jg	.L1701
.L1699:
	movq	%r14, %rdi
	.p2align 4,,5
	call	ferror
	testl	%eax, %eax
	.p2align 4,,2
	jne	.L1725
	movl	$5104, %edi
	call	malloc
	testq	%rax, %rax
	movq	%rax, %rbp
	je	.L1726
	testq	%r12, %r12
	je	.L1711
	movl	$0, (%r12)
.L1711:
	testl	%ebx, %ebx
	movl	$0, 5096(%rbp)
	movb	$0, 5100(%rbp)
	movq	%r14, (%rbp)
	movl	$0, 5008(%rbp)
	movb	$0, 5012(%rbp)
	movq	$0, 5072(%rbp)
	movq	$0, 5080(%rbp)
	movq	$0, 5088(%rbp)
	jle	.L1713
	xorl	%ecx, %ecx
	.p2align 4,,7
.L1714:
	movzbl	(%r13), %eax
	movslq	%ecx,%rdx
	addq	$1, %r13
	addl	$1, %ecx
	subl	$1, %ebx
	movb	%al, 8(%rbp,%rdx)
	jne	.L1714
	movl	%ecx, 5008(%rbp)
.L1713:
	movl	4(%rsp), %esi
	leaq	5016(%rbp), %rdi
	movl	%r15d, %edx
	call	BZ2_bzDecompressInit
	testl	%eax, %eax
	jne	.L1727
	movl	5008(%rbp), %eax
	movb	$1, 5100(%rbp)
	movl	%eax, 5024(%rbp)
	leaq	8(%rbp), %rax
	movq	%rax, 5016(%rbp)
	movq	%rbp, %rax
	jmp	.L1704
	.p2align 4,,7
.L1723:
	cmpl	$1, %r15d
	je	.L1693
.L1691:
.L1701:
	testq	%r12, %r12
	jne	.L1728
.L1702:
	xorl	%eax, %eax
.L1704:
	movq	8(%rsp), %rbx
	movq	16(%rsp), %rbp
	movq	24(%rsp), %r12
	movq	32(%rsp), %r13
	movq	40(%rsp), %r14
	movq	48(%rsp), %r15
	addq	$56, %rsp
	ret
	.p2align 4,,7
.L1728:
	xorl	%eax, %eax
	movl	$-2, (%r12)
	jmp	.L1704
.L1724:
	testl	%ebx, %ebx
	jne	.L1701
	.p2align 4,,5
	jmp	.L1699
.L1725:
	testq	%r12, %r12
	.p2align 4,,7
	je	.L1702
	xorl	%eax, %eax
	movl	$-6, (%r12)
	.p2align 4,,5
	jmp	.L1704
.L1727:
	testq	%r12, %r12
	.p2align 4,,3
	je	.L1717
	movl	%eax, (%r12)
.L1717:
	movq	%rbp, %rdi
	movl	%eax, 5096(%rbp)
	call	free
	xorl	%eax, %eax
	jmp	.L1704
.L1726:
	testq	%r12, %r12
	je	.L1702
	xorl	%eax, %eax
	movl	$-3, (%r12)
	.p2align 4,,3
	jmp	.L1704
.LFE54:
	.size	BZ2_bzReadOpen, .-BZ2_bzReadOpen
	.p2align 4,,15
.globl BZ2_bzWriteOpen
	.type	BZ2_bzWriteOpen, @function
BZ2_bzWriteOpen:
.LFB50:
	movq	%rbx, -48(%rsp)
.LCFI95:
	movq	%rbp, -40(%rsp)
.LCFI96:
	movq	%rdi, %rbx
	movq	%r12, -32(%rsp)
.LCFI97:
	movq	%r13, -24(%rsp)
.LCFI98:
	movq	%rsi, %r12
	movq	%r14, -16(%rsp)
.LCFI99:
	movq	%r15, -8(%rsp)
.LCFI100:
	subq	$56, %rsp
.LCFI101:
	testq	%rdi, %rdi
	movl	%edx, %ebp
	movl	%ecx, %r14d
	movl	%r8d, %r13d
	je	.L1730
	movl	$0, (%rdi)
.L1730:
	testq	%r12, %r12
	je	.L1740
	testl	%ebp, %ebp
	jle	.L1740
	cmpl	$9, %ebp
	.p2align 4,,3
	jg	.L1740
	testl	%r13d, %r13d
	.p2align 4,,5
	js	.L1740
	cmpl	$250, %r13d
	.p2align 4,,5
	jg	.L1740
	testl	%r14d, %r14d
	.p2align 4,,5
	js	.L1740
	cmpl	$4, %r14d
	.p2align 4,,5
	jle	.L1739
.L1732:
	.p2align 4,,7
.L1740:
	testq	%rbx, %rbx
	.p2align 4,,5
	jne	.L1759
.L1741:
	xorl	%eax, %eax
.L1743:
	movq	8(%rsp), %rbx
	movq	16(%rsp), %rbp
	movq	24(%rsp), %r12
	movq	32(%rsp), %r13
	movq	40(%rsp), %r14
	movq	48(%rsp), %r15
	addq	$56, %rsp
	ret
	.p2align 4,,7
.L1759:
	xorl	%eax, %eax
	movl	$-2, (%rbx)
	jmp	.L1743
.L1739:
	movq	%r12, %rdi
	call	ferror
	testl	%eax, %eax
	jne	.L1760
	movl	$5104, %edi
	call	malloc
	testq	%rax, %rax
	movq	%rax, %r15
	je	.L1761
	testq	%rbx, %rbx
	je	.L1750
	movl	$0, (%rbx)
.L1750:
	testl	%r13d, %r13d
	movl	$30, %eax
	leaq	5016(%r15), %rdi
	cmove	%eax, %r13d
	movl	%r14d, %edx
	movl	%ebp, %esi
	movl	%r13d, %ecx
	movl	$0, 5096(%r15)
	movb	$0, 5100(%r15)
	movl	$0, 5008(%r15)
	movq	%r12, (%r15)
	movb	$1, 5012(%r15)
	movq	$0, 5072(%r15)
	movq	$0, 5080(%r15)
	movq	$0, 5088(%r15)
	call	BZ2_bzCompressInit
	testl	%eax, %eax
	jne	.L1762
	movq	%r15, %rax
	movl	$0, 5024(%r15)
	movb	$1, 5100(%r15)
	jmp	.L1743
.L1760:
	testq	%rbx, %rbx
	je	.L1741
	xorl	%eax, %eax
	movl	$-6, (%rbx)
	jmp	.L1743
.L1762:
	testq	%rbx, %rbx
	.p2align 4,,3
	je	.L1756
	movl	%eax, (%rbx)
.L1756:
	movq	%r15, %rdi
	movl	%eax, 5096(%r15)
	call	free
	xorl	%eax, %eax
	jmp	.L1743
.L1761:
	testq	%rbx, %rbx
	je	.L1741
	xorl	%eax, %eax
	movl	$-3, (%rbx)
	.p2align 4,,3
	jmp	.L1743
.LFE50:
	.size	BZ2_bzWriteOpen, .-BZ2_bzWriteOpen
	.p2align 4,,15
	.type	myfeof, @function
myfeof:
.LFB49:
	pushq	%rbx
.LCFI102:
	movq	%rdi, %rbx
	call	fgetc
	cmpl	$-1, %eax
	movl	$1, %edx
	je	.L1766
	movq	%rbx, %rsi
	movl	%eax, %edi
	call	ungetc
	xorl	%edx, %edx
.L1766:
	popq	%rbx
	movl	%edx, %eax
	ret
.LFE49:
	.size	myfeof, .-myfeof
	.p2align 4,,15
.globl BZ2_bzRead
	.type	BZ2_bzRead, @function
BZ2_bzRead:
.LFB56:
	movq	%rbx, -40(%rsp)
.LCFI103:
	movq	%rbp, -32(%rsp)
.LCFI104:
	movq	%rsi, %rbx
	movq	%r12, -24(%rsp)
.LCFI105:
	movq	%r13, -16(%rsp)
.LCFI106:
	movq	%rdi, %rbp
	movq	%r14, -8(%rsp)
.LCFI107:
	subq	$40, %rsp
.LCFI108:
	testq	%rdi, %rdi
	movl	%ecx, %r12d
	je	.L1769
	movl	$0, (%rdi)
.L1769:
	testq	%rbx, %rbx
	je	.L1771
	testq	%rdx, %rdx
	movl	$0, 5096(%rbx)
	je	.L1771
	testl	%r12d, %r12d
	js	.L1771
	cmpb	$0, 5012(%rbx)
	je	.L1780
	testq	%rbp, %rbp
	.p2align 4,,2
	je	.L1782
	movl	$-1, (%rbp)
.L1782:
	movl	$-1, 5096(%rbx)
	xorl	%r12d, %r12d
	.p2align 4,,7
.L1779:
	movl	%r12d, %eax
	movq	(%rsp), %rbx
	movq	8(%rsp), %rbp
	movq	16(%rsp), %r12
	movq	24(%rsp), %r13
	movq	32(%rsp), %r14
	addq	$40, %rsp
	ret
	.p2align 4,,7
.L1780:
	testl	%r12d, %r12d
	je	.L1809
	leaq	8(%rbx), %r14
	movl	%r12d, 5048(%rbx)
	movq	%rdx, 5040(%rbx)
	jmp	.L1788
	.p2align 4,,7
.L1793:
	leaq	5016(%rbx), %rdi
	call	BZ2_bzDecompress
	testl	%eax, %eax
	jne	.L1821
	movq	(%rbx), %rdi
	call	myfeof
	testb	%al, %al
	.p2align 4,,2
	je	.L1806
	movl	5024(%rbx), %ecx
	testl	%ecx, %ecx
	je	.L1822
.L1806:
	movl	5048(%rbx), %eax
	testl	%eax, %eax
	je	.L1809
.L1788:
	movq	(%rbx), %rdi
	call	ferror
	testl	%eax, %eax
	jne	.L1819
	movl	5024(%rbx), %esi
	testl	%esi, %esi
	jne	.L1793
	movq	(%rbx), %rdi
	call	myfeof
	testb	%al, %al
	jne	.L1793
	movq	(%rbx), %rcx
	movl	$5000, %edx
	movl	$1, %esi
	movq	%r14, %rdi
	call	fread
	movq	(%rbx), %rdi
	movq	%rax, %r13
	call	ferror
	testl	%eax, %eax
	jne	.L1819
	movl	%r13d, 5008(%rbx)
	movl	%r13d, 5024(%rbx)
	movq	%r14, 5016(%rbx)
	jmp	.L1793
	.p2align 4,,7
.L1809:
	testq	%rbp, %rbp
	je	.L1815
	movl	$0, (%rbp)
.L1815:
	movl	$0, 5096(%rbx)
	jmp	.L1779
	.p2align 4,,7
.L1771:
	testq	%rbp, %rbp
	je	.L1775
	movl	$-2, (%rbp)
.L1775:
	xorl	%r12d, %r12d
	testq	%rbx, %rbx
	je	.L1779
	movl	$-2, 5096(%rbx)
	jmp	.L1779
.L1819:
	testq	%rbp, %rbp
	je	.L1798
	movl	$-6, (%rbp)
.L1798:
	xorl	%r12d, %r12d
	movl	$-6, 5096(%rbx)
	jmp	.L1779
.L1821:
	cmpl	$4, %eax
	je	.L1802
	testq	%rbp, %rbp
	je	.L1804
	movl	%eax, (%rbp)
.L1804:
	xorl	%r12d, %r12d
	movl	%eax, 5096(%rbx)
	jmp	.L1779
.L1822:
	movl	5048(%rbx), %edx
	testl	%edx, %edx
	je	.L1809
	testq	%rbp, %rbp
	je	.L1811
	movl	$-7, (%rbp)
.L1811:
	xorl	%r12d, %r12d
	movl	$-7, 5096(%rbx)
	jmp	.L1779
.L1802:
	testq	%rbp, %rbp
	je	.L1813
	movl	$4, (%rbp)
.L1813:
	subl	5048(%rbx), %r12d
	movl	$4, 5096(%rbx)
	jmp	.L1779
.LFE56:
	.size	BZ2_bzRead, .-BZ2_bzRead
	.p2align 4,,15
.globl BZ2_bzread
	.type	BZ2_bzread, @function
BZ2_bzread:
.LFB64:
	subq	$24, %rsp
.LCFI109:
	xorl	%ecx, %ecx
	cmpl	$4, 5096(%rdi)
	movq	%rdi, %rax
	je	.L1826
	leaq	20(%rsp), %rdi
	movl	%edx, %ecx
	movq	%rsi, %rdx
	movq	%rax, %rsi
	call	BZ2_bzRead
	movl	%eax, %ecx
	movl	20(%rsp), %eax
	testl	%eax, %eax
	jne	.L1830
.L1826:
	movl	%ecx, %eax
	addq	$24, %rsp
	ret
.L1830:
	cmpl	$4, %eax
	movl	$-1, %eax
	cmovne	%eax, %ecx
	jmp	.L1826
.LFE64:
	.size	BZ2_bzread, .-BZ2_bzread
	.section	.rodata.str1.8
	.align 8
.LC34:
	.string	"\n%s: I/O or other error, bailing out.  Possible reason follows.\n"
	.text
	.p2align 4,,15
	.type	ioError, @function
ioError:
.LFB88:
	subq	$8, %rsp
.LCFI110:
	movq	progName(%rip), %rdx
	movq	stderr(%rip), %rdi
	movl	$.LC34, %esi
	xorl	%eax, %eax
	call	fprintf
	movq	progName(%rip), %rdi
	call	perror
	call	showFileNames
	movl	$1, %edi
	call	cleanUpAndFail
.LFE88:
	.size	ioError, .-ioError
	.p2align 4,,15
	.type	applySavedMetaInfoToOutputFile, @function
applySavedMetaInfoToOutputFile:
.LFB100:
	pushq	%rbx
.LCFI111:
	movq	%rdi, %rbx
	subq	$16, %rsp
.LCFI112:
	movq	fileMetaInfo+72(%rip), %rax
	movl	fileMetaInfo+24(%rip), %esi
	movq	%rax, (%rsp)
	movq	fileMetaInfo+88(%rip), %rax
	movq	%rax, 8(%rsp)
	call	chmod
	testl	%eax, %eax
	jne	.L1839
	movq	%rsp, %rsi
	movq	%rbx, %rdi
	call	utime
	testl	%eax, %eax
	jne	.L1839
	movl	fileMetaInfo+32(%rip), %edx
	movl	fileMetaInfo+28(%rip), %esi
	movq	%rbx, %rdi
	call	chown
	addq	$16, %rsp
	popq	%rbx
	ret
.L1839:
	call	ioError
.LFE100:
	.size	applySavedMetaInfoToOutputFile, .-applySavedMetaInfoToOutputFile
	.p2align 4,,15
	.type	saveInputFileMetaInfo, @function
saveInputFileMetaInfo:
.LFB99:
	subq	$8, %rsp
.LCFI113:
	movq	%rdi, %rsi
	movl	$fileMetaInfo, %edx
	movl	$1, %edi
	call	__xstat
	testl	%eax, %eax
	jne	.L1844
	addq	$8, %rsp
	ret
.L1844:
	.p2align 4,,8
	call	ioError
.LFE99:
	.size	saveInputFileMetaInfo, .-saveInputFileMetaInfo
	.section	.rodata.str1.1
.LC35:
	.string	"testf: bad modes\n"
.LC36:
	.string	"(none)"
.LC37:
	.string	"(stdin)"
	.section	.rodata.str1.8
	.align 8
.LC38:
	.string	"%s: There are no files matching `%s'.\n"
	.section	.rodata.str1.1
.LC39:
	.string	"%s: Can't open input %s: %s.\n"
	.section	.rodata.str1.8
	.align 8
.LC40:
	.string	"%s: Input file %s is a directory.\n"
	.align 8
.LC41:
	.string	"%s: I won't read compressed data from a terminal.\n"
	.align 8
.LC42:
	.string	"%s: For help, type: `%s --help'.\n"
	.align 8
.LC43:
	.string	"%s: Can't open input file %s:%s.\n"
	.section	.rodata.str1.1
.LC44:
	.string	"testf: bad srcMode"
.LC45:
	.string	"  %s: "
.LC46:
	.string	"test:bzReadGetUnused"
.LC47:
	.string	"\n    "
.LC48:
	.string	"%s: %s: "
	.section	.rodata.str1.8
	.align 8
.LC49:
	.string	"data integrity (CRC) error in data\n"
	.section	.rodata.str1.1
.LC50:
	.string	"file ends unexpectedly\n"
	.section	.rodata.str1.8
	.align 8
.LC51:
	.string	"bad magic number (file not created by bzip2)\n"
	.align 8
.LC52:
	.string	"trailing garbage after EOF ignored\n"
	.section	.rodata.str1.1
.LC53:
	.string	"test:unexpected error"
.LC54:
	.string	"ok\n"
	.text
	.p2align 4,,15
	.type	testf, @function
testf:
.LFB106:
	pushq	%r15
.LCFI114:
	pushq	%r14
.LCFI115:
	pushq	%r13
.LCFI116:
	pushq	%r12
.LCFI117:
	pushq	%rbp
.LCFI118:
	pushq	%rbx
.LCFI119:
	movq	%rdi, %rbx
	subq	$10200, %rsp
.LCFI120:
	testq	%rdi, %rdi
	movb	$0, deleteOutputOnInterrupt(%rip)
	je	.L1928
.L1846:
	movl	$.LC36, %esi
	movl	$outName, %edi
	call	copyFileName
	movl	srcMode(%rip), %eax
	cmpl	$2, %eax
	je	.L1851
	cmpl	$3, %eax
	je	.L1851
	subl	$1, %eax
	je	.L1929
.L1849:
	cmpl	$1, srcMode(%rip)
	.p2align 4,,2
	je	.L1853
	movl	$inName, %edi
	call	containsDubiousChars
	testb	%al, %al
	je	.L1855
	cmpb	$0, noisy(%rip)
	jne	.L1930
.L1926:
	movl	$1, %edi
	call	setExit
.L1912:
	addq	$10200, %rsp
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
.L1853:
	movq	stdin(%rip), %rdi
	call	fileno
	movl	%eax, %edi
	call	isatty
	testl	%eax, %eax
	jne	.L1931
	movq	stdin(%rip), %r14
.L1869:
	movl	verbosity(%rip), %r12d
	testl	%r12d, %r12d
	jle	.L1871
	movq	stderr(%rip), %rdi
	movl	$inName, %edx
	movl	$.LC45, %esi
	xorl	%eax, %eax
	call	fprintf
	movl	$inName, %edi
	call	pad
	movq	stderr(%rip), %rdi
	call	fflush
.L1871:
	movq	%r14, %rdi
	movq	$0, outputHandleJustInCase(%rip)
	movl	$0, 10180(%rsp)
	call	ferror
	testl	%eax, %eax
	jne	.L1873
	leaq	10188(%rsp), %r12
	xorl	%r15d, %r15d
	movq	%rsp, %r13
.L1927:
	movzbl	smallMode(%rip), %ecx
	movl	10180(%rsp), %r9d
	movq	%rsp, %r8
	movl	verbosity(%rip), %edx
	movq	%r14, %rsi
	movq	%r12, %rdi
	call	BZ2_bzReadOpen
	testq	%rax, %rax
	movq	%rax, %rbx
	je	.L1876
	movl	10188(%rsp), %ebp
	testl	%ebp, %ebp
	jne	.L1876
	leaq	5008(%rsp), %rbp
	addl	$1, %r15d
	.p2align 4,,7
.L1879:
	movl	$5000, %ecx
	movq	%rbp, %rdx
	movq	%rbx, %rsi
	movq	%r12, %rdi
	call	BZ2_bzRead
	movl	10188(%rsp), %eax
	cmpl	$-5, %eax
	je	.L1876
	testl	%eax, %eax
	je	.L1879
	cmpl	$4, %eax
	jne	.L1876
	leaq	10168(%rsp), %rdx
	leaq	10180(%rsp), %rcx
	movq	%rbx, %rsi
	movq	%r12, %rdi
	call	BZ2_bzReadGetUnused
	movl	10188(%rsp), %r11d
	testl	%r11d, %r11d
	jne	.L1925
	movl	10180(%rsp), %edi
	testl	%edi, %edi
	jle	.L1886
	movq	10168(%rsp), %rsi
	xorl	%ecx, %ecx
	xorl	%edx, %edx
	.p2align 4,,7
.L1887:
	movzbl	(%rdx,%rsi), %eax
	addl	$1, %ecx
	movb	%al, (%rdx,%r13)
	addq	$1, %rdx
	cmpl	%edi, %ecx
	jne	.L1887
.L1886:
	movq	%rbx, %rsi
	movq	%r12, %rdi
	call	BZ2_bzReadClose
	movl	10188(%rsp), %r10d
	testl	%r10d, %r10d
	jne	.L1925
	movl	10180(%rsp), %r9d
	testl	%r9d, %r9d
	jne	.L1927
	movq	%r14, %rdi
	call	myfeof
	testb	%al, %al
	je	.L1927
	movq	%r14, %rdi
	call	ferror
	testl	%eax, %eax
	.p2align 4,,2
	jne	.L1873
	movq	%r14, %rdi
	call	fclose
	addl	$1, %eax
	.p2align 4,,2
	je	.L1873
	cmpl	$1, verbosity(%rip)
	jle	.L1894
	movq	stderr(%rip), %rcx
	movl	$5, %edx
	movl	$1, %esi
	movl	$.LC47, %edi
	call	fwrite
	jmp	.L1894
	.p2align 4,,7
.L1851:
	movq	%rbx, %rsi
	movl	$inName, %edi
	call	copyFileName
	jmp	.L1849
.L1876:
	leaq	10184(%rsp), %rdi
	movq	%rbx, %rsi
	call	BZ2_bzReadClose
	movl	verbosity(%rip), %r8d
	testl	%r8d, %r8d
	je	.L1932
.L1896:
	movl	10188(%rsp), %eax
	addl	$9, %eax
	cmpl	$6, %eax
	jbe	.L1933
.L1898:
	movl	$.LC53, %edi
	call	panic
.L1928:
	cmpl	$1, srcMode(%rip)
	je	.L1846
	movl	$.LC35, %edi
	call	panic
	.p2align 4,,7
.L1933:
	mov	%eax, %eax
	jmp	*.L1904(,%rax,8)
	.section	.rodata
	.align 8
	.align 4
.L1904:
	.quad	.L1899
	.quad	.L1898
	.quad	.L1900
	.quad	.L1873
	.quad	.L1901
	.quad	.L1902
	.quad	.L1903
	.text
.L1931:
	movq	progName(%rip), %rdx
	movq	stderr(%rip), %rdi
	movl	$.LC41, %esi
	xorl	%eax, %eax
	call	fprintf
	movq	progName(%rip), %rdx
	movl	$.LC42, %esi
	movq	%rdx, %rcx
.L1923:
	movq	stderr(%rip), %rdi
	xorl	%eax, %eax
	call	fprintf
	jmp	.L1926
.L1855:
	movl	$inName, %edi
	call	fileExists
	testb	%al, %al
	je	.L1934
	cmpl	$1, srcMode(%rip)
	je	.L1853
	leaq	10016(%rsp), %rdx
	movl	$inName, %esi
	movl	$1, %edi
	call	__xstat
	movl	10040(%rsp), %eax
	andl	$61440, %eax
	cmpl	$16384, %eax
	je	.L1935
	movl	srcMode(%rip), %eax
	cmpl	$1, %eax
	je	.L1853
	jl	.L1865
	cmpl	$3, %eax
	.p2align 4,,2
	jg	.L1865
	movl	$.LC31, %esi
	movl	$inName, %edi
	call	fopen
	testq	%rax, %rax
	movq	%rax, %r14
	jne	.L1869
	call	__errno_location
	movl	(%rax), %edi
	call	strerror
	movq	progName(%rip), %rdx
	movq	stderr(%rip), %rdi
	movq	%rax, %r8
	movl	$inName, %ecx
	movl	$.LC43, %esi
	xorl	%eax, %eax
	call	fprintf
	jmp	.L1926
	.p2align 4,,7
.L1929:
	movl	$.LC37, %esi
	movl	$inName, %edi
	call	copyFileName
	jmp	.L1849
.L1932:
	movq	progName(%rip), %rdx
	movq	stderr(%rip), %rdi
	movl	$inName, %ecx
	movl	$.LC48, %esi
	xorl	%eax, %eax
	call	fprintf
	jmp	.L1896
.L1930:
	movq	progName(%rip), %rdx
	movq	stderr(%rip), %rdi
	movl	$inName, %ecx
	movl	$.LC38, %esi
	xorl	%eax, %eax
	call	fprintf
	jmp	.L1926
.L1873:
	call	ioError
.L1899:
	.p2align 4,,8
	call	configError
.L1900:
	movq	stderr(%rip), %rcx
	movl	$23, %edx
	movl	$1, %esi
	movl	$.LC50, %edi
	call	fwrite
.L1905:
	movb	$1, testFailsExist(%rip)
	jmp	.L1912
.L1901:
	cmpq	stdin(%rip), %r14
	je	.L1906
	movq	%r14, %rdi
	call	fclose
.L1906:
	subl	$1, %r15d
	je	.L1936
	cmpb	$0, noisy(%rip)
	jne	.L1937
.L1894:
	movl	verbosity(%rip), %edi
	testl	%edi, %edi
	jle	.L1912
	movq	stderr(%rip), %rcx
	movl	$3, %edx
	movl	$1, %esi
	movl	$.LC54, %edi
	call	fwrite
	jmp	.L1912
.L1902:
	movq	stderr(%rip), %rcx
	movl	$35, %edx
	movl	$1, %esi
	movl	$.LC49, %edi
	call	fwrite
	jmp	.L1905
.L1903:
	call	outOfMemory
.L1935:
	movq	progName(%rip), %rdx
	movl	$inName, %ecx
	movl	$.LC40, %esi
	jmp	.L1923
.L1934:
	call	__errno_location
	movl	(%rax), %edi
	call	strerror
	movq	progName(%rip), %rdx
	movq	stderr(%rip), %rdi
	movq	%rax, %r8
	movl	$inName, %ecx
	movl	$.LC39, %esi
	xorl	%eax, %eax
	call	fprintf
	jmp	.L1926
.L1865:
	movl	$.LC44, %edi
	call	panic
.L1925:
	movl	$.LC46, %edi
	call	panic
.L1937:
	movq	stderr(%rip), %rcx
	movl	$35, %edx
	movl	$1, %esi
	movl	$.LC52, %edi
	call	fwrite
	jmp	.L1894
.L1936:
	movq	stderr(%rip), %rcx
	movl	$45, %edx
	movl	$1, %esi
	movl	$.LC51, %edi
	call	fwrite
	jmp	.L1905
.LFE106:
	.size	testf, .-testf
	.section	.rodata.str1.1
.LC55:
	.string	"w"
.LC56:
	.string	"r"
	.text
	.p2align 4,,15
	.type	bzopen_or_bzdopen, @function
bzopen_or_bzdopen:
.LFB61:
	movq	%r12, -32(%rsp)
.LCFI121:
	movq	%r13, -24(%rsp)
.LCFI122:
	movq	%rdi, %r12
	movq	%rbx, -48(%rsp)
.LCFI123:
	movq	%rbp, -40(%rsp)
.LCFI124:
	movl	%ecx, %r13d
	movq	%r14, -16(%rsp)
.LCFI125:
	movq	%r15, -8(%rsp)
.LCFI126:
	subq	$5096, %rsp
.LCFI127:
	testq	%rdx, %rdx
	movl	%esi, 8(%rsp)
	jne	.L1982
.L1939:
	xorl	%ebx, %ebx
.L1972:
	movq	%rbx, %rax
	movq	5056(%rsp), %rbp
	movq	5048(%rsp), %rbx
	movq	5064(%rsp), %r12
	movq	5072(%rsp), %r13
	movq	5080(%rsp), %r14
	movq	5088(%rsp), %r15
	addq	$5096, %rsp
	ret
	.p2align 4,,7
.L1982:
	movb	$0, 5024(%rsp)
	movb	$0, 5025(%rsp)
	movb	$0, 5026(%rsp)
	movb	$0, 5027(%rsp)
	movb	$0, 5028(%rsp)
	movb	$0, 5029(%rsp)
	movb	$0, 5030(%rsp)
	movb	$0, 5031(%rsp)
	movb	$0, 5032(%rsp)
	movb	$0, 5033(%rsp)
	movzbl	(%rdx), %ebx
	testb	%bl, %bl
	je	.L1983
	movq	%rdx, %rbp
	movl	$9, %r14d
	xorl	%r15d, %r15d
	movl	$0, 12(%rsp)
	jmp	.L1944
	.p2align 4,,7
.L1986:
	cmpb	$119, %bl
	je	.L1948
	cmpb	$114, %bl
	je	.L1984
	.p2align 4,,7
	call	__ctype_b_loc
	movq	(%rax), %rax
	movsbq	%bl,%rdx
	testb	$8, 1(%rax,%rdx,2)
	je	.L1949
	movsbl	%bl,%eax
	leal	-48(%rax), %r14d
	.p2align 4,,7
.L1949:
	movzbl	1(%rbp), %ebx
	addq	$1, %rbp
	testb	%bl, %bl
	je	.L1985
.L1944:
	cmpb	$115, %bl
	jne	.L1986
	movl	$1, 12(%rsp)
	movzbl	1(%rbp), %ebx
	addq	$1, %rbp
	testb	%bl, %bl
	jne	.L1944
.L1985:
	testl	%r15d, %r15d
	movl	$.LC55, %esi
	je	.L1943
.L1953:
	leaq	5024(%rsp), %rbx
	movq	%rbx, %rdi
	call	strcat
	movq	%rbx, %rcx
.L1954:
	movl	(%rcx), %eax
	addq	$4, %rcx
	leal	-16843009(%rax), %edx
	notl	%eax
	andl	%eax, %edx
	andl	$-2139062144, %edx
	je	.L1954
	movl	%edx, %eax
	shrl	$16, %eax
	testl	$32896, %edx
	cmove	%eax, %edx
	leaq	2(%rcx), %rax
	cmove	%rax, %rcx
	addb	%dl, %dl
	sbbq	$3, %rcx
	testl	%r13d, %r13d
	movw	$98, (%rcx)
	jne	.L1956
	testq	%r12, %r12
	je	.L1958
	cmpb	$0, (%r12)
	jne	.L1960
.L1958:
	movq	stdout(%rip), %rax
	movq	stdin(%rip), %rbp
	testl	%r15d, %r15d
	cmovne	%rax, %rbp
.L1963:
	testq	%rbp, %rbp
	je	.L1939
	testl	%r15d, %r15d
	je	.L1965
	testl	%r14d, %r14d
	jle	.L1987
	cmpl	$10, %r14d
	movl	$9, %eax
	cmovge	%eax, %r14d
.L1969:
	leaq	5036(%rsp), %rdi
	movl	$30, %r8d
	xorl	%ecx, %ecx
	movl	%r14d, %edx
	movq	%rbp, %rsi
	call	BZ2_bzWriteOpen
	movq	%rax, %rbx
.L1971:
	testq	%rbx, %rbx
	jne	.L1972
	cmpq	stdin(%rip), %rbp
	je	.L1972
	cmpq	stdout(%rip), %rbp
	je	.L1972
	movq	%rbp, %rdi
	call	fclose
	jmp	.L1972
	.p2align 4,,7
.L1948:
	movl	$1, %r15d
	.p2align 4,,4
	jmp	.L1949
	.p2align 4,,7
.L1984:
	xorl	%r15d, %r15d
	.p2align 4,,7
	jmp	.L1949
.L1983:
	movl	$0, 12(%rsp)
	xorl	%r15d, %r15d
	movl	$9, %r14d
.L1943:
	movl	$.LC56, %esi
	jmp	.L1953
.L1956:
	movl	8(%rsp), %edi
	movq	%rbx, %rsi
	call	fdopen
	movq	%rax, %rbp
	jmp	.L1963
.L1965:
	movl	12(%rsp), %ecx
	leaq	5036(%rsp), %rdi
	leaq	16(%rsp), %r8
	xorl	%r9d, %r9d
	xorl	%edx, %edx
	movq	%rbp, %rsi
	call	BZ2_bzReadOpen
	movq	%rax, %rbx
	jmp	.L1971
.L1960:
	movq	%rbx, %rsi
	movq	%r12, %rdi
	call	fopen
	movq	%rax, %rbp
	jmp	.L1963
.L1987:
	movl	$1, %r14d
	jmp	.L1969
.LFE61:
	.size	bzopen_or_bzdopen, .-bzopen_or_bzdopen
	.p2align 4,,15
.globl BZ2_bzdopen
	.type	BZ2_bzdopen, @function
BZ2_bzdopen:
.LFB63:
	movq	%rsi, %rdx
	movl	$1, %ecx
	movl	%edi, %esi
	xorl	%edi, %edi
	jmp	bzopen_or_bzdopen
.LFE63:
	.size	BZ2_bzdopen, .-BZ2_bzdopen
	.p2align 4,,15
.globl BZ2_bzopen
	.type	BZ2_bzopen, @function
BZ2_bzopen:
.LFB62:
	movq	%rsi, %rdx
	xorl	%ecx, %ecx
	movl	$-1, %esi
	jmp	bzopen_or_bzdopen
.LFE62:
	.size	BZ2_bzopen, .-BZ2_bzopen
	.p2align 4,,15
.globl fopen_output_safely
	.type	fopen_output_safely, @function
fopen_output_safely:
.LFB96:
	movq	%rbx, -24(%rsp)
.LCFI128:
	movq	%rbp, -16(%rsp)
.LCFI129:
	xorl	%eax, %eax
	movq	%r12, -8(%rsp)
.LCFI130:
	movl	$384, %edx
	subq	$24, %rsp
.LCFI131:
	movq	%rsi, %r12
	movl	$193, %esi
	xorl	%ebp, %ebp
	call	open
	cmpl	$-1, %eax
	movl	%eax, %ebx
	je	.L1995
	movq	%r12, %rsi
	movl	%eax, %edi
	call	fdopen
	testq	%rax, %rax
	movq	%rax, %rbp
	je	.L1998
.L1995:
	movq	%rbp, %rax
	movq	(%rsp), %rbx
	movq	8(%rsp), %rbp
	movq	16(%rsp), %r12
	addq	$24, %rsp
	ret
.L1998:
	movl	%ebx, %edi
	call	close
	jmp	.L1995
.LFE96:
	.size	fopen_output_safely, .-fopen_output_safely
	.section	.rodata.str1.1
.LC57:
	.string	"uncompress: bad modes\n"
.LC58:
	.string	"(stdout)"
	.section	.rodata.str1.8
	.align 8
.LC59:
	.string	"%s: Can't open input file %s: %s.\n"
	.align 8
.LC60:
	.string	"%s: Input file %s is not a normal file.\n"
	.align 8
.LC61:
	.string	"%s: Can't guess original name for %s -- using %s\n"
	.align 8
.LC62:
	.string	"%s: Output file %s already exists.\n"
	.section	.rodata.str1.1
.LC63:
	.string	""
.LC64:
	.string	"s"
	.section	.rodata.str1.8
	.align 8
.LC65:
	.string	"%s: Input file %s has %d other link%s.\n"
	.section	.rodata.str1.1
.LC66:
	.string	"wb"
	.section	.rodata.str1.8
	.align 8
.LC67:
	.string	"%s: Can't create output file %s: %s.\n"
	.section	.rodata.str1.1
.LC68:
	.string	"uncompress: bad srcMode"
.LC69:
	.string	"decompress:bzReadGetUnused"
	.section	.rodata.str1.8
	.align 8
.LC70:
	.string	"\n%s: Data integrity error when decompressing.\n"
	.align 8
.LC71:
	.string	"\n%s: Compressed file ends unexpectedly;\n\tperhaps it is corrupted?  *Possible* reason follows.\n"
	.align 8
.LC72:
	.string	"\n%s: %s: trailing garbage after EOF ignored\n"
	.section	.rodata.str1.1
.LC73:
	.string	"decompress:unexpected error"
.LC74:
	.string	"done\n"
.LC75:
	.string	"not a bzip2 file.\n"
.LC76:
	.string	"%s: %s is not a bzip2 file.\n"
	.text
	.p2align 4,,15
	.type	uncompress, @function
uncompress:
.LFB105:
	pushq	%r15
.LCFI132:
	pushq	%r14
.LCFI133:
	pushq	%r13
.LCFI134:
	pushq	%r12
.LCFI135:
	pushq	%rbp
.LCFI136:
	pushq	%rbx
.LCFI137:
	movq	%rdi, %rbx
	subq	$10216, %rsp
.LCFI138:
	testq	%rdi, %rdi
	movb	$0, deleteOutputOnInterrupt(%rip)
	je	.L2148
	movl	srcMode(%rip), %eax
	cmpl	$2, %eax
	je	.L2005
	cmpl	$3, %eax
	je	.L2006
	xorl	%ebx, %ebx
	subl	$1, %eax
	je	.L2002
	.p2align 4,,7
.L2007:
	cmpl	$1, srcMode(%rip)
	je	.L2012
	movl	$inName, %edi
	call	containsDubiousChars
	testb	%al, %al
	je	.L2014
	cmpb	$0, noisy(%rip)
	jne	.L2149
.L2144:
	movl	$1, %edi
	call	setExit
.L2127:
	addq	$10216, %rsp
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
.L2014:
	movl	$inName, %edi
	call	fileExists
	testb	%al, %al
	je	.L2150
	movl	srcMode(%rip), %eax
	cmpl	$3, %eax
	je	.L2021
	cmpl	$2, %eax
	je	.L2021
	.p2align 4,,7
.L2012:
	testb	%bl, %bl
	je	.L2030
	cmpb	$0, noisy(%rip)
	.p2align 4,,2
	jne	.L2151
.L2030:
	cmpl	$3, srcMode(%rip)
	je	.L2152
.L2033:
	movl	srcMode(%rip), %eax
	cmpl	$2, %eax
	je	.L2049
	cmpl	$3, %eax
	je	.L2050
	subl	$1, %eax
	je	.L2153
	movl	$.LC68, %edi
	call	panic
.L2148:
	cmpl	$1, srcMode(%rip)
	jne	.L2154
.L2002:
	movl	$.LC37, %esi
.L2140:
	movl	$inName, %edi
	xorl	%ebx, %ebx
	call	copyFileName
	movl	$.LC58, %esi
	movl	$outName, %edi
	call	copyFileName
	jmp	.L2007
	.p2align 4,,7
.L2005:
	movq	%rdi, %rsi
	jmp	.L2140
.L2006:
	movq	%rdi, %rsi
	movl	$inName, %edi
	call	copyFileName
	movq	%rbx, %rsi
	movl	$outName, %edi
	xorl	%ebx, %ebx
	call	copyFileName
	.p2align 4,,7
.L2008:
	movq	zSuffix(,%rbx,8), %rbp
	movl	$outName, %edi
	movq	unzSuffix(,%rbx,8), %r12
	movq	%rbp, %rsi
	call	hasSuffix
	testb	%al, %al
	jne	.L2129
	addq	$1, %rbx
	cmpq	$4, %rbx
	jne	.L2008
	movl	$outName, %edi
	movl	$1, %ebx
	call	strlen
	movl	$1953853230, outName(%rax)
	movb	$0, outName+4(%rax)
	jmp	.L2007
.L2049:
	movl	$.LC31, %esi
	movl	$inName, %edi
	call	fopen
	testq	%rax, %rax
	movq	%rax, %r13
	movq	stdout(%rip), %rbp
	je	.L2155
.L2051:
	movl	verbosity(%rip), %eax
	testl	%eax, %eax
	jle	.L2059
	movq	stderr(%rip), %rdi
	movl	$inName, %edx
	movl	$.LC45, %esi
	xorl	%eax, %eax
	call	fprintf
	movl	$inName, %edi
	call	pad
	movq	stderr(%rip), %rdi
	call	fflush
.L2059:
	movq	%rbp, %rdi
	movq	%rbp, outputHandleJustInCase(%rip)
	movb	$1, deleteOutputOnInterrupt(%rip)
	movl	$0, 10196(%rsp)
	call	ferror
	testl	%eax, %eax
	jne	.L2146
	movq	%r13, %rdi
	call	ferror
	testl	%eax, %eax
	.p2align 4,,2
	jne	.L2146
	leaq	16(%rsp), %r15
	leaq	10204(%rsp), %r14
	movl	$0, 12(%rsp)
.L2147:
	movzbl	smallMode(%rip), %ecx
	movl	10196(%rsp), %r9d
	movq	%r15, %r8
	movl	verbosity(%rip), %edx
	movq	%r13, %rsi
	movq	%r14, %rdi
	call	BZ2_bzReadOpen
	testq	%rax, %rax
	movq	%rax, %rbx
	je	.L2065
	movl	10204(%rsp), %eax
	testl	%eax, %eax
	jne	.L2065
	addl	$1, 12(%rsp)
	leaq	5024(%rsp), %r12
	jmp	.L2068
	.p2align 4,,7
.L2157:
	cmpl	$4, %edx
	je	.L2071
.L2073:
	movq	%rbp, %rdi
	call	ferror
	testl	%eax, %eax
	.p2align 4,,2
	jne	.L2146
	movl	10204(%rsp), %eax
	testl	%eax, %eax
	jne	.L2156
.L2068:
	movq	%r12, %rdx
	movl	$5000, %ecx
	movq	%rbx, %rsi
	movq	%r14, %rdi
	call	BZ2_bzRead
	movl	10204(%rsp), %edx
	cmpl	$-5, %edx
	je	.L2069
	testl	%edx, %edx
	jne	.L2157
.L2071:
	testl	%eax, %eax
	jle	.L2073
	movslq	%eax,%rdx
	movq	%rbp, %rcx
	movl	$1, %esi
	movq	%r12, %rdi
	call	fwrite
	jmp	.L2073
.L2069:
	cmpb	$0, forceOverwrite(%rip)
	je	.L2065
	movq	%r13, %rdi
	call	rewind
.L2096:
	movq	%r13, %rdi
	call	myfeof
	testb	%al, %al
	jne	.L2086
	movq	%r13, %rcx
	movl	$5000, %edx
	movl	$1, %esi
	movq	%r12, %rdi
	call	fread
	movq	%r13, %rdi
	movq	%rax, %rbx
	call	ferror
	testl	%eax, %eax
	jne	.L2146
	testl	%ebx, %ebx
	jle	.L2099
	movslq	%ebx,%rdx
	movq	%rbp, %rcx
	movl	$1, %esi
	movq	%r12, %rdi
	call	fwrite
.L2099:
	movq	%rbp, %rdi
	call	ferror
	testl	%eax, %eax
	je	.L2096
	.p2align 4,,7
.L2146:
	call	ioError
	.p2align 4,,7
.L2156:
	cmpl	$4, %eax
	.p2align 4,,4
	jne	.L2065
	leaq	10184(%rsp), %rdx
	leaq	10196(%rsp), %rcx
	movq	%rbx, %rsi
	movq	%r14, %rdi
	call	BZ2_bzReadGetUnused
	movl	10204(%rsp), %eax
	testl	%eax, %eax
	jne	.L2145
	movl	10196(%rsp), %edi
	testl	%edi, %edi
	jle	.L2081
	movq	10184(%rsp), %rsi
	xorl	%ecx, %ecx
	xorl	%edx, %edx
	.p2align 4,,7
.L2082:
	movzbl	(%rdx,%rsi), %eax
	addl	$1, %ecx
	movb	%al, (%rdx,%r15)
	addq	$1, %rdx
	cmpl	%edi, %ecx
	jne	.L2082
.L2081:
	movq	%rbx, %rsi
	movq	%r14, %rdi
	call	BZ2_bzReadClose
	movl	10204(%rsp), %eax
	testl	%eax, %eax
	jne	.L2145
	movl	10196(%rsp), %eax
	testl	%eax, %eax
	jne	.L2147
	movq	%r13, %rdi
	call	myfeof
	testb	%al, %al
	je	.L2147
.L2086:
	movq	%r13, %rdi
	call	ferror
	testl	%eax, %eax
	.p2align 4,,2
	jne	.L2146
	movq	%r13, %rdi
	call	fclose
	addl	$1, %eax
	.p2align 4,,2
	je	.L2146
	movq	%rbp, %rdi
	call	ferror
	testl	%eax, %eax
	.p2align 4,,2
	jne	.L2146
	movq	%rbp, %rdi
	call	fflush
	testl	%eax, %eax
	.p2align 4,,2
	jne	.L2146
	cmpq	stdout(%rip), %rbp
	je	.L2091
	movq	%rbp, %rdi
	call	fclose
	addl	$1, %eax
	movq	$0, outputHandleJustInCase(%rip)
	je	.L2146
.L2091:
	cmpl	$1, verbosity(%rip)
	movq	$0, outputHandleJustInCase(%rip)
	jle	.L2093
	movq	stderr(%rip), %rcx
	movl	$5, %edx
	movl	$1, %esi
	movl	$.LC47, %edi
	call	fwrite
	jmp	.L2093
	.p2align 4,,7
.L2050:
	movl	$.LC31, %esi
	movl	$inName, %edi
	call	fopen
	movl	$.LC66, %esi
	movl	$outName, %edi
	movq	%rax, %r13
	call	fopen_output_safely
	testq	%rax, %rax
	movq	%rax, %rbp
	je	.L2158
	testq	%r13, %r13
	jne	.L2051
	.p2align 4,,5
	call	__errno_location
	movl	(%rax), %edi
	call	strerror
	movq	progName(%rip), %rdx
	movq	stderr(%rip), %rdi
	movq	%rax, %r8
	movl	$inName, %ecx
	movl	$.LC59, %esi
	xorl	%eax, %eax
	call	fprintf
	movq	%rbp, %rdi
	call	fclose
	jmp	.L2144
	.p2align 4,,7
.L2149:
	movq	progName(%rip), %rdx
	movq	stderr(%rip), %rdi
	movl	$inName, %ecx
	movl	$.LC38, %esi
	xorl	%eax, %eax
	call	fprintf
	jmp	.L2144
.L2151:
	movq	progName(%rip), %rdx
	movq	stderr(%rip), %rdi
	movl	$outName, %r8d
	movl	$inName, %ecx
	movl	$.LC61, %esi
	xorl	%eax, %eax
	call	fprintf
	jmp	.L2030
.L2152:
	movl	$outName, %edi
	call	fileExists
	testb	%al, %al
	je	.L2035
	cmpb	$0, forceOverwrite(%rip)
	je	.L2037
	movl	$outName, %edi
	call	remove
.L2035:
	cmpl	$3, srcMode(%rip)
	jne	.L2033
	cmpb	$0, forceOverwrite(%rip)
	je	.L2159
.L2040:
	movl	$inName, %edi
	call	saveInputFileMetaInfo
	jmp	.L2033
.L2153:
	movq	stdin(%rip), %r13
	movq	stdout(%rip), %rbp
	movq	%r13, %rdi
	call	fileno
	movl	%eax, %edi
	call	isatty
	testl	%eax, %eax
	je	.L2051
	movq	progName(%rip), %rdx
	movq	stderr(%rip), %rdi
	movl	$.LC41, %esi
	xorl	%eax, %eax
	call	fprintf
	movq	progName(%rip), %rdx
	movq	stderr(%rip), %rdi
	movl	$.LC42, %esi
	xorl	%eax, %eax
	movq	%rdx, %rcx
	call	fprintf
	movl	$1, %edi
	call	setExit
	jmp	.L2127
.L2065:
	leaq	10200(%rsp), %rdi
	movq	%rbx, %rsi
	call	BZ2_bzReadClose
	movl	10204(%rsp), %eax
	addl	$9, %eax
	cmpl	$6, %eax
	jbe	.L2160
.L2101:
	movl	$.LC73, %edi
	call	panic
.L2160:
	mov	%eax, %eax
	jmp	*.L2107(,%rax,8)
	.section	.rodata
	.align 8
	.align 4
.L2107:
	.quad	.L2102
	.quad	.L2101
	.quad	.L2103
	.quad	.L2146
	.quad	.L2104
	.quad	.L2105
	.quad	.L2106
	.text
.L2021:
	leaq	10032(%rsp), %rdx
	movl	$inName, %esi
	movl	$1, %edi
	call	__xstat
	movl	10056(%rsp), %eax
	andl	$61440, %eax
	cmpl	$16384, %eax
	je	.L2161
	cmpl	$3, srcMode(%rip)
	jne	.L2012
	cmpb	$0, forceOverwrite(%rip)
	jne	.L2012
	movl	$inName, %edi
	call	notAStandardFile
	testb	%al, %al
	je	.L2012
	cmpb	$0, noisy(%rip)
	je	.L2144
	movq	progName(%rip), %rdx
	movq	stderr(%rip), %rdi
	movl	$inName, %ecx
	movl	$.LC60, %esi
	xorl	%eax, %eax
	call	fprintf
	jmp	.L2144
	.p2align 4,,7
.L2102:
	call	configError
.L2103:
	cmpb	$0, noisy(%rip)
	.p2align 4,,3
	jne	.L2162
.L2108:
	movl	$2, %edi
	call	cleanUpAndFail
.L2104:
	cmpq	stdin(%rip), %r13
	je	.L2110
	movq	%r13, %rdi
	call	fclose
.L2110:
	cmpq	stdout(%rip), %rbp
	je	.L2112
	movq	%rbp, %rdi
	call	fclose
.L2112:
	cmpl	$1, 12(%rsp)
	je	.L2114
	cmpb	$0, noisy(%rip)
	jne	.L2163
.L2093:
	cmpl	$3, srcMode(%rip)
	movq	$0, outputHandleJustInCase(%rip)
	je	.L2164
.L2117:
	movl	verbosity(%rip), %r14d
	movb	$0, deleteOutputOnInterrupt(%rip)
	testl	%r14d, %r14d
	jle	.L2127
	movq	stderr(%rip), %rcx
	movl	$5, %edx
	movl	$1, %esi
	movl	$.LC74, %edi
	call	fwrite
	jmp	.L2127
.L2061:
.L2105:
	movq	stderr(%rip), %rdi
	movq	progName(%rip), %rdx
	movl	$.LC70, %esi
	xorl	%eax, %eax
	call	fprintf
	call	showFileNames
	call	cadvise
	movl	$2, %edi
	call	cleanUpAndFail
.L2106:
	call	outOfMemory
.L2154:
	movl	$.LC57, %edi
	call	panic
	.p2align 4,,7
.L2150:
	call	__errno_location
	movl	(%rax), %edi
	call	strerror
	movq	progName(%rip), %rdx
	movq	stderr(%rip), %rdi
	movq	%rax, %r8
	movl	$inName, %ecx
	movl	$.LC59, %esi
	xorl	%eax, %eax
	call	fprintf
	jmp	.L2144
.L2129:
	movl	$outName, %edi
	call	strlen
	movq	%rbp, %rdi
	movq	%rax, %rbx
	call	strlen
	subq	%rax, %rbx
	movq	%r12, %rsi
	movl	$outName, %edi
	movb	$0, outName(%rbx)
	xorl	%ebx, %ebx
	call	strcat
	jmp	.L2007
.L2037:
	movq	progName(%rip), %rdx
	movl	$outName, %ecx
	movl	$.LC62, %esi
.L2142:
	movq	stderr(%rip), %rdi
	xorl	%eax, %eax
	call	fprintf
	jmp	.L2144
.L2155:
	call	__errno_location
	movl	(%rax), %edi
	.p2align 4,,6
	call	strerror
	movq	progName(%rip), %rdx
	movq	stderr(%rip), %rdi
	movq	%rax, %r8
	movl	$inName, %ecx
	movl	$.LC43, %esi
	xorl	%eax, %eax
	call	fprintf
	movl	$1, %edi
	call	setExit
	jmp	.L2127
.L2158:
	.p2align 4,,6
	call	__errno_location
	movl	(%rax), %edi
	.p2align 4,,6
	call	strerror
	movq	progName(%rip), %rdx
	movq	stderr(%rip), %rdi
	movq	%rax, %r8
	movl	$outName, %ecx
	xorl	%eax, %eax
	movl	$.LC67, %esi
	call	fprintf
	testq	%r13, %r13
	je	.L2144
	movq	%r13, %rdi
	call	fclose
	.p2align 4,,4
	jmp	.L2144
.L2161:
	movq	progName(%rip), %rdx
	movl	$inName, %ecx
	movl	$.LC40, %esi
	jmp	.L2142
.L2159:
	movl	$inName, %edi
	call	countHardLinks
	testl	%eax, %eax
	movl	%eax, %edx
	jle	.L2042
	movl	%edx, %r8d
	movq	stderr(%rip), %rdi
	movq	progName(%rip), %rdx
	cmpl	$1, %eax
	movl	$.LC64, %r9d
	movl	$.LC63, %eax
	cmove	%rax, %r9
	movl	$inName, %ecx
	movl	$.LC65, %esi
	xorl	%eax, %eax
	call	fprintf
	movl	$1, %edi
	call	setExit
	jmp	.L2127
.L2145:
	movl	$.LC69, %edi
	call	panic
.L2042:
	cmpl	$3, srcMode(%rip)
	jne	.L2033
	jmp	.L2040
.L2162:
	movq	progName(%rip), %rdx
	movq	stderr(%rip), %rdi
	movl	$.LC71, %esi
	xorl	%eax, %eax
	call	fprintf
	movq	progName(%rip), %rdi
	call	perror
	call	showFileNames
	call	cadvise
	.p2align 4,,5
	jmp	.L2108
.L2114:
	cmpl	$3, srcMode(%rip)
	movq	$0, outputHandleJustInCase(%rip)
	movb	$1, unzFailsExist(%rip)
	movb	$0, deleteOutputOnInterrupt(%rip)
	je	.L2121
.L2122:
	movl	$2, %edi
	movb	$0, deleteOutputOnInterrupt(%rip)
	call	setExit
	movl	verbosity(%rip), %r13d
	testl	%r13d, %r13d
	jle	.L2165
	movq	stderr(%rip), %rcx
	movl	$18, %edx
	movl	$1, %esi
	movl	$.LC75, %edi
	call	fwrite
	jmp	.L2127
.L2163:
	movq	progName(%rip), %rdx
	movq	stderr(%rip), %rdi
	movl	$inName, %ecx
	movl	$.LC72, %esi
	xorl	%eax, %eax
	call	fprintf
	jmp	.L2093
.L2164:
	movl	$outName, %edi
	call	applySavedMetaInfoToOutputFile
	cmpb	$0, keepInputFiles(%rip)
	movb	$0, deleteOutputOnInterrupt(%rip)
	jne	.L2117
	movl	$inName, %edi
	call	remove
	testl	%eax, %eax
	je	.L2117
	jmp	.L2146
	.p2align 4,,7
.L2121:
	movl	$outName, %edi
	call	remove
	testl	%eax, %eax
	je	.L2122
	jmp	.L2146
	.p2align 4,,7
.L2165:
	movq	progName(%rip), %rdx
	movq	stderr(%rip), %rdi
	movl	$inName, %ecx
	movl	$.LC76, %esi
	xorl	%eax, %eax
	call	fprintf
	jmp	.L2127
.LFE105:
	.size	uncompress, .-uncompress
	.p2align 4,,15
	.type	bsPutUChar, @function
bsPutUChar:
.LFB21:
	movl	644(%rdi), %edx
	cmpl	$7, %edx
	jle	.L2167
	.p2align 4,,7
.L2170:
	movslq	116(%rdi),%rcx
	movzbl	643(%rdi), %eax
	movq	80(%rdi), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rdi), %edx
	addl	$1, 116(%rdi)
	sall	$8, 640(%rdi)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rdi)
	jg	.L2170
.L2167:
	movl	$24, %ecx
	movzbl	%sil, %eax
	addl	$8, 644(%rdi)
	subl	%edx, %ecx
	sall	%cl, %eax
	orl	%eax, 640(%rdi)
	ret
.LFE21:
	.size	bsPutUChar, .-bsPutUChar
	.p2align 4,,15
	.type	bsPutUInt32, @function
bsPutUInt32:
.LFB20:
	movl	644(%rdi), %edx
	pushq	%rbx
.LCFI139:
	movl	%esi, %ebx
	cmpl	$7, %edx
	jle	.L2173
	.p2align 4,,7
.L2182:
	movslq	116(%rdi),%rcx
	movzbl	643(%rdi), %eax
	movq	80(%rdi), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rdi), %edx
	addl	$1, 116(%rdi)
	sall	$8, 640(%rdi)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rdi)
	jg	.L2182
.L2173:
	movl	$24, %ecx
	movl	%ebx, %eax
	subl	%edx, %ecx
	movl	644(%rdi), %edx
	shrl	$24, %eax
	sall	%cl, %eax
	orl	%eax, 640(%rdi)
	addl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rdi)
	jle	.L2175
	.p2align 4,,7
.L2183:
	movslq	116(%rdi),%rcx
	movzbl	643(%rdi), %eax
	movq	80(%rdi), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rdi), %edx
	addl	$1, 116(%rdi)
	sall	$8, 640(%rdi)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rdi)
	jg	.L2183
.L2175:
	movl	$24, %ecx
	movl	%ebx, %eax
	subl	%edx, %ecx
	movl	644(%rdi), %edx
	shrl	$16, %eax
	andl	$255, %eax
	sall	%cl, %eax
	orl	%eax, 640(%rdi)
	addl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rdi)
	jle	.L2177
	.p2align 4,,7
.L2184:
	movslq	116(%rdi),%rcx
	movzbl	643(%rdi), %eax
	movq	80(%rdi), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rdi), %edx
	addl	$1, 116(%rdi)
	sall	$8, 640(%rdi)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rdi)
	jg	.L2184
.L2177:
	movl	$24, %ecx
	movzbl	%bh, %eax
	subl	%edx, %ecx
	movl	644(%rdi), %edx
	sall	%cl, %eax
	orl	%eax, 640(%rdi)
	addl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rdi)
	jle	.L2179
	.p2align 4,,7
.L2185:
	movslq	116(%rdi),%rcx
	movzbl	643(%rdi), %eax
	movq	80(%rdi), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rdi), %edx
	addl	$1, 116(%rdi)
	sall	$8, 640(%rdi)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rdi)
	jg	.L2185
.L2179:
	movl	$24, %ecx
	movzbl	%bl,%eax
	addl	$8, 644(%rdi)
	subl	%edx, %ecx
	sall	%cl, %eax
	orl	%eax, 640(%rdi)
	popq	%rbx
	ret
.LFE20:
	.size	bsPutUInt32, .-bsPutUInt32
	.section	.rodata.str1.8
	.align 8
.LC77:
	.string	"        main sort initialise ...\n"
	.align 8
.LC78:
	.string	"        qsort [0x%x, 0x%x]   done %d   this %d\n"
	.align 8
.LC79:
	.string	"        %d pointers, %d sorted, %d scanned\n"
	.align 8
.LC80:
	.string	"      %d work, %d block, ratio %5.2f\n"
	.align 8
.LC81:
	.string	"    too repetitive; using fallback sorting algorithm\n"
	.text
	.p2align 4,,15
.globl BZ2_blockSort
	.type	BZ2_blockSort, @function
BZ2_blockSort:
.LFB13:
	pushq	%r15
.LCFI140:
	pushq	%r14
.LCFI141:
	pushq	%r13
.LCFI142:
	pushq	%r12
.LCFI143:
	pushq	%rbp
.LCFI144:
	pushq	%rbx
.LCFI145:
	subq	$5272, %rsp
.LCFI146:
	movl	108(%rdi), %ecx
	movq	56(%rdi), %rax
	movq	64(%rdi), %rdx
	movl	656(%rdi), %ebx
	movq	%rdi, 368(%rsp)
	movq	40(%rdi), %r14
	movl	%ecx, 392(%rsp)
	cmpl	$9999, 392(%rsp)
	movq	%rax, 376(%rsp)
	movq	%rdx, 384(%rsp)
	movl	%ebx, 396(%rsp)
	movl	88(%rdi), %ecx
	jle	.L2658
	movl	392(%rsp), %eax
	movl	392(%rsp), %edx
	movl	$0, 408(%rsp)
	addl	$34, %eax
	addl	$35, %edx
	testb	$1, %al
	cmovne	%edx, %eax
	cltq
	addq	384(%rsp), %rax
	testl	%ecx, %ecx
	movq	%rax, 400(%rsp)
	jle	.L2198
	cmpl	$100, %ecx
	jg	.L2659
	subl	$1, %ecx
	movl	$1431655766, %esi
	movl	%ecx, %eax
	sarl	$31, %ecx
	imull	%esi
	subl	%ecx, %edx
	imull	392(%rsp), %edx
	movl	%edx, 408(%rsp)
.L2198:
	cmpl	$3, 396(%rsp)
	jg	.L2660
.L2201:
	leaq	262144(%r14), %rax
	movl	$65536, %edx
	.p2align 4,,7
.L2203:
	subl	$1, %edx
	movl	$0, (%rax)
	subq	$4, %rax
	cmpl	$-1, %edx
	jne	.L2203
	movq	384(%rsp), %rbx
	movl	392(%rsp), %r15d
	movzbl	(%rbx), %ecx
	subl	$1, %r15d
	movl	%r15d, 420(%rsp)
	movl	%r15d, %edi
	sall	$8, %ecx
	cmpl	$2, %r15d
	jle	.L2207
	movq	400(%rsp), %rdx
	movslq	%r15d,%rax
	movq	%rbx, %rsi
	addq	%rax, %rsi
	leaq	(%rdx,%rax,2), %r8
.L2208:
	movw	$0, (%r8)
	movzbl	(%rsi), %edx
	sarl	$8, %ecx
	movw	$0, -2(%r8)
	subl	$4, %edi
	sall	$8, %edx
	orl	%ecx, %edx
	movslq	%edx,%rax
	sarl	$8, %edx
	addl	$1, (%r14,%rax,4)
	movzbl	-1(%rsi), %eax
	movw	$0, -4(%r8)
	sall	$8, %eax
	orl	%eax, %edx
	movslq	%edx,%rax
	sarl	$8, %edx
	addl	$1, (%r14,%rax,4)
	movzbl	-2(%rsi), %eax
	movw	$0, -6(%r8)
	subq	$8, %r8
	sall	$8, %eax
	orl	%eax, %edx
	movslq	%edx,%rax
	movl	%edx, %ecx
	addl	$1, (%r14,%rax,4)
	sarl	$8, %ecx
	movzbl	-3(%rsi), %eax
	subq	$4, %rsi
	sall	$8, %eax
	orl	%eax, %ecx
	movslq	%ecx,%rax
	addl	$1, (%r14,%rax,4)
	cmpl	$2, %edi
	jg	.L2208
.L2207:
	testl	%edi, %edi
	js	.L2209
	movq	400(%rsp), %rbx
	movq	384(%rsp), %rdx
	movslq	%edi,%rax
	leaq	(%rbx,%rax,2), %rsi
	addq	%rax, %rdx
.L2211:
	movw	$0, (%rsi)
	movzbl	(%rdx), %eax
	sarl	$8, %ecx
	subl	$1, %edi
	subq	$2, %rsi
	subq	$1, %rdx
	sall	$8, %eax
	orl	%eax, %ecx
	movslq	%ecx,%rax
	addl	$1, (%r14,%rax,4)
	cmpl	$-1, %edi
	jne	.L2211
.L2209:
	movslq	392(%rsp),%rax
	movq	400(%rsp), %r15
	xorl	%esi, %esi
	movq	384(%rsp), %rcx
	leaq	(%r15,%rax,2), %rdx
	addq	%rax, %rcx
.L2212:
	movq	384(%rsp), %rbx
	movzbl	(%rsi,%rbx), %eax
	addq	$1, %rsi
	movb	%al, (%rcx)
	movw	$0, (%rdx)
	addq	$1, %rcx
	addq	$2, %rdx
	cmpq	$34, %rsi
	jne	.L2212
	cmpl	$3, 396(%rsp)
	jg	.L2661
.L2214:
	xorl	%edx, %edx
	.p2align 4,,7
.L2216:
	movl	(%r14,%rdx,4), %eax
	addl	%eax, 4(%r14,%rdx,4)
	addq	$1, %rdx
	cmpq	$65536, %rdx
	jne	.L2216
	movq	384(%rsp), %r15
	movl	420(%rsp), %esi
	movzbw	(%r15), %cx
	sall	$8, %ecx
	cmpl	$2, %esi
	jle	.L2220
	movslq	%esi,%rax
	movq	%r15, %rdi
	addq	%rax, %rdi
.L2221:
	movzbl	(%rdi), %eax
	shrw	$8, %cx
	movq	376(%rsp), %rbx
	sall	$8, %eax
	orl	%eax, %ecx
	movzwl	%cx, %eax
	shrw	$8, %cx
	leaq	(%r14,%rax,4), %rax
	movl	(%rax), %edx
	subl	$1, %edx
	movl	%edx, (%rax)
	movq	376(%rsp), %rax
	movslq	%edx,%rdx
	movl	%esi, (%rax,%rdx,4)
	movzbl	-1(%rdi), %eax
	sall	$8, %eax
	orl	%eax, %ecx
	movzwl	%cx, %eax
	shrw	$8, %cx
	leaq	(%r14,%rax,4), %rax
	movl	(%rax), %edx
	subl	$1, %edx
	movl	%edx, (%rax)
	leal	-1(%rsi), %eax
	movslq	%edx,%rdx
	movl	%eax, (%rbx,%rdx,4)
	movzbl	-2(%rdi), %eax
	sall	$8, %eax
	orl	%eax, %ecx
	movzwl	%cx, %eax
	shrw	$8, %cx
	leaq	(%r14,%rax,4), %rax
	movl	(%rax), %edx
	subl	$1, %edx
	movl	%edx, (%rax)
	leal	-2(%rsi), %eax
	movslq	%edx,%rdx
	movl	%eax, (%rbx,%rdx,4)
	movzbl	-3(%rdi), %eax
	subq	$4, %rdi
	sall	$8, %eax
	orl	%eax, %ecx
	movzwl	%cx, %eax
	leaq	(%r14,%rax,4), %rax
	movl	(%rax), %edx
	subl	$1, %edx
	movl	%edx, (%rax)
	leal	-3(%rsi), %eax
	subl	$4, %esi
	movslq	%edx,%rdx
	cmpl	$2, %esi
	movl	%eax, (%rbx,%rdx,4)
	jg	.L2221
.L2220:
	testl	%esi, %esi
	js	.L2222
	movq	384(%rsp), %rdi
	movslq	%esi,%rax
	addq	%rax, %rdi
.L2224:
	movzbl	(%rdi), %eax
	shrw	$8, %cx
	movq	376(%rsp), %r15
	subq	$1, %rdi
	sall	$8, %eax
	orl	%eax, %ecx
	movzwl	%cx, %eax
	leaq	(%r14,%rax,4), %rax
	movl	(%rax), %edx
	subl	$1, %edx
	movl	%edx, (%rax)
	movslq	%edx,%rdx
	movl	%esi, (%r15,%rdx,4)
	subl	$1, %esi
	cmpl	$-1, %esi
	jne	.L2224
.L2222:
	leaq	5008(%rsp), %rdx
	xorl	%eax, %eax
	.p2align 4,,7
.L2225:
	movb	$0, (%rax,%rdx)
	movl	%eax, 2784(%rsp,%rax,4)
	addq	$1, %rax
	cmpq	$256, %rax
	jne	.L2225
	movl	$121, %r13d
	xorl	%r15d, %r15d
	movl	$121, %ebp
.L2227:
	movl	%ebp, %r11d
	leal	-1(%rbp), %r12d
	negl	%r11d
	.p2align 4,,7
.L2559:
	movslq	%r13d,%rax
	movl	%r15d, %edi
	movl	%r13d, %esi
	movl	2784(%rsp,%rax,4), %ebx
	leal	1(%rbx), %edx
	movl	%ebx, %eax
	sall	$8, %eax
	sall	$8, %edx
	cltq
	movslq	%edx,%rdx
	movl	(%r14,%rdx,4), %r10d
	subl	(%r14,%rax,4), %r10d
	jmp	.L2232
	.p2align 4,,7
.L2663:
	movl	%r9d, %esi
.L2232:
	leal	(%r11,%rsi), %r9d
	movslq	%r9d,%r8
	movl	2784(%rsp,%r8,4), %ecx
	leal	1(%rcx), %eax
	movl	%ecx, %edx
	sall	$8, %edx
	sall	$8, %eax
	movslq	%edx,%rdx
	cltq
	movl	(%r14,%rax,4), %eax
	subl	(%r14,%rdx,4), %eax
	cmpl	%r10d, %eax
	jbe	.L2662
	movslq	%esi,%rax
	addl	%r11d, %edi
	movl	%ecx, 2784(%rsp,%rax,4)
	leal	(%rdi,%rbp), %eax
	cmpl	%eax, %r12d
	jl	.L2663
.L2234:
	addl	$1, %r13d
	addq	$1, %r15
	movl	%ebx, 2784(%rsp,%r8,4)
	cmpl	$256, %r13d
	jne	.L2559
.L2644:
	cmpl	$1, %ebp
	je	.L2664
	movl	$1431655766, %eax
	xorl	%r15d, %r15d
	imull	%ebp
	sarl	$31, %ebp
	movl	%edx, %r13d
	subl	%ebp, %r13d
	cmpl	$255, %r13d
	jg	.L2665
	movl	%r13d, %ebp
	jmp	.L2227
.L2662:
	movslq	%esi,%r8
	jmp	.L2234
.L2664:
	movl	408(%rsp), %ecx
	movl	$0, 412(%rsp)
	movq	$0, 496(%rsp)
	movl	%ecx, 524(%rsp)
.L2238:
	movq	496(%rsp), %rbx
	movl	$0, 504(%rsp)
	movl	2784(%rsp,%rbx,4), %ebx
	movl	%ebx, 416(%rsp)
.L2239:
	movl	416(%rsp), %r15d
	cmpl	%r15d, 504(%rsp)
	je	.L2240
	movl	%r15d, %eax
	sall	$8, %eax
	addl	504(%rsp), %eax
	cltq
	salq	$2, %rax
	leaq	(%r14,%rax), %rdx
	movq	%rdx, 424(%rsp)
	movl	(%rdx), %edx
	testl	$2097152, %edx
	jne	.L2242
	movl	4(%r14,%rax), %eax
	andl	$-2097153, %edx
	movl	%edx, 444(%rsp)
	andl	$-2097153, %eax
	subl	$1, %eax
	cmpl	%eax, %edx
	movl	%eax, 440(%rsp)
	jge	.L2242
	cmpl	$3, 396(%rsp)
	jg	.L2666
.L2245:
	movl	444(%rsp), %ebx
	movl	440(%rsp), %r15d
	movl	$2, 3808(%rsp)
	movl	$1, 436(%rsp)
	movl	%ebx, 4608(%rsp)
	movl	%r15d, 4208(%rsp)
.L2247:
	cmpl	$99, 436(%rsp)
	jg	.L2667
.L2248:
	subl	$1, 436(%rsp)
	movslq	436(%rsp),%r10
	movl	4608(%rsp,%r10,4), %eax
	movl	4208(%rsp,%r10,4), %edx
	movl	3808(%rsp,%r10,4), %r11d
	movl	%eax, 520(%rsp)
	movl	%edx, %eax
	subl	520(%rsp), %eax
	movl	%edx, 732(%rsp)
	cmpl	$19, %eax
	jle	.L2250
	cmpl	$14, %r11d
	jle	.L2252
.L2250:
	leal	1(%rax), %ecx
	cmpl	$1, %ecx
	jle	.L2253
	xorl	%edx, %edx
.L2255:
	movl	incs+4(,%rdx,4), %eax
	addq	$1, %rdx
	cmpl	%eax, %ecx
	jg	.L2255
	movl	%edx, %ecx
	subl	$1, %ecx
	movl	%ecx, 452(%rsp)
	js	.L2253
.L2612:
	movslq	452(%rsp),%rax
	movl	520(%rsp), %ebx
	movl	incs(,%rax,4), %r12d
	addl	%r12d, %ebx
	cmpl	%ebx, 732(%rsp)
	movl	%ebx, 512(%rsp)
	jl	.L2258
	movl	512(%rsp), %r15d
	subl	%r12d, %ebx
	movl	%r12d, %r10d
	movl	%ebx, 508(%rsp)
	movq	$0, 480(%rsp)
	negl	%r10d
	movslq	%r15d,%rax
	movl	%r15d, %r13d
	movl	%r15d, 516(%rsp)
	movq	%rax, 96(%rsp)
	subl	$1, %r13d
.L2260:
	movq	376(%rsp), %rdx
	addq	480(%rsp), %rdx
	movq	96(%rsp), %rcx
	movl	516(%rsp), %edi
	movl	508(%rsp), %esi
	movl	(%rdx,%rcx,4), %ecx
	movq	%rdx, 488(%rsp)
	leal	(%r11,%rcx), %r8d
	movl	%ecx, 448(%rsp)
	mov	%r8d, %eax
	addq	384(%rsp), %rax
	leal	1(%r8), %ebx
	leal	2(%r8), %r15d
	leal	4(%r8), %edx
	leal	5(%r8), %ecx
	movq	%rbx, 360(%rsp)
	leal	6(%r8), %ebx
	movq	%r15, 352(%rsp)
	movq	%rdx, 336(%rsp)
	leal	7(%r8), %r15d
	movq	%rax, 88(%rsp)
	leal	3(%r8), %eax
	movq	%rcx, 328(%rsp)
	movq	%rbx, 320(%rsp)
	leal	9(%r8), %edx
	leal	10(%r8), %ecx
	movq	%rax, 344(%rsp)
	leal	11(%r8), %ebx
	leal	8(%r8), %eax
	movq	%r15, 312(%rsp)
	movq	%rdx, 296(%rsp)
	movq	%rax, 304(%rsp)
	movq	%rcx, 288(%rsp)
	movq	%rbx, 280(%rsp)
	jmp	.L2261
	.p2align 4,,7
.L2561:
	seta	%al
	movzbl	%al, %eax
	testl	%eax, %eax
	je	.L2668
	movq	376(%rsp), %r15
	movslq	%edi,%rax
	addl	%r10d, %esi
	movl	%ebx, (%r15,%rax,4)
	leal	(%rsi,%r12), %eax
	cmpl	%eax, %r13d
	jge	.L2325
	movl	%ebp, %edi
.L2261:
	movq	376(%rsp), %r15
	leal	(%r10,%rdi), %ebp
	movslq	%ebp,%rax
	leaq	(%r15,%rax,4), %r9
	movq	88(%rsp), %r15
	movl	(%r9), %ebx
	movzbl	(%r15), %eax
	movq	384(%rsp), %r15
	leal	(%rbx,%r11), %ecx
	mov	%ecx, %edx
	cmpb	%al, (%r15,%rdx)
	jne	.L2561
	movq	360(%rsp), %r15
	movq	384(%rsp), %rax
	leal	1(%rcx), %edx
	movzbl	(%rax,%r15), %eax
	movq	384(%rsp), %r15
	cmpb	%al, (%r15,%rdx)
	jne	.L2561
	movq	352(%rsp), %r15
	movq	384(%rsp), %rax
	leal	2(%rcx), %edx
	movzbl	(%rax,%r15), %eax
	movq	384(%rsp), %r15
	cmpb	%al, (%r15,%rdx)
	jne	.L2561
	movq	344(%rsp), %r15
	movq	384(%rsp), %rax
	leal	3(%rcx), %edx
	movzbl	(%rax,%r15), %eax
	movq	384(%rsp), %r15
	cmpb	%al, (%r15,%rdx)
	jne	.L2561
	movq	336(%rsp), %r15
	movq	384(%rsp), %rax
	leal	4(%rcx), %edx
	movzbl	(%rax,%r15), %eax
	movq	384(%rsp), %r15
	cmpb	%al, (%r15,%rdx)
	jne	.L2561
	movq	328(%rsp), %r15
	movq	384(%rsp), %rax
	leal	5(%rcx), %edx
	movzbl	(%rax,%r15), %eax
	movq	384(%rsp), %r15
	cmpb	%al, (%r15,%rdx)
	jne	.L2561
	movq	320(%rsp), %r15
	movq	384(%rsp), %rax
	leal	6(%rcx), %edx
	movzbl	(%rax,%r15), %eax
	movq	384(%rsp), %r15
	cmpb	%al, (%r15,%rdx)
	jne	.L2561
	movq	312(%rsp), %r15
	movq	384(%rsp), %rax
	leal	7(%rcx), %edx
	movzbl	(%rax,%r15), %eax
	movq	384(%rsp), %r15
	cmpb	%al, (%r15,%rdx)
	jne	.L2561
	movq	304(%rsp), %r15
	movq	384(%rsp), %rax
	leal	8(%rcx), %edx
	movzbl	(%rax,%r15), %eax
	movq	384(%rsp), %r15
	cmpb	%al, (%r15,%rdx)
	jne	.L2561
	movq	296(%rsp), %r15
	movq	384(%rsp), %rax
	leal	9(%rcx), %edx
	movzbl	(%rax,%r15), %eax
	movq	384(%rsp), %r15
	cmpb	%al, (%r15,%rdx)
	jne	.L2561
	movq	288(%rsp), %r15
	movq	384(%rsp), %rax
	leal	10(%rcx), %edx
	movzbl	(%rax,%r15), %eax
	movq	384(%rsp), %r15
	cmpb	%al, (%r15,%rdx)
	jne	.L2561
	movq	280(%rsp), %r15
	movq	384(%rsp), %rax
	leal	11(%rcx), %edx
	movzbl	(%rax,%r15), %eax
	movq	384(%rsp), %r15
	cmpb	%al, (%r15,%rdx)
	jne	.L2561
	leal	12(%rcx), %edx
	movl	392(%rsp), %ecx
	leal	12(%r8), %eax
	movl	%eax, 460(%rsp)
	addl	$8, %ecx
	movl	%ecx, 456(%rsp)
.L2287:
	mov	460(%rsp), %eax
	movq	384(%rsp), %rcx
	mov	%edx, %r15d
	movq	%r15, 528(%rsp)
	movq	%rax, 8(%rsp)
	movzbl	(%rcx,%rax), %ecx
	movq	384(%rsp), %rax
	cmpb	%cl, (%rax,%r15)
	jne	.L2561
	movq	8(%rsp), %rcx
	movq	400(%rsp), %r15
	movzwl	(%r15,%rcx,2), %eax
	movq	528(%rsp), %rcx
	cmpw	%ax, (%r15,%rcx,2)
	jne	.L2561
	movl	460(%rsp), %eax
	movq	384(%rsp), %rcx
	leal	1(%rdx), %r15d
	movq	%r15, 536(%rsp)
	addl	$1, %eax
	movq	%rax, 8(%rsp)
	movzbl	(%rcx,%rax), %ecx
	movq	384(%rsp), %rax
	cmpb	%cl, (%rax,%r15)
	jne	.L2561
	movq	8(%rsp), %rcx
	movq	400(%rsp), %r15
	movzwl	(%r15,%rcx,2), %eax
	movq	536(%rsp), %rcx
	cmpw	%ax, (%r15,%rcx,2)
	jne	.L2561
	movl	460(%rsp), %eax
	movq	384(%rsp), %rcx
	leal	2(%rdx), %r15d
	movq	%r15, 544(%rsp)
	addl	$2, %eax
	movq	%rax, 8(%rsp)
	movzbl	(%rcx,%rax), %ecx
	movq	384(%rsp), %rax
	cmpb	%cl, (%rax,%r15)
	jne	.L2561
	movq	8(%rsp), %rcx
	movq	400(%rsp), %r15
	movzwl	(%r15,%rcx,2), %eax
	movq	544(%rsp), %rcx
	cmpw	%ax, (%r15,%rcx,2)
	jne	.L2561
	movl	460(%rsp), %eax
	movq	384(%rsp), %rcx
	leal	3(%rdx), %r15d
	movq	%r15, 552(%rsp)
	addl	$3, %eax
	movq	%rax, 8(%rsp)
	movzbl	(%rcx,%rax), %ecx
	movq	384(%rsp), %rax
	cmpb	%cl, (%rax,%r15)
	jne	.L2561
	movq	8(%rsp), %rcx
	movq	400(%rsp), %r15
	movzwl	(%r15,%rcx,2), %eax
	movq	552(%rsp), %rcx
	cmpw	%ax, (%r15,%rcx,2)
	jne	.L2561
	movl	460(%rsp), %eax
	movq	384(%rsp), %rcx
	leal	4(%rdx), %r15d
	movq	%r15, 560(%rsp)
	addl	$4, %eax
	movq	%rax, 8(%rsp)
	movzbl	(%rcx,%rax), %ecx
	movq	384(%rsp), %rax
	cmpb	%cl, (%rax,%r15)
	jne	.L2561
	movq	8(%rsp), %rcx
	movq	400(%rsp), %r15
	movzwl	(%r15,%rcx,2), %eax
	movq	560(%rsp), %rcx
	cmpw	%ax, (%r15,%rcx,2)
	jne	.L2561
	movl	460(%rsp), %eax
	movq	384(%rsp), %rcx
	leal	5(%rdx), %r15d
	movq	%r15, 568(%rsp)
	addl	$5, %eax
	movq	%rax, 8(%rsp)
	movzbl	(%rcx,%rax), %ecx
	movq	384(%rsp), %rax
	cmpb	%cl, (%rax,%r15)
	jne	.L2561
	movq	8(%rsp), %rcx
	movq	400(%rsp), %r15
	movzwl	(%r15,%rcx,2), %eax
	movq	568(%rsp), %rcx
	cmpw	%ax, (%r15,%rcx,2)
	jne	.L2561
	movl	460(%rsp), %eax
	movq	384(%rsp), %rcx
	leal	6(%rdx), %r15d
	movq	%r15, 576(%rsp)
	addl	$6, %eax
	movq	%rax, 8(%rsp)
	movzbl	(%rcx,%rax), %ecx
	movq	384(%rsp), %rax
	cmpb	%cl, (%rax,%r15)
	jne	.L2561
	movq	8(%rsp), %rcx
	movq	400(%rsp), %r15
	movzwl	(%r15,%rcx,2), %eax
	movq	576(%rsp), %rcx
	cmpw	%ax, (%r15,%rcx,2)
	jne	.L2561
	movl	460(%rsp), %eax
	movq	384(%rsp), %rcx
	leal	7(%rdx), %r15d
	movq	%r15, 584(%rsp)
	addl	$7, %eax
	movq	%rax, 8(%rsp)
	movzbl	(%rcx,%rax), %ecx
	movq	384(%rsp), %rax
	cmpb	%cl, (%rax,%r15)
	jne	.L2561
	movq	8(%rsp), %rcx
	movq	400(%rsp), %r15
	movzwl	(%r15,%rcx,2), %eax
	movq	584(%rsp), %rcx
	cmpw	%ax, (%r15,%rcx,2)
	jne	.L2561
	addl	$8, %edx
	addl	$8, 460(%rsp)
	movl	%edx, %eax
	subl	392(%rsp), %eax
	cmpl	%edx, 392(%rsp)
	movl	460(%rsp), %r15d
	cmovbe	%eax, %edx
	movl	460(%rsp), %eax
	subl	392(%rsp), %eax
	cmpl	%r15d, 392(%rsp)
	cmova	%r15d, %eax
	subl	$1, 524(%rsp)
	subl	$8, 456(%rsp)
	movl	%eax, 460(%rsp)
	jns	.L2287
	movq	376(%rsp), %rdx
	movslq	%edi,%rax
	leaq	(%rdx,%rax,4), %r9
.L2325:
	movl	516(%rsp), %esi
	movl	448(%rsp), %eax
	addl	$1, %esi
	cmpl	%esi, 732(%rsp)
	movl	%eax, (%r9)
	jl	.L2258
	movl	512(%rsp), %eax
	movq	488(%rsp), %rdx
	movl	508(%rsp), %edi
	addl	$1, %eax
	cltq
	addl	$1, %edi
	movl	(%rdx,%rax,4), %eax
	leal	(%r11,%rax), %r8d
	movl	%eax, 724(%rsp)
	mov	%r8d, %eax
	addq	384(%rsp), %rax
	leal	1(%r8), %ecx
	leal	2(%r8), %ebx
	leal	3(%r8), %r15d
	leal	5(%r8), %edx
	movq	%rcx, 272(%rsp)
	leal	6(%r8), %ecx
	movq	%rbx, 264(%rsp)
	movq	%r15, 256(%rsp)
	leal	7(%r8), %ebx
	movq	%rax, 88(%rsp)
	leal	4(%r8), %eax
	movq	%rdx, 240(%rsp)
	movq	%rcx, 232(%rsp)
	leal	8(%r8), %r15d
	leal	10(%r8), %edx
	movq	%rax, 248(%rsp)
	leal	11(%r8), %ecx
	leal	9(%r8), %eax
	movq	%rbx, 224(%rsp)
	movq	%r15, 216(%rsp)
	movq	%rax, 208(%rsp)
	movq	%rdx, 200(%rsp)
	movq	%rcx, 192(%rsp)
	jmp	.L2330
	.p2align 4,,7
.L2577:
	seta	%al
	movzbl	%al, %eax
	testl	%eax, %eax
	je	.L2669
	movq	376(%rsp), %r15
	movslq	%esi,%rax
	addl	%r10d, %edi
	movl	%ebx, (%r15,%rax,4)
	leal	(%rdi,%r12), %eax
	cmpl	%eax, %r13d
	jge	.L2394
	movl	%ebp, %esi
.L2330:
	movq	376(%rsp), %rbx
	leal	(%rsi,%r10), %ebp
	movq	88(%rsp), %r15
	movslq	%ebp,%rax
	leaq	(%rbx,%rax,4), %r9
	movzbl	(%r15), %eax
	movq	384(%rsp), %r15
	movl	(%r9), %ebx
	leal	(%r11,%rbx), %ecx
	mov	%ecx, %edx
	cmpb	%al, (%r15,%rdx)
	jne	.L2577
	movq	272(%rsp), %r15
	movq	384(%rsp), %rax
	leal	1(%rcx), %edx
	movzbl	(%rax,%r15), %eax
	movq	384(%rsp), %r15
	cmpb	%al, (%r15,%rdx)
	jne	.L2577
	movq	264(%rsp), %r15
	movq	384(%rsp), %rax
	leal	2(%rcx), %edx
	movzbl	(%rax,%r15), %eax
	movq	384(%rsp), %r15
	cmpb	%al, (%r15,%rdx)
	jne	.L2577
	movq	256(%rsp), %r15
	movq	384(%rsp), %rax
	leal	3(%rcx), %edx
	movzbl	(%rax,%r15), %eax
	movq	384(%rsp), %r15
	cmpb	%al, (%r15,%rdx)
	jne	.L2577
	movq	248(%rsp), %r15
	movq	384(%rsp), %rax
	leal	4(%rcx), %edx
	movzbl	(%rax,%r15), %eax
	movq	384(%rsp), %r15
	cmpb	%al, (%r15,%rdx)
	jne	.L2577
	movq	240(%rsp), %r15
	movq	384(%rsp), %rax
	leal	5(%rcx), %edx
	movzbl	(%rax,%r15), %eax
	movq	384(%rsp), %r15
	cmpb	%al, (%r15,%rdx)
	jne	.L2577
	movq	232(%rsp), %r15
	movq	384(%rsp), %rax
	leal	6(%rcx), %edx
	movzbl	(%rax,%r15), %eax
	movq	384(%rsp), %r15
	cmpb	%al, (%r15,%rdx)
	jne	.L2577
	movq	224(%rsp), %r15
	movq	384(%rsp), %rax
	leal	7(%rcx), %edx
	movzbl	(%rax,%r15), %eax
	movq	384(%rsp), %r15
	cmpb	%al, (%r15,%rdx)
	jne	.L2577
	movq	216(%rsp), %r15
	movq	384(%rsp), %rax
	leal	8(%rcx), %edx
	movzbl	(%rax,%r15), %eax
	movq	384(%rsp), %r15
	cmpb	%al, (%r15,%rdx)
	jne	.L2577
	movq	208(%rsp), %r15
	movq	384(%rsp), %rax
	leal	9(%rcx), %edx
	movzbl	(%rax,%r15), %eax
	movq	384(%rsp), %r15
	cmpb	%al, (%r15,%rdx)
	jne	.L2577
	movq	200(%rsp), %r15
	movq	384(%rsp), %rax
	leal	10(%rcx), %edx
	movzbl	(%rax,%r15), %eax
	movq	384(%rsp), %r15
	cmpb	%al, (%r15,%rdx)
	jne	.L2577
	movq	192(%rsp), %r15
	movq	384(%rsp), %rax
	leal	11(%rcx), %edx
	movzbl	(%rax,%r15), %eax
	movq	384(%rsp), %r15
	cmpb	%al, (%r15,%rdx)
	jne	.L2577
	leal	12(%rcx), %edx
	movl	392(%rsp), %ecx
	leal	12(%r8), %eax
	movl	%eax, 468(%rsp)
	addl	$8, %ecx
	movl	%ecx, 464(%rsp)
.L2356:
	mov	468(%rsp), %eax
	movq	384(%rsp), %rcx
	mov	%edx, %r15d
	movq	%r15, 592(%rsp)
	movq	%rax, 8(%rsp)
	movzbl	(%rcx,%rax), %ecx
	movq	384(%rsp), %rax
	cmpb	%cl, (%rax,%r15)
	jne	.L2577
	movq	8(%rsp), %rcx
	movq	400(%rsp), %r15
	movzwl	(%r15,%rcx,2), %eax
	movq	592(%rsp), %rcx
	cmpw	%ax, (%r15,%rcx,2)
	jne	.L2577
	movl	468(%rsp), %eax
	movq	384(%rsp), %rcx
	leal	1(%rdx), %r15d
	movq	%r15, 600(%rsp)
	addl	$1, %eax
	movq	%rax, 8(%rsp)
	movzbl	(%rcx,%rax), %ecx
	movq	384(%rsp), %rax
	cmpb	%cl, (%rax,%r15)
	jne	.L2577
	movq	8(%rsp), %rcx
	movq	400(%rsp), %r15
	movzwl	(%r15,%rcx,2), %eax
	movq	600(%rsp), %rcx
	cmpw	%ax, (%r15,%rcx,2)
	jne	.L2577
	movl	468(%rsp), %eax
	movq	384(%rsp), %rcx
	leal	2(%rdx), %r15d
	movq	%r15, 608(%rsp)
	addl	$2, %eax
	movq	%rax, 8(%rsp)
	movzbl	(%rcx,%rax), %ecx
	movq	384(%rsp), %rax
	cmpb	%cl, (%rax,%r15)
	jne	.L2577
	movq	8(%rsp), %rcx
	movq	400(%rsp), %r15
	movzwl	(%r15,%rcx,2), %eax
	movq	608(%rsp), %rcx
	cmpw	%ax, (%r15,%rcx,2)
	jne	.L2577
	movl	468(%rsp), %eax
	movq	384(%rsp), %rcx
	leal	3(%rdx), %r15d
	movq	%r15, 616(%rsp)
	addl	$3, %eax
	movq	%rax, 8(%rsp)
	movzbl	(%rcx,%rax), %ecx
	movq	384(%rsp), %rax
	cmpb	%cl, (%rax,%r15)
	jne	.L2577
	movq	8(%rsp), %rcx
	movq	400(%rsp), %r15
	movzwl	(%r15,%rcx,2), %eax
	movq	616(%rsp), %rcx
	cmpw	%ax, (%r15,%rcx,2)
	jne	.L2577
	movl	468(%rsp), %eax
	movq	384(%rsp), %rcx
	leal	4(%rdx), %r15d
	movq	%r15, 624(%rsp)
	addl	$4, %eax
	movq	%rax, 8(%rsp)
	movzbl	(%rcx,%rax), %ecx
	movq	384(%rsp), %rax
	cmpb	%cl, (%rax,%r15)
	jne	.L2577
	movq	8(%rsp), %rcx
	movq	400(%rsp), %r15
	movzwl	(%r15,%rcx,2), %eax
	movq	624(%rsp), %rcx
	cmpw	%ax, (%r15,%rcx,2)
	jne	.L2577
	movl	468(%rsp), %eax
	movq	384(%rsp), %rcx
	leal	5(%rdx), %r15d
	movq	%r15, 632(%rsp)
	addl	$5, %eax
	movq	%rax, 8(%rsp)
	movzbl	(%rcx,%rax), %ecx
	movq	384(%rsp), %rax
	cmpb	%cl, (%rax,%r15)
	jne	.L2577
	movq	8(%rsp), %rcx
	movq	400(%rsp), %r15
	movzwl	(%r15,%rcx,2), %eax
	movq	632(%rsp), %rcx
	cmpw	%ax, (%r15,%rcx,2)
	jne	.L2577
	movl	468(%rsp), %eax
	movq	384(%rsp), %rcx
	leal	6(%rdx), %r15d
	movq	%r15, 640(%rsp)
	addl	$6, %eax
	movq	%rax, 8(%rsp)
	movzbl	(%rcx,%rax), %ecx
	movq	384(%rsp), %rax
	cmpb	%cl, (%rax,%r15)
	jne	.L2577
	movq	8(%rsp), %rcx
	movq	400(%rsp), %r15
	movzwl	(%r15,%rcx,2), %eax
	movq	640(%rsp), %rcx
	cmpw	%ax, (%r15,%rcx,2)
	jne	.L2577
	movl	468(%rsp), %eax
	movq	384(%rsp), %rcx
	leal	7(%rdx), %r15d
	movq	%r15, 648(%rsp)
	addl	$7, %eax
	movq	%rax, 8(%rsp)
	movzbl	(%rcx,%rax), %ecx
	movq	384(%rsp), %rax
	cmpb	%cl, (%rax,%r15)
	jne	.L2577
	movq	8(%rsp), %rcx
	movq	400(%rsp), %r15
	movzwl	(%r15,%rcx,2), %eax
	movq	648(%rsp), %rcx
	cmpw	%ax, (%r15,%rcx,2)
	jne	.L2577
	addl	$8, %edx
	addl	$8, 468(%rsp)
	movl	%edx, %eax
	subl	392(%rsp), %eax
	cmpl	%edx, 392(%rsp)
	movl	468(%rsp), %r15d
	cmovbe	%eax, %edx
	movl	468(%rsp), %eax
	subl	392(%rsp), %eax
	cmpl	%r15d, 392(%rsp)
	cmova	%r15d, %eax
	subl	$1, 524(%rsp)
	subl	$8, 464(%rsp)
	movl	%eax, 468(%rsp)
	jns	.L2356
	movq	376(%rsp), %rdx
	movslq	%esi,%rax
	leaq	(%rdx,%rax,4), %r9
.L2394:
	movl	516(%rsp), %esi
	movl	724(%rsp), %eax
	addl	$2, %esi
	cmpl	%esi, 732(%rsp)
	movl	%eax, (%r9)
	jl	.L2258
	movl	512(%rsp), %eax
	movq	488(%rsp), %rdx
	movl	508(%rsp), %edi
	addl	$2, %eax
	cltq
	addl	$2, %edi
	movl	(%rdx,%rax,4), %eax
	leal	(%r11,%rax), %r8d
	movl	%eax, 728(%rsp)
	mov	%r8d, %eax
	addq	384(%rsp), %rax
	leal	1(%r8), %ecx
	leal	2(%r8), %ebx
	leal	3(%r8), %r15d
	leal	5(%r8), %edx
	movq	%rcx, 184(%rsp)
	leal	6(%r8), %ecx
	movq	%rbx, 176(%rsp)
	movq	%r15, 168(%rsp)
	leal	7(%r8), %ebx
	movq	%rax, 88(%rsp)
	leal	4(%r8), %eax
	movq	%rdx, 152(%rsp)
	movq	%rcx, 144(%rsp)
	leal	8(%r8), %r15d
	leal	10(%r8), %edx
	movq	%rax, 160(%rsp)
	leal	11(%r8), %ecx
	leal	9(%r8), %eax
	movq	%rbx, 136(%rsp)
	movq	%r15, 128(%rsp)
	movq	%rax, 120(%rsp)
	movq	%rdx, 112(%rsp)
	movq	%rcx, 104(%rsp)
	jmp	.L2399
	.p2align 4,,7
.L2593:
	seta	%al
	movzbl	%al, %eax
	testl	%eax, %eax
	je	.L2670
	movq	376(%rsp), %r15
	movslq	%esi,%rax
	addl	%r10d, %edi
	movl	%ebx, (%r15,%rax,4)
	leal	(%rdi,%r12), %eax
	cmpl	%eax, %r13d
	jge	.L2463
	movl	%ebp, %esi
.L2399:
	movq	376(%rsp), %rbx
	leal	(%rsi,%r10), %ebp
	movq	88(%rsp), %r15
	movslq	%ebp,%rax
	leaq	(%rbx,%rax,4), %r9
	movzbl	(%r15), %eax
	movq	384(%rsp), %r15
	movl	(%r9), %ebx
	leal	(%r11,%rbx), %ecx
	mov	%ecx, %edx
	cmpb	%al, (%r15,%rdx)
	jne	.L2593
	movq	184(%rsp), %r15
	movq	384(%rsp), %rax
	leal	1(%rcx), %edx
	movzbl	(%rax,%r15), %eax
	movq	384(%rsp), %r15
	cmpb	%al, (%r15,%rdx)
	jne	.L2593
	movq	176(%rsp), %r15
	movq	384(%rsp), %rax
	leal	2(%rcx), %edx
	movzbl	(%rax,%r15), %eax
	movq	384(%rsp), %r15
	cmpb	%al, (%r15,%rdx)
	jne	.L2593
	movq	168(%rsp), %r15
	movq	384(%rsp), %rax
	leal	3(%rcx), %edx
	movzbl	(%rax,%r15), %eax
	movq	384(%rsp), %r15
	cmpb	%al, (%r15,%rdx)
	jne	.L2593
	movq	160(%rsp), %r15
	movq	384(%rsp), %rax
	leal	4(%rcx), %edx
	movzbl	(%rax,%r15), %eax
	movq	384(%rsp), %r15
	cmpb	%al, (%r15,%rdx)
	jne	.L2593
	movq	152(%rsp), %r15
	movq	384(%rsp), %rax
	leal	5(%rcx), %edx
	movzbl	(%rax,%r15), %eax
	movq	384(%rsp), %r15
	cmpb	%al, (%r15,%rdx)
	jne	.L2593
	movq	144(%rsp), %r15
	movq	384(%rsp), %rax
	leal	6(%rcx), %edx
	movzbl	(%rax,%r15), %eax
	movq	384(%rsp), %r15
	cmpb	%al, (%r15,%rdx)
	jne	.L2593
	movq	136(%rsp), %r15
	movq	384(%rsp), %rax
	leal	7(%rcx), %edx
	movzbl	(%rax,%r15), %eax
	movq	384(%rsp), %r15
	cmpb	%al, (%r15,%rdx)
	jne	.L2593
	movq	128(%rsp), %r15
	movq	384(%rsp), %rax
	leal	8(%rcx), %edx
	movzbl	(%rax,%r15), %eax
	movq	384(%rsp), %r15
	cmpb	%al, (%r15,%rdx)
	jne	.L2593
	movq	120(%rsp), %r15
	movq	384(%rsp), %rax
	leal	9(%rcx), %edx
	movzbl	(%rax,%r15), %eax
	movq	384(%rsp), %r15
	cmpb	%al, (%r15,%rdx)
	jne	.L2593
	movq	112(%rsp), %r15
	movq	384(%rsp), %rax
	leal	10(%rcx), %edx
	movzbl	(%rax,%r15), %eax
	movq	384(%rsp), %r15
	cmpb	%al, (%r15,%rdx)
	jne	.L2593
	movq	104(%rsp), %r15
	movq	384(%rsp), %rax
	leal	11(%rcx), %edx
	movzbl	(%rax,%r15), %eax
	movq	384(%rsp), %r15
	cmpb	%al, (%r15,%rdx)
	jne	.L2593
	leal	12(%rcx), %edx
	movl	392(%rsp), %ecx
	leal	12(%r8), %eax
	movl	%eax, 476(%rsp)
	addl	$8, %ecx
	movl	%ecx, 472(%rsp)
.L2425:
	mov	476(%rsp), %eax
	movq	384(%rsp), %rcx
	mov	%edx, %r15d
	movq	%r15, 656(%rsp)
	movq	%rax, 8(%rsp)
	movzbl	(%rcx,%rax), %ecx
	movq	384(%rsp), %rax
	cmpb	%cl, (%rax,%r15)
	jne	.L2593
	movq	8(%rsp), %rcx
	movq	400(%rsp), %r15
	movzwl	(%r15,%rcx,2), %eax
	movq	656(%rsp), %rcx
	cmpw	%ax, (%r15,%rcx,2)
	jne	.L2593
	movl	476(%rsp), %eax
	movq	384(%rsp), %rcx
	leal	1(%rdx), %r15d
	movq	%r15, 664(%rsp)
	addl	$1, %eax
	movq	%rax, 8(%rsp)
	movzbl	(%rcx,%rax), %ecx
	movq	384(%rsp), %rax
	cmpb	%cl, (%rax,%r15)
	jne	.L2593
	movq	8(%rsp), %rcx
	movq	400(%rsp), %r15
	movzwl	(%r15,%rcx,2), %eax
	movq	664(%rsp), %rcx
	cmpw	%ax, (%r15,%rcx,2)
	jne	.L2593
	movl	476(%rsp), %eax
	movq	384(%rsp), %rcx
	leal	2(%rdx), %r15d
	movq	%r15, 672(%rsp)
	addl	$2, %eax
	movq	%rax, 8(%rsp)
	movzbl	(%rcx,%rax), %ecx
	movq	384(%rsp), %rax
	cmpb	%cl, (%rax,%r15)
	jne	.L2593
	movq	8(%rsp), %rcx
	movq	400(%rsp), %r15
	movzwl	(%r15,%rcx,2), %eax
	movq	672(%rsp), %rcx
	cmpw	%ax, (%r15,%rcx,2)
	jne	.L2593
	movl	476(%rsp), %eax
	movq	384(%rsp), %rcx
	leal	3(%rdx), %r15d
	movq	%r15, 680(%rsp)
	addl	$3, %eax
	movq	%rax, 8(%rsp)
	movzbl	(%rcx,%rax), %ecx
	movq	384(%rsp), %rax
	cmpb	%cl, (%rax,%r15)
	jne	.L2593
	movq	8(%rsp), %rcx
	movq	400(%rsp), %r15
	movzwl	(%r15,%rcx,2), %eax
	movq	680(%rsp), %rcx
	cmpw	%ax, (%r15,%rcx,2)
	jne	.L2593
	movl	476(%rsp), %eax
	movq	384(%rsp), %rcx
	leal	4(%rdx), %r15d
	movq	%r15, 688(%rsp)
	addl	$4, %eax
	movq	%rax, 8(%rsp)
	movzbl	(%rcx,%rax), %ecx
	movq	384(%rsp), %rax
	cmpb	%cl, (%rax,%r15)
	jne	.L2593
	movq	8(%rsp), %rcx
	movq	400(%rsp), %r15
	movzwl	(%r15,%rcx,2), %eax
	movq	688(%rsp), %rcx
	cmpw	%ax, (%r15,%rcx,2)
	jne	.L2593
	movl	476(%rsp), %eax
	movq	384(%rsp), %rcx
	leal	5(%rdx), %r15d
	movq	%r15, 696(%rsp)
	addl	$5, %eax
	movq	%rax, 8(%rsp)
	movzbl	(%rcx,%rax), %ecx
	movq	384(%rsp), %rax
	cmpb	%cl, (%rax,%r15)
	jne	.L2593
	movq	8(%rsp), %rcx
	movq	400(%rsp), %r15
	movzwl	(%r15,%rcx,2), %eax
	movq	696(%rsp), %rcx
	cmpw	%ax, (%r15,%rcx,2)
	jne	.L2593
	movl	476(%rsp), %eax
	movq	384(%rsp), %rcx
	leal	6(%rdx), %r15d
	movq	%r15, 704(%rsp)
	addl	$6, %eax
	movq	%rax, 8(%rsp)
	movzbl	(%rcx,%rax), %ecx
	movq	384(%rsp), %rax
	cmpb	%cl, (%rax,%r15)
	jne	.L2593
	movq	8(%rsp), %rcx
	movq	400(%rsp), %r15
	movzwl	(%r15,%rcx,2), %eax
	movq	704(%rsp), %rcx
	cmpw	%ax, (%r15,%rcx,2)
	jne	.L2593
	movl	476(%rsp), %eax
	movq	384(%rsp), %rcx
	leal	7(%rdx), %r15d
	movq	%r15, 712(%rsp)
	addl	$7, %eax
	movq	%rax, 8(%rsp)
	movzbl	(%rcx,%rax), %ecx
	movq	384(%rsp), %rax
	cmpb	%cl, (%rax,%r15)
	jne	.L2593
	movq	8(%rsp), %rcx
	movq	400(%rsp), %r15
	movzwl	(%r15,%rcx,2), %eax
	movq	712(%rsp), %rcx
	cmpw	%ax, (%r15,%rcx,2)
	jne	.L2593
	addl	$8, %edx
	addl	$8, 476(%rsp)
	movl	%edx, %eax
	subl	392(%rsp), %eax
	cmpl	%edx, 392(%rsp)
	movl	476(%rsp), %r15d
	cmovbe	%eax, %edx
	movl	476(%rsp), %eax
	subl	392(%rsp), %eax
	cmpl	%r15d, 392(%rsp)
	cmova	%r15d, %eax
	subl	$1, 524(%rsp)
	subl	$8, 472(%rsp)
	movl	%eax, 476(%rsp)
	jns	.L2425
	movq	376(%rsp), %rdx
	movslq	%esi,%rax
	leaq	(%rdx,%rax,4), %r9
.L2463:
	movl	524(%rsp), %r8d
	movl	728(%rsp), %eax
	testl	%r8d, %r8d
	movl	%eax, (%r9)
	js	.L2467
	addl	$3, 516(%rsp)
	addq	$12, 480(%rsp)
	movl	516(%rsp), %edx
	addl	$3, 508(%rsp)
	cmpl	%edx, 732(%rsp)
	jge	.L2260
.L2258:
	subl	$1, 452(%rsp)
	cmpl	$-1, 452(%rsp)
	jne	.L2612
.L2253:
	movl	524(%rsp), %edi
	testl	%edi, %edi
	js	.L2467
.L2469:
	movl	436(%rsp), %esi
	testl	%esi, %esi
	jg	.L2247
	movl	524(%rsp), %ecx
	testl	%ecx, %ecx
	js	.L2467
	movl	412(%rsp), %ecx
	movl	440(%rsp), %ebx
	movl	444(%rsp), %r15d
	leal	1(%rcx,%rbx), %ecx
	subl	%r15d, %ecx
	movl	%ecx, 412(%rsp)
.L2242:
	movq	424(%rsp), %rax
	orl	$2097152, (%rax)
.L2240:
	addl	$1, 504(%rsp)
	cmpl	$256, 504(%rsp)
	jne	.L2239
	movslq	416(%rsp),%rbx
	cmpb	$0, 5008(%rsp,%rbx)
	jne	.L2671
.L2505:
	leaq	4(%r14), %rsi
	leaq	1760(%rsp), %r8
	leaq	736(%rsp), %rdi
	xorl	%ecx, %ecx
.L2507:
	movl	%ecx, %eax
	sall	$8, %eax
	addl	416(%rsp), %eax
	cltq
	salq	$2, %rax
	movl	(%r14,%rax), %edx
	andl	$-2097153, %edx
	movl	%edx, (%r8,%rcx,4)
	movl	(%rsi,%rax), %eax
	andl	$-2097153, %eax
	subl	$1, %eax
	movl	%eax, (%rdi,%rcx,4)
	addq	$1, %rcx
	cmpq	$256, %rcx
	jne	.L2507
	movl	416(%rsp), %eax
	movl	1760(%rsp,%rbx,4), %r9d
	sall	$8, %eax
	cltq
	leaq	(%r14,%rax,4), %rbp
	movl	(%rbp), %edi
	andl	$-2097153, %edi
	cmpl	%r9d, %edi
	jge	.L2509
	movq	376(%rsp), %rdx
	movslq	%edi,%rax
	leaq	(%rdx,%rax,4), %r8
.L2511:
	movl	(%r8), %esi
	movl	392(%rsp), %ecx
	movq	384(%rsp), %r15
	subl	$1, %esi
	leal	(%rsi,%rcx), %eax
	cmpl	$-1, %esi
	cmovle	%eax, %esi
	movslq	%esi,%rax
	movzbl	(%r15,%rax), %ecx
	cmpb	$0, 5008(%rsp,%rcx)
	jne	.L2514
	movslq	1760(%rsp,%rcx,4),%rax
	leal	1(%rax), %edx
	movl	%edx, 1760(%rsp,%rcx,4)
	movq	376(%rsp), %rdx
	movl	%esi, (%rdx,%rax,4)
.L2514:
	movl	1760(%rsp,%rbx,4), %r9d
	addl	$1, %edi
	addq	$4, %r8
	cmpl	%edi, %r9d
	jg	.L2511
.L2509:
	movl	416(%rsp), %eax
	movl	736(%rsp,%rbx,4), %edx
	addl	$1, %eax
	sall	$8, %eax
	cltq
	leaq	(%r14,%rax,4), %r12
	movl	(%r12), %eax
	andl	$-2097153, %eax
	leal	-1(%rax), %edi
	cmpl	%edx, %edi
	jle	.L2516
	movq	376(%rsp), %rcx
	movslq	%edi,%rax
	leaq	(%rcx,%rax,4), %r8
.L2518:
	movl	(%r8), %esi
	movl	392(%rsp), %r15d
	movq	384(%rsp), %rdx
	subl	$1, %esi
	leal	(%rsi,%r15), %eax
	cmpl	$-1, %esi
	cmovle	%eax, %esi
	movslq	%esi,%rax
	movzbl	(%rdx,%rax), %ecx
	cmpb	$0, 5008(%rsp,%rcx)
	jne	.L2521
	movslq	736(%rsp,%rcx,4),%rax
	leal	-1(%rax), %edx
	movl	%edx, 736(%rsp,%rcx,4)
	movq	376(%rsp), %rcx
	movl	%esi, (%rcx,%rax,4)
.L2521:
	movl	736(%rsp,%rbx,4), %edx
	subl	$1, %edi
	subq	$4, %r8
	cmpl	%edi, %edx
	jl	.L2518
.L2516:
	leal	-1(%r9), %eax
	cmpl	%eax, %edx
	je	.L2523
	testl	%r9d, %r9d
	jne	.L2525
	cmpl	%edx, 420(%rsp)
	je	.L2523
.L2525:
	movl	$1007, %edi
	call	BZ2_bz__AssertH__fail
.L2523:
	movl	416(%rsp), %edx
	xorl	%ecx, %ecx
.L2528:
	movslq	%edx,%rax
	addl	$1, %ecx
	addl	$256, %edx
	orl	$2097152, (%r14,%rax,4)
	cmpl	$256, %ecx
	jne	.L2528
	cmpl	$254, 496(%rsp)
	jg	.L2530
	movl	(%rbp), %esi
	movl	(%r12), %edx
	xorl	%r9d, %r9d
	andl	$-2097153, %esi
	andl	$-2097153, %edx
	subl	%esi, %edx
	cmpl	$65534, %edx
	jle	.L2534
.L2535:
	addl	$1, %r9d
	movl	%edx, %eax
	movl	%r9d, %ecx
	sarl	%cl, %eax
	cmpl	$65534, %eax
	jg	.L2535
.L2534:
	movl	%edx, %edi
	subl	$1, %edi
	js	.L2536
	leal	-1(%rsi,%rdx), %eax
	movq	376(%rsp), %r15
	movl	%edi, %r10d
	cltq
	leaq	(%r15,%rax,4), %rsi
.L2538:
	movl	(%rsi), %edx
	movq	400(%rsp), %r15
	movl	%r10d, %r8d
	movl	%r9d, %ecx
	sarl	%cl, %r8d
	movslq	%edx,%rax
	cmpl	$33, %edx
	movw	%r8w, (%r15,%rax,2)
	jg	.L2539
	movl	392(%rsp), %ecx
	leal	(%rdx,%rcx), %eax
	cltq
	movw	%r8w, (%r15,%rax,2)
.L2539:
	subl	$1, %r10d
	subq	$4, %rsi
	cmpl	$-1, %r10d
	jne	.L2538
.L2536:
	movl	%r9d, %ecx
	sarl	%cl, %edi
	cmpl	$65535, %edi
	jle	.L2530
	movl	$1002, %edi
	call	BZ2_bz__AssertH__fail
.L2530:
	addq	$1, 496(%rsp)
	cmpq	$256, 496(%rsp)
	je	.L2542
	movb	$1, 5008(%rsp,%rbx)
	jmp	.L2238
	.p2align 4,,7
.L2668:
	movq	376(%rsp), %rcx
	movslq	%edi,%rax
	leaq	(%rcx,%rax,4), %r9
	jmp	.L2325
.L2669:
	movq	376(%rsp), %rcx
	movslq	%esi,%rax
	leaq	(%rcx,%rax,4), %r9
	jmp	.L2394
.L2670:
	movq	376(%rsp), %rcx
	movslq	%esi,%rax
	leaq	(%rcx,%rax,4), %r9
	jmp	.L2463
.L2467:
	cmpl	$2, 396(%rsp)
	jg	.L2545
	movl	524(%rsp), %edx
	testl	%edx, %edx
	js	.L2672
.L2193:
	movq	368(%rsp), %r15
	movl	108(%r15), %esi
	movl	$-1, 48(%r15)
	testl	%esi, %esi
	jle	.L2550
	movq	376(%rsp), %rdx
	xorl	%ecx, %ecx
	movl	(%rdx), %eax
	testl	%eax, %eax
	jne	.L2555
	jmp	.L2681
	.p2align 4,,7
.L2554:
	movl	4(%rdx), %eax
	addq	$4, %rdx
	testl	%eax, %eax
	je	.L2674
.L2555:
	addl	$1, %ecx
	cmpl	%esi, %ecx
	jne	.L2554
.L2550:
	addq	$5272, %rsp
	movl	$1003, %edi
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	BZ2_bz__AssertH__fail
.L2665:
	movl	%r13d, %ebp
	jmp	.L2644
.L2542:
	cmpl	$3, 396(%rsp)
	jle	.L2467
	movl	392(%rsp), %r8d
	subl	412(%rsp), %r8d
	movl	$.LC79, %esi
	movl	412(%rsp), %ecx
	movl	392(%rsp), %edx
	xorl	%eax, %eax
	movq	stderr(%rip), %rdi
	call	fprintf
.L2545:
	movl	408(%rsp), %edx
	subl	524(%rsp), %edx
	movl	$.LC80, %esi
	cvtsi2ss	392(%rsp), %xmm1
	movl	392(%rsp), %ecx
	movq	stderr(%rip), %rdi
	movl	$1, %eax
	cvtsi2ss	%edx, %xmm0
	divss	%xmm1, %xmm0
	cvtss2sd	%xmm0, %xmm0
	call	fprintf
	movl	524(%rsp), %edx
	testl	%edx, %edx
	jns	.L2193
.L2672:
	cmpl	$1, 396(%rsp)
	jle	.L2548
	movq	stderr(%rip), %rcx
	movl	$53, %edx
	movl	$1, %esi
	movl	$.LC81, %edi
	call	fwrite
.L2548:
	movq	368(%rsp), %rbx
	movl	396(%rsp), %r8d
	movq	%r14, %rdx
	movl	392(%rsp), %ecx
	movq	32(%rbx), %rsi
	movq	24(%rbx), %rdi
	call	fallbackSort
	jmp	.L2193
.L2252:
	movslq	520(%rsp),%rax
	movq	376(%rsp), %rcx
	movq	384(%rsp), %rbx
	movq	376(%rsp), %r15
	leaq	0(,%rax,4), %r12
	movl	%r11d, %eax
	addl	(%rcx,%r12), %eax
	movzbl	(%rbx,%rax), %esi
	movslq	732(%rsp),%rax
	movl	(%r15,%rax,4), %r15d
	movl	%esi, %ecx
	leal	(%r11,%r15), %eax
	movq	384(%rsp), %r15
	movzbl	(%rbx,%rax), %edi
	movl	732(%rsp), %eax
	addl	520(%rsp), %eax
	movq	376(%rsp), %rbx
	movl	%edi, %edx
	sarl	%eax
	cmpb	%dil, %sil
	cltq
	movl	(%rbx,%rax,4), %ebx
	leal	(%r11,%rbx), %eax
	movzbl	(%r15,%rax), %eax
	jbe	.L2675
.L2470:
	cmpb	%al, %cl
	ja	.L2676
.L2472:
	movl	520(%rsp), %r9d
	movl	732(%rsp), %r8d
	movzbl	%cl, %r13d
	movl	%r9d, %ebx
	movl	%r8d, %ebp
.L2657:
	cmpl	%r8d, %r9d
	jg	.L2480
	movq	376(%rsp), %rdx
	movslq	%r9d,%rax
	movq	384(%rsp), %r15
	leaq	(%rdx,%rax,4), %rdi
	movl	(%rdi), %ecx
	leal	(%rcx,%r11), %eax
	movzbl	(%r15,%rax), %eax
	subl	%r13d, %eax
	cmpl	$0, %eax
	je	.L2677
	jg	.L2611
	jmp	.L2682
	.p2align 4,,7
.L2679:
	movslq	%ebp,%rax
	subl	$1, %r8d
	subl	$1, %ebp
	leaq	(%rdx,%rax,4), %rax
	cmpl	%r8d, %r9d
	movl	(%rax), %edx
	movl	%edx, (%rsi)
	movl	%ecx, (%rax)
	jg	.L2480
.L2611:
	movq	376(%rsp), %rdx
	movslq	%r8d,%rax
	movq	384(%rsp), %r15
	leaq	(%rdx,%rax,4), %rsi
	movl	(%rsi), %ecx
	leal	(%rcx,%r11), %eax
	movzbl	(%r15,%rax), %eax
	subl	%r13d, %eax
	cmpl	$0, %eax
	je	.L2679
	jl	.L2484
	subl	$1, %r8d
	cmpl	%r8d, %r9d
	jle	.L2611
.L2480:
	cmpl	%ebx, %ebp
	.p2align 4,,5
	jl	.L2680
	movl	%ebx, %eax
	subl	520(%rsp), %eax
	movl	%r9d, %esi
	subl	%ebx, %esi
	cmpl	%eax, %esi
	cmovg	%eax, %esi
	testl	%esi, %esi
	jle	.L2488
	movl	%r9d, %eax
	movq	376(%rsp), %r15
	movq	376(%rsp), %rdi
	subl	%esi, %eax
	cltq
	leaq	(%r15,%rax,4), %rcx
	addq	%r12, %rdi
.L2490:
	movl	(%rdi), %edx
	movl	(%rcx), %eax
	movl	%eax, (%rdi)
	movl	%edx, (%rcx)
	addq	$4, %rdi
	addq	$4, %rcx
	subl	$1, %esi
	jne	.L2490
.L2488:
	movl	%ebp, %eax
	subl	%r8d, %eax
	movl	%eax, %r8d
	movl	732(%rsp), %eax
	subl	%ebp, %eax
	movl	732(%rsp), %ebp
	cmpl	%eax, %r8d
	movl	%eax, %edi
	cmovle	%r8d, %edi
	addl	$1, %ebp
	testl	%edi, %edi
	jle	.L2491
	movq	376(%rsp), %rdx
	movslq	%r9d,%rax
	leaq	(%rdx,%rax,4), %rsi
	movl	%ebp, %eax
	subl	%edi, %eax
	cltq
	leaq	(%rdx,%rax,4), %rcx
.L2493:
	movl	(%rsi), %edx
	movl	(%rcx), %eax
	movl	%eax, (%rsi)
	movl	%edx, (%rcx)
	addq	$4, %rsi
	addq	$4, %rcx
	subl	$1, %edi
	jne	.L2493
.L2491:
	movl	520(%rsp), %r15d
	movl	%ebp, %esi
	subl	%r8d, %esi
	leal	-1(%rsi), %ebp
	leal	-1(%r15,%r9), %ecx
	movl	732(%rsp), %r9d
	subl	%ebx, %ecx
	leal	1(%r11), %ebx
	movl	%ecx, %edx
	subl	%esi, %r9d
	leal	1(%rcx), %r12d
	subl	%r15d, %edx
	cmpl	%r9d, %edx
	jge	.L2494
	movl	%ecx, %eax
	movl	%esi, 520(%rsp)
	movl	732(%rsp), %ecx
	movl	%r15d, %esi
	movl	%eax, 732(%rsp)
.L2496:
	movl	%ebp, %edi
	subl	%r12d, %edi
	cmpl	%edx, %edi
	jle	.L2497
	movl	%esi, %eax
	movl	%r12d, %esi
	movl	%ebx, %r8d
	movl	%eax, %r12d
	movl	732(%rsp), %eax
	movl	%r11d, %ebx
	movl	%ebp, 732(%rsp)
	movl	%eax, %ebp
.L2499:
	cmpl	%r9d, %edi
	jle	.L2500
	movl	520(%rsp), %eax
	movl	%esi, 520(%rsp)
	movl	%eax, %esi
	movl	%ecx, %eax
	movl	732(%rsp), %ecx
	movl	%eax, 732(%rsp)
	movl	%r11d, %eax
	movl	%r8d, %r11d
	movl	%eax, %r8d
.L2500:
	movl	520(%rsp), %eax
	movl	732(%rsp), %edx
	movl	%ecx, 4208(%rsp,%r10,4)
	movl	%r11d, 3808(%rsp,%r10,4)
	movl	%eax, 4608(%rsp,%r10,4)
	movl	436(%rsp), %eax
	addl	$1, %eax
	cltq
	movl	%esi, 4608(%rsp,%rax,4)
	movl	%edx, 4208(%rsp,%rax,4)
	movl	%r8d, 3808(%rsp,%rax,4)
	movl	436(%rsp), %eax
	addl	$3, 436(%rsp)
	addl	$2, %eax
	cltq
	movl	%r12d, 4608(%rsp,%rax,4)
	movl	%ebp, 4208(%rsp,%rax,4)
	movl	%ebx, 3808(%rsp,%rax,4)
	jmp	.L2469
.L2659:
	movl	392(%rsp), %eax
	sall	$5, %eax
	addl	392(%rsp), %eax
	cmpl	$3, 396(%rsp)
	movl	%eax, 408(%rsp)
	jle	.L2201
.L2660:
	movq	stderr(%rip), %rcx
	movl	$33, %edx
	movl	$1, %esi
	movl	$.LC77, %edi
	call	fwrite
	jmp	.L2201
.L2661:
	movq	stderr(%rip), %rcx
	movl	$27, %edx
	movb	$1, %sil
	movl	$.LC14, %edi
	call	fwrite
	jmp	.L2214
.L2674:
	movq	368(%rsp), %rax
	movl	%ecx, 48(%rax)
	addl	$1, %ecx
	je	.L2550
.L2558:
	addq	$5272, %rsp
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
.L2667:
	movl	$1001, %edi
	call	BZ2_bz__AssertH__fail
	jmp	.L2248
.L2658:
	movq	32(%rdi), %rsi
	movl	392(%rsp), %ecx
	movl	%ebx, %r8d
	movq	24(%rdi), %rdi
	movq	%r14, %rdx
	call	fallbackSort
	jmp	.L2193
.L2666:
	movl	%eax, %r9d
	movl	412(%rsp), %r8d
	movl	504(%rsp), %ecx
	movq	stderr(%rip), %rdi
	subl	%edx, %r9d
	movl	$.LC78, %esi
	addl	$1, %r9d
	movl	%r15d, %edx
	xorl	%eax, %eax
	call	fprintf
	jmp	.L2245
.L2680:
	movl	520(%rsp), %eax
	addl	$1, 436(%rsp)
	movl	732(%rsp), %edx
	movl	%eax, 4608(%rsp,%r10,4)
	leal	1(%r11), %eax
	movl	%edx, 4208(%rsp,%r10,4)
	movl	%eax, 3808(%rsp,%r10,4)
	jmp	.L2469
.L2676:
	cmpb	%al, %dl
	movl	%eax, %ecx
	cmovae	%edx, %ecx
	jmp	.L2472
.L2675:
	movl	%esi, %edx
	movl	%edi, %ecx
	jmp	.L2470
.L2677:
	movslq	%ebx,%rax
	addl	$1, %r9d
	addl	$1, %ebx
	leaq	(%rdx,%rax,4), %rax
	movl	(%rax), %edx
	movl	%edx, (%rdi)
	movl	%ecx, (%rax)
	jmp	.L2657
.L2494:
	movl	%r9d, %eax
	movl	%edx, %r9d
	movl	%eax, %edx
	jmp	.L2496
.L2497:
	movl	%r11d, %r8d
	movl	%edx, %edi
	jmp	.L2499
.L2484:
	movl	(%rdi), %eax
	addl	$1, %r9d
	subl	$1, %r8d
	movl	%ecx, (%rdi)
	movl	%eax, (%rsi)
	jmp	.L2657
.L2682:
	addl	$1, %r9d
	jmp	.L2657
.L2681:
	movq	368(%rsp), %rdx
	movl	$0, 48(%rdx)
	jmp	.L2558
.L2671:
	movl	$1006, %edi
	call	BZ2_bz__AssertH__fail
	jmp	.L2505
.LFE13:
	.size	BZ2_blockSort, .-BZ2_blockSort
	.section	.rodata.str1.8
	.align 8
.LC82:
	.string	"    block %d: crc = 0x%8x, combined CRC = 0x%8x, size = %d\n"
	.align 8
.LC83:
	.string	"      %d in block, %d after MTF & 1-2 coding, %d+2 syms in use\n"
	.align 8
.LC85:
	.string	"      initial group %d, [%d .. %d], has %d syms (%4.1f%%)\n"
	.align 8
.LC86:
	.string	"      pass %d: size is %d, grp uses are "
	.section	.rodata.str1.1
.LC87:
	.string	"%d "
.LC88:
	.string	"      bytes: mapping %d, "
.LC89:
	.string	"selectors %d, "
.LC90:
	.string	"code lengths %d, "
.LC91:
	.string	"codes %d\n"
	.section	.rodata.str1.8
	.align 8
.LC92:
	.string	"    final combined CRC = 0x%x\n   "
	.section	.rodata.cst8
	.align 8
.LC84:
	.long	0
	.long	1079574528
	.text
	.p2align 4,,15
.globl BZ2_compressBlock
	.type	BZ2_compressBlock, @function
BZ2_compressBlock:
.LFB25:
	pushq	%r15
.LCFI147:
	pushq	%r14
.LCFI148:
	pushq	%r13
.LCFI149:
	pushq	%r12
.LCFI150:
	pushq	%rbp
.LCFI151:
	movq	%rdi, %rbp
	pushq	%rbx
.LCFI152:
	subq	$744, %rsp
.LCFI153:
	movl	108(%rdi), %eax
	movb	%sil, 371(%rsp)
	testl	%eax, %eax
	jle	.L2684
	movl	652(%rdi), %eax
	notl	648(%rdi)
	rorl	$31, %eax
	xorl	648(%rdi), %eax
	cmpl	$1, 660(%rdi)
	movl	%eax, 652(%rdi)
	jle	.L2686
	movl	$0, 116(%rdi)
.L2686:
	cmpl	$1, 656(%rbp)
	jle	.L2688
	movl	648(%rbp), %ecx
	movl	660(%rbp), %edx
	movl	%eax, %r8d
	movl	108(%rbp), %r9d
	movq	stderr(%rip), %rdi
	movl	$.LC82, %esi
	xorl	%eax, %eax
	call	fprintf
.L2688:
	movq	%rbp, %rdi
	call	BZ2_blockSort
.L2684:
	movslq	108(%rbp),%rax
	addq	32(%rbp), %rax
	cmpl	$1, 660(%rbp)
	movq	%rax, 80(%rbp)
	je	.L3244
.L2690:
	movl	108(%rbp), %eax
	testl	%eax, %eax
	jle	.L2692
	movl	$49, %esi
	movq	%rbp, %rdi
	call	bsPutUChar
	movl	$65, %esi
	movq	%rbp, %rdi
	call	bsPutUChar
	movl	$89, %esi
	movq	%rbp, %rdi
	call	bsPutUChar
	movl	$38, %esi
	movq	%rbp, %rdi
	call	bsPutUChar
	movl	$83, %esi
	movq	%rbp, %rdi
	call	bsPutUChar
	movl	$89, %esi
	movq	%rbp, %rdi
	call	bsPutUChar
	movl	648(%rbp), %esi
	movq	%rbp, %rdi
	call	bsPutUInt32
	cmpl	$7, 644(%rbp)
	jle	.L2694
.L3079:
	movzbl	643(%rbp), %eax
	movslq	116(%rbp),%rcx
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %eax
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %eax
	cmpl	$7, %eax
	movl	%eax, 644(%rbp)
	jg	.L3079
.L2694:
	movl	644(%rbp), %eax
	movl	48(%rbp), %esi
	addl	$1, %eax
	cmpl	$7, %eax
	movl	%eax, 644(%rbp)
	jle	.L2696
.L3080:
	movzbl	643(%rbp), %eax
	movslq	116(%rbp),%rcx
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %eax
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %eax
	cmpl	$7, %eax
	movl	%eax, 644(%rbp)
	jg	.L3080
.L2696:
	movl	$8, %ecx
	movq	72(%rbp), %rdx
	addl	$24, 644(%rbp)
	subl	%eax, %ecx
	movq	56(%rbp), %r11
	movq	64(%rbp), %rbx
	sall	%cl, %esi
	orl	%esi, 640(%rbp)
	movl	$0, 124(%rbp)
	movq	%rdx, 16(%rsp)
	xorl	%edx, %edx
.L2698:
	cmpb	$0, 128(%rdx,%rbp)
	je	.L2699
	movl	124(%rbp), %eax
	addl	$1, 124(%rbp)
	movb	%al, 384(%rdx,%rbp)
.L2699:
	addq	$1, %rdx
	cmpq	$256, %rdx
	jne	.L2698
	movl	124(%rbp), %ecx
	movl	%ecx, %r10d
	movl	%ecx, 12(%rsp)
	addl	$1, %r10d
	js	.L2702
	mov	%r10d, %eax
	xorw	%dx, %dx
	addq	$1, %rax
.L2704:
	movl	$0, 672(%rbp,%rdx,4)
	addq	$1, %rdx
	cmpq	%rax, %rdx
	jne	.L2704
.L2702:
	movl	124(%rbp), %eax
	testl	%eax, %eax
	jle	.L2705
	movl	124(%rbp), %ecx
	leaq	400(%rsp), %r12
	xorl	%edx, %edx
.L2707:
	leal	1(%rdx), %eax
	movb	%dl, (%rdx,%r12)
	addq	$1, %rdx
	cmpl	%eax, %ecx
	jg	.L2707
.L2705:
	movl	108(%rbp), %r13d
	xorl	%eax, %eax
	movl	$1, %ecx
	testl	%r13d, %r13d
	jle	.L2710
	xorl	%r8d, %r8d
	xorl	%esi, %esi
	xorl	%r9d, %r9d
.L2711:
	movl	(%r11,%r9,4), %eax
	subl	$1, %eax
	leal	(%rax,%r13), %edx
	cmpl	$-1, %eax
	cmovle	%edx, %eax
	cltq
	movzbl	(%rax,%rbx), %eax
	movzbl	384(%rax,%rbp), %edi
	cmpb	%dil, 400(%rsp)
	je	.L3245
	testl	%r8d, %r8d
	jle	.L2717
	leal	-1(%r8), %edx
	jmp	.L2719
.L3246:
	movq	16(%rsp), %rcx
	movslq	%esi,%rax
	addl	$1, 676(%rbp)
	addl	$1, %esi
	cmpl	$1, %edx
	movw	$1, (%rcx,%rax,2)
	jle	.L2723
.L3247:
	subl	$2, %edx
	movl	%edx, %eax
	shrl	$31, %eax
	leal	(%rax,%rdx), %edx
	sarl	%edx
.L2719:
	testb	$1, %dl
	jne	.L3246
	movq	16(%rsp), %rcx
	movslq	%esi,%rax
	addl	$1, 672(%rbp)
	addl	$1, %esi
	cmpl	$1, %edx
	movw	$0, (%rcx,%rax,2)
	jg	.L3247
.L2723:
	xorl	%r8d, %r8d
.L2717:
	movzbl	401(%rsp), %ecx
	movzbl	400(%rsp), %eax
	leaq	400(%rsp), %r12
	cmpb	%cl, %dil
	movb	%al, 401(%rsp)
	leaq	1(%r12), %rax
	je	.L2727
	leaq	400(%rsp), %r12
	leaq	1(%r12), %rax
	jmp	.L2728
.L3248:
	movl	%edx, %ecx
.L2728:
	movzbl	1(%rax), %edx
	movb	%cl, 1(%rax)
	addq	$1, %rax
	cmpb	%dl, %dil
	jne	.L3248
.L2727:
	subl	%r12d, %eax
	movb	%dil, 400(%rsp)
	movq	16(%rsp), %rdi
	leal	1(%rax), %edx
	addl	$1, %eax
	movslq	%esi,%rcx
	cltq
	addl	$1, %esi
	addl	$1, 672(%rbp,%rax,4)
	movw	%dx, (%rdi,%rcx,2)
.L2716:
	leal	1(%r9), %eax
	addq	$1, %r9
	cmpl	%r13d, %eax
	jl	.L2711
	testl	%r8d, %r8d
	jle	.L2732
	leal	-1(%r8), %edx
	jmp	.L2734
.L3249:
	movq	16(%rsp), %rcx
	movslq	%esi,%rax
	addl	$1, 676(%rbp)
	addl	$1, %esi
	cmpl	$1, %edx
	movw	$1, (%rcx,%rax,2)
	jle	.L2732
.L3250:
	subl	$2, %edx
	movl	%edx, %eax
	shrl	$31, %eax
	leal	(%rax,%rdx), %edx
	sarl	%edx
.L2734:
	testb	$1, %dl
	jne	.L3249
	movq	16(%rsp), %rbx
	movslq	%esi,%rax
	addl	$1, 672(%rbp)
	addl	$1, %esi
	cmpl	$1, %edx
	movw	$0, (%rbx,%rax,2)
	jg	.L3250
.L2732:
	movslq	%esi,%rax
	leal	1(%rsi), %ecx
	addq	%rax, %rax
.L2710:
	movq	16(%rsp), %rsi
	movl	%ecx, 668(%rbp)
	movw	%r10w, (%rsi,%rax)
	movslq	%r10d,%rax
	addl	$1, 672(%rbp,%rax,4)
	cmpl	$2, 656(%rbp)
	jg	.L3251
.L2740:
	movl	12(%rsp), %eax
	xorl	%esi, %esi
	xorl	%edi, %edi
	addl	$2, %eax
	movl	%eax, 376(%rsp)
.L3242:
	cmpl	376(%rsp), %esi
	jl	.L2743
.L3252:
	addl	$1, %edi
	cmpl	$5, %edi
	jg	.L2745
	xorl	%esi, %esi
	cmpl	376(%rsp), %esi
	jge	.L3252
.L2743:
	movslq	%edi,%rdx
	movslq	%esi,%rax
	addl	$1, %esi
	movq	%rdx, %rcx
	salq	$8, %rcx
	leaq	(%rcx,%rdx,2), %rdx
	addq	%rbp, %rdx
	movb	$15, 37708(%rdx,%rax)
	jmp	.L3242
.L3245:
	addl	$1, %r8d
	jmp	.L2716
.L3273:
	movl	%r14d, %eax
	subl	%r12d, %eax
	cmpl	$49, %eax
	jne	.L2941
	movzbl	1704(%r15), %eax
	movq	%rax, %rdx
	salq	$8, %rdx
	leaq	37696(%rdx,%rax,2), %rdx
	leaq	12(%rbp,%rdx), %r8
	movq	%rax, %rdx
	salq	$10, %rdx
	leaq	39248(%rdx,%rax,8), %rax
	movq	16(%rsp), %rdx
	leaq	8(%rbp,%rax), %r9
	movslq	%r12d,%rax
	leaq	(%rax,%rax), %rdi
	movzwl	(%rdx,%rax,2), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	movl	644(%rbp), %eax
	cmpl	$7, %eax
	jle	.L2946
.L3081:
	movzbl	643(%rbp), %eax
	movslq	116(%rbp),%rcx
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %eax
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %eax
	cmpl	$7, %eax
	movl	%eax, 644(%rbp)
	jg	.L3081
.L2946:
	movl	%r13d, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%eax, %ecx
	subl	%r10d, %ecx
	sall	%cl, %esi
	movq	16(%rsp), %rcx
	orl	%esi, 640(%rbp)
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	2(%rcx,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L2948
.L3082:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3082
.L2948:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	orl	%esi, 640(%rbp)
	movq	16(%rsp), %rsi
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	4(%rsi,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L2950
.L3083:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3083
.L2950:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	movq	16(%rsp), %rcx
	orl	%esi, 640(%rbp)
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	6(%rcx,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L2952
.L3084:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3084
.L2952:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	orl	%esi, 640(%rbp)
	movq	16(%rsp), %rsi
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	8(%rsi,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L2954
.L3085:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3085
.L2954:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	movq	16(%rsp), %rcx
	orl	%esi, 640(%rbp)
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	10(%rcx,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L2956
.L3086:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3086
.L2956:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	orl	%esi, 640(%rbp)
	movq	16(%rsp), %rsi
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	12(%rsi,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L2958
.L3087:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3087
.L2958:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	movq	16(%rsp), %rcx
	orl	%esi, 640(%rbp)
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	14(%rcx,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L2960
.L3088:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3088
.L2960:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	orl	%esi, 640(%rbp)
	movq	16(%rsp), %rsi
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	16(%rsi,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L2962
.L3089:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3089
.L2962:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	movq	16(%rsp), %rcx
	orl	%esi, 640(%rbp)
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	18(%rcx,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L2964
.L3090:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3090
.L2964:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	orl	%esi, 640(%rbp)
	movq	16(%rsp), %rsi
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	20(%rsi,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L2966
.L3091:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3091
.L2966:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	movq	16(%rsp), %rcx
	orl	%esi, 640(%rbp)
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	22(%rcx,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L2968
.L3092:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3092
.L2968:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	orl	%esi, 640(%rbp)
	movq	16(%rsp), %rsi
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	24(%rsi,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L2970
.L3093:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3093
.L2970:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	movq	16(%rsp), %rcx
	orl	%esi, 640(%rbp)
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	26(%rcx,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L2972
.L3094:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3094
.L2972:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	orl	%esi, 640(%rbp)
	movq	16(%rsp), %rsi
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	28(%rsi,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L2974
.L3095:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3095
.L2974:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	movq	16(%rsp), %rcx
	orl	%esi, 640(%rbp)
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	30(%rcx,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L2976
.L3096:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3096
.L2976:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	orl	%esi, 640(%rbp)
	movq	16(%rsp), %rsi
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	32(%rsi,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L2978
.L3097:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3097
.L2978:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	movq	16(%rsp), %rcx
	orl	%esi, 640(%rbp)
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	34(%rcx,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L2980
.L3098:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3098
.L2980:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	orl	%esi, 640(%rbp)
	movq	16(%rsp), %rsi
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	36(%rsi,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L2982
.L3099:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3099
.L2982:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	movq	16(%rsp), %rcx
	orl	%esi, 640(%rbp)
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	38(%rcx,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L2984
.L3100:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3100
.L2984:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	orl	%esi, 640(%rbp)
	movq	16(%rsp), %rsi
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	40(%rsi,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L2986
.L3101:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3101
.L2986:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	movq	16(%rsp), %rcx
	orl	%esi, 640(%rbp)
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	42(%rcx,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L2988
.L3102:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3102
.L2988:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	orl	%esi, 640(%rbp)
	movq	16(%rsp), %rsi
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	44(%rsi,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L2990
.L3103:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3103
.L2990:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	movq	16(%rsp), %rcx
	orl	%esi, 640(%rbp)
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	46(%rcx,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L2992
.L3104:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3104
.L2992:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	orl	%esi, 640(%rbp)
	movq	16(%rsp), %rsi
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	48(%rsi,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L2994
.L3105:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3105
.L2994:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	movq	16(%rsp), %rcx
	orl	%esi, 640(%rbp)
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	50(%rcx,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L2996
.L3106:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3106
.L2996:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	orl	%esi, 640(%rbp)
	movq	16(%rsp), %rsi
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	52(%rsi,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L2998
.L3107:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3107
.L2998:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	movq	16(%rsp), %rcx
	orl	%esi, 640(%rbp)
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	54(%rcx,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L3000
.L3108:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3108
.L3000:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	orl	%esi, 640(%rbp)
	movq	16(%rsp), %rsi
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	56(%rsi,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L3002
.L3109:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3109
.L3002:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	movq	16(%rsp), %rcx
	orl	%esi, 640(%rbp)
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	58(%rcx,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L3004
.L3110:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3110
.L3004:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	orl	%esi, 640(%rbp)
	movq	16(%rsp), %rsi
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	60(%rsi,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L3006
.L3111:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3111
.L3006:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	movq	16(%rsp), %rcx
	orl	%esi, 640(%rbp)
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	62(%rcx,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L3008
.L3112:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3112
.L3008:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	orl	%esi, 640(%rbp)
	movq	16(%rsp), %rsi
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	64(%rsi,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L3010
.L3113:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3113
.L3010:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	movq	16(%rsp), %rcx
	orl	%esi, 640(%rbp)
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	66(%rcx,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L3012
.L3114:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3114
.L3012:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	orl	%esi, 640(%rbp)
	movq	16(%rsp), %rsi
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	68(%rsi,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L3014
.L3115:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3115
.L3014:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	movq	16(%rsp), %rcx
	orl	%esi, 640(%rbp)
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	70(%rcx,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L3016
.L3116:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3116
.L3016:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	orl	%esi, 640(%rbp)
	movq	16(%rsp), %rsi
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	72(%rsi,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L3018
.L3117:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3117
.L3018:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	movq	16(%rsp), %rcx
	orl	%esi, 640(%rbp)
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	74(%rcx,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L3020
.L3118:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3118
.L3020:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	orl	%esi, 640(%rbp)
	movq	16(%rsp), %rsi
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	76(%rsi,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L3022
.L3119:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3119
.L3022:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	movq	16(%rsp), %rcx
	orl	%esi, 640(%rbp)
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	78(%rcx,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L3024
.L3120:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3120
.L3024:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	orl	%esi, 640(%rbp)
	movq	16(%rsp), %rsi
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	80(%rsi,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L3026
.L3121:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3121
.L3026:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	movq	16(%rsp), %rcx
	orl	%esi, 640(%rbp)
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	82(%rcx,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L3028
.L3122:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3122
.L3028:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	orl	%esi, 640(%rbp)
	movq	16(%rsp), %rsi
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	84(%rsi,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L3030
.L3123:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3123
.L3030:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	movq	16(%rsp), %rcx
	orl	%esi, 640(%rbp)
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	86(%rcx,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L3032
.L3124:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3124
.L3032:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	orl	%esi, 640(%rbp)
	movq	16(%rsp), %rsi
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	88(%rsi,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L3034
.L3125:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3125
.L3034:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	movq	16(%rsp), %rcx
	orl	%esi, 640(%rbp)
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	90(%rcx,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L3036
.L3126:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3126
.L3036:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	orl	%esi, 640(%rbp)
	movq	16(%rsp), %rsi
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	92(%rsi,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L3038
.L3127:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3127
.L3038:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	movq	16(%rsp), %rcx
	orl	%esi, 640(%rbp)
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	94(%rcx,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L3040
.L3128:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3128
.L3040:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	orl	%esi, 640(%rbp)
	movq	16(%rsp), %rsi
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	96(%rsi,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %r10d
	jle	.L3042
.L3129:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3129
.L3042:
	movl	%r13d, %ecx
	subl	%edx, %ecx
	movl	%r10d, %edx
	addl	644(%rbp), %edx
	subl	%r10d, %ecx
	sall	%cl, %esi
	movq	16(%rsp), %rcx
	orl	%esi, 640(%rbp)
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	movzwl	98(%rcx,%rdi), %eax
	movl	(%r9,%rax,4), %esi
	movzbl	(%rax,%r8), %edi
	jle	.L3044
.L3130:
	movslq	116(%rbp),%rcx
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %edx
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %edx
	cmpl	$7, %edx
	movl	%edx, 644(%rbp)
	jg	.L3130
.L3044:
	movl	%r13d, %ecx
	addl	%edi, 644(%rbp)
	subl	%edx, %ecx
	subl	%edi, %ecx
	sall	%cl, %esi
	orl	%esi, 640(%rbp)
.L2945:
	movl	668(%rbp), %edx
	leal	1(%r14), %r12d
	addq	$1, %r15
	cmpl	%r12d, %edx
	jg	.L2936
	movl	%r15d, %eax
	subl	%ebp, %eax
.L2935:
	cmpl	%eax, 380(%rsp)
	je	.L3050
	movl	$3007, %edi
	call	BZ2_bz__AssertH__fail
.L3050:
	cmpl	$2, 656(%rbp)
	jle	.L2692
	movl	116(%rbp), %edx
	movq	stderr(%rip), %rdi
	movl	$.LC91, %esi
	xorl	%eax, %eax
	subl	%ebx, %edx
	call	fprintf
.L2692:
	cmpb	$0, 371(%rsp)
	jne	.L3253
.L3058:
	addq	$744, %rsp
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
.L2745:
	movl	668(%rbp), %eax
	testl	%eax, %eax
	jle	.L3254
.L2747:
	movl	668(%rbp), %r15d
	movl	$2, 372(%rsp)
	cmpl	$199, %r15d
	jle	.L2751
	cmpl	$599, %r15d
	movl	$3, 372(%rsp)
	jle	.L2751
	cmpl	$1199, %r15d
	movl	$4, 372(%rsp)
	jg	.L3255
.L2751:
	movl	376(%rsp), %edx
	movl	12(%rsp), %ecx
	xorl	%r14d, %r14d
	movl	372(%rsp), %r13d
	subl	$1, %edx
	addl	$2, %ecx
	movl	%edx, 8(%rsp)
	movl	%ecx, 4(%rsp)
.L2758:
	movl	%r15d, %edx
	movl	%r15d, %eax
	leal	-1(%r14), %ebx
	sarl	$31, %edx
	idivl	%r13d
	testl	%eax, %eax
	movl	%eax, %ecx
	jle	.L2759
	xorl	%r12d, %r12d
	cmpl	%ebx, 8(%rsp)
	jg	.L2762
	jmp	.L2759
	.p2align 4,,7
.L2763:
	cmpl	%ebx, 8(%rsp)
	.p2align 4,,3
	jle	.L2764
.L2762:
	addl	$1, %ebx
	movslq	%ebx,%rax
	addl	672(%rbp,%rax,4), %r12d
	cmpl	%r12d, %ecx
	jg	.L2763
.L2764:
	cmpl	%r14d, %ebx
	jle	.L2765
	cmpl	372(%rsp), %r13d
	je	.L2765
	cmpl	$1, %r13d
	je	.L2765
	movl	372(%rsp), %eax
	subl	%r13d, %eax
	movl	%eax, %edx
	shrl	$31, %edx
	addl	%edx, %eax
	andl	$1, %eax
	subl	%edx, %eax
	subl	$1, %eax
	je	.L3256
.L2765:
	cmpl	$2, 656(%rbp)
	jg	.L3257
	movl	376(%rsp), %eax
	testl	%eax, %eax
	jle	.L3258
.L2772:
	subl	$1, %r13d
	xorl	%esi, %esi
	movslq	%r13d,%rax
	movq	%rax, %rdx
	salq	$8, %rdx
	leaq	37696(%rdx,%rax,2), %rax
	leaq	12(%rbp,%rax), %rcx
	jmp	.L2775
.L3259:
	xorl	%eax, %eax
	movl	$15, %edx
	cmpl	%esi, %ebx
	cmovl	%edx, %eax
	addl	$1, %esi
	movb	%al, (%rcx)
	addq	$1, %rcx
	cmpl	4(%rsp), %esi
	je	.L2774
.L2775:
	cmpl	%esi, %r14d
	jle	.L3259
	movb	$15, (%rcx)
	addl	$1, %esi
	addq	$1, %rcx
	cmpl	4(%rsp), %esi
	jne	.L2775
.L2774:
	testl	%r13d, %r13d
	jle	.L3074
.L3260:
	leal	1(%rbx), %r14d
	subl	%r12d, %r15d
	jmp	.L2758
.L3253:
	movl	$23, %esi
	movq	%rbp, %rdi
	call	bsPutUChar
	movl	$114, %esi
	movq	%rbp, %rdi
	call	bsPutUChar
	movl	$69, %esi
	movq	%rbp, %rdi
	call	bsPutUChar
	movl	$56, %esi
	movq	%rbp, %rdi
	call	bsPutUChar
	movl	$80, %esi
	movq	%rbp, %rdi
	call	bsPutUChar
	movl	$144, %esi
	movq	%rbp, %rdi
	call	bsPutUChar
	movl	652(%rbp), %esi
	movq	%rbp, %rdi
	call	bsPutUInt32
	cmpl	$1, 656(%rbp)
	jle	.L3055
	movl	652(%rbp), %edx
	movq	stderr(%rip), %rdi
	movl	$.LC92, %esi
	xorl	%eax, %eax
	call	fprintf
.L3055:
	movl	644(%rbp), %r9d
	testl	%r9d, %r9d
	jle	.L3058
.L3132:
	movzbl	643(%rbp), %eax
	movslq	116(%rbp),%rcx
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %eax
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %eax
	testl	%eax, %eax
	movl	%eax, 644(%rbp)
	jg	.L3132
	jmp	.L3058
.L3257:
	cvtsi2ss	%r12d, %xmm0
	movq	stderr(%rip), %rdi
	movl	%r12d, %r9d
	movl	%ebx, %r8d
	cvtsi2ss	668(%rbp), %xmm1
	movl	%r14d, %ecx
	movl	%r13d, %edx
	movl	$.LC85, %esi
	movl	$1, %eax
	cvtss2sd	%xmm1, %xmm1
	cvtss2sd	%xmm0, %xmm0
	mulsd	.LC84(%rip), %xmm0
	divsd	%xmm1, %xmm0
	call	fprintf
	movl	376(%rsp), %eax
	testl	%eax, %eax
	jg	.L2772
.L3258:
	subl	$1, %r13d
	testl	%r13d, %r13d
	jg	.L3260
.L3074:
	xorl	%edx, %edx
	movl	$0, 384(%rsp)
.L3243:
	cmpl	372(%rsp), %edx
	jl	.L2784
.L3265:
	movl	372(%rsp), %r12d
	xorl	%esi, %esi
	testl	%r12d, %r12d
	jle	.L2786
.L2788:
	movl	376(%rsp), %ebx
	testl	%ebx, %ebx
	jle	.L2790
	movslq	%esi,%rax
	movq	%rax, %rdx
	salq	$10, %rdx
	leaq	45440(%rdx,%rax,8), %rax
	xorl	%edx, %edx
	leaq	8(%rbp,%rax), %rcx
	.p2align 4,,7
.L2789:
	movl	12(%rsp), %eax
	addl	$1, %edx
	movl	$0, (%rcx)
	addq	$4, %rcx
	addl	$2, %eax
	cmpl	%eax, %edx
	jne	.L2789
.L2790:
	addl	$1, %esi
	cmpl	372(%rsp), %esi
	jne	.L2788
.L2786:
	cmpl	$6, 372(%rsp)
	je	.L3261
.L2792:
	movl	668(%rbp), %edx
	movl	$0, 388(%rsp)
	movl	$0, 380(%rsp)
	testl	%edx, %edx
	movl	%edx, 28(%rsp)
	jle	.L2798
	movq	%rbp, 392(%rsp)
	xorl	%r15d, %r15d
.L2799:
	movl	28(%rsp), %eax
	leal	49(%r15), %r14d
	movl	372(%rsp), %r10d
	subl	$1, %eax
	cmpl	28(%rsp), %r14d
	cmovge	%eax, %r14d
	testl	%r10d, %r10d
	jle	.L2802
	leaq	704(%rsp), %rax
	xorl	%edx, %edx
	.p2align 4,,7
.L2804:
	movw	$0, (%rax)
	addl	$1, %edx
	addq	$2, %rax
	cmpl	372(%rsp), %edx
	jne	.L2804
.L2802:
	cmpl	$6, 372(%rsp)
	je	.L3262
.L2805:
	cmpl	%r15d, %r14d
	jl	.L2809
	movq	16(%rsp), %rsi
	movslq	%r15d,%rax
	movl	%r15d, %edi
	leaq	(%rsi,%rax,2), %r8
	.p2align 4,,7
.L2812:
	movl	372(%rsp), %r9d
	movzwl	(%r8), %eax
	testl	%r9d, %r9d
	jle	.L2813
	movzwl	%ax, %eax
	leaq	704(%rsp), %rdx
	xorl	%esi, %esi
	leaq	37708(%rax,%rbp), %rcx
	.p2align 4,,7
.L2815:
	movzbw	(%rcx), %ax
	addl	$1, %esi
	addq	$258, %rcx
	addw	%ax, (%rdx)
	addq	$2, %rdx
	cmpl	372(%rsp), %esi
	jne	.L2815
.L2813:
	addl	$1, %edi
	addq	$2, %r8
	cmpl	%edi, %r14d
	jge	.L2812
.L2809:
	movl	372(%rsp), %esi
	movl	$-1, %eax
	movl	$999999999, %r8d
	movl	%eax, %ecx
	testl	%esi, %esi
	jle	.L2817
.L2810:
	leaq	704(%rsp), %rdi
	movl	$-1, %eax
	movl	$999999999, %r8d
	xorl	%esi, %esi
	.p2align 4,,7
.L2811:
	movzwl	(%rdi,%rsi,2), %ecx
	cmpl	%ecx, %r8d
	jle	.L2818
	movl	%esi, %eax
	movl	%ecx, %r8d
.L2818:
	addq	$1, %rsi
	cmpl	%esi, 372(%rsp)
	jg	.L2811
	movl	%eax, %ecx
.L2817:
	movq	392(%rsp), %rdi
	movslq	%eax,%rsi
	addl	%r8d, 388(%rsp)
	addl	$1, 656(%rsp,%rsi,4)
	movb	%cl, 1704(%rdi)
	cmpl	$6, 372(%rsp)
	je	.L3263
.L2821:
	cmpl	%r15d, %r14d
	jl	.L2825
	movq	16(%rsp), %rdi
	movslq	%r15d,%rax
	leaq	(%rdi,%rax,2), %rcx
	movq	%rsi, %rax
	salq	$8, %rax
	leaq	(%rax,%rsi,2), %rdx
	.p2align 4,,7
.L2826:
	movzwl	(%rcx), %eax
	addl	$1, %r15d
	addq	$2, %rcx
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	cmpl	%r15d, %r14d
	jge	.L2826
.L2825:
	leal	1(%r14), %r15d
	addq	$1, 392(%rsp)
	cmpl	%r15d, 28(%rsp)
	jg	.L2799
	movl	392(%rsp), %eax
	subl	%ebp, %eax
	movl	%eax, 380(%rsp)
.L2798:
	cmpl	$2, 656(%rbp)
	jg	.L3264
.L2828:
	movl	372(%rsp), %eax
	testl	%eax, %eax
	jle	.L2833
	leaq	37708(%rbp), %r12
	leaq	45448(%rbp), %rbx
	xorl	%r13d, %r13d
.L2835:
	movl	376(%rsp), %edx
	movq	%rbx, %rsi
	movq	%r12, %rdi
	movl	$20, %ecx
	addl	$1, %r13d
	addq	$1032, %rbx
	addq	$258, %r12
	call	BZ2_hbMakeCodeLengths
	cmpl	372(%rsp), %r13d
	jne	.L2835
.L2833:
	addl	$1, 384(%rsp)
	cmpl	$3, 384(%rsp)
	jg	.L2836
	xorl	%edx, %edx
	cmpl	372(%rsp), %edx
	jge	.L3265
.L2784:
	movslq	%edx,%rax
	addl	$1, %edx
	movl	$0, 656(%rsp,%rax,4)
	jmp	.L3243
.L2759:
	xorl	%r12d, %r12d
	jmp	.L2764
.L3256:
	movslq	%ebx,%rax
	subl	$1, %ebx
	subl	672(%rbp,%rax,4), %r12d
	jmp	.L2765
	.p2align 4,,7
.L3263:
	movl	%r14d, %eax
	subl	%r15d, %eax
	cmpl	$49, %eax
	jne	.L2821
	movq	16(%rsp), %rdx
	movslq	%r15d,%rax
	movq	16(%rsp), %rbx
	leaq	(%rax,%rax), %rcx
	movzwl	(%rdx,%rax,2), %eax
	movq	%rsi, %rdx
	salq	$8, %rdx
	leaq	(%rdx,%rsi,2), %rdx
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	2(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	4(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	6(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	8(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	10(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	12(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	14(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	16(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	18(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	20(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	22(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	24(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	26(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	28(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	30(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	32(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	34(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	36(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	38(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	40(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	42(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	44(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	46(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	48(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	50(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	52(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	54(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	56(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	58(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	60(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	62(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	64(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	66(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	68(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	70(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	72(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	74(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	76(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	78(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	80(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	82(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	84(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	86(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	88(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	90(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	92(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	94(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	96(%rbx,%rcx), %eax
	leaq	(%rdx,%rax), %rax
	addl	$1, 45448(%rbp,%rax,4)
	movzwl	98(%rbx,%rcx), %eax
	addq	%rax, %rdx
	addl	$1, 45448(%rbp,%rdx,4)
	jmp	.L2825
.L3262:
	movl	%r14d, %eax
	subl	%r15d, %eax
	cmpl	$49, %eax
	jne	.L2805
	movq	16(%rsp), %rcx
	movslq	%r15d,%rdx
	movq	16(%rsp), %rsi
	leaq	(%rdx,%rdx), %rax
	movzwl	(%rcx,%rdx,2), %r12d
	movzwl	2(%rcx,%rax), %ebx
	movzwl	4(%rsi,%rax), %r13d
	movzwl	6(%rsi,%rax), %edi
	movzwl	8(%rsi,%rax), %edx
	salq	$4, %rbx
	salq	$4, %r12
	addq	%rbp, %rbx
	addq	%rbp, %r12
	salq	$4, %r13
	addq	%rbp, %r13
	salq	$4, %rdi
	movl	51640(%r12), %ecx
	addl	51640(%rbx), %ecx
	movq	%rdi, 360(%rsp)
	salq	$4, %rdx
	addq	%rbp, %rdi
	addl	51640(%r13), %ecx
	addq	%rbp, %rdx
	addl	51640(%rdi), %ecx
	movq	%rdx, 344(%rsp)
	addl	51640(%rdx), %ecx
	movzwl	10(%rsi,%rax), %edx
	movq	%rdi, 352(%rsp)
	salq	$4, %rdx
	addq	%rbp, %rdx
	addl	51640(%rdx), %ecx
	movq	%rdx, 336(%rsp)
	movzwl	12(%rsi,%rax), %edx
	salq	$4, %rdx
	addq	%rbp, %rdx
	addl	51640(%rdx), %ecx
	movq	%rdx, 328(%rsp)
	movzwl	14(%rsi,%rax), %edx
	salq	$4, %rdx
	addq	%rbp, %rdx
	addl	51640(%rdx), %ecx
	movq	%rdx, 320(%rsp)
	movzwl	16(%rsi,%rax), %edx
	salq	$4, %rdx
	addq	%rbp, %rdx
	addl	51640(%rdx), %ecx
	movq	%rdx, 312(%rsp)
	movzwl	18(%rsi,%rax), %edx
	salq	$4, %rdx
	addq	%rbp, %rdx
	addl	51640(%rdx), %ecx
	movq	%rdx, 304(%rsp)
	movzwl	20(%rsi,%rax), %edx
	salq	$4, %rdx
	addq	%rbp, %rdx
	addl	51640(%rdx), %ecx
	movq	%rdx, 296(%rsp)
	movzwl	22(%rsi,%rax), %edx
	salq	$4, %rdx
	addq	%rbp, %rdx
	movq	%rdx, 288(%rsp)
	addl	51640(%rdx), %ecx
	movzwl	24(%rsi,%rax), %edx
	salq	$4, %rdx
	addq	%rbp, %rdx
	addl	51640(%rdx), %ecx
	movq	%rdx, 280(%rsp)
	movzwl	26(%rsi,%rax), %edx
	salq	$4, %rdx
	addq	%rbp, %rdx
	addl	51640(%rdx), %ecx
	movq	%rdx, 272(%rsp)
	movzwl	28(%rsi,%rax), %edx
	salq	$4, %rdx
	addq	%rbp, %rdx
	addl	51640(%rdx), %ecx
	movq	%rdx, 264(%rsp)
	movzwl	30(%rsi,%rax), %edx
	salq	$4, %rdx
	addq	%rbp, %rdx
	addl	51640(%rdx), %ecx
	movq	%rdx, 256(%rsp)
	movzwl	32(%rsi,%rax), %edx
	salq	$4, %rdx
	addq	%rbp, %rdx
	addl	51640(%rdx), %ecx
	movq	%rdx, 248(%rsp)
	movzwl	34(%rsi,%rax), %edx
	salq	$4, %rdx
	addq	%rbp, %rdx
	addl	51640(%rdx), %ecx
	movq	%rdx, 240(%rsp)
	movzwl	36(%rsi,%rax), %edx
	salq	$4, %rdx
	addq	%rbp, %rdx
	addl	51640(%rdx), %ecx
	movq	%rdx, 232(%rsp)
	movzwl	38(%rsi,%rax), %edx
	salq	$4, %rdx
	addq	%rbp, %rdx
	addl	51640(%rdx), %ecx
	movq	%rdx, 224(%rsp)
	movzwl	40(%rsi,%rax), %edx
	salq	$4, %rdx
	addq	%rbp, %rdx
	addl	51640(%rdx), %ecx
	movq	%rdx, 216(%rsp)
	movzwl	42(%rsi,%rax), %edx
	salq	$4, %rdx
	addq	%rbp, %rdx
	addl	51640(%rdx), %ecx
	movq	%rdx, 208(%rsp)
	movzwl	44(%rsi,%rax), %edx
	salq	$4, %rdx
	addq	%rbp, %rdx
	addl	51640(%rdx), %ecx
	movq	%rdx, 200(%rsp)
	movzwl	46(%rsi,%rax), %edx
	salq	$4, %rdx
	addq	%rbp, %rdx
	movq	%rdx, 192(%rsp)
	addl	51640(%rdx), %ecx
	movzwl	48(%rsi,%rax), %edx
	salq	$4, %rdx
	addq	%rbp, %rdx
	addl	51640(%rdx), %ecx
	movq	%rdx, 184(%rsp)
	movzwl	50(%rsi,%rax), %edx
	salq	$4, %rdx
	addq	%rbp, %rdx
	addl	51640(%rdx), %ecx
	movq	%rdx, 176(%rsp)
	movzwl	52(%rsi,%rax), %edx
	salq	$4, %rdx
	addq	%rbp, %rdx
	addl	51640(%rdx), %ecx
	movq	%rdx, 168(%rsp)
	movzwl	54(%rsi,%rax), %edx
	salq	$4, %rdx
	addq	%rbp, %rdx
	addl	51640(%rdx), %ecx
	movq	%rdx, 160(%rsp)
	movzwl	56(%rsi,%rax), %edx
	salq	$4, %rdx
	addq	%rbp, %rdx
	addl	51640(%rdx), %ecx
	movq	%rdx, 152(%rsp)
	movzwl	58(%rsi,%rax), %edx
	salq	$4, %rdx
	addq	%rbp, %rdx
	addl	51640(%rdx), %ecx
	movq	%rdx, 144(%rsp)
	movzwl	60(%rsi,%rax), %edx
	salq	$4, %rdx
	addq	%rbp, %rdx
	addl	51640(%rdx), %ecx
	movq	%rdx, 136(%rsp)
	movzwl	62(%rsi,%rax), %edx
	salq	$4, %rdx
	addq	%rbp, %rdx
	addl	51640(%rdx), %ecx
	movq	%rdx, 128(%rsp)
	movzwl	64(%rsi,%rax), %edx
	salq	$4, %rdx
	addq	%rbp, %rdx
	addl	51640(%rdx), %ecx
	movq	%rdx, 120(%rsp)
	movzwl	66(%rsi,%rax), %edx
	salq	$4, %rdx
	addq	%rbp, %rdx
	addl	51640(%rdx), %ecx
	movq	%rdx, 112(%rsp)
	movzwl	68(%rsi,%rax), %edx
	salq	$4, %rdx
	addq	%rbp, %rdx
	addl	51640(%rdx), %ecx
	movq	%rdx, 104(%rsp)
	movzwl	70(%rsi,%rax), %edx
	salq	$4, %rdx
	addq	%rbp, %rdx
	movq	%rdx, 96(%rsp)
	addl	51640(%rdx), %ecx
	movzwl	72(%rsi,%rax), %edx
	salq	$4, %rdx
	addq	%rbp, %rdx
	movq	%rdx, 88(%rsp)
	addl	51640(%rdx), %ecx
	movzwl	74(%rsi,%rax), %edx
	salq	$4, %rdx
	addq	%rbp, %rdx
	movq	%rdx, 80(%rsp)
	addl	51640(%rdx), %ecx
	movzwl	76(%rsi,%rax), %edx
	salq	$4, %rdx
	addq	%rbp, %rdx
	movq	%rdx, 72(%rsp)
	addl	51640(%rdx), %ecx
	movzwl	78(%rsi,%rax), %edx
	salq	$4, %rdx
	addq	%rbp, %rdx
	movq	%rdx, 64(%rsp)
	addl	51640(%rdx), %ecx
	movzwl	80(%rsi,%rax), %edx
	salq	$4, %rdx
	addq	%rbp, %rdx
	movq	%rdx, 56(%rsp)
	addl	51640(%rdx), %ecx
	movzwl	82(%rsi,%rax), %edx
	salq	$4, %rdx
	addq	%rbp, %rdx
	movq	%rdx, 48(%rsp)
	addl	51640(%rdx), %ecx
	movzwl	84(%rsi,%rax), %edx
	salq	$4, %rdx
	addq	%rbp, %rdx
	movq	%rdx, 40(%rsp)
	addl	51640(%rdx), %ecx
	movzwl	86(%rsi,%rax), %edx
	salq	$4, %rdx
	addq	%rbp, %rdx
	movq	%rdx, 32(%rsp)
	addl	51640(%rdx), %ecx
	movq	%rsi, %rdx
	movzwl	88(%rsi,%rax), %esi
	movzwl	90(%rdx,%rax), %edi
	movzwl	92(%rdx,%rax), %r8d
	movzwl	94(%rdx,%rax), %r9d
	movzwl	96(%rdx,%rax), %r10d
	movzwl	98(%rdx,%rax), %r11d
	movq	352(%rsp), %rax
	movl	51644(%r12), %edx
	salq	$4, %rsi
	addq	%rbp, %rsi
	addl	51644(%rbx), %edx
	salq	$4, %rdi
	addl	51644(%r13), %edx
	addq	%rbp, %rdi
	addl	51640(%rsi), %ecx
	addl	51644(%rax), %edx
	movq	344(%rsp), %rax
	salq	$4, %r8
	addq	%rbp, %r8
	addl	51640(%rdi), %ecx
	salq	$4, %r9
	addq	%rbp, %r9
	addl	51640(%r8), %ecx
	salq	$4, %r10
	addl	51644(%rax), %edx
	movq	336(%rsp), %rax
	addq	%rbp, %r10
	addl	51640(%r9), %ecx
	salq	$4, %r11
	addq	%rbp, %r11
	addl	51640(%r10), %ecx
	addl	51644(%rax), %edx
	movq	328(%rsp), %rax
	addl	51640(%r11), %ecx
	addl	51644(%rax), %edx
	movq	320(%rsp), %rax
	addl	51644(%rax), %edx
	movq	312(%rsp), %rax
	addl	51644(%rax), %edx
	movq	304(%rsp), %rax
	addl	51644(%rax), %edx
	movq	296(%rsp), %rax
	addl	51644(%rax), %edx
	movq	288(%rsp), %rax
	addl	51644(%rax), %edx
	movq	280(%rsp), %rax
	addl	51644(%rax), %edx
	movq	272(%rsp), %rax
	addl	51644(%rax), %edx
	movq	264(%rsp), %rax
	addl	51644(%rax), %edx
	movq	256(%rsp), %rax
	addl	51644(%rax), %edx
	movq	248(%rsp), %rax
	addl	51644(%rax), %edx
	movq	240(%rsp), %rax
	addl	51644(%rax), %edx
	movq	232(%rsp), %rax
	addl	51644(%rax), %edx
	movq	224(%rsp), %rax
	addl	51644(%rax), %edx
	movq	216(%rsp), %rax
	addl	51644(%rax), %edx
	movq	208(%rsp), %rax
	addl	51644(%rax), %edx
	movq	200(%rsp), %rax
	addl	51644(%rax), %edx
	movq	192(%rsp), %rax
	addl	51644(%rax), %edx
	movq	184(%rsp), %rax
	addl	51644(%rax), %edx
	movq	176(%rsp), %rax
	addl	51644(%rax), %edx
	movq	168(%rsp), %rax
	addl	51644(%rax), %edx
	movq	160(%rsp), %rax
	addl	51644(%rax), %edx
	movq	152(%rsp), %rax
	addl	51644(%rax), %edx
	movq	144(%rsp), %rax
	addl	51644(%rax), %edx
	movq	136(%rsp), %rax
	addl	51644(%rax), %edx
	movq	128(%rsp), %rax
	addl	51644(%rax), %edx
	movq	120(%rsp), %rax
	addl	51644(%rax), %edx
	movq	112(%rsp), %rax
	addl	51644(%rax), %edx
	movq	104(%rsp), %rax
	addl	51644(%rax), %edx
	movq	96(%rsp), %rax
	addl	51644(%rax), %edx
	movq	88(%rsp), %rax
	addl	51644(%rax), %edx
	movq	80(%rsp), %rax
	addl	51644(%rax), %edx
	movq	72(%rsp), %rax
	addl	51644(%rax), %edx
	movq	64(%rsp), %rax
	addl	51644(%rax), %edx
	movq	56(%rsp), %rax
	addl	51644(%rax), %edx
	movq	48(%rsp), %rax
	addl	51644(%rax), %edx
	movq	40(%rsp), %rax
	addl	51644(%rax), %edx
	movq	32(%rsp), %rax
	addl	51644(%rax), %edx
	movl	51648(%r12), %eax
	addl	51644(%rsi), %edx
	addl	51644(%rdi), %edx
	addl	51644(%r8), %edx
	addl	51644(%r9), %edx
	addl	51644(%r10), %edx
	addl	51644(%r11), %edx
	addl	51648(%rbx), %eax
	movq	352(%rsp), %rbx
	addl	51648(%r13), %eax
	addl	51648(%rbx), %eax
	movq	344(%rsp), %rbx
	addl	51648(%rbx), %eax
	movq	336(%rsp), %rbx
	addl	51648(%rbx), %eax
	movq	328(%rsp), %rbx
	addl	51648(%rbx), %eax
	movq	320(%rsp), %rbx
	addl	51648(%rbx), %eax
	movq	312(%rsp), %rbx
	addl	51648(%rbx), %eax
	movq	304(%rsp), %rbx
	addl	51648(%rbx), %eax
	movq	296(%rsp), %rbx
	addl	51648(%rbx), %eax
	movq	288(%rsp), %rbx
	addl	51648(%rbx), %eax
	movq	280(%rsp), %rbx
	addl	51648(%rbx), %eax
	movq	272(%rsp), %rbx
	addl	51648(%rbx), %eax
	movq	264(%rsp), %rbx
	addl	51648(%rbx), %eax
	movq	256(%rsp), %rbx
	addl	51648(%rbx), %eax
	movq	248(%rsp), %rbx
	addl	51648(%rbx), %eax
	movq	240(%rsp), %rbx
	addl	51648(%rbx), %eax
	movq	232(%rsp), %rbx
	addl	51648(%rbx), %eax
	movq	224(%rsp), %rbx
	addl	51648(%rbx), %eax
	movq	216(%rsp), %rbx
	addl	51648(%rbx), %eax
	movq	208(%rsp), %rbx
	addl	51648(%rbx), %eax
	movq	200(%rsp), %rbx
	addl	51648(%rbx), %eax
	movq	192(%rsp), %rbx
	addl	51648(%rbx), %eax
	movq	184(%rsp), %rbx
	addl	51648(%rbx), %eax
	movq	176(%rsp), %rbx
	addl	51648(%rbx), %eax
	movq	168(%rsp), %rbx
	addl	51648(%rbx), %eax
	movq	160(%rsp), %rbx
	addl	51648(%rbx), %eax
	movq	152(%rsp), %rbx
	addl	51648(%rbx), %eax
	movq	144(%rsp), %rbx
	addl	51648(%rbx), %eax
	movq	136(%rsp), %rbx
	addl	51648(%rbx), %eax
	movq	128(%rsp), %rbx
	addl	51648(%rbx), %eax
	movq	120(%rsp), %rbx
	addl	51648(%rbx), %eax
	movq	112(%rsp), %rbx
	addl	51648(%rbx), %eax
	movq	104(%rsp), %rbx
	addl	51648(%rbx), %eax
	movq	96(%rsp), %rbx
	addl	51648(%rbx), %eax
	movq	88(%rsp), %rbx
	addl	51648(%rbx), %eax
	movq	80(%rsp), %rbx
	addl	51648(%rbx), %eax
	movq	72(%rsp), %rbx
	addl	51648(%rbx), %eax
	movq	64(%rsp), %rbx
	addl	51648(%rbx), %eax
	movq	56(%rsp), %rbx
	addl	51648(%rbx), %eax
	movq	48(%rsp), %rbx
	addl	51648(%rbx), %eax
	movq	40(%rsp), %rbx
	addl	51648(%rbx), %eax
	movq	32(%rsp), %rbx
	addl	51648(%rbx), %eax
	addl	51648(%rsi), %eax
	addl	51648(%rdi), %eax
	addl	51648(%r8), %eax
	addl	51648(%r9), %eax
	addl	51648(%r10), %eax
	addl	51648(%r11), %eax
	movw	%cx, 704(%rsp)
	shrl	$16, %ecx
	movw	%dx, 708(%rsp)
	shrl	$16, %edx
	movw	%cx, 706(%rsp)
	movw	%dx, 710(%rsp)
	movw	%ax, 712(%rsp)
	shrl	$16, %eax
	movw	%ax, 714(%rsp)
	jmp	.L2810
.L3261:
	movl	376(%rsp), %r11d
	testl	%r11d, %r11d
	jle	.L2792
	movl	12(%rsp), %r8d
	movq	%rbp, %rcx
	movq	%rbp, %rsi
	xorl	%edi, %edi
	addl	$2, %r8d
.L2795:
	movzbl	37966(%rcx), %eax
	movzbl	37708(%rcx), %edx
	addl	$1, %edi
	sall	$16, %eax
	orl	%edx, %eax
	movzbl	38224(%rcx), %edx
	movl	%eax, 51640(%rsi)
	movzbl	38482(%rcx), %eax
	sall	$16, %eax
	orl	%edx, %eax
	movzbl	38740(%rcx), %edx
	movl	%eax, 51644(%rsi)
	movzbl	38998(%rcx), %eax
	addq	$1, %rcx
	sall	$16, %eax
	orl	%edx, %eax
	movl	%eax, 51648(%rsi)
	addq	$16, %rsi
	cmpl	%r8d, %edi
	jne	.L2795
	jmp	.L2792
.L3264:
	movl	388(%rsp), %edx
	movl	388(%rsp), %eax
	movl	$.LC86, %esi
	movq	stderr(%rip), %rdi
	addl	$7, %eax
	testl	%edx, %edx
	cmovns	388(%rsp), %eax
	movl	384(%rsp), %edx
	movl	%eax, 388(%rsp)
	sarl	$3, %eax
	addl	$1, %edx
	movl	%eax, %ecx
	xorl	%eax, %eax
	call	fprintf
	movl	372(%rsp), %eax
	testl	%eax, %eax
	jle	.L2830
	leaq	656(%rsp), %rbx
	xorl	%r12d, %r12d
.L2832:
	movl	(%rbx), %edx
	movq	stderr(%rip), %rdi
	xorl	%eax, %eax
	movl	$.LC87, %esi
	addl	$1, %r12d
	addq	$4, %rbx
	call	fprintf
	cmpl	372(%rsp), %r12d
	jne	.L2832
.L2830:
	movq	stderr(%rip), %rsi
	movl	$10, %edi
	call	fputc
	jmp	.L2828
.L2836:
	cmpl	$7, 372(%rsp)
	jg	.L3266
.L2838:
	cmpl	$32767, 380(%rsp)
	jg	.L2840
	cmpl	$18002, 380(%rsp)
	jg	.L2840
.L2842:
	movl	372(%rsp), %eax
	testl	%eax, %eax
	jle	.L2844
	leaq	720(%rsp), %rdx
	xorl	%eax, %eax
.L2846:
	movb	%al, (%rdx)
	addl	$1, %eax
	addq	$1, %rdx
	cmpl	372(%rsp), %eax
	jne	.L2846
.L2844:
	movl	380(%rsp), %eax
	testl	%eax, %eax
	jle	.L2847
	leaq	720(%rsp), %r9
	movq	%rbp, %rdi
	xorl	%r8d, %r8d
.L2849:
	movzbl	1704(%rdi), %esi
	movzbl	720(%rsp), %edx
	xorl	%eax, %eax
	cmpb	%dl, %sil
	je	.L2852
	movq	%r9, %rcx
	jmp	.L2853
.L3267:
	movl	%eax, %edx
.L2853:
	movzbl	1(%rcx), %eax
	movb	%dl, 1(%rcx)
	addq	$1, %rcx
	cmpb	%al, %sil
	jne	.L3267
	movl	%ecx, %eax
	subb	%r9b, %al
.L2852:
	movb	%al, 19706(%rdi)
	addl	$1, %r8d
	addq	$1, %rdi
	cmpl	380(%rsp), %r8d
	je	.L2847
	movb	%sil, 720(%rsp)
	jmp	.L2849
.L2847:
	movl	372(%rsp), %eax
	xorl	%r14d, %r14d
	xorl	%r13d, %r13d
	testl	%eax, %eax
	jle	.L2857
.L2859:
	movl	376(%rsp), %eax
	xorl	%ebx, %ebx
	movl	$32, %r12d
	testl	%eax, %eax
	jle	.L2864
	movslq	%r14d,%rax
	movq	%rax, %rdx
	salq	$8, %rdx
	leaq	37696(%rdx,%rax,2), %rax
	xorl	%edx, %edx
	leaq	12(%rbp,%rax), %rcx
.L2860:
	movzbl	(%rcx), %eax
	cmpl	%eax, %ebx
	cmovl	%eax, %ebx
	cmpl	%eax, %r12d
	cmovg	%eax, %r12d
	movl	12(%rsp), %eax
	addl	$1, %edx
	addq	$1, %rcx
	addl	$2, %eax
	cmpl	%eax, %edx
	jne	.L2860
	cmpl	$20, %ebx
	jle	.L2862
	movl	$3004, %edi
	call	BZ2_bz__AssertH__fail
.L2862:
	testl	%r12d, %r12d
	.p2align 4,,2
	jle	.L3268
.L2864:
	movq	%r13, %rax
	movl	376(%rsp), %r8d
	movl	%ebx, %ecx
	salq	$8, %rax
	movl	%r12d, %edx
	addl	$1, %r14d
	leaq	37696(%rax,%r13,2), %rax
	leaq	12(%rbp,%rax), %rsi
	movq	%r13, %rax
	salq	$10, %rax
	leaq	39248(%rax,%r13,8), %rax
	addq	$1, %r13
	leaq	8(%rbp,%rax), %rdi
	call	BZ2_hbAssignCodes
	cmpl	372(%rsp), %r14d
	jne	.L2859
.L2857:
	leaq	688(%rsp), %r11
	xorl	%esi, %esi
.L2868:
	movl	%esi, %ecx
	movb	$0, (%rsi,%r11)
	xorl	%edx, %edx
	sall	$4, %ecx
.L2869:
	leal	(%rcx,%rdx), %eax
	cltq
	cmpb	$0, 128(%rax,%rbp)
	je	.L2870
	movb	$1, (%rsi,%r11)
.L2870:
	addl	$1, %edx
	cmpl	$16, %edx
	jne	.L2869
	addq	$1, %rsi
	cmpq	$16, %rsi
	jne	.L2868
	movl	116(%rbp), %ebx
	xorb	%sil, %sil
	movl	$32, %edi
	movl	$1, %r8d
	movl	%ebx, %r12d
.L2874:
	cmpb	$0, (%rsi,%r11)
	je	.L2875
	movl	644(%rbp), %eax
	cmpl	$7, %eax
	jg	.L3222
	jmp	.L2878
	.p2align 4,,7
.L3061:
	movl	116(%rbp), %ebx
.L3222:
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movslq	%ebx,%rcx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %eax
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %eax
	cmpl	$7, %eax
	movl	%eax, 644(%rbp)
	jg	.L3061
	movl	116(%rbp), %ebx
.L2878:
	movl	%edi, %ecx
	movl	%r8d, %edx
	addl	$1, 644(%rbp)
	subl	%eax, %ecx
	subl	$1, %ecx
	sall	%cl, %edx
	orl	%edx, 640(%rbp)
.L2881:
	addq	$1, %rsi
	cmpq	$16, %rsi
	jne	.L2874
	xorl	%r8d, %r8d
.L2883:
	cmpb	$0, (%r8,%r11)
	je	.L2884
	movl	%r8d, %edi
	xorl	%esi, %esi
	movl	$32, %r9d
	sall	$4, %edi
	movl	$1, %r10d
.L2886:
	leal	(%rdi,%rsi), %eax
	cltq
	cmpb	$0, 128(%rax,%rbp)
	je	.L2887
	movl	644(%rbp), %eax
	cmpl	$7, %eax
	jg	.L3225
	jmp	.L2890
.L3063:
	movl	116(%rbp), %ebx
.L3225:
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movslq	%ebx,%rcx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %eax
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %eax
	cmpl	$7, %eax
	movl	%eax, 644(%rbp)
	jg	.L3063
	movl	116(%rbp), %ebx
.L2890:
	movl	%r9d, %ecx
	addl	$1, 644(%rbp)
	subl	%eax, %ecx
	movl	%r10d, %eax
	subl	$1, %ecx
	sall	%cl, %eax
	orl	%eax, 640(%rbp)
.L2893:
	addl	$1, %esi
	cmpl	$16, %esi
	jne	.L2886
.L2884:
	addq	$1, %r8
	cmpq	$16, %r8
	jne	.L2883
	cmpl	$2, 656(%rbp)
	jg	.L3269
.L2895:
	movl	644(%rbp), %eax
	movl	%ebx, %r8d
	cmpl	$7, %eax
	jg	.L3228
	jmp	.L2897
	.p2align 4,,7
.L3065:
	movl	116(%rbp), %ebx
.L3228:
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movslq	%ebx,%rcx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %eax
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %eax
	cmpl	$7, %eax
	movl	%eax, 644(%rbp)
	jg	.L3065
	movl	116(%rbp), %ebx
.L2897:
	movl	$29, %ecx
	movl	372(%rsp), %edx
	subl	%eax, %ecx
	movl	644(%rbp), %eax
	sall	%cl, %edx
	orl	%edx, 640(%rbp)
	addl	$3, %eax
	cmpl	$7, %eax
	movl	%eax, 644(%rbp)
	jg	.L3229
	jmp	.L2899
	.p2align 4,,7
.L3066:
	movl	116(%rbp), %ebx
.L3229:
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movslq	%ebx,%rcx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %eax
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %eax
	cmpl	$7, %eax
	movl	%eax, 644(%rbp)
	jg	.L3066
	movl	116(%rbp), %ebx
.L2899:
	movl	380(%rsp), %esi
	movl	380(%rsp), %r15d
	movl	$17, %ecx
	subl	%eax, %ecx
	addl	$15, 644(%rbp)
	xorl	%r9d, %r9d
	movq	%rbp, %rdi
	sall	%cl, %esi
	orl	%esi, 640(%rbp)
	testl	%r15d, %r15d
	jle	.L2901
.L2903:
	xorl	%esi, %esi
	cmpb	$0, 19706(%rdi)
	je	.L2906
.L2907:
	movl	644(%rbp), %eax
	cmpl	$7, %eax
	jg	.L3230
	jmp	.L2905
.L3067:
	movl	116(%rbp), %ebx
.L3230:
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movslq	%ebx,%rcx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %eax
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %eax
	cmpl	$7, %eax
	movl	%eax, 644(%rbp)
	jg	.L3067
	movl	116(%rbp), %ebx
.L2905:
	notl	%eax
	addl	$1, %esi
	addl	$1, 644(%rbp)
	leal	32(%rax), %ecx
	movl	$1, %eax
	sall	%cl, %eax
	orl	%eax, 640(%rbp)
	movzbl	19706(%rdi), %eax
	cmpl	%esi, %eax
	jg	.L2907
.L2906:
	cmpl	$7, 644(%rbp)
	jg	.L3231
	jmp	.L2908
	.p2align 4,,7
.L3068:
	movl	116(%rbp), %ebx
.L3231:
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movslq	%ebx,%rcx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %eax
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %eax
	cmpl	$7, %eax
	movl	%eax, 644(%rbp)
	jg	.L3068
	movl	116(%rbp), %ebx
.L2908:
	addl	$1, 644(%rbp)
	addl	$1, %r9d
	addq	$1, %rdi
	cmpl	380(%rsp), %r9d
	jne	.L2903
.L2901:
	cmpl	$2, 656(%rbp)
	jg	.L3270
.L2911:
	movl	372(%rsp), %r14d
	movl	%ebx, %r10d
	testl	%r14d, %r14d
	jle	.L2913
	movq	%rbp, %r12
	xorl	%r11d, %r11d
	movl	$32, %r8d
.L2915:
	movl	644(%rbp), %eax
	movzbl	37708(%r12), %esi
	cmpl	$7, %eax
	jg	.L3232
	jmp	.L2916
	.p2align 4,,7
.L3069:
	movl	116(%rbp), %ebx
.L3232:
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movslq	%ebx,%rcx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %eax
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %eax
	cmpl	$7, %eax
	movl	%eax, 644(%rbp)
	jg	.L3069
	movl	116(%rbp), %ebx
.L2916:
	movl	%r8d, %ecx
	movl	376(%rsp), %r13d
	movl	%esi, %edi
	subl	%eax, %ecx
	addl	$5, 644(%rbp)
	subl	$5, %ecx
	sall	%cl, %edi
	orl	%edi, 640(%rbp)
	testl	%r13d, %r13d
	jle	.L2918
	movslq	%r11d,%rax
	xorl	%r9d, %r9d
	movq	%rax, %rdx
	salq	$8, %rdx
	leaq	37696(%rdx,%rax,2), %rax
	leaq	12(%rbp,%rax), %rdi
.L2920:
	movzbl	(%rdi), %eax
	cmpl	%eax, %esi
	jge	.L2923
	.p2align 4,,7
.L3234:
	movl	644(%rbp), %eax
	cmpl	$7, %eax
	jg	.L3233
	jmp	.L2922
	.p2align 4,,7
.L3070:
	movl	116(%rbp), %ebx
.L3233:
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movslq	%ebx,%rcx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %eax
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %eax
	cmpl	$7, %eax
	movl	%eax, 644(%rbp)
	jg	.L3070
	movl	116(%rbp), %ebx
.L2922:
	movl	%r8d, %ecx
	addl	$1, %esi
	addl	$2, 644(%rbp)
	subl	%eax, %ecx
	movl	$2, %eax
	subl	$2, %ecx
	sall	%cl, %eax
	orl	%eax, 640(%rbp)
	movzbl	(%rdi), %eax
	cmpl	%esi, %eax
	jg	.L3234
.L2923:
	movzbl	(%rdi), %eax
	cmpl	%eax, %esi
	jle	.L2926
	.p2align 4,,7
.L3236:
	movl	644(%rbp), %eax
	cmpl	$7, %eax
	jg	.L3235
	jmp	.L2928
	.p2align 4,,7
.L3071:
	movl	116(%rbp), %ebx
.L3235:
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movslq	%ebx,%rcx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %eax
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %eax
	cmpl	$7, %eax
	movl	%eax, 644(%rbp)
	jg	.L3071
	movl	116(%rbp), %ebx
.L2928:
	movl	%r8d, %ecx
	subl	$1, %esi
	addl	$2, 644(%rbp)
	subl	%eax, %ecx
	movl	$3, %eax
	subl	$2, %ecx
	sall	%cl, %eax
	orl	%eax, 640(%rbp)
	movzbl	(%rdi), %eax
	cmpl	%esi, %eax
	jl	.L3236
.L2926:
	cmpl	$7, 644(%rbp)
	jg	.L3237
	jmp	.L2929
.L3072:
	movl	116(%rbp), %ebx
.L3237:
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movslq	%ebx,%rcx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %eax
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %eax
	cmpl	$7, %eax
	movl	%eax, 644(%rbp)
	jg	.L3072
	movl	116(%rbp), %ebx
.L2929:
	movl	12(%rsp), %eax
	addl	$1, %r9d
	addl	$1, 644(%rbp)
	addq	$1, %rdi
	addl	$2, %eax
	cmpl	%eax, %r9d
	jne	.L2920
.L2918:
	addl	$1, %r11d
	addq	$258, %r12
	cmpl	372(%rsp), %r11d
	jne	.L2915
.L2913:
	cmpl	$2, 656(%rbp)
	jg	.L3271
.L2931:
	movl	668(%rbp), %edx
	xorl	%eax, %eax
	testl	%edx, %edx
	jle	.L2935
	movq	%rbp, %r15
	xorl	%r12d, %r12d
	movl	$32, %r13d
.L2936:
	leal	49(%r12), %r14d
	leal	-1(%rdx), %eax
	cmpl	%r14d, %edx
	cmovle	%eax, %r14d
	movzbl	1704(%r15), %eax
	cmpl	%eax, 372(%rsp)
	jle	.L3272
.L2939:
	cmpl	$6, 372(%rsp)
	je	.L3273
.L2941:
	cmpl	%r12d, %r14d
	jl	.L2945
	movq	16(%rsp), %rsi
	movslq	%r12d,%rax
	movl	$32, %r9d
	leaq	(%rsi,%rax,2), %rdi
.L3046:
	movzbl	1704(%r15), %eax
	movzwl	(%rdi), %ecx
	movq	%rax, %rdx
	salq	$8, %rdx
	leaq	(%rdx,%rax,2), %rax
	leaq	(%rax,%rcx), %rdx
	addq	%rbp, %rax
	movzbl	37708(%rax,%rcx), %r8d
	movl	644(%rbp), %eax
	movl	39256(%rbp,%rdx,4), %esi
	cmpl	$7, %eax
	jle	.L3047
.L3131:
	movzbl	643(%rbp), %eax
	movslq	116(%rbp),%rcx
	movq	80(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %eax
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %eax
	cmpl	$7, %eax
	movl	%eax, 644(%rbp)
	jg	.L3131
.L3047:
	movl	%r9d, %ecx
	addl	$1, %r12d
	addl	%r8d, 644(%rbp)
	subl	%eax, %ecx
	addq	$2, %rdi
	subl	%r8d, %ecx
	sall	%cl, %esi
	orl	%esi, 640(%rbp)
	cmpl	%r12d, %r14d
	jge	.L3046
	jmp	.L2945
.L2887:
	cmpl	$7, 644(%rbp)
	jg	.L3226
	.p2align 4,,6
	jmp	.L2892
.L3064:
	movl	116(%rbp), %ebx
.L3226:
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movslq	%ebx,%rcx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %eax
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %eax
	cmpl	$7, %eax
	movl	%eax, 644(%rbp)
	jg	.L3064
	movl	116(%rbp), %ebx
.L2892:
	addl	$1, 644(%rbp)
	jmp	.L2893
.L2875:
	cmpl	$7, 644(%rbp)
	jg	.L3223
	jmp	.L2880
	.p2align 4,,7
.L3062:
	movl	116(%rbp), %ebx
.L3223:
	movzbl	643(%rbp), %eax
	movq	80(%rbp), %rdx
	movslq	%ebx,%rcx
	movb	%al, (%rdx,%rcx)
	movl	644(%rbp), %eax
	addl	$1, 116(%rbp)
	sall	$8, 640(%rbp)
	subl	$8, %eax
	cmpl	$7, %eax
	movl	%eax, 644(%rbp)
	jg	.L3062
	movl	116(%rbp), %ebx
.L2880:
	addl	$1, 644(%rbp)
	jmp	.L2881
.L3272:
	movl	$3006, %edi
	call	BZ2_bz__AssertH__fail
	jmp	.L2939
.L3268:
	movl	$3005, %edi
	call	BZ2_bz__AssertH__fail
	.p2align 4,,6
	jmp	.L2864
.L3244:
	movq	%rbp, %rdi
	call	BZ2_bsInitWrite
	movq	%rbp, %rdi
	movl	$66, %esi
	call	bsPutUChar
	movq	%rbp, %rdi
	movl	$90, %esi
	call	bsPutUChar
	movq	%rbp, %rdi
	movl	$104, %esi
	call	bsPutUChar
	movzbl	664(%rbp), %eax
	movq	%rbp, %rdi
	leal	48(%rax), %esi
	movzbl	%sil, %esi
	call	bsPutUChar
	jmp	.L2690
.L3271:
	movq	stderr(%rip), %rdi
	subl	%r10d, %ebx
	movl	$.LC90, %esi
	movl	%ebx, %edx
	xorl	%eax, %eax
	call	fprintf
	movl	116(%rbp), %ebx
	jmp	.L2931
.L3270:
	movq	stderr(%rip), %rdi
	subl	%r8d, %ebx
	movl	$.LC89, %esi
	movl	%ebx, %edx
	xorl	%eax, %eax
	call	fprintf
	movl	116(%rbp), %ebx
	jmp	.L2911
.L3269:
	movq	stderr(%rip), %rdi
	subl	%r12d, %ebx
	movl	$.LC88, %esi
	movl	%ebx, %edx
	xorl	%eax, %eax
	call	fprintf
	movl	116(%rbp), %ebx
	jmp	.L2895
.L2840:
	movl	$3003, %edi
	call	BZ2_bz__AssertH__fail
	.p2align 4,,4
	jmp	.L2842
.L3266:
	movl	$3002, %edi
	call	BZ2_bz__AssertH__fail
	.p2align 4,,6
	jmp	.L2838
.L3255:
	xorl	%eax, %eax
	cmpl	$2399, %r15d
	setg	%al
	addl	$5, %eax
	movl	%eax, 372(%rsp)
	jmp	.L2751
.L3254:
	movl	$3001, %edi
	call	BZ2_bz__AssertH__fail
	jmp	.L2747
.L3251:
	movq	stderr(%rip), %rdi
	movl	12(%rsp), %r8d
	movl	%r13d, %edx
	movl	$.LC83, %esi
	xorl	%eax, %eax
	call	fprintf
	movl	124(%rbp), %edi
	movl	%edi, 12(%rsp)
	jmp	.L2740
.LFE25:
	.size	BZ2_compressBlock, .-BZ2_compressBlock
	.p2align 4,,15
	.type	handle_compress, @function
handle_compress:
.LFB40:
	pushq	%r13
.LCFI154:
	xorl	%r13d, %r13d
	pushq	%r12
.LCFI155:
	xorl	%r12d, %r12d
	pushq	%rbp
.LCFI156:
	pushq	%rbx
.LCFI157:
	subq	$8, %rsp
.LCFI158:
	movq	48(%rdi), %rbx
	movl	12(%rbx), %eax
.L3348:
	cmpl	$1, %eax
	je	.L3342
	cmpl	$2, %eax
	jne	.L3348
.L3290:
	xorl	%eax, %eax
	cmpl	$2, 8(%rbx)
	je	.L3295
.L3292:
	movl	112(%rbx), %eax
	cmpl	108(%rbx), %eax
	jle	.L3296
	movq	(%rbx), %rcx
	movl	8(%rcx), %r11d
	testl	%r11d, %r11d
	je	.L3296
	movl	16(%rbx), %r10d
	testl	%r10d, %r10d
	jne	.L3346
	jmp	.L3296
	.p2align 4,,7
.L3351:
	cmpl	$1, 96(%rbx)
	.p2align 4,,5
	je	.L3349
.L3320:
	cmpl	$255, %esi
	.p2align 4,,7
	jbe	.L3350
.L3324:
	movl	%ebp, 92(%rbx)
	movl	$1, 96(%rbx)
.L3322:
	addl	$1, 12(%rcx)
	addq	$1, (%rcx)
	movl	12(%rcx), %esi
	subl	$1, 8(%rcx)
	testl	%esi, %esi
	jne	.L3326
	addl	$1, 16(%rcx)
.L3326:
	movl	16(%rbx), %eax
	movl	108(%rbx), %esi
	subl	$1, %eax
	cmpl	112(%rbx), %esi
	movl	%eax, 16(%rbx)
	jge	.L3316
	movl	8(%rcx), %edi
	testl	%edi, %edi
	je	.L3316
	testl	%eax, %eax
	je	.L3316
.L3346:
	movq	(%rcx), %rax
	movl	92(%rbx), %esi
	movzbl	(%rax), %ebp
	cmpl	%ebp, %esi
	jne	.L3351
	movl	96(%rbx), %eax
	cmpl	$255, %eax
	je	.L3320
	addl	$1, %eax
	movl	%eax, 96(%rbx)
	jmp	.L3322
	.p2align 4,,7
.L3354:
	cmpl	$1, 96(%rbx)
	je	.L3352
.L3306:
	cmpl	$255, %edi
	.p2align 4,,3
	jbe	.L3353
.L3310:
	movl	%ebp, 92(%rbx)
	movl	$1, 96(%rbx)
.L3308:
	addl	$1, 12(%rcx)
	addq	$1, (%rcx)
	movl	12(%rcx), %r8d
	subl	$1, 8(%rcx)
	testl	%r8d, %r8d
	jne	.L3312
	addl	$1, 16(%rcx)
.L3312:
	movl	$1, %eax
.L3295:
	movl	108(%rbx), %esi
	cmpl	112(%rbx), %esi
	jge	.L3303
	movq	(%rbx), %rcx
	movl	8(%rcx), %r9d
	testl	%r9d, %r9d
	je	.L3303
	movq	(%rcx), %rax
	movl	92(%rbx), %edi
	movzbl	(%rax), %ebp
	cmpl	%ebp, %edi
	jne	.L3354
	movl	96(%rbx), %eax
	cmpl	$255, %eax
	je	.L3306
	addl	$1, %eax
	movl	%eax, 96(%rbx)
	jmp	.L3308
.L3316:
	movl	$1, %eax
.L3303:
	orl	%eax, %r13d
	cmpl	$2, 8(%rbx)
	je	.L3328
.L3296:
	movl	16(%rbx), %edx
	testl	%edx, %edx
	jne	.L3339
	cmpl	$255, 92(%rbx)
	jbe	.L3355
.L3330:
	movq	%rbx, %rdi
	call	init_RL
	xorl	%esi, %esi
	cmpl	$4, 8(%rbx)
	movq	%rbx, %rdi
	sete	%sil
	call	BZ2_compressBlock
	movl	$1, 12(%rbx)
.L3342:
	movq	(%rbx), %rcx
	xorl	%edx, %edx
	jmp	.L3278
	.p2align 4,,7
.L3279:
	movl	120(%rbx), %eax
	cmpl	116(%rbx), %eax
	jge	.L3280
	movslq	%eax,%rdx
	movq	80(%rbx), %rax
	movq	24(%rcx), %rcx
	movzbl	(%rax,%rdx), %eax
	movb	%al, (%rcx)
	movq	(%rbx), %rcx
	addl	$1, 120(%rbx)
	addl	$1, 36(%rcx)
	subl	$1, 32(%rcx)
	movl	36(%rcx), %eax
	addq	$1, 24(%rcx)
	testl	%eax, %eax
	jne	.L3282
	addl	$1, 40(%rcx)
.L3282:
	movl	$1, %edx
.L3278:
	movl	32(%rcx), %eax
	testl	%eax, %eax
	jne	.L3279
	orl	%edx, %r12d
	movl	120(%rbx), %eax
	cmpl	116(%rbx), %eax
	jl	.L3285
	cmpl	$4, 8(%rbx)
	je	.L3356
.L3287:
	movq	%rbx, %rdi
	call	prepare_new_block
	cmpl	$3, 8(%rbx)
	movl	$2, 12(%rbx)
	jne	.L3290
	movl	16(%rbx), %ebp
	testl	%ebp, %ebp
	jne	.L3292
	movq	%rbx, %rdi
	call	isempty_RL
	testb	%al, %al
	je	.L3292
.L3285:
	testb	%r13b, %r13b
	movl	$1, %eax
	jne	.L3336
	testb	%r12b, %r12b
	setne	%al
	andl	$1, %eax
.L3336:
	addq	$8, %rsp
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	ret
.L3339:
	movl	108(%rbx), %esi
.L3328:
	cmpl	112(%rbx), %esi
	jge	.L3357
	movq	(%rbx), %rcx
	movl	8(%rcx), %eax
	testl	%eax, %eax
	je	.L3285
	movl	12(%rbx), %eax
	jmp	.L3348
.L3349:
	movl	648(%rbx), %edx
	movl	%edx, %eax
	sall	$8, %edx
	shrl	$24, %eax
	xorl	%esi, %eax
	movzbl	%al, %eax
	xorl	BZ2_crc32Table(,%rax,4), %edx
	mov	%esi, %eax
	movb	$1, 128(%rax,%rbx)
	movq	64(%rbx), %rax
	movl	%edx, 648(%rbx)
	movslq	108(%rbx),%rdx
	movb	%sil, (%rax,%rdx)
	addl	$1, 108(%rbx)
	movq	(%rbx), %rcx
	movl	%ebp, 92(%rbx)
	jmp	.L3322
.L3350:
	movq	%rbx, %rdi
	call	add_pair_to_block
	movq	(%rbx), %rcx
	jmp	.L3324
.L3280:
	orl	%edx, %r12d
	cmpl	$4, 8(%rbx)
	jne	.L3287
.L3356:
	movl	16(%rbx), %eax
	testl	%eax, %eax
	jne	.L3287
	movq	%rbx, %rdi
	call	isempty_RL
	testb	%al, %al
	je	.L3287
	.p2align 4,,4
	jmp	.L3285
	.p2align 4,,7
.L3352:
	movl	648(%rbx), %edx
	movl	%edx, %eax
	sall	$8, %edx
	shrl	$24, %eax
	xorl	%edi, %eax
	movzbl	%al, %eax
	xorl	BZ2_crc32Table(,%rax,4), %edx
	mov	%edi, %eax
	movb	$1, 128(%rax,%rbx)
	movq	64(%rbx), %rax
	movl	%edx, 648(%rbx)
	movslq	%esi,%rdx
	movb	%dil, (%rax,%rdx)
	addl	$1, 108(%rbx)
	movq	(%rbx), %rcx
	movl	%ebp, 92(%rbx)
	jmp	.L3308
.L3353:
	movq	%rbx, %rdi
	call	add_pair_to_block
	movq	(%rbx), %rcx
	jmp	.L3310
.L3357:
	xorl	%esi, %esi
	movq	%rbx, %rdi
	call	BZ2_compressBlock
	movl	$1, 12(%rbx)
	jmp	.L3342
.L3355:
	movq	%rbx, %rdi
	call	add_pair_to_block
	.p2align 4,,4
	jmp	.L3330
.LFE40:
	.size	handle_compress, .-handle_compress
	.p2align 4,,15
.globl BZ2_bzCompress
	.type	BZ2_bzCompress, @function
BZ2_bzCompress:
.LFB41:
	testq	%rdi, %rdi
	pushq	%rbx
.LCFI159:
	jne	.L3397
.L3359:
	movl	$-2, %edx
.L3369:
	popq	%rbx
	movl	%edx, %eax
	ret
	.p2align 4,,7
.L3397:
	movq	48(%rdi), %rbx
	testq	%rbx, %rbx
	je	.L3359
	cmpq	%rdi, (%rbx)
	jne	.L3359
.L3396:
	movl	8(%rbx), %eax
	cmpl	$2, %eax
	je	.L3365
	.p2align 4,,5
	jg	.L3368
	subl	$1, %eax
	.p2align 4,,5
	je	.L3364
.L3363:
	xorl	%edx, %edx
	.p2align 4,,7
	jmp	.L3369
.L3364:
	movl	$-1, %edx
	.p2align 4,,5
	jmp	.L3369
.L3368:
	cmpl	$3, %eax
	.p2align 4,,5
	je	.L3366
	cmpl	$4, %eax
	.p2align 4,,5
	jne	.L3363
	cmpl	$2, %esi
	.p2align 4,,5
	jne	.L3364
	movq	(%rbx), %rdx
	movl	16(%rbx), %eax
	cmpl	8(%rdx), %eax
	jne	.L3364
	call	handle_compress
	testb	%al, %al
	je	.L3364
	movl	16(%rbx), %eax
	testl	%eax, %eax
	.p2align 4,,2
	jne	.L3385
	movq	%rbx, %rdi
	call	isempty_RL
	testb	%al, %al
	je	.L3385
	movl	120(%rbx), %eax
	cmpl	116(%rbx), %eax
	jl	.L3385
	movl	$4, %edx
	movl	$1, 8(%rbx)
	jmp	.L3369
	.p2align 4,,7
.L3365:
	testl	%esi, %esi
	je	.L3398
	cmpl	$1, %esi
	.p2align 4,,3
	jne	.L3373
	movl	8(%rdi), %eax
	movl	$3, 8(%rbx)
	movl	%eax, 16(%rbx)
	jmp	.L3396
.L3366:
	subl	$1, %esi
	jne	.L3364
	movq	(%rbx), %rdx
	movl	16(%rbx), %eax
	cmpl	8(%rdx), %eax
	jne	.L3364
	call	handle_compress
	movl	16(%rbx), %eax
	testl	%eax, %eax
	jne	.L3378
	movq	%rbx, %rdi
	call	isempty_RL
	testb	%al, %al
	je	.L3378
	movl	120(%rbx), %eax
	cmpl	116(%rbx), %eax
	jl	.L3378
	movl	$1, %edx
	movl	$2, 8(%rbx)
	jmp	.L3369
	.p2align 4,,7
.L3373:
	cmpl	$2, %esi
	jne	.L3359
	movl	8(%rdi), %eax
	movl	$4, 8(%rbx)
	movl	%eax, 16(%rbx)
	jmp	.L3396
.L3385:
	movl	$3, %edx
	jmp	.L3369
.L3398:
	call	handle_compress
	testb	%al, %al
	movl	$1, %edx
	.p2align 4,,2
	jne	.L3369
	.p2align 4,,4
	jmp	.L3359
.L3378:
	movl	$2, %edx
	.p2align 4,,5
	jmp	.L3369
.LFE41:
	.size	BZ2_bzCompress, .-BZ2_bzCompress
	.p2align 4,,15
.globl BZ2_bzBuffToBuffCompress
	.type	BZ2_bzBuffToBuffCompress, @function
BZ2_bzBuffToBuffCompress:
.LFB58:
	movq	%rbp, -40(%rsp)
.LCFI160:
	movq	%r12, -32(%rsp)
.LCFI161:
	movq	%rdi, %rbp
	movq	%r13, -24(%rsp)
.LCFI162:
	movq	%r14, -16(%rsp)
.LCFI163:
	movq	%rsi, %r12
	movq	%rbx, -48(%rsp)
.LCFI164:
	movq	%r15, -8(%rsp)
.LCFI165:
	subq	$136, %rsp
.LCFI166:
	testq	%rdi, %rdi
	movq	%rdx, %r13
	movl	%ecx, %r14d
	movl	%r8d, %esi
	movl	%r9d, %edx
	movl	144(%rsp), %ecx
	jne	.L3419
.L3400:
	movl	$-2, %ebx
.L3412:
	movl	%ebx, %eax
	movq	96(%rsp), %rbp
	movq	88(%rsp), %rbx
	movq	104(%rsp), %r12
	movq	112(%rsp), %r13
	movq	120(%rsp), %r14
	movq	128(%rsp), %r15
	addq	$136, %rsp
	ret
	.p2align 4,,7
.L3419:
	testq	%r12, %r12
	je	.L3400
	testq	%r13, %r13
	je	.L3400
	testl	%r8d, %r8d
	.p2align 4,,5
	jle	.L3400
	cmpl	$9, %r8d
	.p2align 4,,5
	jg	.L3400
	testl	%r9d, %r9d
	.p2align 4,,5
	js	.L3400
	cmpl	$4, %r9d
	.p2align 4,,5
	jg	.L3400
	testl	%ecx, %ecx
	.p2align 4,,5
	js	.L3400
	cmpl	$250, %ecx
	.p2align 4,,5
	jg	.L3400
	testl	%ecx, %ecx
	movl	$30, %eax
	movq	%rsp, %rdi
	cmove	%eax, %ecx
	movq	$0, 56(%rsp)
	movq	$0, 64(%rsp)
	movq	$0, 72(%rsp)
	call	BZ2_bzCompressInit
	testl	%eax, %eax
	movl	%eax, %ebx
	jne	.L3412
	movl	(%r12), %eax
	movl	$2, %esi
	movq	%rsp, %rdi
	movq	%rbp, 24(%rsp)
	movq	%r13, (%rsp)
	movl	%r14d, 8(%rsp)
	movl	%eax, 32(%rsp)
	call	BZ2_bzCompress
	cmpl	$3, %eax
	movl	%eax, %ebp
	je	.L3414
	cmpl	$4, %eax
	je	.L3420
.L3416:
	movq	%rsp, %rdi
	movl	%ebp, %ebx
	call	BZ2_bzCompressEnd
	.p2align 4,,2
	jmp	.L3412
.L3414:
	movq	%rsp, %rdi
	movl	$-8, %ebx
	call	BZ2_bzCompressEnd
	.p2align 4,,4
	jmp	.L3412
.L3420:
	movl	32(%rsp), %eax
	subl	%eax, (%r12)
	movq	%rsp, %rdi
	call	BZ2_bzCompressEnd
	jmp	.L3412
.LFE58:
	.size	BZ2_bzBuffToBuffCompress, .-BZ2_bzBuffToBuffCompress
	.p2align 4,,15
.globl BZ2_bzWriteClose64
	.type	BZ2_bzWriteClose64, @function
BZ2_bzWriteClose64:
.LFB53:
	movq	%rbx, -48(%rsp)
.LCFI167:
	movq	%rbp, -40(%rsp)
.LCFI168:
	movl	%edx, %ebx
	movq	%r13, -24(%rsp)
.LCFI169:
	movq	%r14, -16(%rsp)
.LCFI170:
	movq	%rdi, %r13
	movq	%r15, -8(%rsp)
.LCFI171:
	movq	%r12, -32(%rsp)
.LCFI172:
	subq	$88, %rsp
.LCFI173:
	movq	96(%rsp), %rax
	testq	%rsi, %rsi
	movq	%rsi, %rbp
	movq	%rcx, %r14
	movq	%r8, %r15
	movq	%r9, 32(%rsp)
	movq	%rax, 24(%rsp)
	je	.L3474
	cmpb	$0, 5012(%rsi)
	jne	.L3426
	testq	%rdi, %rdi
	je	.L3428
	movl	$-1, (%rdi)
.L3428:
	movl	$-1, 5096(%rbp)
.L3471:
	movq	40(%rsp), %rbx
	movq	48(%rsp), %rbp
	movq	56(%rsp), %r12
	movq	64(%rsp), %r13
	movq	72(%rsp), %r14
	movq	80(%rsp), %r15
	addq	$88, %rsp
	ret
	.p2align 4,,7
.L3426:
	movq	(%rsi), %rdi
	call	ferror
	testl	%eax, %eax
	jne	.L3473
	testq	%r14, %r14
	.p2align 4,,2
	je	.L3434
	movl	$0, (%r14)
.L3434:
	testq	%r15, %r15
	.p2align 4,,3
	je	.L3436
	movl	$0, (%r15)
.L3436:
	cmpq	$0, 32(%rsp)
	je	.L3438
	movq	32(%rsp), %rdx
	movl	$0, (%rdx)
.L3438:
	cmpq	$0, 24(%rsp)
	je	.L3440
	movq	24(%rsp), %rax
	movl	$0, (%rax)
.L3440:
	testl	%ebx, %ebx
	jne	.L3442
	movl	5096(%rbp), %eax
	testl	%eax, %eax
	jne	.L3444
	leaq	8(%rbp), %rdx
	leaq	5016(%rbp), %rax
	movq	%rdx, 8(%rsp)
	movq	%rax, 16(%rsp)
	jmp	.L3445
.L3453:
	.p2align 4,,7
.L3451:
	cmpl	$4, %r12d
	je	.L3444
.L3445:
	movq	8(%rsp), %rdx
	movq	16(%rsp), %rdi
	movl	$2, %esi
	movl	$5000, 5048(%rbp)
	movq	%rdx, 5040(%rbp)
	call	BZ2_bzCompress
	cmpl	$3, %eax
	movl	%eax, %r12d
	je	.L3446
	cmpl	$4, %eax
	jne	.L3475
.L3446:
	movl	5048(%rbp), %eax
	cmpl	$4999, %eax
	ja	.L3451
	movl	$5000, %ebx
	movq	(%rbp), %rcx
	movq	8(%rsp), %rdi
	subl	%eax, %ebx
	movl	$1, %esi
	movslq	%ebx,%rdx
	call	fwrite
	cmpl	%eax, %ebx
	jne	.L3473
	movq	(%rbp), %rdi
	call	ferror
	testl	%eax, %eax
	.p2align 4,,2
	je	.L3451
	.p2align 4,,7
.L3473:
	testq	%r13, %r13
	.p2align 4,,2
	je	.L3459
	movl	$-6, (%r13)
.L3459:
	movl	$-6, 5096(%rbp)
	jmp	.L3471
.L3444:
	movq	(%rbp), %rdi
	call	ferror
	testl	%eax, %eax
	je	.L3476
.L3442:
	testq	%r14, %r14
	.p2align 4,,2
	je	.L3461
	movl	5028(%rbp), %eax
	movl	%eax, (%r14)
.L3461:
	testq	%r15, %r15
	je	.L3463
	movl	5032(%rbp), %eax
	movl	%eax, (%r15)
.L3463:
	cmpq	$0, 32(%rsp)
	je	.L3465
	movl	5052(%rbp), %eax
	movq	32(%rsp), %rdx
	movl	%eax, (%rdx)
.L3465:
	cmpq	$0, 24(%rsp)
	je	.L3467
	movl	5056(%rbp), %eax
	movq	24(%rsp), %rdx
	movl	%eax, (%rdx)
.L3467:
	testq	%r13, %r13
	je	.L3469
	movl	$0, (%r13)
.L3469:
	leaq	5016(%rbp), %rdi
	movl	$0, 5096(%rbp)
	call	BZ2_bzCompressEnd
	movq	%rbp, %rdi
	movq	40(%rsp), %rbx
	movq	48(%rsp), %rbp
	movq	56(%rsp), %r12
	movq	64(%rsp), %r13
	movq	72(%rsp), %r14
	movq	80(%rsp), %r15
	addq	$88, %rsp
	jmp	free
	.p2align 4,,7
.L3474:
	testq	%rdi, %rdi
	je	.L3471
	movl	$0, (%rdi)
	jmp	.L3471
.L3476:
	movq	(%rbp), %rdi
	.p2align 4,,5
	call	fflush
	movq	(%rbp), %rdi
	call	ferror
	testl	%eax, %eax
	je	.L3442
	.p2align 4,,4
	jmp	.L3473
.L3475:
	testq	%r13, %r13
	.p2align 4,,7
	je	.L3449
	movl	%eax, (%r13)
.L3449:
	movl	%r12d, 5096(%rbp)
	.p2align 4,,2
	jmp	.L3471
.LFE53:
	.size	BZ2_bzWriteClose64, .-BZ2_bzWriteClose64
	.p2align 4,,15
.globl BZ2_bzWriteClose
	.type	BZ2_bzWriteClose, @function
BZ2_bzWriteClose:
.LFB52:
	subq	$8, %rsp
.LCFI174:
	movq	%r8, %r9
	xorl	%r8d, %r8d
	movq	$0, (%rsp)
	call	BZ2_bzWriteClose64
	addq	$8, %rsp
	ret
.LFE52:
	.size	BZ2_bzWriteClose, .-BZ2_bzWriteClose
	.p2align 4,,15
.globl BZ2_bzclose
	.type	BZ2_bzclose, @function
BZ2_bzclose:
.LFB67:
	movq	%rbx, -16(%rsp)
.LCFI175:
	movq	%rbp, -8(%rsp)
.LCFI176:
	subq	$40, %rsp
.LCFI177:
	cmpb	$0, 5012(%rdi)
	movq	%rdi, %rbx
	movq	(%rdi), %rbp
	je	.L3480
	leaq	20(%rsp), %rdi
	xorl	%edx, %edx
	xorl	%r8d, %r8d
	xorl	%ecx, %ecx
	movq	%rbx, %rsi
	call	BZ2_bzWriteClose
	movl	20(%rsp), %edx
	testl	%edx, %edx
	jne	.L3488
.L3482:
	cmpq	stdin(%rip), %rbp
	je	.L3487
	cmpq	stdout(%rip), %rbp
	je	.L3487
	movq	%rbp, %rdi
	call	fclose
.L3487:
	movq	24(%rsp), %rbx
	movq	32(%rsp), %rbp
	addq	$40, %rsp
	ret
	.p2align 4,,7
.L3480:
	leaq	20(%rsp), %rdi
	movq	%rbx, %rsi
	call	BZ2_bzReadClose
	jmp	.L3482
	.p2align 4,,7
.L3488:
	xorl	%r8d, %r8d
	xorl	%ecx, %ecx
	movl	$1, %edx
	movq	%rbx, %rsi
	xorl	%edi, %edi
	call	BZ2_bzWriteClose
	jmp	.L3482
.LFE67:
	.size	BZ2_bzclose, .-BZ2_bzclose
	.p2align 4,,15
.globl BZ2_bzWrite
	.type	BZ2_bzWrite, @function
BZ2_bzWrite:
.LFB51:
	movq	%rbx, -48(%rsp)
.LCFI178:
	movq	%rbp, -40(%rsp)
.LCFI179:
	movl	%ecx, %ebx
	movq	%r12, -32(%rsp)
.LCFI180:
	movq	%r13, -24(%rsp)
.LCFI181:
	movq	%rdi, %r12
	movq	%r14, -16(%rsp)
.LCFI182:
	movq	%r15, -8(%rsp)
.LCFI183:
	subq	$56, %rsp
.LCFI184:
	testq	%rdi, %rdi
	movq	%rsi, %rbp
	movq	%rdx, %r13
	je	.L3490
	movl	$0, (%rdi)
.L3490:
	testq	%rbp, %rbp
	je	.L3492
	testq	%r13, %r13
	movl	$0, 5096(%rbp)
	je	.L3492
	testl	%ebx, %ebx
	js	.L3492
	cmpb	$0, 5012(%rbp)
	jne	.L3500
	testq	%r12, %r12
	.p2align 4,,2
	je	.L3502
	movl	$-1, (%r12)
.L3502:
	movl	$-1, 5096(%rbp)
.L3526:
	movq	8(%rsp), %rbx
	movq	16(%rsp), %rbp
	movq	24(%rsp), %r12
	movq	32(%rsp), %r13
	movq	40(%rsp), %r14
	movq	48(%rsp), %r15
	addq	$56, %rsp
	ret
	.p2align 4,,7
.L3492:
	testq	%r12, %r12
	je	.L3496
	movl	$-2, (%r12)
.L3496:
	testq	%rbp, %rbp
	je	.L3526
	movl	$-2, 5096(%rbp)
	jmp	.L3526
	.p2align 4,,7
.L3500:
	movq	(%rbp), %rdi
	call	ferror
	testl	%eax, %eax
	jne	.L3519
	testl	%ebx, %ebx
	.p2align 4,,2
	je	.L3531
	movq	%r13, 5016(%rbp)
	leaq	5016(%rbp), %r14
	leaq	8(%rbp), %r13
	movl	$5000, %r15d
	movl	%ebx, 5024(%rbp)
	jmp	.L3512
	.p2align 4,,7
.L3517:
	movl	5024(%rbp), %ecx
	testl	%ecx, %ecx
	je	.L3531
.L3512:
	xorl	%esi, %esi
	movl	$5000, 5048(%rbp)
	movq	%r13, 5040(%rbp)
	movq	%r14, %rdi
	call	BZ2_bzCompress
	cmpl	$1, %eax
	jne	.L3532
	movl	5048(%rbp), %eax
	cmpl	$4999, %eax
	ja	.L3517
	movl	%r15d, %ebx
	movq	(%rbp), %rcx
	movl	$1, %esi
	subl	%eax, %ebx
	movq	%r13, %rdi
	movslq	%ebx,%rdx
	call	fwrite
	cmpl	%eax, %ebx
	jne	.L3519
	movq	(%rbp), %rdi
	call	ferror
	testl	%eax, %eax
	.p2align 4,,2
	je	.L3517
.L3519:
	testq	%r12, %r12
	.p2align 4,,2
	je	.L3521
	movl	$-6, (%r12)
.L3521:
	movl	$-6, 5096(%rbp)
	jmp	.L3526
.L3531:
	testq	%r12, %r12
	je	.L3524
	movl	$0, (%r12)
.L3524:
	movl	$0, 5096(%rbp)
	jmp	.L3526
.L3532:
	testq	%r12, %r12
	je	.L3515
	movl	%eax, (%r12)
.L3515:
	movl	%eax, 5096(%rbp)
	jmp	.L3526
.LFE51:
	.size	BZ2_bzWrite, .-BZ2_bzWrite
	.section	.rodata.str1.1
.LC93:
	.string	"compress: bad modes\n"
	.section	.rodata.str1.8
	.align 8
.LC94:
	.string	"%s: Input file %s already has %s suffix.\n"
	.align 8
.LC95:
	.string	"%s: I won't write compressed data to a terminal.\n"
	.section	.rodata.str1.1
.LC96:
	.string	"compress: bad srcMode"
.LC97:
	.string	" no data compressed.\n"
	.section	.rodata.str1.8
	.align 8
.LC101:
	.string	"%6.3f:1, %6.3f bits/byte, %5.2f%% saved, %s in, %s out.\n"
	.section	.rodata.str1.1
.LC102:
	.string	"compress:unexpected error"
	.section	.rodata.cst8
	.align 8
.LC98:
	.long	0
	.long	1072693248
	.align 8
.LC99:
	.long	0
	.long	1079574528
	.align 8
.LC100:
	.long	0
	.long	1075838976
	.text
	.p2align 4,,15
	.type	compress, @function
compress:
.LFB104:
	movq	%rbx, -48(%rsp)
.LCFI185:
	movq	%rbp, -40(%rsp)
.LCFI186:
	movq	%rdi, %rbx
	movq	%r12, -32(%rsp)
.LCFI187:
	movq	%r13, -24(%rsp)
.LCFI188:
	movq	%r14, -16(%rsp)
.LCFI189:
	movq	%r15, -8(%rsp)
.LCFI190:
	subq	$5368, %rsp
.LCFI191:
	testq	%rdi, %rdi
	movb	$0, deleteOutputOnInterrupt(%rip)
	je	.L3650
	movl	srcMode(%rip), %eax
	cmpl	$2, %eax
	je	.L3539
	cmpl	$3, %eax
	je	.L3540
	subl	$1, %eax
	je	.L3536
	.p2align 4,,7
.L3538:
	cmpl	$1, srcMode(%rip)
	.p2align 4,,2
	je	.L3541
	movl	$inName, %edi
	call	containsDubiousChars
	testb	%al, %al
	je	.L3543
	cmpb	$0, noisy(%rip)
	jne	.L3651
.L3648:
	movl	$1, %edi
	call	setExit
.L3628:
	movq	5320(%rsp), %rbx
	movq	5328(%rsp), %rbp
	movq	5336(%rsp), %r12
	movq	5344(%rsp), %r13
	movq	5352(%rsp), %r14
	movq	5360(%rsp), %r15
	addq	$5368, %rsp
	ret
.L3543:
	movl	$inName, %edi
	call	fileExists
	testb	%al, %al
	je	.L3649
	.p2align 4,,7
.L3541:
	xorl	%ebx, %ebx
	.p2align 4,,7
.L3549:
	movq	zSuffix(,%rbx,8), %r12
	movl	$inName, %edi
	movq	%r12, %rsi
	call	hasSuffix
	testb	%al, %al
	jne	.L3652
	addq	$1, %rbx
	cmpq	$4, %rbx
	jne	.L3549
	movl	srcMode(%rip), %eax
	cmpl	$3, %eax
	je	.L3555
	cmpl	$2, %eax
	je	.L3555
.L3635:
	cmpl	$3, %eax
	je	.L3582
	subl	$1, %eax
	.p2align 4,,5
	je	.L3653
	movl	$.LC96, %edi
	call	panic
.L3650:
	cmpl	$1, srcMode(%rip)
	jne	.L3654
.L3536:
	movl	$.LC37, %esi
.L3639:
	movl	$inName, %edi
	call	copyFileName
	movl	$.LC58, %esi
	movl	$outName, %edi
	call	copyFileName
	jmp	.L3538
	.p2align 4,,7
.L3539:
	movq	%rdi, %rsi
	jmp	.L3639
.L3540:
	movq	%rdi, %rsi
	movl	$inName, %edi
	call	copyFileName
	movq	%rbx, %rsi
	movl	$outName, %edi
	call	copyFileName
	movl	$outName, %edi
	call	strlen
	movl	$846881326, outName(%rax)
	movb	$0, outName+4(%rax)
	jmp	.L3538
.L3555:
	leaq	5056(%rsp), %rdx
	movl	$inName, %esi
	movl	$1, %edi
	call	__xstat
	movl	5080(%rsp), %eax
	andl	$61440, %eax
	cmpl	$16384, %eax
	je	.L3655
	cmpl	$3, srcMode(%rip)
	je	.L3656
.L3632:
	movl	srcMode(%rip), %eax
.L3557:
	cmpl	$2, %eax
	jne	.L3635
	movl	$.LC31, %esi
	movl	$inName, %edi
	call	fopen
	movq	stdout(%rip), %r13
	movq	%rax, %r12
	movq	%r13, %rdi
	call	fileno
	movl	%eax, %edi
	call	isatty
	testl	%eax, %eax
	jne	.L3657
	testq	%r12, %r12
	.p2align 4,,2
	jne	.L3583
.L3649:
	.p2align 4,,7
	call	__errno_location
	movl	(%rax), %edi
	call	strerror
	movq	progName(%rip), %rdx
	movq	stderr(%rip), %rdi
	movq	%rax, %r8
	movl	$inName, %ecx
	movl	$.LC59, %esi
	xorl	%eax, %eax
	call	fprintf
	jmp	.L3648
.L3582:
	movl	$.LC31, %esi
	movl	$inName, %edi
	call	fopen
	movl	$.LC66, %esi
	movl	$outName, %edi
	movq	%rax, %r12
	call	fopen_output_safely
	testq	%rax, %rax
	movq	%rax, %r13
	je	.L3658
	testq	%r12, %r12
	je	.L3659
.L3583:
	movl	verbosity(%rip), %ebx
	testl	%ebx, %ebx
	jle	.L3595
	movq	stderr(%rip), %rdi
	movl	$inName, %edx
	movl	$.LC45, %esi
	xorl	%eax, %eax
	call	fprintf
	movl	$inName, %edi
	call	pad
	movq	stderr(%rip), %rdi
	call	fflush
.L3595:
	movq	%r12, %rdi
	movq	%r13, outputHandleJustInCase(%rip)
	movb	$1, deleteOutputOnInterrupt(%rip)
	call	ferror
	testl	%eax, %eax
	jne	.L3645
	movq	%r13, %rdi
	call	ferror
	testl	%eax, %eax
	.p2align 4,,2
	je	.L3660
.L3645:
	call	ioError
.L3651:
	movq	progName(%rip), %rdx
	movq	stderr(%rip), %rdi
	movl	$inName, %ecx
	movl	$.LC38, %esi
	xorl	%eax, %eax
	call	fprintf
	jmp	.L3648
.L3652:
	cmpb	$0, noisy(%rip)
	je	.L3648
	movq	progName(%rip), %rdx
	movq	stderr(%rip), %rdi
	movq	%r12, %r8
	movl	$inName, %ecx
	movl	$.LC94, %esi
	xorl	%eax, %eax
	call	fprintf
	jmp	.L3648
.L3653:
	movq	stdout(%rip), %r13
	movq	stdin(%rip), %r12
	movq	%r13, %rdi
	call	fileno
	movl	%eax, %edi
	call	isatty
	testl	%eax, %eax
	je	.L3583
	movq	progName(%rip), %rdx
	movq	stderr(%rip), %rdi
	movl	$.LC95, %esi
	xorl	%eax, %eax
	call	fprintf
	movq	progName(%rip), %rdx
	movl	$.LC42, %esi
	movq	%rdx, %rcx
.L3646:
	movq	stderr(%rip), %rdi
	xorl	%eax, %eax
	call	fprintf
	jmp	.L3648
.L3660:
	leaq	5292(%rsp), %r15
	movl	workFactor(%rip), %r8d
	movl	verbosity(%rip), %ecx
	movl	blockSize100k(%rip), %edx
	movq	%r13, %rsi
	movq	%r15, %rdi
	call	BZ2_bzWriteOpen
	movl	5292(%rsp), %r11d
	movq	%rax, %r14
	testl	%r11d, %r11d
	jne	.L3634
	cmpl	$1, verbosity(%rip)
	jle	.L3638
	movq	stderr(%rip), %rsi
	movl	$10, %edi
	call	fputc
	.p2align 4,,7
.L3638:
	movq	%r12, %rdi
	call	myfeof
	testb	%al, %al
	jne	.L3604
	leaq	48(%rsp), %rbp
	movq	%r12, %rcx
	movl	$5000, %edx
	movl	$1, %esi
	movq	%rbp, %rdi
	call	fread
	movq	%r12, %rdi
	movq	%rax, %rbx
	call	ferror
	testl	%eax, %eax
	jne	.L3645
	testl	%ebx, %ebx
	jle	.L3607
	movl	%ebx, %ecx
	movq	%rbp, %rdx
	movq	%r14, %rsi
	movq	%r15, %rdi
	call	BZ2_bzWrite
.L3607:
	movl	5292(%rsp), %r10d
	testl	%r10d, %r10d
	je	.L3638
.L3634:
	leaq	5300(%rsp), %rbp
	leaq	5304(%rsp), %r10
	leaq	5308(%rsp), %r11
	leaq	5296(%rsp), %rbx
.L3600:
	leaq	5288(%rsp), %rdi
	movq	%rbp, %r9
	movq	%r10, %r8
	movq	%r11, %rcx
	movl	$1, %edx
	movq	%r14, %rsi
	movq	%rbx, (%rsp)
	call	BZ2_bzWriteClose64
	movl	5292(%rsp), %eax
	cmpl	$-6, %eax
	je	.L3645
	cmpl	$-3, %eax
	je	.L3623
	cmpl	$-9, %eax
	je	.L3661
.L3597:
	movl	$.LC102, %edi
	call	panic
.L3654:
	movl	$.LC93, %edi
	call	panic
	.p2align 4,,7
.L3656:
	cmpb	$0, forceOverwrite(%rip)
	je	.L3662
.L3561:
	movl	$outName, %edi
	call	fileExists
	testb	%al, %al
	je	.L3567
	cmpb	$0, forceOverwrite(%rip)
	je	.L3569
	movl	$outName, %edi
	call	remove
.L3567:
	cmpl	$3, srcMode(%rip)
	jne	.L3632
	cmpb	$0, forceOverwrite(%rip)
	je	.L3663
.L3572:
	movl	$inName, %edi
	call	saveInputFileMetaInfo
	movl	srcMode(%rip), %eax
	jmp	.L3557
.L3655:
	movq	progName(%rip), %rdx
	movl	$inName, %ecx
	movl	$.LC40, %esi
	jmp	.L3646
.L3662:
	movl	$inName, %edi
	call	notAStandardFile
	testb	%al, %al
	je	.L3563
	cmpb	$0, noisy(%rip)
	je	.L3648
	movq	progName(%rip), %rdx
	movq	stderr(%rip), %rdi
	movl	$inName, %ecx
	movl	$.LC60, %esi
	xorl	%eax, %eax
	call	fprintf
	jmp	.L3648
.L3658:
	call	__errno_location
	movl	(%rax), %edi
	.p2align 4,,6
	call	strerror
	movq	progName(%rip), %rdx
	movq	stderr(%rip), %rdi
	movq	%rax, %r8
	movl	$outName, %ecx
	xorl	%eax, %eax
	movl	$.LC67, %esi
	call	fprintf
	testq	%r12, %r12
	je	.L3648
.L3647:
	movq	%r12, %rdi
	call	fclose
	.p2align 4,,4
	jmp	.L3648
.L3659:
	.p2align 4,,6
	call	__errno_location
	movl	(%rax), %edi
	.p2align 4,,6
	call	strerror
	movq	progName(%rip), %rdx
	movq	stderr(%rip), %rdi
	movq	%rax, %r8
	movl	$inName, %ecx
	movl	$.LC59, %esi
	xorl	%eax, %eax
	call	fprintf
	movq	%r13, %rdi
	call	fclose
	jmp	.L3648
.L3569:
	movq	progName(%rip), %rdx
	movl	$outName, %ecx
	movl	$.LC62, %esi
	jmp	.L3646
.L3623:
	call	outOfMemory
.L3563:
	cmpl	$3, srcMode(%rip)
	jne	.L3632
	.p2align 4,,3
	jmp	.L3561
.L3604:
	leaq	5304(%rsp), %r10
	leaq	5308(%rsp), %r11
	leaq	5300(%rsp), %rbp
	leaq	5296(%rsp), %rbx
	xorl	%edx, %edx
	movq	%r14, %rsi
	movq	%rbp, %r9
	movq	%r10, %r8
	movq	%r11, %rcx
	movq	%r15, %rdi
	movq	%r10, 24(%rsp)
	movq	%r11, 16(%rsp)
	movq	%rbx, (%rsp)
	call	BZ2_bzWriteClose64
	movl	5292(%rsp), %r9d
	movq	24(%rsp), %r10
	movq	16(%rsp), %r11
	testl	%r9d, %r9d
	jne	.L3600
	movq	%r13, %rdi
	call	ferror
	testl	%eax, %eax
	jne	.L3645
	movq	%r13, %rdi
	call	fflush
	addl	$1, %eax
	.p2align 4,,2
	je	.L3645
	cmpq	stdout(%rip), %r13
	je	.L3612
	movq	%r13, %rdi
	call	fclose
	addl	$1, %eax
	movq	$0, outputHandleJustInCase(%rip)
	je	.L3645
.L3612:
	movq	%r12, %rdi
	movq	$0, outputHandleJustInCase(%rip)
	call	ferror
	testl	%eax, %eax
	jne	.L3645
	movq	%r12, %rdi
	call	fclose
	addl	$1, %eax
	.p2align 4,,2
	je	.L3645
	movl	verbosity(%rip), %r8d
	testl	%r8d, %r8d
	jle	.L3616
	movl	5308(%rsp), %esi
	testl	%esi, %esi
	jne	.L3618
	movl	5304(%rsp), %edi
	testl	%edi, %edi
	je	.L3664
.L3618:
	leaq	5280(%rsp), %rbx
	movl	5304(%rsp), %edx
	leaq	5264(%rsp), %r12
	leaq	5232(%rsp), %r13
	movq	%rbx, %rdi
	call	uInt64_from_UInt32s
	movl	5296(%rsp), %edx
	movl	5300(%rsp), %esi
	movq	%r12, %rdi
	call	uInt64_from_UInt32s
	movq	%rbx, %rdi
	call	uInt64_to_double
	movq	%r12, %rdi
	movsd	%xmm0, 40(%rsp)
	call	uInt64_to_double
	movq	%rbx, %rsi
	leaq	5200(%rsp), %rbx
	movq	%r13, %rdi
	movsd	%xmm0, 32(%rsp)
	call	uInt64_toAscii
	movq	%r12, %rsi
	movq	%rbx, %rdi
	call	uInt64_toAscii
	movsd	32(%rsp), %xmm0
	movq	stderr(%rip), %rdi
	movsd	32(%rsp), %xmm1
	movq	%rbx, %rcx
	divsd	40(%rsp), %xmm0
	movsd	.LC98(%rip), %xmm2
	mulsd	.LC100(%rip), %xmm1
	movq	%r13, %rdx
	movl	$.LC101, %esi
	movl	$3, %eax
	subsd	%xmm0, %xmm2
	movsd	40(%rsp), %xmm0
	divsd	40(%rsp), %xmm1
	divsd	32(%rsp), %xmm0
	mulsd	.LC99(%rip), %xmm2
	call	fprintf
.L3616:
	cmpl	$3, srcMode(%rip)
	movq	$0, outputHandleJustInCase(%rip)
	je	.L3665
.L3624:
	movb	$0, deleteOutputOnInterrupt(%rip)
	jmp	.L3628
	.p2align 4,,7
.L3661:
	call	configError
.L3663:
	movl	$inName, %edi
	call	countHardLinks
	testl	%eax, %eax
	movl	%eax, %edx
	jle	.L3574
	movl	%edx, %r8d
	movq	stderr(%rip), %rdi
	movq	progName(%rip), %rdx
	cmpl	$1, %eax
	movl	$.LC64, %r9d
	movl	$.LC63, %eax
	cmove	%rax, %r9
	movl	$inName, %ecx
	movl	$.LC65, %esi
	xorl	%eax, %eax
	call	fprintf
	movl	$1, %edi
	call	setExit
	jmp	.L3628
.L3574:
	cmpl	$3, srcMode(%rip)
	jne	.L3632
	.p2align 4,,6
	jmp	.L3572
.L3665:
	movl	$outName, %edi
	call	applySavedMetaInfoToOutputFile
	cmpb	$0, keepInputFiles(%rip)
	movb	$0, deleteOutputOnInterrupt(%rip)
	jne	.L3624
	movl	$inName, %edi
	call	remove
	testl	%eax, %eax
	je	.L3624
	jmp	.L3645
.L3664:
	movq	stderr(%rip), %rcx
	movl	$21, %edx
	movl	$1, %esi
	movl	$.LC97, %edi
	call	fwrite
	jmp	.L3616
.L3657:
	movq	progName(%rip), %rdx
	movq	stderr(%rip), %rdi
	movl	$.LC95, %esi
	xorl	%eax, %eax
	call	fprintf
	movq	progName(%rip), %rdx
	movq	stderr(%rip), %rdi
	xorl	%eax, %eax
	movl	$.LC42, %esi
	movq	%rdx, %rcx
	call	fprintf
	testq	%r12, %r12
	jne	.L3647
	jmp	.L3648
.LFE104:
	.size	compress, .-compress
	.section	.rodata.str1.1
.LC103:
	.string	"BZIP2"
.LC104:
	.string	"BZIP"
.LC105:
	.string	"--"
.LC106:
	.string	"unzip"
.LC107:
	.string	"UNZIP"
.LC108:
	.string	"z2cat"
.LC109:
	.string	"Z2CAT"
.LC110:
	.string	"zcat"
.LC111:
	.string	"ZCAT"
.LC112:
	.string	"%s: Bad flag `%s'\n"
.LC113:
	.string	"--stdout"
.LC114:
	.string	"--decompress"
.LC115:
	.string	"--compress"
.LC116:
	.string	"--force"
.LC117:
	.string	"--test"
.LC118:
	.string	"--keep"
.LC119:
	.string	"--small"
.LC120:
	.string	"--quiet"
.LC121:
	.string	"--version"
.LC122:
	.string	"--license"
.LC123:
	.string	"--exponential"
.LC124:
	.string	"--repetitive-best"
.LC125:
	.string	"--repetitive-fast"
.LC126:
	.string	"--fast"
.LC127:
	.string	"--best"
.LC128:
	.string	"--verbose"
.LC129:
	.string	"--help"
	.section	.rodata.str1.8
	.align 8
.LC130:
	.string	"%s: -c and -t cannot be used together.\n"
	.align 8
.LC131:
	.string	"\nYou can use the `bzip2recover' program to attempt to recover\ndata from undamaged sections of corrupted files.\n\n"
	.text
	.p2align 4,,15
.globl main
	.type	main, @function
main:
.LFB114:
	pushq	%r13
.LCFI192:
	pushq	%r12
.LCFI193:
	movq	%rsi, %r12
	movl	$mySIGSEGVorSIGBUScatcher, %esi
	pushq	%rbp
.LCFI194:
	movl	%edi, %ebp
	movl	$11, %edi
	pushq	%rbx
.LCFI195:
	subq	$24, %rsp
.LCFI196:
	movq	$0, outputHandleJustInCase(%rip)
	movb	$0, smallMode(%rip)
	movb	$0, keepInputFiles(%rip)
	movb	$0, forceOverwrite(%rip)
	movb	$1, noisy(%rip)
	movl	$0, verbosity(%rip)
	movl	$9, blockSize100k(%rip)
	movb	$0, testFailsExist(%rip)
	movb	$0, unzFailsExist(%rip)
	movl	$0, numFileNames(%rip)
	movl	$0, numFilesProcessed(%rip)
	movl	$30, workFactor(%rip)
	movb	$0, deleteOutputOnInterrupt(%rip)
	movl	$0, exitValue(%rip)
	call	signal
	movl	$mySIGSEGVorSIGBUScatcher, %esi
	movl	$7, %edi
	call	signal
	movl	$.LC36, %esi
	movl	$inName, %edi
	call	copyFileName
	movl	$.LC36, %esi
	movl	$outName, %edi
	call	copyFileName
	movq	(%r12), %rsi
	movl	$progNameReally, %edi
	call	copyFileName
	movzbl	progNameReally(%rip), %eax
	movq	$progNameReally, progName(%rip)
	testb	%al, %al
	je	.L3667
	movl	$progNameReally, %edx
	movq	%rdx, %rcx
	jmp	.L3669
	.p2align 4,,7
.L3850:
	addq	$1, %rdx
	movzbl	(%rdx), %eax
	testb	%al, %al
	je	.L3849
.L3669:
	cmpb	$47, %al
	jne	.L3850
	addq	$1, %rdx
	movzbl	(%rdx), %eax
	movq	%rdx, %rcx
	testb	%al, %al
	jne	.L3669
.L3849:
	movq	%rcx, progName(%rip)
.L3667:
	leaq	16(%rsp), %rbx
	movl	$.LC103, %esi
	movq	$0, 16(%rsp)
	subl	$1, %ebp
	movq	%rbx, %rdi
	call	addFlagsFromEnvVar
	movl	$.LC104, %esi
	movq	%rbx, %rdi
	call	addFlagsFromEnvVar
	testl	%ebp, %ebp
	jle	.L3673
	xorl	%ebx, %ebx
	.p2align 4,,7
.L3675:
	movq	8(%r12,%rbx,8), %rsi
	movq	16(%rsp), %rdi
	addq	$1, %rbx
	call	snocString
	movq	%rax, 16(%rsp)
	leal	1(%rbx), %eax
	cmpl	%eax, %ebp
	jge	.L3675
.L3673:
	movq	16(%rsp), %rbp
	xorl	%r12d, %r12d
	movl	$7, longestFileName(%rip)
	movl	$0, numFileNames(%rip)
	testq	%rbp, %rbp
	je	.L3678
	movq	%rbp, %rbx
	movl	$1, %r12d
	movl	$.LC105, %r13d
	jmp	.L3679
	.p2align 4,,7
.L3852:
	xorl	%r12d, %r12d
.L3682:
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	je	.L3851
.L3679:
	movq	(%rbx), %r8
	movl	$3, %eax
	movq	%r13, %rdi
	cld
	movq	%rax, %rcx
	movq	%r8, %rsi
	repz
	cmpsb
	je	.L3852
	cmpb	$45, (%r8)
	jne	.L3683
	testb	%r12b, %r12b
	jne	.L3682
	.p2align 4,,7
.L3683:
	addl	$1, numFileNames(%rip)
	movq	%r8, %rdi
	call	strlen
	cmpl	%eax, longestFileName(%rip)
	jge	.L3682
	movq	8(%rbx), %rbx
	movl	%eax, longestFileName(%rip)
	testq	%rbx, %rbx
	jne	.L3679
.L3851:
	movl	numFileNames(%rip), %r12d
	testl	%r12d, %r12d
	je	.L3678
	movl	$3, srcMode(%rip)
.L3688:
	movq	progName(%rip), %rbx
	movl	$1, opMode(%rip)
	movl	$.LC106, %esi
	movq	%rbx, %rdi
	call	strstr
	testq	%rax, %rax
	je	.L3853
.L3843:
	movl	$2, opMode(%rip)
.L3691:
	movl	$.LC108, %esi
	movq	%rbx, %rdi
	call	strstr
	testq	%rax, %rax
	je	.L3854
.L3693:
	cmpl	$1, %r12d
	movl	$2, opMode(%rip)
	sbbl	%eax, %eax
	addl	$2, %eax
	movl	%eax, srcMode(%rip)
.L3697:
	testq	%rbp, %rbp
	je	.L3700
	movl	$.LC105, %r13d
	.p2align 4,,7
.L3701:
	movq	(%rbp), %r8
	movl	$3, %ecx
	movq	%r13, %rdi
	cld
	movq	%r8, %rsi
	repz
	cmpsb
	je	.L3702
	cmpb	$45, (%r8)
	je	.L3855
.L3704:
	movq	8(%rbp), %rbp
	testq	%rbp, %rbp
	jne	.L3701
.L3702:
	movq	16(%rsp), %rbx
	testq	%rbx, %rbx
	jne	.L3829
	.p2align 4,,2
	jmp	.L3700
	.p2align 4,,7
.L3856:
	movl	$2, srcMode(%rip)
.L3736:
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	je	.L3700
.L3829:
	movq	(%rbx), %r8
	movl	$3, %ecx
	movq	%r13, %rdi
	cld
	movq	%r8, %rsi
	repz
	cmpsb
	je	.L3700
	movl	$.LC113, %edi
	movl	$9, %ecx
	movq	%r8, %rsi
	repz
	cmpsb
	je	.L3856
	cld
	movl	$.LC114, %edi
	movl	$13, %ecx
	movq	%r8, %rsi
	repz
	cmpsb
	jne	.L3737
	movl	$2, opMode(%rip)
	jmp	.L3736
	.p2align 4,,7
.L3855:
	movzbl	1(%r8), %eax
	cmpb	$45, %al
	je	.L3704
	testb	%al, %al
	je	.L3704
	movl	$2, %ebx
.L3708:
	subl	$49, %eax
	cmpb	$73, %al
	ja	.L3847
	movzbl	%al, %eax
	jmp	*.L3730(,%rax,8)
	.section	.rodata
	.align 8
	.align 4
.L3730:
	.quad	.L3710
	.quad	.L3711
	.quad	.L3712
	.quad	.L3713
	.quad	.L3714
	.quad	.L3715
	.quad	.L3716
	.quad	.L3717
	.quad	.L3718
	.quad	.L3847
	.quad	.L3847
	.quad	.L3847
	.quad	.L3847
	.quad	.L3847
	.quad	.L3847
	.quad	.L3847
	.quad	.L3847
	.quad	.L3847
	.quad	.L3847
	.quad	.L3847
	.quad	.L3847
	.quad	.L3847
	.quad	.L3847
	.quad	.L3847
	.quad	.L3847
	.quad	.L3847
	.quad	.L3847
	.quad	.L3719
	.quad	.L3847
	.quad	.L3847
	.quad	.L3847
	.quad	.L3847
	.quad	.L3847
	.quad	.L3847
	.quad	.L3847
	.quad	.L3847
	.quad	.L3847
	.quad	.L3719
	.quad	.L3847
	.quad	.L3847
	.quad	.L3847
	.quad	.L3847
	.quad	.L3847
	.quad	.L3847
	.quad	.L3847
	.quad	.L3847
	.quad	.L3847
	.quad	.L3847
	.quad	.L3847
	.quad	.L3847
	.quad	.L3720
	.quad	.L3721
	.quad	.L3847
	.quad	.L3722
	.quad	.L3847
	.quad	.L3846
	.quad	.L3847
	.quad	.L3847
	.quad	.L3724
	.quad	.L3847
	.quad	.L3847
	.quad	.L3847
	.quad	.L3847
	.quad	.L3847
	.quad	.L3725
	.quad	.L3847
	.quad	.L3726
	.quad	.L3727
	.quad	.L3847
	.quad	.L3728
	.quad	.L3847
	.quad	.L3847
	.quad	.L3847
	.quad	.L3729
	.text
.L3765:
	cld
	movl	$.LC129, %edi
	movl	$7, %ecx
	movq	%r8, %rsi
	repz
	cmpsb
	je	.L3846
	cld
	movl	$2, %ecx
	movq	%r8, %rsi
	movq	%r13, %rdi
	repz
	cmpsb
	jne	.L3736
.L3847:
	movq	progName(%rip), %rdx
	movq	stderr(%rip), %rdi
	movq	%r8, %rcx
	movl	$.LC112, %esi
	xorl	%eax, %eax
	call	fprintf
	movq	progName(%rip), %rdi
	call	usage
	movl	$1, %edi
	call	exit
.L3678:
	movl	$1, srcMode(%rip)
	jmp	.L3688
.L3700:
	cmpl	$4, verbosity(%rip)
	jg	.L3857
	movl	opMode(%rip), %eax
	cmpl	$1, %eax
	je	.L3858
.L3772:
	cmpl	$3, %eax
	je	.L3859
.L3774:
	cmpl	$2, srcMode(%rip)
	je	.L3860
.L3780:
	cmpl	$1, %eax
	.p2align 4,,2
	je	.L3783
.L3778:
	movl	$0, blockSize100k(%rip)
.L3783:
	cmpl	$3, srcMode(%rip)
	je	.L3861
.L3784:
	cmpl	$1, %eax
	je	.L3862
	cmpl	$2, %eax
	je	.L3863
	cmpl	$1, srcMode(%rip)
	movb	$0, testFailsExist(%rip)
	je	.L3864
	movq	16(%rsp), %rbx
	testq	%rbx, %rbx
	je	.L3791
	movl	$1, %ebp
	movl	$.LC105, %r13d
	jmp	.L3816
.L3865:
	xorl	%ebp, %ebp
.L3819:
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	je	.L3814
.L3816:
	movq	(%rbx), %r8
	movl	$3, %eax
	movq	%r13, %rdi
	cld
	movq	%rax, %rcx
	movq	%r8, %rsi
	repz
	cmpsb
	je	.L3865
	cmpb	$45, (%r8)
	jne	.L3820
	testb	%bpl, %bpl
	jne	.L3819
.L3820:
	addl	$1, numFilesProcessed(%rip)
	movq	%r8, %rdi
	call	testf
	jmp	.L3819
.L3857:
	movl	opMode(%rip), %eax
	movl	$4, verbosity(%rip)
	cmpl	$1, %eax
	jne	.L3772
.L3858:
	cmpb	$0, smallMode(%rip)
	je	.L3774
	cmpl	$2, blockSize100k(%rip)
	jle	.L3774
	movl	$2, blockSize100k(%rip)
	jmp	.L3774
	.p2align 4,,7
.L3737:
	cld
	movl	$.LC115, %edi
	movl	$11, %ecx
	movq	%r8, %rsi
	repz
	cmpsb
	jne	.L3739
	movl	$1, opMode(%rip)
	jmp	.L3736
.L3864:
	xorl	%edi, %edi
	call	testf
.L3814:
	cmpb	$0, testFailsExist(%rip)
	je	.L3790
	cmpb	$0, noisy(%rip)
	jne	.L3866
.L3790:
	movq	16(%rsp), %rbx
	testq	%rbx, %rbx
	je	.L3791
	.p2align 4,,7
.L3830:
	movq	(%rbx), %rdi
	movq	8(%rbx), %rbp
	testq	%rdi, %rdi
	je	.L3825
	call	free
.L3825:
	movq	%rbx, %rdi
	movq	%rbp, %rbx
	call	free
	testq	%rbp, %rbp
	jne	.L3830
.L3791:
	movl	exitValue(%rip), %eax
	addq	$24, %rsp
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	ret
.L3739:
	cld
	movl	$.LC116, %edi
	movl	$8, %ecx
	movq	%r8, %rsi
	repz
	cmpsb
	jne	.L3741
	movb	$1, forceOverwrite(%rip)
	jmp	.L3736
.L3853:
	movl	$.LC107, %esi
	movq	%rbx, %rdi
	call	strstr
	testq	%rax, %rax
	jne	.L3843
	jmp	.L3691
.L3854:
	movl	$.LC109, %esi
	movq	%rbx, %rdi
	call	strstr
	testq	%rax, %rax
	jne	.L3693
	movl	$.LC110, %esi
	movq	%rbx, %rdi
	call	strstr
	testq	%rax, %rax
	jne	.L3693
	movl	$.LC111, %esi
	movq	%rbx, %rdi
	call	strstr
	testq	%rax, %rax
	jne	.L3693
	jmp	.L3697
	.p2align 4,,7
.L3862:
	cmpl	$1, srcMode(%rip)
	.p2align 4,,4
	je	.L3867
	movq	16(%rsp), %rbx
	testq	%rbx, %rbx
	je	.L3791
	movl	$1, %ebp
	movl	$.LC105, %r13d
	jmp	.L3793
.L3868:
	xorl	%ebp, %ebp
.L3796:
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	je	.L3790
.L3793:
	movq	(%rbx), %r8
	movl	$3, %eax
	movq	%r13, %rdi
	cld
	movq	%rax, %rcx
	movq	%r8, %rsi
	repz
	cmpsb
	je	.L3868
	cmpb	$45, (%r8)
	jne	.L3797
	testb	%bpl, %bpl
	jne	.L3796
.L3797:
	addl	$1, numFilesProcessed(%rip)
	movq	%r8, %rdi
	call	compress
	jmp	.L3796
.L3710:
	movl	$1, blockSize100k(%rip)
.L3731:
	movzbl	(%rbx,%r8), %eax
	addq	$1, %rbx
	testb	%al, %al
	jne	.L3708
	jmp	.L3704
.L3711:
	movl	$2, blockSize100k(%rip)
	jmp	.L3731
.L3712:
	movl	$3, blockSize100k(%rip)
	jmp	.L3731
.L3713:
	movl	$4, blockSize100k(%rip)
	jmp	.L3731
.L3714:
	movl	$5, blockSize100k(%rip)
	jmp	.L3731
.L3715:
	movl	$6, blockSize100k(%rip)
	jmp	.L3731
.L3716:
	movl	$7, blockSize100k(%rip)
	jmp	.L3731
.L3717:
	movl	$8, blockSize100k(%rip)
	jmp	.L3731
.L3718:
	movl	$9, blockSize100k(%rip)
	jmp	.L3731
.L3719:
	call	license
	movq	(%rbp), %r8
	jmp	.L3731
.L3720:
	movl	$2, srcMode(%rip)
	jmp	.L3731
.L3721:
	movl	$2, opMode(%rip)
	jmp	.L3731
.L3722:
	movb	$1, forceOverwrite(%rip)
	movq	(%rbp), %r8
	jmp	.L3731
.L3846:
	movq	progName(%rip), %rdi
	call	usage
	xorl	%edi, %edi
	call	exit
.L3724:
	movb	$1, keepInputFiles(%rip)
	movq	(%rbp), %r8
	jmp	.L3731
.L3725:
	movb	$0, noisy(%rip)
	movq	(%rbp), %r8
	jmp	.L3731
.L3726:
	movb	$1, smallMode(%rip)
	movq	(%rbp), %r8
	jmp	.L3731
.L3727:
	movl	$3, opMode(%rip)
	jmp	.L3731
.L3728:
	addl	$1, verbosity(%rip)
	jmp	.L3731
.L3729:
	movl	$1, opMode(%rip)
	jmp	.L3731
.L3860:
	movl	numFileNames(%rip), %ebp
	testl	%ebp, %ebp
	jne	.L3780
	movl	$1, srcMode(%rip)
	jmp	.L3780
.L3859:
	cmpl	$2, srcMode(%rip)
	jne	.L3778
	movq	stderr(%rip), %rdi
	movq	progName(%rip), %rdx
	movl	$.LC130, %esi
	xorl	%eax, %eax
	call	fprintf
	movl	$1, %edi
	call	exit
	.p2align 4,,7
.L3863:
	cmpl	$1, srcMode(%rip)
	movb	$0, unzFailsExist(%rip)
	je	.L3869
	movq	16(%rsp), %rbx
	testq	%rbx, %rbx
	je	.L3791
	movl	$1, %ebp
	movl	$.LC105, %r13d
	jmp	.L3805
.L3870:
	xorl	%ebp, %ebp
.L3808:
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	je	.L3803
.L3805:
	movq	(%rbx), %r8
	movl	$3, %eax
	movq	%r13, %rdi
	cld
	movq	%rax, %rcx
	movq	%r8, %rsi
	repz
	cmpsb
	je	.L3870
	cmpb	$45, (%r8)
	jne	.L3809
	testb	%bpl, %bpl
	jne	.L3808
.L3809:
	addl	$1, numFilesProcessed(%rip)
	movq	%r8, %rdi
	call	uncompress
	jmp	.L3808
.L3869:
	xorl	%edi, %edi
	call	uncompress
.L3803:
	cmpb	$0, unzFailsExist(%rip)
	je	.L3790
.L3848:
	movl	$2, %edi
	call	setExit
	movl	exitValue(%rip), %edi
	call	exit
	.p2align 4,,7
.L3861:
	movl	$mySignalCatcher, %esi
	movl	$2, %edi
	call	signal
	movl	$mySignalCatcher, %esi
	movl	$15, %edi
	call	signal
	movl	$mySignalCatcher, %esi
	movl	$1, %edi
	call	signal
	movl	opMode(%rip), %eax
	jmp	.L3784
.L3741:
	cld
	movl	$.LC117, %edi
	movl	$7, %ecx
	movq	%r8, %rsi
	repz
	cmpsb
	jne	.L3743
	movl	$3, opMode(%rip)
	jmp	.L3736
.L3867:
	xorl	%edi, %edi
	call	compress
	jmp	.L3790
.L3743:
	cld
	movl	$.LC118, %edi
	movl	$7, %ecx
	movq	%r8, %rsi
	repz
	cmpsb
	jne	.L3745
	movb	$1, keepInputFiles(%rip)
	jmp	.L3736
.L3745:
	cld
	movl	$.LC119, %edi
	movl	$8, %ecx
	movq	%r8, %rsi
	repz
	cmpsb
	jne	.L3747
	movb	$1, smallMode(%rip)
	jmp	.L3736
.L3866:
	movq	stderr(%rip), %rcx
	movl	$112, %edx
	movl	$1, %esi
	movl	$.LC131, %edi
	call	fwrite
	jmp	.L3848
.L3747:
	cld
	movl	$.LC120, %edi
	movl	$8, %ecx
	movq	%r8, %rsi
	repz
	cmpsb
	jne	.L3749
	movb	$0, noisy(%rip)
	jmp	.L3736
.L3749:
	cld
	movl	$.LC121, %edi
	movl	$10, %ecx
	movq	%r8, %rsi
	repz
	cmpsb
	je	.L3844
	cld
	movl	$.LC122, %edi
	movl	$10, %ecx
	movq	%r8, %rsi
	repz
	cmpsb
	jne	.L3753
.L3844:
	call	license
	jmp	.L3736
.L3753:
	cld
	movl	$.LC123, %edi
	movl	$14, %ecx
	movq	%r8, %rsi
	repz
	cmpsb
	jne	.L3755
	movl	$1, workFactor(%rip)
	jmp	.L3736
.L3755:
	cld
	movl	$.LC124, %edi
	movl	$18, %ecx
	movq	%r8, %rsi
	repz
	cmpsb
	je	.L3845
	cld
	movl	$.LC125, %edi
	movl	$18, %ecx
	movq	%r8, %rsi
	repz
	cmpsb
	jne	.L3759
.L3845:
	movq	%r8, %rdi
	call	redundant
	jmp	.L3736
.L3759:
	cld
	movl	$.LC126, %edi
	movl	$7, %ecx
	movq	%r8, %rsi
	repz
	cmpsb
	jne	.L3761
	movl	$1, blockSize100k(%rip)
	jmp	.L3736
.L3761:
	cld
	movl	$.LC127, %edi
	movl	$7, %ecx
	movq	%r8, %rsi
	repz
	cmpsb
	jne	.L3763
	movl	$9, blockSize100k(%rip)
	jmp	.L3736
.L3763:
	cld
	movl	$.LC128, %edi
	movl	$10, %ecx
	movq	%r8, %rsi
	repz
	cmpsb
	jne	.L3765
	addl	$1, verbosity(%rip)
	jmp	.L3736
.LFE114:
	.size	main, .-main
	.p2align 4,,15
.globl BZ2_bzwrite
	.type	BZ2_bzwrite, @function
BZ2_bzwrite:
.LFB65:
	pushq	%rbx
.LCFI197:
	movq	%rdi, %rax
	movl	%edx, %ebx
	movl	%edx, %ecx
	movq	%rsi, %rdx
	movq	%rax, %rsi
	subq	$16, %rsp
.LCFI198:
	leaq	12(%rsp), %rdi
	call	BZ2_bzWrite
	movl	12(%rsp), %eax
	testl	%eax, %eax
	movl	$-1, %eax
	cmovne	%eax, %ebx
	addq	$16, %rsp
	movl	%ebx, %eax
	popq	%rbx
	ret
.LFE65:
	.size	BZ2_bzwrite, .-BZ2_bzwrite
.globl BZ2_crc32Table
	.data
	.align 32
	.type	BZ2_crc32Table, @object
	.size	BZ2_crc32Table, 1024
BZ2_crc32Table:
	.long	0
	.long	79764919
	.long	159529838
	.long	222504665
	.long	319059676
	.long	398814059
	.long	445009330
	.long	507990021
	.long	638119352
	.long	583659535
	.long	797628118
	.long	726387553
	.long	890018660
	.long	835552979
	.long	1015980042
	.long	944750013
	.long	1276238704
	.long	1221641927
	.long	1167319070
	.long	1095957929
	.long	1595256236
	.long	1540665371
	.long	1452775106
	.long	1381403509
	.long	1780037320
	.long	1859660671
	.long	1671105958
	.long	1733955601
	.long	2031960084
	.long	2111593891
	.long	1889500026
	.long	1952343757
	.long	-1742489888
	.long	-1662866601
	.long	-1851683442
	.long	-1788833735
	.long	-1960329156
	.long	-1880695413
	.long	-2103051438
	.long	-2040207643
	.long	-1104454824
	.long	-1159051537
	.long	-1213636554
	.long	-1284997759
	.long	-1389417084
	.long	-1444007885
	.long	-1532160278
	.long	-1603531939
	.long	-734892656
	.long	-789352409
	.long	-575645954
	.long	-646886583
	.long	-952755380
	.long	-1007220997
	.long	-827056094
	.long	-898286187
	.long	-231047128
	.long	-151282273
	.long	-71779514
	.long	-8804623
	.long	-515967244
	.long	-436212925
	.long	-390279782
	.long	-327299027
	.long	881225847
	.long	809987520
	.long	1023691545
	.long	969234094
	.long	662832811
	.long	591600412
	.long	771767749
	.long	717299826
	.long	311336399
	.long	374308984
	.long	453813921
	.long	533576470
	.long	25881363
	.long	88864420
	.long	134795389
	.long	214552010
	.long	2023205639
	.long	2086057648
	.long	1897238633
	.long	1976864222
	.long	1804852699
	.long	1867694188
	.long	1645340341
	.long	1724971778
	.long	1587496639
	.long	1516133128
	.long	1461550545
	.long	1406951526
	.long	1302016099
	.long	1230646740
	.long	1142491917
	.long	1087903418
	.long	-1398421865
	.long	-1469785312
	.long	-1524105735
	.long	-1578704818
	.long	-1079922613
	.long	-1151291908
	.long	-1239184603
	.long	-1293773166
	.long	-1968362705
	.long	-1905510760
	.long	-2094067647
	.long	-2014441994
	.long	-1716953613
	.long	-1654112188
	.long	-1876203875
	.long	-1796572374
	.long	-525066777
	.long	-462094256
	.long	-382327159
	.long	-302564546
	.long	-206542021
	.long	-143559028
	.long	-97365931
	.long	-17609246
	.long	-960696225
	.long	-1031934488
	.long	-817968335
	.long	-872425850
	.long	-709327229
	.long	-780559564
	.long	-600130067
	.long	-654598054
	.long	1762451694
	.long	1842216281
	.long	1619975040
	.long	1682949687
	.long	2047383090
	.long	2127137669
	.long	1938468188
	.long	2001449195
	.long	1325665622
	.long	1271206113
	.long	1183200824
	.long	1111960463
	.long	1543535498
	.long	1489069629
	.long	1434599652
	.long	1363369299
	.long	622672798
	.long	568075817
	.long	748617968
	.long	677256519
	.long	907627842
	.long	853037301
	.long	1067152940
	.long	995781531
	.long	51762726
	.long	131386257
	.long	177728840
	.long	240578815
	.long	269590778
	.long	349224269
	.long	429104020
	.long	491947555
	.long	-248556018
	.long	-168932423
	.long	-122852000
	.long	-60002089
	.long	-500490030
	.long	-420856475
	.long	-341238852
	.long	-278395381
	.long	-685261898
	.long	-739858943
	.long	-559578920
	.long	-630940305
	.long	-1004286614
	.long	-1058877219
	.long	-845023740
	.long	-916395085
	.long	-1119974018
	.long	-1174433591
	.long	-1262701040
	.long	-1333941337
	.long	-1371866206
	.long	-1426332139
	.long	-1481064244
	.long	-1552294533
	.long	-1690935098
	.long	-1611170447
	.long	-1833673816
	.long	-1770699233
	.long	-2009983462
	.long	-1930228819
	.long	-2119160460
	.long	-2056179517
	.long	1569362073
	.long	1498123566
	.long	1409854455
	.long	1355396672
	.long	1317987909
	.long	1246755826
	.long	1192025387
	.long	1137557660
	.long	2072149281
	.long	2135122070
	.long	1912620623
	.long	1992383480
	.long	1753615357
	.long	1816598090
	.long	1627664531
	.long	1707420964
	.long	295390185
	.long	358241886
	.long	404320391
	.long	483945776
	.long	43990325
	.long	106832002
	.long	186451547
	.long	266083308
	.long	932423249
	.long	861060070
	.long	1041341759
	.long	986742920
	.long	613929101
	.long	542559546
	.long	756411363
	.long	701822548
	.long	-978770311
	.long	-1050133554
	.long	-869589737
	.long	-924188512
	.long	-693284699
	.long	-764654318
	.long	-550540341
	.long	-605129092
	.long	-475935807
	.long	-413084042
	.long	-366743377
	.long	-287118056
	.long	-257573603
	.long	-194731862
	.long	-114850189
	.long	-35218492
	.long	-1984365303
	.long	-1921392450
	.long	-2143631769
	.long	-2063868976
	.long	-1698919467
	.long	-1635936670
	.long	-1824608069
	.long	-1744851700
	.long	-1347415887
	.long	-1418654458
	.long	-1506661409
	.long	-1561119128
	.long	-1129027987
	.long	-1200260134
	.long	-1254728445
	.long	-1309196108
.globl BZ2_rNums
	.align 32
	.type	BZ2_rNums, @object
	.size	BZ2_rNums, 2048
BZ2_rNums:
	.long	619
	.long	720
	.long	127
	.long	481
	.long	931
	.long	816
	.long	813
	.long	233
	.long	566
	.long	247
	.long	985
	.long	724
	.long	205
	.long	454
	.long	863
	.long	491
	.long	741
	.long	242
	.long	949
	.long	214
	.long	733
	.long	859
	.long	335
	.long	708
	.long	621
	.long	574
	.long	73
	.long	654
	.long	730
	.long	472
	.long	419
	.long	436
	.long	278
	.long	496
	.long	867
	.long	210
	.long	399
	.long	680
	.long	480
	.long	51
	.long	878
	.long	465
	.long	811
	.long	169
	.long	869
	.long	675
	.long	611
	.long	697
	.long	867
	.long	561
	.long	862
	.long	687
	.long	507
	.long	283
	.long	482
	.long	129
	.long	807
	.long	591
	.long	733
	.long	623
	.long	150
	.long	238
	.long	59
	.long	379
	.long	684
	.long	877
	.long	625
	.long	169
	.long	643
	.long	105
	.long	170
	.long	607
	.long	520
	.long	932
	.long	727
	.long	476
	.long	693
	.long	425
	.long	174
	.long	647
	.long	73
	.long	122
	.long	335
	.long	530
	.long	442
	.long	853
	.long	695
	.long	249
	.long	445
	.long	515
	.long	909
	.long	545
	.long	703
	.long	919
	.long	874
	.long	474
	.long	882
	.long	500
	.long	594
	.long	612
	.long	641
	.long	801
	.long	220
	.long	162
	.long	819
	.long	984
	.long	589
	.long	513
	.long	495
	.long	799
	.long	161
	.long	604
	.long	958
	.long	533
	.long	221
	.long	400
	.long	386
	.long	867
	.long	600
	.long	782
	.long	382
	.long	596
	.long	414
	.long	171
	.long	516
	.long	375
	.long	682
	.long	485
	.long	911
	.long	276
	.long	98
	.long	553
	.long	163
	.long	354
	.long	666
	.long	933
	.long	424
	.long	341
	.long	533
	.long	870
	.long	227
	.long	730
	.long	475
	.long	186
	.long	263
	.long	647
	.long	537
	.long	686
	.long	600
	.long	224
	.long	469
	.long	68
	.long	770
	.long	919
	.long	190
	.long	373
	.long	294
	.long	822
	.long	808
	.long	206
	.long	184
	.long	943
	.long	795
	.long	384
	.long	383
	.long	461
	.long	404
	.long	758
	.long	839
	.long	887
	.long	715
	.long	67
	.long	618
	.long	276
	.long	204
	.long	918
	.long	873
	.long	777
	.long	604
	.long	560
	.long	951
	.long	160
	.long	578
	.long	722
	.long	79
	.long	804
	.long	96
	.long	409
	.long	713
	.long	940
	.long	652
	.long	934
	.long	970
	.long	447
	.long	318
	.long	353
	.long	859
	.long	672
	.long	112
	.long	785
	.long	645
	.long	863
	.long	803
	.long	350
	.long	139
	.long	93
	.long	354
	.long	99
	.long	820
	.long	908
	.long	609
	.long	772
	.long	154
	.long	274
	.long	580
	.long	184
	.long	79
	.long	626
	.long	630
	.long	742
	.long	653
	.long	282
	.long	762
	.long	623
	.long	680
	.long	81
	.long	927
	.long	626
	.long	789
	.long	125
	.long	411
	.long	521
	.long	938
	.long	300
	.long	821
	.long	78
	.long	343
	.long	175
	.long	128
	.long	250
	.long	170
	.long	774
	.long	972
	.long	275
	.long	999
	.long	639
	.long	495
	.long	78
	.long	352
	.long	126
	.long	857
	.long	956
	.long	358
	.long	619
	.long	580
	.long	124
	.long	737
	.long	594
	.long	701
	.long	612
	.long	669
	.long	112
	.long	134
	.long	694
	.long	363
	.long	992
	.long	809
	.long	743
	.long	168
	.long	974
	.long	944
	.long	375
	.long	748
	.long	52
	.long	600
	.long	747
	.long	642
	.long	182
	.long	862
	.long	81
	.long	344
	.long	805
	.long	988
	.long	739
	.long	511
	.long	655
	.long	814
	.long	334
	.long	249
	.long	515
	.long	897
	.long	955
	.long	664
	.long	981
	.long	649
	.long	113
	.long	974
	.long	459
	.long	893
	.long	228
	.long	433
	.long	837
	.long	553
	.long	268
	.long	926
	.long	240
	.long	102
	.long	654
	.long	459
	.long	51
	.long	686
	.long	754
	.long	806
	.long	760
	.long	493
	.long	403
	.long	415
	.long	394
	.long	687
	.long	700
	.long	946
	.long	670
	.long	656
	.long	610
	.long	738
	.long	392
	.long	760
	.long	799
	.long	887
	.long	653
	.long	978
	.long	321
	.long	576
	.long	617
	.long	626
	.long	502
	.long	894
	.long	679
	.long	243
	.long	440
	.long	680
	.long	879
	.long	194
	.long	572
	.long	640
	.long	724
	.long	926
	.long	56
	.long	204
	.long	700
	.long	707
	.long	151
	.long	457
	.long	449
	.long	797
	.long	195
	.long	791
	.long	558
	.long	945
	.long	679
	.long	297
	.long	59
	.long	87
	.long	824
	.long	713
	.long	663
	.long	412
	.long	693
	.long	342
	.long	606
	.long	134
	.long	108
	.long	571
	.long	364
	.long	631
	.long	212
	.long	174
	.long	643
	.long	304
	.long	329
	.long	343
	.long	97
	.long	430
	.long	751
	.long	497
	.long	314
	.long	983
	.long	374
	.long	822
	.long	928
	.long	140
	.long	206
	.long	73
	.long	263
	.long	980
	.long	736
	.long	876
	.long	478
	.long	430
	.long	305
	.long	170
	.long	514
	.long	364
	.long	692
	.long	829
	.long	82
	.long	855
	.long	953
	.long	676
	.long	246
	.long	369
	.long	970
	.long	294
	.long	750
	.long	807
	.long	827
	.long	150
	.long	790
	.long	288
	.long	923
	.long	804
	.long	378
	.long	215
	.long	828
	.long	592
	.long	281
	.long	565
	.long	555
	.long	710
	.long	82
	.long	896
	.long	831
	.long	547
	.long	261
	.long	524
	.long	462
	.long	293
	.long	465
	.long	502
	.long	56
	.long	661
	.long	821
	.long	976
	.long	991
	.long	658
	.long	869
	.long	905
	.long	758
	.long	745
	.long	193
	.long	768
	.long	550
	.long	608
	.long	933
	.long	378
	.long	286
	.long	215
	.long	979
	.long	792
	.long	961
	.long	61
	.long	688
	.long	793
	.long	644
	.long	986
	.long	403
	.long	106
	.long	366
	.long	905
	.long	644
	.long	372
	.long	567
	.long	466
	.long	434
	.long	645
	.long	210
	.long	389
	.long	550
	.long	919
	.long	135
	.long	780
	.long	773
	.long	635
	.long	389
	.long	707
	.long	100
	.long	626
	.long	958
	.long	165
	.long	504
	.long	920
	.long	176
	.long	193
	.long	713
	.long	857
	.long	265
	.long	203
	.long	50
	.long	668
	.long	108
	.long	645
	.long	990
	.long	626
	.long	197
	.long	510
	.long	357
	.long	358
	.long	850
	.long	858
	.long	364
	.long	936
	.long	638
.globl zSuffix
	.section	.rodata.str1.1
.LC132:
	.string	".bz2"
.LC133:
	.string	".bz"
.LC134:
	.string	".tbz2"
.LC135:
	.string	".tbz"
	.data
	.align 32
	.type	zSuffix, @object
	.size	zSuffix, 32
zSuffix:
	.quad	.LC132
	.quad	.LC133
	.quad	.LC134
	.quad	.LC135
.globl unzSuffix
	.section	.rodata.str1.1
.LC136:
	.string	".tar"
	.data
	.align 32
	.type	unzSuffix, @object
	.size	unzSuffix, 32
unzSuffix:
	.quad	.LC63
	.quad	.LC63
	.quad	.LC136
	.quad	.LC136
	.section	.rodata.str1.1
.LC137:
	.string	"OK"
.LC138:
	.string	"SEQUENCE_ERROR"
.LC139:
	.string	"PARAM_ERROR"
.LC140:
	.string	"MEM_ERROR"
.LC141:
	.string	"DATA_ERROR"
.LC142:
	.string	"DATA_ERROR_MAGIC"
.LC143:
	.string	"IO_ERROR"
.LC144:
	.string	"UNEXPECTED_EOF"
.LC145:
	.string	"OUTBUFF_FULL"
.LC146:
	.string	"CONFIG_ERROR"
.LC147:
	.string	"???"
	.section	.rodata
	.align 32
	.type	bzerrorstrings, @object
	.size	bzerrorstrings, 128
bzerrorstrings:
	.quad	.LC137
	.quad	.LC138
	.quad	.LC139
	.quad	.LC140
	.quad	.LC141
	.quad	.LC142
	.quad	.LC143
	.quad	.LC144
	.quad	.LC145
	.quad	.LC146
	.quad	.LC147
	.quad	.LC147
	.quad	.LC147
	.quad	.LC147
	.quad	.LC147
	.quad	.LC147
	.align 32
	.type	incs, @object
	.size	incs, 56
incs:
	.long	1
	.long	4
	.long	13
	.long	40
	.long	121
	.long	364
	.long	1093
	.long	3280
	.long	9841
	.long	29524
	.long	88573
	.long	265720
	.long	797161
	.long	2391484
	.local	fileMetaInfo
	.comm	fileMetaInfo,144,32
	.comm	verbosity,4,4
	.comm	keepInputFiles,1,1
	.comm	smallMode,1,1
	.comm	deleteOutputOnInterrupt,1,1
	.comm	forceOverwrite,1,1
	.comm	testFailsExist,1,1
	.comm	unzFailsExist,1,1
	.comm	noisy,1,1
	.comm	numFileNames,4,4
	.comm	numFilesProcessed,4,4
	.comm	blockSize100k,4,4
	.comm	exitValue,4,4
	.comm	opMode,4,4
	.comm	srcMode,4,4
	.comm	longestFileName,4,4
	.comm	inName,1034,32
	.comm	outName,1034,32
	.comm	tmpName,1034,32
	.comm	progName,8,8
	.comm	progNameReally,1034,32
	.comm	outputHandleJustInCase,8,8
	.comm	workFactor,4,4
	.section	.eh_frame,"a",@progbits
.Lframe1:
	.long	.LECIE1-.LSCIE1
.LSCIE1:
	.long	0x0
	.byte	0x1
	.string	"zR"
	.uleb128 0x1
	.sleb128 -8
	.byte	0x10
	.uleb128 0x1
	.byte	0x3
	.byte	0xc
	.uleb128 0x7
	.uleb128 0x8
	.byte	0x90
	.uleb128 0x1
	.align 8
.LECIE1:
.LSFDE1:
	.long	.LEFDE1-.LASFDE1
.LASFDE1:
	.long	.LASFDE1-.Lframe1
	.long	.LFB15
	.long	.LFE15-.LFB15
	.uleb128 0x0
	.align 8
.LEFDE1:
.LSFDE3:
	.long	.LEFDE3-.LASFDE3
.LASFDE3:
	.long	.LASFDE3-.Lframe1
	.long	.LFB16
	.long	.LFE16-.LFB16
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI0-.LFB16
	.byte	0xe
	.uleb128 0x10
	.byte	0x8c
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI1-.LCFI0
	.byte	0xe
	.uleb128 0x18
	.byte	0x86
	.uleb128 0x3
	.byte	0x4
	.long	.LCFI2-.LCFI1
	.byte	0xe
	.uleb128 0x20
	.byte	0x83
	.uleb128 0x4
	.align 8
.LEFDE3:
.LSFDE5:
	.long	.LEFDE5-.LASFDE5
.LASFDE5:
	.long	.LASFDE5-.Lframe1
	.long	.LFB17
	.long	.LFE17-.LFB17
	.uleb128 0x0
	.align 8
.LEFDE5:
.LSFDE7:
	.long	.LEFDE7-.LASFDE7
.LASFDE7:
	.long	.LASFDE7-.Lframe1
	.long	.LFB29
	.long	.LFE29-.LFB29
	.uleb128 0x0
	.align 8
.LEFDE7:
.LSFDE9:
	.long	.LEFDE9-.LASFDE9
.LASFDE9:
	.long	.LASFDE9-.Lframe1
	.long	.LFB32
	.long	.LFE32-.LFB32
	.uleb128 0x0
	.align 8
.LEFDE9:
.LSFDE11:
	.long	.LEFDE11-.LASFDE11
.LASFDE11:
	.long	.LASFDE11-.Lframe1
	.long	.LFB33
	.long	.LFE33-.LFB33
	.uleb128 0x0
	.align 8
.LEFDE11:
.LSFDE13:
	.long	.LEFDE13-.LASFDE13
.LASFDE13:
	.long	.LASFDE13-.Lframe1
	.long	.LFB34
	.long	.LFE34-.LFB34
	.uleb128 0x0
	.align 8
.LEFDE13:
.LSFDE15:
	.long	.LEFDE15-.LASFDE15
.LASFDE15:
	.long	.LASFDE15-.Lframe1
	.long	.LFB35
	.long	.LFE35-.LFB35
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI4-.LFB35
	.byte	0x86
	.uleb128 0x6
	.byte	0x83
	.uleb128 0x7
	.byte	0x4
	.long	.LCFI9-.LCFI4
	.byte	0xe
	.uleb128 0x40
	.byte	0x8f
	.uleb128 0x2
	.byte	0x8e
	.uleb128 0x3
	.byte	0x8d
	.uleb128 0x4
	.byte	0x8c
	.uleb128 0x5
	.align 8
.LEFDE15:
.LSFDE17:
	.long	.LEFDE17-.LASFDE17
.LASFDE17:
	.long	.LASFDE17-.Lframe1
	.long	.LFB36
	.long	.LFE36-.LFB36
	.uleb128 0x0
	.align 8
.LEFDE17:
.LSFDE19:
	.long	.LEFDE19-.LASFDE19
.LASFDE19:
	.long	.LASFDE19-.Lframe1
	.long	.LFB42
	.long	.LFE42-.LFB42
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI12-.LFB42
	.byte	0xe
	.uleb128 0x20
	.byte	0x86
	.uleb128 0x2
	.byte	0x83
	.uleb128 0x3
	.align 8
.LEFDE19:
.LSFDE21:
	.long	.LEFDE21-.LASFDE21
.LASFDE21:
	.long	.LASFDE21-.Lframe1
	.long	.LFB43
	.long	.LFE43-.LFB43
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI14-.LFB43
	.byte	0x86
	.uleb128 0x3
	.byte	0x83
	.uleb128 0x4
	.byte	0x4
	.long	.LCFI16-.LCFI14
	.byte	0xe
	.uleb128 0x20
	.byte	0x8c
	.uleb128 0x2
	.align 8
.LEFDE21:
.LSFDE23:
	.long	.LEFDE23-.LASFDE23
.LASFDE23:
	.long	.LASFDE23-.Lframe1
	.long	.LFB45
	.long	.LFE45-.LFB45
	.uleb128 0x0
	.align 8
.LEFDE23:
.LSFDE25:
	.long	.LEFDE25-.LASFDE25
.LASFDE25:
	.long	.LASFDE25-.Lframe1
	.long	.LFB48
	.long	.LFE48-.LFB48
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI19-.LFB48
	.byte	0xe
	.uleb128 0x20
	.byte	0x86
	.uleb128 0x2
	.byte	0x83
	.uleb128 0x3
	.align 8
.LEFDE25:
.LSFDE27:
	.long	.LEFDE27-.LASFDE27
.LASFDE27:
	.long	.LASFDE27-.Lframe1
	.long	.LFB57
	.long	.LFE57-.LFB57
	.uleb128 0x0
	.align 8
.LEFDE27:
.LSFDE29:
	.long	.LEFDE29-.LASFDE29
.LASFDE29:
	.long	.LASFDE29-.Lframe1
	.long	.LFB60
	.long	.LFE60-.LFB60
	.uleb128 0x0
	.align 8
.LEFDE29:
.LSFDE31:
	.long	.LEFDE31-.LASFDE31
.LASFDE31:
	.long	.LASFDE31-.Lframe1
	.long	.LFB66
	.long	.LFE66-.LFB66
	.uleb128 0x0
	.align 8
.LEFDE31:
.LSFDE33:
	.long	.LEFDE33-.LASFDE33
.LASFDE33:
	.long	.LASFDE33-.Lframe1
	.long	.LFB68
	.long	.LFE68-.LFB68
	.uleb128 0x0
	.align 8
.LEFDE33:
.LSFDE35:
	.long	.LEFDE35-.LASFDE35
.LASFDE35:
	.long	.LASFDE35-.Lframe1
	.long	.LFB73
	.long	.LFE73-.LFB73
	.uleb128 0x0
	.align 8
.LEFDE35:
.LSFDE37:
	.long	.LEFDE37-.LASFDE37
.LASFDE37:
	.long	.LASFDE37-.Lframe1
	.long	.LFB74
	.long	.LFE74-.LFB74
	.uleb128 0x0
	.align 8
.LEFDE37:
.LSFDE39:
	.long	.LEFDE39-.LASFDE39
.LASFDE39:
	.long	.LASFDE39-.Lframe1
	.long	.LFB77
	.long	.LFE77-.LFB77
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI20-.LFB77
	.byte	0xe
	.uleb128 0x10
	.byte	0x83
	.uleb128 0x2
	.align 8
.LEFDE39:
.LSFDE41:
	.long	.LEFDE41-.LASFDE41
.LASFDE41:
	.long	.LASFDE41-.Lframe1
	.long	.LFB81
	.long	.LFE81-.LFB81
	.uleb128 0x0
	.align 8
.LEFDE41:
.LSFDE43:
	.long	.LEFDE43-.LASFDE43
.LASFDE43:
	.long	.LASFDE43-.Lframe1
	.long	.LFB101
	.long	.LFE101-.LFB101
	.uleb128 0x0
	.align 8
.LEFDE43:
.LSFDE45:
	.long	.LEFDE45-.LASFDE45
.LASFDE45:
	.long	.LASFDE45-.Lframe1
	.long	.LFB102
	.long	.LFE102-.LFB102
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI22-.LFB102
	.byte	0x86
	.uleb128 0x3
	.byte	0x83
	.uleb128 0x4
	.byte	0x4
	.long	.LCFI24-.LCFI22
	.byte	0xe
	.uleb128 0x20
	.byte	0x8c
	.uleb128 0x2
	.align 8
.LEFDE45:
.LSFDE47:
	.long	.LEFDE47-.LASFDE47
.LASFDE47:
	.long	.LASFDE47-.Lframe1
	.long	.LFB109
	.long	.LFE109-.LFB109
	.uleb128 0x0
	.align 8
.LEFDE47:
.LSFDE49:
	.long	.LEFDE49-.LASFDE49
.LASFDE49:
	.long	.LASFDE49-.Lframe1
	.long	.LFB108
	.long	.LFE108-.LFB108
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI25-.LFB108
	.byte	0xe
	.uleb128 0x10
	.byte	0x83
	.uleb128 0x2
	.align 8
.LEFDE49:
.LSFDE51:
	.long	.LEFDE51-.LASFDE51
.LASFDE51:
	.long	.LASFDE51-.Lframe1
	.long	.LFB107
	.long	.LFE107-.LFB107
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI26-.LFB107
	.byte	0xe
	.uleb128 0x10
	.align 8
.LEFDE51:
.LSFDE53:
	.long	.LEFDE53-.LASFDE53
.LASFDE53:
	.long	.LASFDE53-.Lframe1
	.long	.LFB83
	.long	.LFE83-.LFB83
	.uleb128 0x0
	.align 8
.LEFDE53:
.LSFDE55:
	.long	.LEFDE55-.LASFDE55
.LASFDE55:
	.long	.LASFDE55-.Lframe1
	.long	.LFB92
	.long	.LFE92-.LFB92
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI27-.LFB92
	.byte	0xe
	.uleb128 0x10
	.align 8
.LEFDE55:
.LSFDE57:
	.long	.LEFDE57-.LASFDE57
.LASFDE57:
	.long	.LASFDE57-.Lframe1
	.long	.LFB82
	.long	.LFE82-.LFB82
	.uleb128 0x0
	.align 8
.LEFDE57:
.LSFDE59:
	.long	.LEFDE59-.LASFDE59
.LASFDE59:
	.long	.LASFDE59-.Lframe1
	.long	.LFB28
	.long	.LFE28-.LFB28
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI28-.LFB28
	.byte	0xe
	.uleb128 0x10
	.byte	0x83
	.uleb128 0x2
	.align 8
.LEFDE59:
.LSFDE61:
	.long	.LEFDE61-.LASFDE61
.LASFDE61:
	.long	.LASFDE61-.Lframe1
	.long	.LFB14
	.long	.LFE14-.LFB14
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI29-.LFB14
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.long	.LCFI30-.LCFI29
	.byte	0xe
	.uleb128 0x18
	.byte	0x4
	.long	.LCFI31-.LCFI30
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.long	.LCFI32-.LCFI31
	.byte	0xe
	.uleb128 0x28
	.byte	0x4
	.long	.LCFI33-.LCFI32
	.byte	0xe
	.uleb128 0x30
	.byte	0x4
	.long	.LCFI34-.LCFI33
	.byte	0xe
	.uleb128 0x38
	.byte	0x4
	.long	.LCFI35-.LCFI34
	.byte	0xe
	.uleb128 0x1480
	.byte	0x83
	.uleb128 0x7
	.byte	0x86
	.uleb128 0x6
	.byte	0x8c
	.uleb128 0x5
	.byte	0x8d
	.uleb128 0x4
	.byte	0x8e
	.uleb128 0x3
	.byte	0x8f
	.uleb128 0x2
	.align 8
.LEFDE61:
.LSFDE63:
	.long	.LEFDE63-.LASFDE63
.LASFDE63:
	.long	.LASFDE63-.Lframe1
	.long	.LFB27
	.long	.LFE27-.LFB27
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI36-.LFB27
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.long	.LCFI37-.LCFI36
	.byte	0xe
	.uleb128 0x18
	.byte	0x4
	.long	.LCFI38-.LCFI37
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.long	.LCFI39-.LCFI38
	.byte	0xe
	.uleb128 0x28
	.byte	0x4
	.long	.LCFI40-.LCFI39
	.byte	0xe
	.uleb128 0x30
	.byte	0x4
	.long	.LCFI41-.LCFI40
	.byte	0xe
	.uleb128 0x38
	.byte	0x83
	.uleb128 0x7
	.byte	0x86
	.uleb128 0x6
	.byte	0x8c
	.uleb128 0x5
	.byte	0x8d
	.uleb128 0x4
	.byte	0x8e
	.uleb128 0x3
	.byte	0x8f
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI42-.LCFI41
	.byte	0xe
	.uleb128 0xd0
	.align 8
.LEFDE63:
.LSFDE65:
	.long	.LEFDE65-.LASFDE65
.LASFDE65:
	.long	.LASFDE65-.Lframe1
	.long	.LFB7
	.long	.LFE7-.LFB7
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI43-.LFB7
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.long	.LCFI44-.LCFI43
	.byte	0xe
	.uleb128 0x18
	.byte	0x4
	.long	.LCFI45-.LCFI44
	.byte	0xe
	.uleb128 0x20
	.byte	0x8d
	.uleb128 0x4
	.byte	0x8e
	.uleb128 0x3
	.byte	0x8f
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI46-.LCFI45
	.byte	0xe
	.uleb128 0x28
	.byte	0x4
	.long	.LCFI47-.LCFI46
	.byte	0xe
	.uleb128 0x30
	.byte	0x4
	.long	.LCFI48-.LCFI47
	.byte	0xe
	.uleb128 0x38
	.byte	0x4
	.long	.LCFI49-.LCFI48
	.byte	0xe
	.uleb128 0xbd0
	.byte	0x83
	.uleb128 0x7
	.byte	0x86
	.uleb128 0x6
	.byte	0x8c
	.uleb128 0x5
	.align 8
.LEFDE65:
.LSFDE67:
	.long	.LEFDE67-.LASFDE67
.LASFDE67:
	.long	.LASFDE67-.Lframe1
	.long	.LFB55
	.long	.LFE55-.LFB55
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI50-.LFB55
	.byte	0xe
	.uleb128 0x10
	.byte	0x83
	.uleb128 0x2
	.align 8
.LEFDE67:
.LSFDE69:
	.long	.LEFDE69-.LASFDE69
.LASFDE69:
	.long	.LASFDE69-.Lframe1
	.long	.LFB31
	.long	.LFE31-.LFB31
	.uleb128 0x0
	.align 8
.LEFDE69:
.LSFDE71:
	.long	.LEFDE71-.LASFDE71
.LASFDE71:
	.long	.LASFDE71-.Lframe1
	.long	.LFB94
	.long	.LFE94-.LFB94
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI53-.LFB94
	.byte	0xe
	.uleb128 0x20
	.byte	0x86
	.uleb128 0x2
	.byte	0x83
	.uleb128 0x3
	.align 8
.LEFDE71:
.LSFDE73:
	.long	.LEFDE73-.LASFDE73
.LASFDE73:
	.long	.LASFDE73-.Lframe1
	.long	.LFB30
	.long	.LFE30-.LFB30
	.uleb128 0x0
	.align 8
.LEFDE73:
.LSFDE75:
	.long	.LEFDE75-.LASFDE75
.LASFDE75:
	.long	.LASFDE75-.Lframe1
	.long	.LFB84
	.long	.LFE84-.LFB84
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI54-.LFB84
	.byte	0xe
	.uleb128 0x10
	.byte	0x83
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI55-.LCFI54
	.byte	0xe
	.uleb128 0xa0
	.align 8
.LEFDE75:
.LSFDE77:
	.long	.LEFDE77-.LASFDE77
.LASFDE77:
	.long	.LASFDE77-.Lframe1
	.long	.LFB91
	.long	.LFE91-.LFB91
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI56-.LFB91
	.byte	0xe
	.uleb128 0x10
	.align 8
.LEFDE77:
.LSFDE79:
	.long	.LEFDE79-.LASFDE79
.LASFDE79:
	.long	.LASFDE79-.Lframe1
	.long	.LFB110
	.long	.LFE110-.LFB110
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI57-.LFB110
	.byte	0xe
	.uleb128 0x10
	.align 8
.LEFDE79:
.LSFDE81:
	.long	.LEFDE81-.LASFDE81
.LASFDE81:
	.long	.LASFDE81-.Lframe1
	.long	.LFB112
	.long	.LFE112-.LFB112
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI58-.LFB112
	.byte	0xe
	.uleb128 0x10
	.byte	0x8c
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI59-.LCFI58
	.byte	0xe
	.uleb128 0x18
	.byte	0x86
	.uleb128 0x3
	.byte	0x4
	.long	.LCFI60-.LCFI59
	.byte	0xe
	.uleb128 0x20
	.byte	0x83
	.uleb128 0x4
	.align 8
.LEFDE81:
.LSFDE83:
	.long	.LEFDE83-.LASFDE83
.LASFDE83:
	.long	.LASFDE83-.Lframe1
	.long	.LFB113
	.long	.LFE113-.LFB113
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI61-.LFB113
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.long	.LCFI62-.LCFI61
	.byte	0xe
	.uleb128 0x18
	.byte	0x8c
	.uleb128 0x3
	.byte	0x8d
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI63-.LCFI62
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.long	.LCFI64-.LCFI63
	.byte	0xe
	.uleb128 0x28
	.byte	0x4
	.long	.LCFI65-.LCFI64
	.byte	0xe
	.uleb128 0x30
	.byte	0x83
	.uleb128 0x5
	.byte	0x86
	.uleb128 0x4
	.align 8
.LEFDE83:
.LSFDE85:
	.long	.LEFDE85-.LASFDE85
.LASFDE85:
	.long	.LASFDE85-.Lframe1
	.long	.LFB90
	.long	.LFE90-.LFB90
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI66-.LFB90
	.byte	0xe
	.uleb128 0x10
	.align 8
.LEFDE85:
.LSFDE87:
	.long	.LEFDE87-.LASFDE87
.LASFDE87:
	.long	.LASFDE87-.Lframe1
	.long	.LFB89
	.long	.LFE89-.LFB89
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI67-.LFB89
	.byte	0xe
	.uleb128 0x10
	.align 8
.LEFDE87:
.LSFDE89:
	.long	.LEFDE89-.LASFDE89
.LASFDE89:
	.long	.LASFDE89-.Lframe1
	.long	.LFB85
	.long	.LFE85-.LFB85
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI68-.LFB85
	.byte	0xe
	.uleb128 0x10
	.align 8
.LEFDE89:
.LSFDE91:
	.long	.LEFDE91-.LASFDE91
.LASFDE91:
	.long	.LASFDE91-.Lframe1
	.long	.LFB95
	.long	.LFE95-.LFB95
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI69-.LFB95
	.byte	0xe
	.uleb128 0x10
	.byte	0x83
	.uleb128 0x2
	.align 8
.LEFDE91:
.LSFDE93:
	.long	.LEFDE93-.LASFDE93
.LASFDE93:
	.long	.LASFDE93-.Lframe1
	.long	.LFB98
	.long	.LFE98-.LFB98
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI70-.LFB98
	.byte	0xe
	.uleb128 0xa0
	.align 8
.LEFDE93:
.LSFDE95:
	.long	.LEFDE95-.LASFDE95
.LASFDE95:
	.long	.LASFDE95-.Lframe1
	.long	.LFB97
	.long	.LFE97-.LFB97
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI71-.LFB97
	.byte	0xe
	.uleb128 0xa0
	.align 8
.LEFDE95:
.LSFDE97:
	.long	.LEFDE97-.LASFDE97
.LASFDE97:
	.long	.LASFDE97-.Lframe1
	.long	.LFB93
	.long	.LFE93-.LFB93
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI72-.LFB93
	.byte	0xe
	.uleb128 0x10
	.byte	0x86
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI73-.LCFI72
	.byte	0xe
	.uleb128 0x18
	.byte	0x4
	.long	.LCFI74-.LCFI73
	.byte	0xe
	.uleb128 0x20
	.byte	0x83
	.uleb128 0x3
	.align 8
.LEFDE97:
.LSFDE99:
	.long	.LEFDE99-.LASFDE99
.LASFDE99:
	.long	.LASFDE99-.Lframe1
	.long	.LFB47
	.long	.LFE47-.LFB47
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI80-.LFB47
	.byte	0xe
	.uleb128 0x30
	.byte	0x8e
	.uleb128 0x2
	.byte	0x8d
	.uleb128 0x3
	.byte	0x8c
	.uleb128 0x4
	.byte	0x86
	.uleb128 0x5
	.byte	0x83
	.uleb128 0x6
	.align 8
.LEFDE99:
.LSFDE101:
	.long	.LEFDE101-.LASFDE101
.LASFDE101:
	.long	.LASFDE101-.Lframe1
	.long	.LFB59
	.long	.LFE59-.LFB59
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI82-.LFB59
	.byte	0x8c
	.uleb128 0x5
	.byte	0x86
	.uleb128 0x6
	.byte	0x4
	.long	.LCFI87-.LCFI82
	.byte	0xe
	.uleb128 0x90
	.byte	0x8f
	.uleb128 0x2
	.byte	0x83
	.uleb128 0x7
	.byte	0x8e
	.uleb128 0x3
	.byte	0x8d
	.uleb128 0x4
	.align 8
.LEFDE101:
.LSFDE103:
	.long	.LEFDE103-.LASFDE103
.LASFDE103:
	.long	.LASFDE103-.Lframe1
	.long	.LFB54
	.long	.LFE54-.LFB54
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI89-.LFB54
	.byte	0x8c
	.uleb128 0x5
	.byte	0x83
	.uleb128 0x7
	.byte	0x4
	.long	.LCFI94-.LCFI89
	.byte	0xe
	.uleb128 0x40
	.byte	0x86
	.uleb128 0x6
	.byte	0x8f
	.uleb128 0x2
	.byte	0x8e
	.uleb128 0x3
	.byte	0x8d
	.uleb128 0x4
	.align 8
.LEFDE103:
.LSFDE105:
	.long	.LEFDE105-.LASFDE105
.LASFDE105:
	.long	.LASFDE105-.Lframe1
	.long	.LFB50
	.long	.LFE50-.LFB50
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI96-.LFB50
	.byte	0x86
	.uleb128 0x6
	.byte	0x83
	.uleb128 0x7
	.byte	0x4
	.long	.LCFI98-.LCFI96
	.byte	0x8d
	.uleb128 0x4
	.byte	0x8c
	.uleb128 0x5
	.byte	0x4
	.long	.LCFI101-.LCFI98
	.byte	0xe
	.uleb128 0x40
	.byte	0x8f
	.uleb128 0x2
	.byte	0x8e
	.uleb128 0x3
	.align 8
.LEFDE105:
.LSFDE107:
	.long	.LEFDE107-.LASFDE107
.LASFDE107:
	.long	.LASFDE107-.Lframe1
	.long	.LFB49
	.long	.LFE49-.LFB49
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI102-.LFB49
	.byte	0xe
	.uleb128 0x10
	.byte	0x83
	.uleb128 0x2
	.align 8
.LEFDE107:
.LSFDE109:
	.long	.LEFDE109-.LASFDE109
.LASFDE109:
	.long	.LASFDE109-.Lframe1
	.long	.LFB56
	.long	.LFE56-.LFB56
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI104-.LFB56
	.byte	0x86
	.uleb128 0x5
	.byte	0x83
	.uleb128 0x6
	.byte	0x4
	.long	.LCFI108-.LCFI104
	.byte	0xe
	.uleb128 0x30
	.byte	0x8e
	.uleb128 0x2
	.byte	0x8d
	.uleb128 0x3
	.byte	0x8c
	.uleb128 0x4
	.align 8
.LEFDE109:
.LSFDE111:
	.long	.LEFDE111-.LASFDE111
.LASFDE111:
	.long	.LASFDE111-.Lframe1
	.long	.LFB64
	.long	.LFE64-.LFB64
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI109-.LFB64
	.byte	0xe
	.uleb128 0x20
	.align 8
.LEFDE111:
.LSFDE113:
	.long	.LEFDE113-.LASFDE113
.LASFDE113:
	.long	.LASFDE113-.Lframe1
	.long	.LFB88
	.long	.LFE88-.LFB88
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI110-.LFB88
	.byte	0xe
	.uleb128 0x10
	.align 8
.LEFDE113:
.LSFDE115:
	.long	.LEFDE115-.LASFDE115
.LASFDE115:
	.long	.LASFDE115-.Lframe1
	.long	.LFB100
	.long	.LFE100-.LFB100
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI111-.LFB100
	.byte	0xe
	.uleb128 0x10
	.byte	0x83
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI112-.LCFI111
	.byte	0xe
	.uleb128 0x20
	.align 8
.LEFDE115:
.LSFDE117:
	.long	.LEFDE117-.LASFDE117
.LASFDE117:
	.long	.LASFDE117-.Lframe1
	.long	.LFB99
	.long	.LFE99-.LFB99
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI113-.LFB99
	.byte	0xe
	.uleb128 0x10
	.align 8
.LEFDE117:
.LSFDE119:
	.long	.LEFDE119-.LASFDE119
.LASFDE119:
	.long	.LASFDE119-.Lframe1
	.long	.LFB106
	.long	.LFE106-.LFB106
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI114-.LFB106
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.long	.LCFI115-.LCFI114
	.byte	0xe
	.uleb128 0x18
	.byte	0x4
	.long	.LCFI116-.LCFI115
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.long	.LCFI117-.LCFI116
	.byte	0xe
	.uleb128 0x28
	.byte	0x4
	.long	.LCFI118-.LCFI117
	.byte	0xe
	.uleb128 0x30
	.byte	0x4
	.long	.LCFI119-.LCFI118
	.byte	0xe
	.uleb128 0x38
	.byte	0x83
	.uleb128 0x7
	.byte	0x86
	.uleb128 0x6
	.byte	0x8c
	.uleb128 0x5
	.byte	0x8d
	.uleb128 0x4
	.byte	0x8e
	.uleb128 0x3
	.byte	0x8f
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI120-.LCFI119
	.byte	0xe
	.uleb128 0x2810
	.align 8
.LEFDE119:
.LSFDE121:
	.long	.LEFDE121-.LASFDE121
.LASFDE121:
	.long	.LASFDE121-.Lframe1
	.long	.LFB61
	.long	.LFE61-.LFB61
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI122-.LFB61
	.byte	0x8d
	.uleb128 0x4
	.byte	0x8c
	.uleb128 0x5
	.byte	0x4
	.long	.LCFI127-.LCFI122
	.byte	0xe
	.uleb128 0x13f0
	.byte	0x8f
	.uleb128 0x2
	.byte	0x8e
	.uleb128 0x3
	.byte	0x86
	.uleb128 0x6
	.byte	0x83
	.uleb128 0x7
	.align 8
.LEFDE121:
.LSFDE123:
	.long	.LEFDE123-.LASFDE123
.LASFDE123:
	.long	.LASFDE123-.Lframe1
	.long	.LFB63
	.long	.LFE63-.LFB63
	.uleb128 0x0
	.align 8
.LEFDE123:
.LSFDE125:
	.long	.LEFDE125-.LASFDE125
.LASFDE125:
	.long	.LASFDE125-.Lframe1
	.long	.LFB62
	.long	.LFE62-.LFB62
	.uleb128 0x0
	.align 8
.LEFDE125:
.LSFDE127:
	.long	.LEFDE127-.LASFDE127
.LASFDE127:
	.long	.LASFDE127-.Lframe1
	.long	.LFB96
	.long	.LFE96-.LFB96
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI131-.LFB96
	.byte	0xe
	.uleb128 0x20
	.byte	0x8c
	.uleb128 0x2
	.byte	0x86
	.uleb128 0x3
	.byte	0x83
	.uleb128 0x4
	.align 8
.LEFDE127:
.LSFDE129:
	.long	.LEFDE129-.LASFDE129
.LASFDE129:
	.long	.LASFDE129-.Lframe1
	.long	.LFB105
	.long	.LFE105-.LFB105
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI132-.LFB105
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.long	.LCFI133-.LCFI132
	.byte	0xe
	.uleb128 0x18
	.byte	0x4
	.long	.LCFI134-.LCFI133
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.long	.LCFI135-.LCFI134
	.byte	0xe
	.uleb128 0x28
	.byte	0x4
	.long	.LCFI136-.LCFI135
	.byte	0xe
	.uleb128 0x30
	.byte	0x4
	.long	.LCFI137-.LCFI136
	.byte	0xe
	.uleb128 0x38
	.byte	0x83
	.uleb128 0x7
	.byte	0x86
	.uleb128 0x6
	.byte	0x8c
	.uleb128 0x5
	.byte	0x8d
	.uleb128 0x4
	.byte	0x8e
	.uleb128 0x3
	.byte	0x8f
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI138-.LCFI137
	.byte	0xe
	.uleb128 0x2820
	.align 8
.LEFDE129:
.LSFDE131:
	.long	.LEFDE131-.LASFDE131
.LASFDE131:
	.long	.LASFDE131-.Lframe1
	.long	.LFB21
	.long	.LFE21-.LFB21
	.uleb128 0x0
	.align 8
.LEFDE131:
.LSFDE133:
	.long	.LEFDE133-.LASFDE133
.LASFDE133:
	.long	.LASFDE133-.Lframe1
	.long	.LFB20
	.long	.LFE20-.LFB20
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI139-.LFB20
	.byte	0xe
	.uleb128 0x10
	.byte	0x83
	.uleb128 0x2
	.align 8
.LEFDE133:
.LSFDE135:
	.long	.LEFDE135-.LASFDE135
.LASFDE135:
	.long	.LASFDE135-.Lframe1
	.long	.LFB13
	.long	.LFE13-.LFB13
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI140-.LFB13
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.long	.LCFI141-.LCFI140
	.byte	0xe
	.uleb128 0x18
	.byte	0x4
	.long	.LCFI142-.LCFI141
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.long	.LCFI143-.LCFI142
	.byte	0xe
	.uleb128 0x28
	.byte	0x4
	.long	.LCFI144-.LCFI143
	.byte	0xe
	.uleb128 0x30
	.byte	0x4
	.long	.LCFI145-.LCFI144
	.byte	0xe
	.uleb128 0x38
	.byte	0x4
	.long	.LCFI146-.LCFI145
	.byte	0xe
	.uleb128 0x14d0
	.byte	0x83
	.uleb128 0x7
	.byte	0x86
	.uleb128 0x6
	.byte	0x8c
	.uleb128 0x5
	.byte	0x8d
	.uleb128 0x4
	.byte	0x8e
	.uleb128 0x3
	.byte	0x8f
	.uleb128 0x2
	.align 8
.LEFDE135:
.LSFDE137:
	.long	.LEFDE137-.LASFDE137
.LASFDE137:
	.long	.LASFDE137-.Lframe1
	.long	.LFB25
	.long	.LFE25-.LFB25
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI147-.LFB25
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.long	.LCFI148-.LCFI147
	.byte	0xe
	.uleb128 0x18
	.byte	0x4
	.long	.LCFI149-.LCFI148
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.long	.LCFI150-.LCFI149
	.byte	0xe
	.uleb128 0x28
	.byte	0x4
	.long	.LCFI151-.LCFI150
	.byte	0xe
	.uleb128 0x30
	.byte	0x86
	.uleb128 0x6
	.byte	0x8c
	.uleb128 0x5
	.byte	0x8d
	.uleb128 0x4
	.byte	0x8e
	.uleb128 0x3
	.byte	0x8f
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI152-.LCFI151
	.byte	0xe
	.uleb128 0x38
	.byte	0x4
	.long	.LCFI153-.LCFI152
	.byte	0xe
	.uleb128 0x320
	.byte	0x83
	.uleb128 0x7
	.align 8
.LEFDE137:
.LSFDE139:
	.long	.LEFDE139-.LASFDE139
.LASFDE139:
	.long	.LASFDE139-.Lframe1
	.long	.LFB40
	.long	.LFE40-.LFB40
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI154-.LFB40
	.byte	0xe
	.uleb128 0x10
	.byte	0x8d
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI155-.LCFI154
	.byte	0xe
	.uleb128 0x18
	.byte	0x8c
	.uleb128 0x3
	.byte	0x4
	.long	.LCFI156-.LCFI155
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.long	.LCFI157-.LCFI156
	.byte	0xe
	.uleb128 0x28
	.byte	0x4
	.long	.LCFI158-.LCFI157
	.byte	0xe
	.uleb128 0x30
	.byte	0x83
	.uleb128 0x5
	.byte	0x86
	.uleb128 0x4
	.align 8
.LEFDE139:
.LSFDE141:
	.long	.LEFDE141-.LASFDE141
.LASFDE141:
	.long	.LASFDE141-.Lframe1
	.long	.LFB41
	.long	.LFE41-.LFB41
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI159-.LFB41
	.byte	0xe
	.uleb128 0x10
	.byte	0x83
	.uleb128 0x2
	.align 8
.LEFDE141:
.LSFDE143:
	.long	.LEFDE143-.LASFDE143
.LASFDE143:
	.long	.LASFDE143-.Lframe1
	.long	.LFB58
	.long	.LFE58-.LFB58
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI161-.LFB58
	.byte	0x8c
	.uleb128 0x5
	.byte	0x86
	.uleb128 0x6
	.byte	0x4
	.long	.LCFI166-.LCFI161
	.byte	0xe
	.uleb128 0x90
	.byte	0x8f
	.uleb128 0x2
	.byte	0x83
	.uleb128 0x7
	.byte	0x8e
	.uleb128 0x3
	.byte	0x8d
	.uleb128 0x4
	.align 8
.LEFDE143:
.LSFDE145:
	.long	.LEFDE145-.LASFDE145
.LASFDE145:
	.long	.LASFDE145-.Lframe1
	.long	.LFB53
	.long	.LFE53-.LFB53
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI168-.LFB53
	.byte	0x86
	.uleb128 0x6
	.byte	0x83
	.uleb128 0x7
	.byte	0x4
	.long	.LCFI170-.LCFI168
	.byte	0x8e
	.uleb128 0x3
	.byte	0x8d
	.uleb128 0x4
	.byte	0x4
	.long	.LCFI173-.LCFI170
	.byte	0xe
	.uleb128 0x60
	.byte	0x8c
	.uleb128 0x5
	.byte	0x8f
	.uleb128 0x2
	.align 8
.LEFDE145:
.LSFDE147:
	.long	.LEFDE147-.LASFDE147
.LASFDE147:
	.long	.LASFDE147-.Lframe1
	.long	.LFB52
	.long	.LFE52-.LFB52
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI174-.LFB52
	.byte	0xe
	.uleb128 0x10
	.align 8
.LEFDE147:
.LSFDE149:
	.long	.LEFDE149-.LASFDE149
.LASFDE149:
	.long	.LASFDE149-.Lframe1
	.long	.LFB67
	.long	.LFE67-.LFB67
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI177-.LFB67
	.byte	0xe
	.uleb128 0x30
	.byte	0x86
	.uleb128 0x2
	.byte	0x83
	.uleb128 0x3
	.align 8
.LEFDE149:
.LSFDE151:
	.long	.LEFDE151-.LASFDE151
.LASFDE151:
	.long	.LASFDE151-.Lframe1
	.long	.LFB51
	.long	.LFE51-.LFB51
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI179-.LFB51
	.byte	0x86
	.uleb128 0x6
	.byte	0x83
	.uleb128 0x7
	.byte	0x4
	.long	.LCFI181-.LCFI179
	.byte	0x8d
	.uleb128 0x4
	.byte	0x8c
	.uleb128 0x5
	.byte	0x4
	.long	.LCFI184-.LCFI181
	.byte	0xe
	.uleb128 0x40
	.byte	0x8f
	.uleb128 0x2
	.byte	0x8e
	.uleb128 0x3
	.align 8
.LEFDE151:
.LSFDE153:
	.long	.LEFDE153-.LASFDE153
.LASFDE153:
	.long	.LASFDE153-.Lframe1
	.long	.LFB104
	.long	.LFE104-.LFB104
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI186-.LFB104
	.byte	0x86
	.uleb128 0x6
	.byte	0x83
	.uleb128 0x7
	.byte	0x4
	.long	.LCFI191-.LCFI186
	.byte	0xe
	.uleb128 0x1500
	.byte	0x8f
	.uleb128 0x2
	.byte	0x8e
	.uleb128 0x3
	.byte	0x8d
	.uleb128 0x4
	.byte	0x8c
	.uleb128 0x5
	.align 8
.LEFDE153:
.LSFDE155:
	.long	.LEFDE155-.LASFDE155
.LASFDE155:
	.long	.LASFDE155-.Lframe1
	.long	.LFB114
	.long	.LFE114-.LFB114
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI192-.LFB114
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.long	.LCFI193-.LCFI192
	.byte	0xe
	.uleb128 0x18
	.byte	0x8c
	.uleb128 0x3
	.byte	0x8d
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI194-.LCFI193
	.byte	0xe
	.uleb128 0x20
	.byte	0x86
	.uleb128 0x4
	.byte	0x4
	.long	.LCFI195-.LCFI194
	.byte	0xe
	.uleb128 0x28
	.byte	0x4
	.long	.LCFI196-.LCFI195
	.byte	0xe
	.uleb128 0x40
	.byte	0x83
	.uleb128 0x5
	.align 8
.LEFDE155:
.LSFDE157:
	.long	.LEFDE157-.LASFDE157
.LASFDE157:
	.long	.LASFDE157-.Lframe1
	.long	.LFB65
	.long	.LFE65-.LFB65
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI197-.LFB65
	.byte	0xe
	.uleb128 0x10
	.byte	0x83
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI198-.LCFI197
	.byte	0xe
	.uleb128 0x20
	.align 8
.LEFDE157:
	.ident	"GCC: (GNU) 4.1.2 20071124 (Red Hat 4.1.2-42)"
	.section	.note.GNU-stack,"",@progbits
