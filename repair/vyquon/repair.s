	.file	"repair.sanity.c"
	.text
	.p2align 4,,15
.globl IsWhitespace
	.type	IsWhitespace, @function
IsWhitespace:
.LFB2:
	cmpb	$32, %dil
	je	.L2
	cmpb	$9, %dil
	je	.L2
	cmpb	$10, %dil
	sete	%al
	movzbl	%al, %eax
	ret
	.p2align 4,,7
.L2:
	movl	$1, %eax
	.p2align 4,,2
	ret
.LFE2:
	.size	IsWhitespace, .-IsWhitespace
	.p2align 4,,15
.globl IsNumeric
	.type	IsNumeric, @function
IsNumeric:
.LFB3:
	xorl	%eax, %eax
	cmpb	$47, %dil
	jle	.L11
	xorl	%eax, %eax
	cmpb	$57, %dil
	setle	%al
.L11:
	rep ; ret
.LFE3:
	.size	IsNumeric, .-IsNumeric
	.p2align 4,,15
.globl SymbolEq
	.type	SymbolEq, @function
SymbolEq:
.LFB29:
	xorl	%eax, %eax
	cmpq	%rdi, %rsi
	sete	%al
	ret
.LFE29:
	.size	SymbolEq, .-SymbolEq
	.p2align 4,,15
.globl WrapObj
	.type	WrapObj, @function
WrapObj:
.LFB37:
	movq	%rdi, %rax
	movq	%rsi, 16(%rdi)
	movq	%rcx, 8(%rdi)
	movl	%edx, (%rdi)
	ret
.LFE37:
	.size	WrapObj, .-WrapObj
	.p2align 4,,15
.globl Obj
	.type	Obj, @function
Obj:
.LFB38:
	movq	24(%rsp), %rax
	ret
.LFE38:
	.size	Obj, .-Obj
	.p2align 4,,15
.globl ListGet
	.type	ListGet, @function
ListGet:
.LFB24:
	movq	%rbx, -16(%rsp)
.LCFI0:
	movq	%rbp, -8(%rsp)
.LCFI1:
	subq	$80, %rsp
.LCFI2:
	testl	%edx, %edx
	movq	%rdi, %rbx
	movl	%edx, %ebp
	jne	.L20
	movq	(%rsi), %rax
	movq	%rax, (%rdi)
	movq	8(%rsi), %rax
	movq	%rax, 8(%rdi)
	movq	16(%rsi), %rax
	movq	%rax, 16(%rdi)
.L19:
	movq	%rbx, %rax
	movq	72(%rsp), %rbp
	movq	64(%rsp), %rbx
	addq	$80, %rsp
	ret
	.p2align 4,,7
.L20:
	movq	24(%rsi), %rax
	movq	%rax, (%rsp)
	movq	32(%rsi), %rax
	movq	%rax, 8(%rsp)
	movq	40(%rsi), %rax
	movq	%rax, 16(%rsp)
	call	Obj
	leaq	24(%rsp), %rdi
	leal	-1(%rbp), %edx
	movq	%rax, %rsi
	call	ListGet
	movq	24(%rsp), %rax
	movq	%rax, (%rbx)
	movq	32(%rsp), %rax
	movq	%rax, 8(%rbx)
	movq	40(%rsp), %rax
	movq	%rax, 16(%rbx)
	jmp	.L19
.LFE24:
	.size	ListGet, .-ListGet
	.p2align 4,,15
.globl Car
	.type	Car, @function
Car:
.LFB22:
	pushq	%rbx
.LCFI3:
	movq	%rdi, %rbx
	subq	$24, %rsp
.LCFI4:
	movq	40(%rsp), %rax
	movq	%rax, (%rsp)
	movq	48(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	56(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	Obj
	movq	(%rax), %rdx
	movq	%rdx, (%rbx)
	movq	8(%rax), %rdx
	movq	%rdx, 8(%rbx)
	movq	16(%rax), %rax
	movq	%rax, 16(%rbx)
	movq	%rbx, %rax
	addq	$24, %rsp
	popq	%rbx
	ret
.LFE22:
	.size	Car, .-Car
	.p2align 4,,15
.globl Cdr
	.type	Cdr, @function
Cdr:
.LFB21:
	pushq	%rbx
.LCFI5:
	movq	%rdi, %rbx
	subq	$24, %rsp
.LCFI6:
	movq	40(%rsp), %rax
	movq	%rax, (%rsp)
	movq	48(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	56(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	Obj
	movq	24(%rax), %rdx
	movq	%rdx, (%rbx)
	movq	32(%rax), %rdx
	movq	%rdx, 8(%rbx)
	movq	40(%rax), %rax
	movq	%rax, 16(%rbx)
	movq	%rbx, %rax
	addq	$24, %rsp
	popq	%rbx
	ret
.LFE21:
	.size	Cdr, .-Cdr
	.p2align 4,,15
.globl Type
	.type	Type, @function
Type:
.LFB39:
	movq	16(%rsp), %rdx
	movl	8(%rsp), %eax
	ret
.LFE39:
	.size	Type, .-Type
	.p2align 4,,15
.globl IsType
	.type	IsType, @function
IsType:
.LFB40:
	pushq	%rbx
.LCFI7:
	movq	%rsi, %rbx
	subq	$80, %rsp
.LCFI8:
	movq	96(%rsp), %rax
	movq	%rax, (%rsp)
	movq	104(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	112(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	Type
	xorl	%eax, %eax
	cmpq	%rbx, %rdx
	movq	%rdx, 72(%rsp)
	sete	%al
	addq	$80, %rsp
	popq	%rbx
	ret
.LFE40:
	.size	IsType, .-IsType
	.p2align 4,,15
.globl CreateType
	.type	CreateType, @function
CreateType:
.LFB41:
	movq	%rsi, %rdx
	movl	%edi, %eax
	ret
.LFE41:
	.size	CreateType, .-CreateType
	.p2align 4,,15
.globl ObjEq
	.type	ObjEq, @function
ObjEq:
.LFB42:
	movq	%rbx, -48(%rsp)
.LCFI9:
	movq	%rbp, -40(%rsp)
.LCFI10:
	movq	%r12, -32(%rsp)
.LCFI11:
	movq	%r13, -24(%rsp)
.LCFI12:
	movq	%r14, -16(%rsp)
.LCFI13:
	movq	%r15, -8(%rsp)
.LCFI14:
	subq	$152, %rsp
.LCFI15:
	movq	160(%rsp), %rax
	movq	168(%rsp), %r15
	movq	176(%rsp), %r14
	movq	%rax, 40(%rsp)
	movq	%rax, (%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	call	Obj
	movq	184(%rsp), %r13
	movq	192(%rsp), %r12
	movq	%rax, %rbx
	movq	200(%rsp), %rbp
	movq	%r13, (%rsp)
	movq	%r12, 8(%rsp)
	movq	%rbp, 16(%rsp)
	call	Obj
	xorl	%edx, %edx
	cmpq	%rax, %rbx
	je	.L39
.L37:
	movq	104(%rsp), %rbx
	movq	112(%rsp), %rbp
	movl	%edx, %eax
	movq	120(%rsp), %r12
	movq	128(%rsp), %r13
	movq	136(%rsp), %r14
	movq	144(%rsp), %r15
	addq	$152, %rsp
	ret
	.p2align 4,,7
.L39:
	movq	%r13, (%rsp)
	movq	%r12, 8(%rsp)
	movq	%rbp, 16(%rsp)
	call	Type
	movl	%eax, 48(%rsp)
	movq	48(%rsp), %rax
	movq	%rdx, %rbx
	movq	%rdx, 88(%rsp)
	movq	%r15, 8(%rsp)
	movq	%r14, 16(%rsp)
	movq	%rax, 80(%rsp)
	movq	40(%rsp), %rax
	movq	%rax, (%rsp)
	call	Type
	movl	%eax, 48(%rsp)
	movq	48(%rsp), %rax
	movq	%rdx, %rdi
	movq	%rbx, %rsi
	movq	%rdx, 56(%rsp)
	movq	%rdx, 72(%rsp)
	movq	%rax, 64(%rsp)
	call	SymbolEq
	xorl	%edx, %edx
	testl	%eax, %eax
	setne	%dl
	jmp	.L37
.LFE42:
	.size	ObjEq, .-ObjEq
	.p2align 4,,15
.globl None
	.type	None, @function
None:
.LFB43:
	movq	TypeNone+8(%rip), %rax
	movq	$0, 16(%rdi)
	movq	%rax, 8(%rdi)
	movl	TypeNone(%rip), %eax
	movl	%eax, (%rdi)
	movq	%rdi, %rax
	ret
.LFE43:
	.size	None, .-None
	.p2align 4,,15
.globl ParseParam
	.type	ParseParam, @function
ParseParam:
.LFB33:
	movq	%rbp, -40(%rsp)
.LCFI16:
	movq	%r13, -24(%rsp)
.LCFI17:
	movq	%rdi, %rbp
	movq	%r15, -8(%rsp)
.LCFI18:
	movq	%rbx, -48(%rsp)
.LCFI19:
	movq	%r12, -32(%rsp)
.LCFI20:
	movq	%r14, -16(%rsp)
.LCFI21:
	subq	$216, %rsp
.LCFI22:
	leaq	96(%rsp), %rdi
	movl	%esi, 44(%rsp)
	movl	%edx, 40(%rsp)
	call	None
	movq	112(%rsp), %rax
	movq	104(%rsp), %r13
	movl	96(%rsp), %r15d
	movq	%rax, 48(%rsp)
	movl	44(%rsp), %eax
	testl	%eax, %eax
	jne	.L48
	movq	224(%rsp), %r14
	movq	232(%rsp), %r12
	movq	240(%rsp), %rbx
.L43:
	movq	%r14, (%rsp)
	movq	%r12, 8(%rsp)
	movq	%rbx, 16(%rsp)
	call	Obj
	movq	48(%rsp), %rdx
	movq	%rax, 8(%rbp)
	movl	40(%rsp), %eax
	movq	%r13, 24(%rbp)
	movl	%r15d, 16(%rbp)
	movq	%rdx, 32(%rbp)
	movl	44(%rsp), %edx
	movl	%eax, 4(%rbp)
	movq	%rbp, %rax
	movl	%edx, (%rbp)
	movq	168(%rsp), %rbx
	movq	176(%rsp), %rbp
	movq	184(%rsp), %r12
	movq	192(%rsp), %r13
	movq	200(%rsp), %r14
	movq	208(%rsp), %r15
	addq	$216, %rsp
	ret
	.p2align 4,,7
.L48:
	movl	TypeCons(%rip), %edi
	movq	TypeCons+8(%rip), %rsi
	movq	224(%rsp), %r14
	movq	232(%rsp), %r12
	movq	240(%rsp), %rbx
	movq	%r14, (%rsp)
	movq	%r12, 8(%rsp)
	movq	%rbx, 16(%rsp)
	call	IsType
	testl	%eax, %eax
	je	.L43
	movq	%r14, (%rsp)
	movq	%r12, 8(%rsp)
	movq	%rbx, 16(%rsp)
	call	Obj
	leaq	64(%rsp), %rdi
	movq	%rax, %rsi
	xorl	%edx, %edx
	movq	%rax, %rbx
	call	ListGet
	movq	80(%rsp), %rdx
	movl	64(%rsp), %r14d
	leaq	128(%rsp), %rdi
	movq	%rbx, %rsi
	movq	72(%rsp), %r12
	movq	%rdx, 56(%rsp)
	xorl	%edx, %edx
	call	ListGet
	movq	56(%rsp), %rdx
	movq	144(%rsp), %rax
	movl	%r14d, 224(%rsp)
	movq	136(%rsp), %r13
	movl	128(%rsp), %r15d
	movq	224(%rsp), %r14
	movq	%rdx, %rbx
	movq	%rax, 48(%rsp)
	movq	%rdx, 240(%rsp)
	movq	%r12, 232(%rsp)
	jmp	.L43
.LFE33:
	.size	ParseParam, .-ParseParam
	.p2align 4,,15
.globl IsNone
	.type	IsNone, @function
IsNone:
.LFB44:
	movl	TypeNone(%rip), %edi
	movq	TypeNone+8(%rip), %rsi
	jmp	IsType
.LFE44:
	.size	IsNone, .-IsNone
	.p2align 4,,15
.globl StackPop
	.type	StackPop, @function
StackPop:
.LFB59:
	movq	%rbx, -40(%rsp)
.LCFI23:
	movq	%r12, -32(%rsp)
.LCFI24:
	movq	%rdi, %r12
	movq	%r13, -24(%rsp)
.LCFI25:
	movq	%r14, -16(%rsp)
.LCFI26:
	movq	%r15, -8(%rsp)
.LCFI27:
	subq	$72, %rsp
.LCFI28:
	movl	stack_index(%rip), %ebx
	movq	%rsp, %rdi
	subl	$1, %ebx
	movl	%ebx, stack_index(%rip)
	movslq	%ebx,%rbx
	leaq	(%rbx,%rbx,2), %rbx
	salq	$3, %rbx
	addq	stack(%rip), %rbx
	movq	16(%rbx), %r13
	movq	8(%rbx), %r14
	movl	(%rbx), %r15d
	call	None
	movq	(%rsp), %rax
	movq	%rax, (%rbx)
	movq	8(%rsp), %rax
	movq	%rax, 8(%rbx)
	movq	16(%rsp), %rax
	movq	%rax, 16(%rbx)
	movq	%r13, 16(%r12)
	movq	%r12, %rax
	movq	%r14, 8(%r12)
	movl	%r15d, (%r12)
	movq	32(%rsp), %rbx
	movq	40(%rsp), %r12
	movq	48(%rsp), %r13
	movq	56(%rsp), %r14
	movq	64(%rsp), %r15
	addq	$72, %rsp
	ret
.LFE59:
	.size	StackPop, .-StackPop
	.p2align 4,,15
.globl PopInstr
	.type	PopInstr, @function
PopInstr:
.LFB50:
	subq	$40, %rsp
.LCFI29:
	movq	%rsp, %rdi
	call	StackPop
	addq	$40, %rsp
	ret
.LFE50:
	.size	PopInstr, .-PopInstr
.globl memmove
	.p2align 4,,15
.globl StackPeek
	.type	StackPeek, @function
StackPeek:
.LFB60:
	movslq	stack_index(%rip),%rsi
	pushq	%rbx
.LCFI30:
	movl	$24, %edx
	movq	%rdi, %rbx
	leaq	(%rsi,%rsi,2), %rsi
	salq	$3, %rsi
	addq	stack(%rip), %rsi
	subq	$24, %rsi
	call	memmove
	movq	%rbx, %rax
	popq	%rbx
	ret
.LFE60:
	.size	StackPeek, .-StackPeek
	.p2align 4,,15
.globl Push
	.type	Push, @function
Push:
.LFB65:
	movq	24(%rsp), %rax
	movl	$20, (%rdi)
	movq	%rax, 24(%rdi)
	movq	16(%rsp), %rax
	movq	%rax, 16(%rdi)
	movl	8(%rsp), %eax
	movl	%eax, 8(%rdi)
	movq	%rdi, %rax
	ret
.LFE65:
	.size	Push, .-Push
	.p2align 4,,15
.globl Pop
	.type	Pop, @function
Pop:
.LFB66:
	movq	%rdi, %rax
	movl	$0, 8(%rdi)
	movl	$10, (%rdi)
	ret
.LFE66:
	.size	Pop, .-Pop
	.p2align 4,,15
.globl Bind
	.type	Bind, @function
Bind:
.LFB67:
	movq	%rdi, %rax
	movl	$0, 8(%rdi)
	movl	$70, (%rdi)
	ret
.LFE67:
	.size	Bind, .-Bind
	.p2align 4,,15
.globl Value
	.type	Value, @function
Value:
.LFB68:
	movq	24(%rsp), %rax
	movl	$60, (%rdi)
	movq	%rax, 24(%rdi)
	movq	16(%rsp), %rax
	movq	%rax, 16(%rdi)
	movl	8(%rsp), %eax
	movl	%eax, 8(%rdi)
	movq	%rdi, %rax
	ret
.LFE68:
	.size	Value, .-Value
	.p2align 4,,15
.globl Func
	.type	Func, @function
Func:
.LFB69:
	movq	%rdi, %rax
	movl	$0, 8(%rdi)
	movl	$80, (%rdi)
	ret
.LFE69:
	.size	Func, .-Func
	.p2align 4,,15
.globl Call
	.type	Call, @function
Call:
.LFB70:
	movq	%rdi, %rax
	movl	%esi, 8(%rdi)
	movl	$30, (%rdi)
	ret
.LFE70:
	.size	Call, .-Call
	.p2align 4,,15
.globl EnterScope
	.type	EnterScope, @function
EnterScope:
.LFB78:
	movq	%rdi, current_scope(%rip)
	ret
.LFE78:
	.size	EnterScope, .-EnterScope
	.p2align 4,,15
.globl NumEq
	.type	NumEq, @function
NumEq:
.LFB96:
	movq	%rbx, -48(%rsp)
.LCFI31:
	movq	%rbp, -40(%rsp)
.LCFI32:
	movq	%r12, -32(%rsp)
.LCFI33:
	movq	%r15, -8(%rsp)
.LCFI34:
	movq	%r13, -24(%rsp)
.LCFI35:
	movq	%r14, -16(%rsp)
.LCFI36:
	subq	$88, %rsp
.LCFI37:
	movq	TypeInt+8(%rip), %rax
	movl	TypeInt(%rip), %r15d
	movq	96(%rsp), %r12
	movq	104(%rsp), %rbp
	movq	112(%rsp), %rbx
	movl	%r15d, %edi
	movq	%rax, %rsi
	movq	%rax, 24(%rsp)
	movq	%r12, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%rbx, 16(%rsp)
	call	IsType
	testl	%eax, %eax
	jne	.L81
.L72:
	movl	TypeFloat(%rip), %r13d
	movq	TypeFloat+8(%rip), %r14
	movq	%r12, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%rbx, 16(%rsp)
	movl	%r13d, %edi
	movq	%r14, %rsi
	call	IsType
	testl	%eax, %eax
	jne	.L82
.L74:
	movq	120(%rsp), %r12
	movq	128(%rsp), %rbp
	movl	%r15d, %edi
	movq	136(%rsp), %rbx
	movq	24(%rsp), %rsi
	movq	%r12, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%rbx, 16(%rsp)
	call	IsType
	testl	%eax, %eax
	jne	.L83
.L76:
	movl	%r13d, %edi
	movq	%r14, %rsi
	movq	%r12, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%rbx, 16(%rsp)
	call	IsType
	testl	%eax, %eax
	je	.L78
	movq	%r12, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%rbx, 16(%rsp)
	call	Obj
	cvtsd2ss	(%rax), %xmm0
	movss	%xmm0, 36(%rsp)
.L78:
	movss	36(%rsp), %xmm0
	movq	40(%rsp), %rbx
	ucomiss	32(%rsp), %xmm0
	movq	48(%rsp), %rbp
	movq	56(%rsp), %r12
	movq	64(%rsp), %r13
	movq	72(%rsp), %r14
	movq	80(%rsp), %r15
	sete	%al
	setnp	%dl
	addq	$88, %rsp
	andl	%edx, %eax
	movzbl	%al, %eax
	ret
	.p2align 4,,7
.L83:
	movq	%r12, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%rbx, 16(%rsp)
	call	Obj
	cvtsi2ss	(%rax), %xmm0
	movss	%xmm0, 36(%rsp)
	jmp	.L76
	.p2align 4,,7
.L82:
	movq	%r12, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%rbx, 16(%rsp)
	call	Obj
	cvtsd2ss	(%rax), %xmm0
	movss	%xmm0, 32(%rsp)
	jmp	.L74
	.p2align 4,,7
.L81:
	movq	%r12, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%rbx, 16(%rsp)
	call	Obj
	cvtsi2ss	(%rax), %xmm0
	movss	%xmm0, 32(%rsp)
	jmp	.L72
.LFE96:
	.size	NumEq, .-NumEq
	.p2align 4,,15
.globl GetCar
	.type	GetCar, @function
GetCar:
.LFB102:
	pushq	%rbx
.LCFI38:
	movq	%rdi, %rbx
	subq	$24, %rsp
.LCFI39:
	movq	(%rsi), %rax
	movq	%rax, (%rsp)
	movq	8(%rsi), %rax
	movq	%rax, 8(%rsp)
	movq	16(%rsi), %rax
	movq	%rax, 16(%rsp)
	call	Car
	movq	%rbx, %rax
	addq	$24, %rsp
	popq	%rbx
	ret
.LFE102:
	.size	GetCar, .-GetCar
	.p2align 4,,15
.globl GetCdr
	.type	GetCdr, @function
GetCdr:
.LFB103:
	pushq	%rbx
.LCFI40:
	movq	%rdi, %rbx
	subq	$24, %rsp
.LCFI41:
	movq	(%rsi), %rax
	movq	%rax, (%rsp)
	movq	8(%rsi), %rax
	movq	%rax, 8(%rsp)
	movq	16(%rsi), %rax
	movq	%rax, 16(%rsp)
	call	Cdr
	movq	%rbx, %rax
	addq	$24, %rsp
	popq	%rbx
	ret
.LFE103:
	.size	GetCdr, .-GetCdr
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC0:
	.string	"TOKEN_NUMFLT"
.LC1:
	.string	"NULL"
.LC2:
	.string	"TOKEN_OPAREN"
.LC3:
	.string	"TOKEN_CPAREN"
.LC4:
	.string	"TOKEN_SYMBOL"
.LC5:
	.string	"TOKEN_QUOTED"
.LC6:
	.string	"TOKEN_STRING"
.LC7:
	.string	"TOKEN_NUMINT"
.LC8:
	.string	"%d: {%s, \"%s\"}\n"
	.text
	.p2align 4,,15
.globl PrintTokens
	.type	PrintTokens, @function
PrintTokens:
.LFB6:
	pushq	%rbp
.LCFI42:
	movq	%rdi, %rbp
	pushq	%rbx
.LCFI43:
	movq	%rsi, %rbx
	subq	$8, %rsp
.LCFI44:
	testq	%rsi, %rsi
	je	.L103
	.p2align 4,,7
.L104:
	movzbl	(%rbx), %eax
	subl	$10, %eax
	cmpb	$40, %al
	ja	.L91
	movzbl	%al, %eax
	jmp	*.L99(,%rax,8)
	.section	.rodata
	.align 8
	.align 4
.L99:
	.quad	.L92
	.quad	.L91
	.quad	.L91
	.quad	.L91
	.quad	.L91
	.quad	.L91
	.quad	.L91
	.quad	.L91
	.quad	.L91
	.quad	.L91
	.quad	.L93
	.quad	.L91
	.quad	.L91
	.quad	.L91
	.quad	.L91
	.quad	.L94
	.quad	.L95
	.quad	.L91
	.quad	.L91
	.quad	.L91
	.quad	.L96
	.quad	.L91
	.quad	.L91
	.quad	.L91
	.quad	.L91
	.quad	.L91
	.quad	.L91
	.quad	.L91
	.quad	.L91
	.quad	.L91
	.quad	.L97
	.quad	.L91
	.quad	.L91
	.quad	.L91
	.quad	.L91
	.quad	.L91
	.quad	.L91
	.quad	.L91
	.quad	.L91
	.quad	.L91
	.quad	.L98
	.text
	.p2align 4,,7
.L91:
	movl	$.LC1, %ecx
.L100:
	movq	8(%rbx), %r8
	movl	16(%rbx), %edx
	movl	$.LC1, %eax
	movl	$.LC8, %esi
	movq	%rbp, %rdi
	testq	%r8, %r8
	cmove	%rax, %r8
	xorl	%eax, %eax
	call	fprintf
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.L104
.L103:
	addq	$8, %rsp
	popq	%rbx
	popq	%rbp
	ret
.L92:
	movl	$.LC2, %ecx
	jmp	.L100
.L93:
	movl	$.LC3, %ecx
	jmp	.L100
.L94:
	movl	$.LC0, %ecx
	jmp	.L100
.L95:
	movl	$.LC7, %ecx
	jmp	.L100
.L96:
	movl	$.LC4, %ecx
	jmp	.L100
.L97:
	movl	$.LC5, %ecx
	jmp	.L100
.L98:
	movl	$.LC6, %ecx
	jmp	.L100
.LFE6:
	.size	PrintTokens, .-PrintTokens
	.p2align 4,,15
.globl DeleteInternedSymbols
	.type	DeleteInternedSymbols, @function
DeleteInternedSymbols:
.LFB30:
	movq	symbol_hash(%rip), %rdi
	jmp	g_hash_table_unref
.LFE30:
	.size	DeleteInternedSymbols, .-DeleteInternedSymbols
	.p2align 4,,15
.globl FinishRuntime
	.type	FinishRuntime, @function
FinishRuntime:
.LFB15:
	jmp	DeleteInternedSymbols
.LFE15:
	.size	FinishRuntime, .-FinishRuntime
	.p2align 4,,15
.globl VyFree
	.type	VyFree, @function
VyFree:
.LFB13:
	jmp	free
.LFE13:
	.size	VyFree, .-VyFree
	.p2align 4,,15
.globl DeleteScope
	.type	DeleteScope, @function
DeleteScope:
.LFB79:
	pushq	%rbx
.LCFI45:
	movq	%rdi, %rbx
	movq	8(%rdi), %rdi
	call	g_hash_table_destroy
	movq	16(%rbx), %rdi
	call	g_hash_table_destroy
	movq	24(%rbx), %rdi
	call	g_hash_table_destroy
	movq	%rbx, %rdi
	popq	%rbx
	jmp	VyFree
.LFE79:
	.size	DeleteScope, .-DeleteScope
	.p2align 4,,15
.globl FreeBytecode
	.type	FreeBytecode, @function
FreeBytecode:
.LFB62:
	pushq	%rbx
.LCFI46:
	movq	%rdi, %rbx
	movq	8(%rdi), %rdi
	call	VyFree
	movq	%rbx, %rdi
	popq	%rbx
	jmp	VyFree
.LFE62:
	.size	FreeBytecode, .-FreeBytecode
	.p2align 4,,15
.globl FreeTokens
	.type	FreeTokens, @function
FreeTokens:
.LFB7:
	pushq	%rbp
.LCFI47:
	movq	%rdi, %rbp
	pushq	%rbx
.LCFI48:
	subq	$8, %rsp
.LCFI49:
	testq	%rdi, %rdi
	je	.L121
	.p2align 4,,7
.L122:
	movq	8(%rbp), %rdi
	testq	%rdi, %rdi
	je	.L119
	call	VyFree
.L119:
	movq	24(%rbp), %rbx
	movq	%rbp, %rdi
	call	VyFree
	testq	%rbx, %rbx
	movq	%rbx, %rbp
	jne	.L122
.L121:
	addq	$8, %rsp
	popq	%rbx
	popq	%rbp
	ret
.LFE7:
	.size	FreeTokens, .-FreeTokens
	.p2align 4,,15
.globl VyRealloc
	.type	VyRealloc, @function
VyRealloc:
.LFB12:
	jmp	realloc
.LFE12:
	.size	VyRealloc, .-VyRealloc
	.p2align 4,,15
.globl ExpandBytecode
	.type	ExpandBytecode, @function
ExpandBytecode:
.LFB63:
	pushq	%rbx
.LCFI50:
	movl	(%rdi), %esi
	movq	%rdi, %rbx
	addl	%esi, %esi
	movl	%esi, (%rdi)
	movq	8(%rdi), %rdi
	movslq	%esi,%rsi
	salq	$5, %rsi
	call	VyRealloc
	movq	%rax, 8(%rbx)
	popq	%rbx
	ret
.LFE63:
	.size	ExpandBytecode, .-ExpandBytecode
	.p2align 4,,15
.globl EmitInstruction
	.type	EmitInstruction, @function
EmitInstruction:
.LFB64:
	movq	%rbx, -16(%rsp)
.LCFI51:
	movq	%rbp, -8(%rsp)
.LCFI52:
	subq	$24, %rsp
.LCFI53:
	movl	4(%rdi), %ebp
	movq	%rdi, %rbx
	leal	1(%rbp), %eax
	cmpl	(%rdi), %eax
	movl	%eax, 4(%rdi)
	jle	.L129
	call	ExpandBytecode
.L129:
	movq	8(%rbx), %rcx
	movq	32(%rsp), %rax
	movslq	%ebp,%rdx
	salq	$5, %rdx
	movq	%rax, (%rdx,%rcx)
	movq	40(%rsp), %rax
	movq	%rax, 8(%rdx,%rcx)
	movq	48(%rsp), %rax
	movq	%rax, 16(%rdx,%rcx)
	movq	56(%rsp), %rax
	movq	%rax, 24(%rdx,%rcx)
	movq	8(%rsp), %rbx
	movq	16(%rsp), %rbp
	addq	$24, %rsp
	ret
.LFE64:
	.size	EmitInstruction, .-EmitInstruction
	.p2align 4,,15
.globl ExpandStack
	.type	ExpandStack, @function
ExpandStack:
.LFB57:
	subq	$8, %rsp
.LCFI54:
	movl	stack_size(%rip), %esi
	movq	stack(%rip), %rdi
	addl	%esi, %esi
	movl	%esi, stack_size(%rip)
	movslq	%esi,%rsi
	leaq	(%rsi,%rsi,2), %rsi
	salq	$3, %rsi
	call	VyRealloc
	movq	%rax, stack(%rip)
	addq	$8, %rsp
	ret
.LFE57:
	.size	ExpandStack, .-ExpandStack
	.p2align 4,,15
.globl VyMalloc
	.type	VyMalloc, @function
VyMalloc:
.LFB11:
	jmp	malloc
.LFE11:
	.size	VyMalloc, .-VyMalloc
	.p2align 4,,15
.globl CreateInt
	.type	CreateInt, @function
CreateInt:
.LFB82:
	movq	%rbx, -16(%rsp)
.LCFI55:
	movq	%r12, -8(%rsp)
.LCFI56:
	subq	$24, %rsp
.LCFI57:
	movq	%rdi, %r12
	movslq	TypeInt(%rip),%rdi
	movl	%esi, %ebx
	call	VyMalloc
	movl	%ebx, (%rax)
	movl	TypeInt(%rip), %edx
	movq	%rax, %rsi
	movq	TypeInt+8(%rip), %rcx
	movq	%r12, %rdi
	call	WrapObj
	movq	%r12, %rax
	movq	8(%rsp), %rbx
	movq	16(%rsp), %r12
	addq	$24, %rsp
	ret
.LFE82:
	.size	CreateInt, .-CreateInt
	.p2align 4,,15
.globl CreateIntFromStr
	.type	CreateIntFromStr, @function
CreateIntFromStr:
.LFB84:
	pushq	%rbx
.LCFI58:
	movq	%rdi, %rbx
	movq	%rsi, %rdi
	call	atoi
	movq	%rbx, %rdi
	movl	%eax, %esi
	call	CreateInt
	movq	%rbx, %rax
	popq	%rbx
	ret
.LFE84:
	.size	CreateIntFromStr, .-CreateIntFromStr
	.p2align 4,,15
.globl CreateFloat
	.type	CreateFloat, @function
CreateFloat:
.LFB81:
	movq	%rbx, -16(%rsp)
.LCFI59:
	movq	%r12, -8(%rsp)
.LCFI60:
	subq	$24, %rsp
.LCFI61:
	movq	%rdi, %r12
	movslq	TypeFloat(%rip),%rdi
	movsd	%xmm0, (%rsp)
	movq	(%rsp), %rbx
	call	VyMalloc
	movl	TypeFloat(%rip), %edx
	movq	TypeFloat+8(%rip), %rcx
	movq	%rax, %rsi
	movq	%rbx, (%rax)
	movq	%r12, %rdi
	call	WrapObj
	movq	%r12, %rax
	movq	8(%rsp), %rbx
	movq	16(%rsp), %r12
	addq	$24, %rsp
	ret
.LFE81:
	.size	CreateFloat, .-CreateFloat
	.p2align 4,,15
.globl DivFun
	.type	DivFun, @function
DivFun:
.LFB95:
	movq	%rbx, -48(%rsp)
.LCFI62:
	movq	%rbp, -40(%rsp)
.LCFI63:
	movq	%rsi, %rbx
	movq	%r12, -32(%rsp)
.LCFI64:
	movq	%r13, -24(%rsp)
.LCFI65:
	movq	%r14, -16(%rsp)
.LCFI66:
	movq	%r15, -8(%rsp)
.LCFI67:
	subq	$104, %rsp
.LCFI68:
	movq	(%rsi), %r13
	movl	TypeInt(%rip), %eax
	movq	%rdi, 40(%rsp)
	movq	%r13, (%rsp)
	movq	8(%rsi), %r12
	movl	%eax, 28(%rsp)
	movq	TypeInt+8(%rip), %rax
	movl	28(%rsp), %edi
	movq	%r12, 8(%rsp)
	movq	16(%rsi), %rbp
	movq	%rax, %rsi
	movq	%rax, 32(%rsp)
	movq	%rbp, 16(%rsp)
	call	IsType
	testl	%eax, %eax
	jne	.L152
.L143:
	movl	TypeFloat(%rip), %r14d
	movq	TypeFloat+8(%rip), %r15
	movq	%r13, (%rsp)
	movq	%r12, 8(%rsp)
	movq	%rbp, 16(%rsp)
	movl	%r14d, %edi
	movq	%r15, %rsi
	call	IsType
	testl	%eax, %eax
	jne	.L153
.L145:
	movq	24(%rbx), %r12
	leaq	24(%rbx), %rax
	movl	28(%rsp), %edi
	movq	32(%rsp), %rsi
	movq	%r12, (%rsp)
	movq	8(%rax), %rbp
	movq	%rbp, 8(%rsp)
	movq	16(%rax), %rbx
	movq	%rbx, 16(%rsp)
	call	IsType
	testl	%eax, %eax
	jne	.L154
.L147:
	movl	%r14d, %edi
	movq	%r15, %rsi
	movq	%r12, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%rbx, 16(%rsp)
	call	IsType
	testl	%eax, %eax
	je	.L149
	movq	%r12, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%rbx, 16(%rsp)
	call	Obj
	cvtsd2ss	(%rax), %xmm0
	movss	%xmm0, 52(%rsp)
.L149:
	movss	48(%rsp), %xmm0
	movq	40(%rsp), %rdi
	divss	52(%rsp), %xmm0
	movss	%xmm0, 48(%rsp)
	cvtss2sd	%xmm0, %xmm0
	call	CreateFloat
	movq	40(%rsp), %rax
	movq	56(%rsp), %rbx
	movq	64(%rsp), %rbp
	movq	72(%rsp), %r12
	movq	80(%rsp), %r13
	movq	88(%rsp), %r14
	movq	96(%rsp), %r15
	addq	$104, %rsp
	ret
	.p2align 4,,7
.L154:
	movq	%r12, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%rbx, 16(%rsp)
	call	Obj
	cvtsi2ss	(%rax), %xmm0
	movss	%xmm0, 52(%rsp)
	jmp	.L147
	.p2align 4,,7
.L153:
	movq	%r13, (%rsp)
	movq	%r12, 8(%rsp)
	movq	%rbp, 16(%rsp)
	call	Obj
	cvtsd2ss	(%rax), %xmm0
	movss	%xmm0, 48(%rsp)
	jmp	.L145
	.p2align 4,,7
.L152:
	movq	%r13, (%rsp)
	movq	%r12, 8(%rsp)
	movq	%rbp, 16(%rsp)
	call	Obj
	cvtsi2ss	(%rax), %xmm0
	movss	%xmm0, 48(%rsp)
	jmp	.L143
.LFE95:
	.size	DivFun, .-DivFun
	.section	.rodata.cst8,"aM",@progbits,8
	.align 8
.LC9:
	.long	0
	.long	1072693248
	.text
	.p2align 4,,15
.globl MulFun
	.type	MulFun, @function
MulFun:
.LFB94:
	pushq	%r15
.LCFI69:
	movl	$1, %eax
	pushq	%r14
.LCFI70:
	pushq	%r13
.LCFI71:
	pushq	%r12
.LCFI72:
	pushq	%rbp
.LCFI73:
	pushq	%rbx
.LCFI74:
	subq	$152, %rsp
.LCFI75:
	testl	%edx, %edx
	movq	%rdi, 56(%rsp)
	movl	%edx, 52(%rsp)
	jle	.L158
	movl	TypeInt(%rip), %eax
	movq	TypeInt+8(%rip), %rdx
	movq	%rsi, %r13
	movq	TypeFloat+8(%rip), %r15
	movsd	.LC9(%rip), %xmm0
	xorl	%r14d, %r14d
	movl	$1, 76(%rsp)
	movl	%eax, 48(%rsp)
	movl	TypeFloat(%rip), %eax
	movsd	%xmm0, 64(%rsp)
	movq	%rdx, 32(%rsp)
	movl	%eax, 44(%rsp)
	jmp	.L159
	.p2align 4,,7
.L160:
	movl	44(%rsp), %edi
	movq	%r15, %rsi
	movq	%r12, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%rbx, 16(%rsp)
	call	IsType
	testl	%eax, %eax
	jne	.L171
	addl	$1, %r14d
	addq	$24, %r13
	cmpl	52(%rsp), %r14d
	je	.L172
.L159:
	movq	(%r13), %r12
	movl	48(%rsp), %edi
	movq	32(%rsp), %rsi
	movq	%r12, (%rsp)
	movq	8(%r13), %rbp
	movq	%rbp, 8(%rsp)
	movq	16(%r13), %rbx
	movq	%rbx, 16(%rsp)
	call	IsType
	testl	%eax, %eax
	je	.L160
	movq	%r12, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%rbx, 16(%rsp)
	call	Obj
	cvtsi2sd	(%rax), %xmm0
	mulsd	64(%rsp), %xmm0
	movsd	%xmm0, 64(%rsp)
	jmp	.L160
	.p2align 4,,7
.L171:
	movq	%r12, (%rsp)
	movq	%rbp, 8(%rsp)
	addl	$1, %r14d
	movq	%rbx, 16(%rsp)
	addq	$24, %r13
	call	Obj
	movsd	64(%rsp), %xmm0
	cmpl	52(%rsp), %r14d
	movl	$0, 76(%rsp)
	mulsd	(%rax), %xmm0
	movsd	%xmm0, 64(%rsp)
	jne	.L159
.L172:
	movl	76(%rsp), %eax
	testl	%eax, %eax
	jne	.L173
	leaq	80(%rsp), %rdi
	movsd	64(%rsp), %xmm0
	call	CreateFloat
	movq	80(%rsp), %rax
	movq	56(%rsp), %rdx
	movq	%rax, (%rdx)
	movq	88(%rsp), %rax
	movq	%rax, 8(%rdx)
	movq	96(%rsp), %rax
	movq	%rax, 16(%rdx)
.L155:
	movq	56(%rsp), %rax
	addq	$152, %rsp
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
.L173:
	movsd	64(%rsp), %xmm0
	cvttsd2si	%xmm0, %eax
.L158:
	leaq	112(%rsp), %rdi
	movl	%eax, %esi
	call	CreateInt
	movq	112(%rsp), %rax
	movq	56(%rsp), %rdx
	movq	%rax, (%rdx)
	movq	120(%rsp), %rax
	movq	%rax, 8(%rdx)
	movq	128(%rsp), %rax
	movq	%rax, 16(%rdx)
	jmp	.L155
.LFE94:
	.size	MulFun, .-MulFun
	.p2align 4,,15
.globl SubFun
	.type	SubFun, @function
SubFun:
.LFB93:
	pushq	%r15
.LCFI76:
	movl	%edx, %r15d
	pushq	%r14
.LCFI77:
	pushq	%r13
.LCFI78:
	pushq	%r12
.LCFI79:
	movq	%rsi, %r12
	pushq	%rbp
.LCFI80:
	pushq	%rbx
.LCFI81:
	subq	$200, %rsp
.LCFI82:
	testl	%edx, %edx
	movq	%rdi, 72(%rsp)
	je	.L201
	cmpl	$1, %edx
	je	.L202
	movq	(%rsi), %r13
	movl	TypeInt(%rip), %eax
	movq	TypeInt+8(%rip), %rdx
	movq	%r13, (%rsp)
	movq	8(%rsi), %rbp
	movl	%eax, %edi
	movl	%eax, 52(%rsp)
	movq	%rdx, 56(%rsp)
	movq	%rbp, 8(%rsp)
	movq	16(%rsi), %rbx
	movq	%rdx, %rsi
	movq	%rbx, 16(%rsp)
	call	IsType
	testl	%eax, %eax
	movl	%eax, %r14d
	jne	.L184
	xorpd	%xmm0, %xmm0
	movsd	%xmm0, 80(%rsp)
.L186:
	movl	TypeFloat(%rip), %eax
	movq	TypeFloat+8(%rip), %rdx
	movq	%r13, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%rbx, 16(%rsp)
	movl	%eax, %edi
	movq	%rdx, %rsi
	movl	%eax, 68(%rsp)
	movq	%rdx, 40(%rsp)
	call	IsType
	testl	%eax, %eax
	jne	.L203
.L187:
	cmpl	$1, %r15d
	movl	%r14d, 92(%rsp)
	jle	.L191
	leaq	24(%r12), %r13
	movl	$1, %r14d
	jmp	.L192
	.p2align 4,,7
.L193:
	movl	68(%rsp), %edi
	movq	40(%rsp), %rsi
	movq	%r12, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%rbx, 16(%rsp)
	call	IsType
	testl	%eax, %eax
	jne	.L204
	addl	$1, %r14d
	addq	$24, %r13
	cmpl	%r15d, %r14d
	je	.L191
.L192:
	movq	(%r13), %r12
	movl	52(%rsp), %edi
	movq	56(%rsp), %rsi
	movq	%r12, (%rsp)
	movq	8(%r13), %rbp
	movq	%rbp, 8(%rsp)
	movq	16(%r13), %rbx
	movq	%rbx, 16(%rsp)
	call	IsType
	testl	%eax, %eax
	je	.L193
	movq	%r12, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%rbx, 16(%rsp)
	call	Obj
	cvtsi2sd	(%rax), %xmm0
	movsd	80(%rsp), %xmm1
	subsd	%xmm0, %xmm1
	movsd	%xmm1, 80(%rsp)
	jmp	.L193
.L202:
	movq	(%r12), %r13
	movl	TypeInt(%rip), %edi
	movq	TypeInt+8(%rip), %rsi
	movq	%r13, (%rsp)
	movq	8(%r12), %rbp
	movq	%rbp, 8(%rsp)
	movq	16(%r12), %rbx
	movq	%rbx, 16(%rsp)
	call	IsType
	testl	%eax, %eax
	jne	.L205
.L180:
	movl	TypeFloat(%rip), %edi
	movq	TypeFloat+8(%rip), %rsi
	movq	%r13, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%rbx, 16(%rsp)
	call	IsType
	testl	%eax, %eax
	jne	.L206
.L182:
	movq	72(%rsp), %rdi
	movl	$24, %edx
	movq	%r12, %rsi
	call	memmove
	.p2align 4,,7
.L174:
	movq	72(%rsp), %rax
	addq	$200, %rsp
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
	.p2align 4,,7
.L204:
	movq	%r12, (%rsp)
	movq	%rbp, 8(%rsp)
	addl	$1, %r14d
	movq	%rbx, 16(%rsp)
	addq	$24, %r13
	call	Obj
	movsd	80(%rsp), %xmm0
	cmpl	%r15d, %r14d
	movl	$0, 92(%rsp)
	subsd	(%rax), %xmm0
	movsd	%xmm0, 80(%rsp)
	jne	.L192
	.p2align 4,,7
.L191:
	movl	92(%rsp), %eax
	testl	%eax, %eax
	je	.L197
	movsd	80(%rsp), %xmm0
	leaq	128(%rsp), %rdi
	cvttsd2si	%xmm0, %esi
	call	CreateInt
	movq	128(%rsp), %rax
	movq	72(%rsp), %rdx
	movq	%rax, (%rdx)
	movq	136(%rsp), %rax
	movq	%rax, 8(%rdx)
	movq	144(%rsp), %rax
	movq	%rax, 16(%rdx)
	jmp	.L174
.L201:
	leaq	160(%rsp), %rdi
	xorl	%esi, %esi
	call	CreateInt
	movq	160(%rsp), %rax
	movq	72(%rsp), %rdx
	movq	%rax, (%rdx)
	movq	168(%rsp), %rax
	movq	%rax, 8(%rdx)
	movq	176(%rsp), %rax
	movq	%rax, 16(%rdx)
	jmp	.L174
.L197:
	leaq	96(%rsp), %rdi
	movsd	80(%rsp), %xmm0
	call	CreateFloat
	movq	96(%rsp), %rax
	movq	72(%rsp), %rdx
	movq	%rax, (%rdx)
	movq	104(%rsp), %rax
	movq	%rax, 8(%rdx)
	movq	112(%rsp), %rax
	movq	%rax, 16(%rdx)
	jmp	.L174
.L206:
	movq	%r13, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%rbx, 16(%rsp)
	call	Obj
	xorb	$-128, 7(%rax)
	jmp	.L182
.L205:
	movq	%r13, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%rbx, 16(%rsp)
	call	Obj
	negl	(%rax)
	movq	(%r12), %r13
	movq	8(%r12), %rbp
	movq	16(%r12), %rbx
	jmp	.L180
.L203:
	movq	%r13, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%rbx, 16(%rsp)
	call	Obj
	movsd	(%rax), %xmm0
	movsd	%xmm0, 80(%rsp)
	jmp	.L187
.L184:
	movq	%r13, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%rbx, 16(%rsp)
	call	Obj
	cvtsi2sd	(%rax), %xmm1
	movsd	%xmm1, 80(%rsp)
	jmp	.L186
.LFE93:
	.size	SubFun, .-SubFun
	.p2align 4,,15
.globl AddFun
	.type	AddFun, @function
AddFun:
.LFB92:
	pushq	%r15
.LCFI83:
	xorl	%eax, %eax
	pushq	%r14
.LCFI84:
	pushq	%r13
.LCFI85:
	pushq	%r12
.LCFI86:
	pushq	%rbp
.LCFI87:
	pushq	%rbx
.LCFI88:
	subq	$152, %rsp
.LCFI89:
	testl	%edx, %edx
	movq	%rdi, 56(%rsp)
	movl	%edx, 52(%rsp)
	jle	.L210
	movl	TypeInt(%rip), %eax
	movq	TypeInt+8(%rip), %rdx
	movq	%rsi, %r13
	movq	TypeFloat+8(%rip), %r15
	xorpd	%xmm0, %xmm0
	xorl	%r14d, %r14d
	movl	$1, 76(%rsp)
	movl	%eax, 48(%rsp)
	movl	TypeFloat(%rip), %eax
	movsd	%xmm0, 64(%rsp)
	movq	%rdx, 32(%rsp)
	movl	%eax, 44(%rsp)
	jmp	.L211
	.p2align 4,,7
.L212:
	movl	44(%rsp), %edi
	movq	%r15, %rsi
	movq	%r12, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%rbx, 16(%rsp)
	call	IsType
	testl	%eax, %eax
	jne	.L223
	addl	$1, %r14d
	addq	$24, %r13
	cmpl	52(%rsp), %r14d
	je	.L224
.L211:
	movq	(%r13), %r12
	movl	48(%rsp), %edi
	movq	32(%rsp), %rsi
	movq	%r12, (%rsp)
	movq	8(%r13), %rbp
	movq	%rbp, 8(%rsp)
	movq	16(%r13), %rbx
	movq	%rbx, 16(%rsp)
	call	IsType
	testl	%eax, %eax
	je	.L212
	movq	%r12, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%rbx, 16(%rsp)
	call	Obj
	cvtsi2sd	(%rax), %xmm0
	addsd	64(%rsp), %xmm0
	movsd	%xmm0, 64(%rsp)
	jmp	.L212
	.p2align 4,,7
.L223:
	movq	%r12, (%rsp)
	movq	%rbp, 8(%rsp)
	addl	$1, %r14d
	movq	%rbx, 16(%rsp)
	addq	$24, %r13
	call	Obj
	movsd	64(%rsp), %xmm0
	cmpl	52(%rsp), %r14d
	movl	$0, 76(%rsp)
	addsd	(%rax), %xmm0
	movsd	%xmm0, 64(%rsp)
	jne	.L211
.L224:
	movl	76(%rsp), %eax
	testl	%eax, %eax
	jne	.L225
	leaq	80(%rsp), %rdi
	movsd	64(%rsp), %xmm0
	call	CreateFloat
	movq	80(%rsp), %rax
	movq	56(%rsp), %rdx
	movq	%rax, (%rdx)
	movq	88(%rsp), %rax
	movq	%rax, 8(%rdx)
	movq	96(%rsp), %rax
	movq	%rax, 16(%rdx)
.L207:
	movq	56(%rsp), %rax
	addq	$152, %rsp
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
.L225:
	movsd	64(%rsp), %xmm0
	cvttsd2si	%xmm0, %eax
.L210:
	leaq	112(%rsp), %rdi
	movl	%eax, %esi
	call	CreateInt
	movq	112(%rsp), %rax
	movq	56(%rsp), %rdx
	movq	%rax, (%rdx)
	movq	120(%rsp), %rax
	movq	%rax, 8(%rdx)
	movq	128(%rsp), %rax
	movq	%rax, 16(%rdx)
	jmp	.L207
.LFE92:
	.size	AddFun, .-AddFun
	.p2align 4,,15
.globl CreateFloatFromStr
	.type	CreateFloatFromStr, @function
CreateFloatFromStr:
.LFB83:
	pushq	%rbx
.LCFI90:
	movq	%rdi, %rbx
	movq	%rsi, %rdi
	call	atof
	movq	%rbx, %rdi
	call	CreateFloat
	movq	%rbx, %rax
	popq	%rbx
	ret
.LFE83:
	.size	CreateFloatFromStr, .-CreateFloatFromStr
	.p2align 4,,15
.globl CreateScope
	.type	CreateScope, @function
CreateScope:
.LFB74:
	pushq	%r12
.LCFI91:
	movq	%rdi, %r12
	movl	$32, %edi
	pushq	%rbx
.LCFI92:
	subq	$8, %rsp
.LCFI93:
	call	VyMalloc
	movl	$g_str_equal, %esi
	movq	%rax, %rbx
	movq	%r12, (%rax)
	movl	$g_str_hash, %edi
	call	g_hash_table_new
	movl	$g_str_equal, %esi
	movq	%rax, 8(%rbx)
	movl	$g_str_hash, %edi
	call	g_hash_table_new
	movl	$g_str_equal, %esi
	movq	%rax, 16(%rbx)
	movl	$g_str_hash, %edi
	call	g_hash_table_new
	movq	%rax, 24(%rbx)
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	ret
.LFE74:
	.size	CreateScope, .-CreateScope
	.p2align 4,,15
.globl CurrentScope
	.type	CurrentScope, @function
CurrentScope:
.LFB75:
	subq	$8, %rsp
.LCFI94:
	cmpq	$0, current_scope(%rip)
	je	.L234
	movq	current_scope(%rip), %rax
	addq	$8, %rsp
	ret
	.p2align 4,,7
.L234:
	xorl	%edi, %edi
	call	CreateScope
	movq	%rax, current_scope(%rip)
	movq	current_scope(%rip), %rax
	addq	$8, %rsp
	ret
.LFE75:
	.size	CurrentScope, .-CurrentScope
	.p2align 4,,15
.globl VariableBind
	.type	VariableBind, @function
VariableBind:
.LFB77:
	pushq	%r12
.LCFI95:
	movq	%rdi, %r12
	pushq	%rbx
.LCFI96:
	subq	$88, %rsp
.LCFI97:
	call	CurrentScope
	movq	%rax, %rbx
	movq	112(%rsp), %rax
	movq	%rax, (%rsp)
	movq	120(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	128(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	Obj
	movq	8(%rbx), %rdi
	movq	(%r12), %rsi
	movq	%rax, %rdx
	call	g_hash_table_insert
	movq	112(%rsp), %rax
	movq	%rax, (%rsp)
	movq	120(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	128(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	Type
	movq	16(%rbx), %rdi
	movq	(%r12), %rsi
	movl	%eax, 32(%rsp)
	movq	32(%rsp), %rax
	movq	%rdx, 40(%rsp)
	movq	%rdx, 72(%rsp)
	movq	%rax, 64(%rsp)
	call	g_hash_table_insert
	movq	112(%rsp), %rax
	movq	%rax, (%rsp)
	movq	120(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	128(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	Type
	movl	%eax, 32(%rsp)
	movq	32(%rsp), %rax
	movq	%rdx, 40(%rsp)
	movq	%rdx, 56(%rsp)
	movq	24(%rbx), %rdi
	movq	(%r12), %rsi
	movq	%rax, 48(%rsp)
	movslq	48(%rsp),%rdx
	addq	$88, %rsp
	popq	%rbx
	popq	%r12
	jmp	g_hash_table_insert
.LFE77:
	.size	VariableBind, .-VariableBind
	.p2align 4,,15
.globl BindInstr
	.type	BindInstr, @function
BindInstr:
.LFB51:
	movq	%rbx, -24(%rsp)
.LCFI98:
	movq	%r12, -16(%rsp)
.LCFI99:
	movq	%r13, -8(%rsp)
.LCFI100:
	subq	$184, %rsp
.LCFI101:
	leaq	96(%rsp), %rdi
	call	StackPop
	movq	96(%rsp), %rax
	leaq	32(%rsp), %rdi
	movq	%rax, 128(%rsp)
	movq	104(%rsp), %rax
	movq	%rax, 136(%rsp)
	movq	112(%rsp), %rax
	movq	%rax, 144(%rsp)
	call	StackPeek
	movq	128(%rsp), %rax
	movq	32(%rsp), %r13
	movq	40(%rsp), %r12
	movq	48(%rsp), %rbx
	movq	%rax, (%rsp)
	movq	136(%rsp), %rax
	movq	%r13, 64(%rsp)
	movq	%r12, 72(%rsp)
	movq	%rbx, 80(%rsp)
	movq	%rax, 8(%rsp)
	movq	144(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	Obj
	movq	%rax, %rdi
	movq	%r13, (%rsp)
	movq	%r12, 8(%rsp)
	movq	%rbx, 16(%rsp)
	call	VariableBind
	movq	160(%rsp), %rbx
	movq	168(%rsp), %r12
	movq	176(%rsp), %r13
	addq	$184, %rsp
	ret
.LFE51:
	.size	BindInstr, .-BindInstr
	.section	.rodata.str1.1
.LC13:
	.string	"Failed to find: %s\n"
	.text
	.p2align 4,,15
.globl VariableValue
	.type	VariableValue, @function
VariableValue:
.LFB76:
	pushq	%r14
.LCFI102:
	movq	%rdi, %r14
	pushq	%r13
.LCFI103:
	movq	%rsi, %r13
	pushq	%r12
.LCFI104:
	pushq	%rbp
.LCFI105:
	pushq	%rbx
.LCFI106:
	subq	$48, %rsp
.LCFI107:
	call	CurrentScope
	movq	%rax, %rbp
	jmp	.L252
	.p2align 4,,7
.L254:
	movq	(%rbp), %rbp
	testq	%rbp, %rbp
	je	.L253
.L252:
	movq	8(%rbp), %rdi
	movq	(%r13), %rsi
	call	g_hash_table_lookup
	testq	%rax, %rax
	je	.L254
	movq	8(%rbp), %rdi
	movq	(%r13), %rsi
	call	g_hash_table_lookup
	movq	16(%rbp), %rdi
	movq	(%r13), %rsi
	movq	%rax, %r12
	call	g_hash_table_lookup
	movq	24(%rbp), %rdi
	movq	(%r13), %rsi
	movq	%rax, %rbx
	call	g_hash_table_lookup
	movq	%rsp, %rdi
	movl	%eax, %edx
	movq	%rbx, %rcx
	movq	%r12, %rsi
	movl	%eax, 32(%rsp)
	movq	%rbx, 40(%rsp)
	call	WrapObj
	movq	16(%rsp), %rax
	movq	%rax, 16(%r14)
	movq	8(%rsp), %rax
	movq	%rax, 8(%r14)
	movl	(%rsp), %eax
	movl	%eax, (%r14)
	addq	$48, %rsp
	movq	%r14, %rax
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	ret
.L253:
	movq	(%r13), %rsi
	movl	$.LC13, %edi
	call	printf
	movq	stdout(%rip), %rdi
	call	fflush
	jmp	.L252
.LFE76:
	.size	VariableValue, .-VariableValue
	.p2align 4,,15
.globl FuncInstr
	.type	FuncInstr, @function
FuncInstr:
.LFB53:
	pushq	%rbx
.LCFI108:
	subq	$96, %rsp
.LCFI109:
	leaq	32(%rsp), %rdi
	call	StackPeek
	movq	32(%rsp), %rcx
	movq	40(%rsp), %rdx
	movq	48(%rsp), %rax
	movq	%rcx, 64(%rsp)
	movq	%rdx, 72(%rsp)
	movq	%rcx, (%rsp)
	movq	%rdx, 8(%rsp)
	movq	%rax, 80(%rsp)
	movq	%rax, 16(%rsp)
	call	Obj
	movq	%rax, %rbx
	call	CurrentScope
	movq	%rax, 32(%rbx)
	addq	$96, %rsp
	popq	%rbx
	ret
.LFE53:
	.size	FuncInstr, .-FuncInstr
	.p2align 4,,15
.globl CreateBytecode
	.type	CreateBytecode, @function
CreateBytecode:
.LFB61:
	pushq	%rbx
.LCFI110:
	movl	$16, %edi
	call	VyMalloc
	movl	$32768, %edi
	movq	%rax, %rbx
	movl	$1024, (%rax)
	movl	$0, 4(%rax)
	call	VyMalloc
	movq	%rax, 8(%rbx)
	movq	%rbx, %rax
	popq	%rbx
	ret
.LFE61:
	.size	CreateBytecode, .-CreateBytecode
	.p2align 4,,15
.globl InitStack
	.type	InitStack, @function
InitStack:
.LFB56:
	subq	$8, %rsp
.LCFI111:
	movslq	stack_size(%rip),%rdi
	leaq	(%rdi,%rdi,2), %rdi
	salq	$3, %rdi
	call	VyMalloc
	movl	$0, stack_index(%rip)
	movq	%rax, stack(%rip)
	addq	$8, %rsp
	ret
.LFE56:
	.size	InitStack, .-InitStack
	.p2align 4,,15
.globl StackPush
	.type	StackPush, @function
StackPush:
.LFB58:
	movq	%rbx, -24(%rsp)
.LCFI112:
	movq	%rbp, -16(%rsp)
.LCFI113:
	movq	%r12, -8(%rsp)
.LCFI114:
	subq	$24, %rsp
.LCFI115:
	cmpq	$0, stack(%rip)
	movq	48(%rsp), %rbx
	movq	40(%rsp), %rbp
	movl	32(%rsp), %r12d
	je	.L267
	movl	stack_index(%rip), %eax
	cmpl	stack_size(%rip), %eax
	je	.L268
.L264:
	cltq
	leaq	(%rax,%rax,2), %rax
	salq	$3, %rax
	addq	stack(%rip), %rax
	movl	%r12d, (%rax)
	addl	$1, stack_index(%rip)
	movq	%rbx, 16(%rax)
	movq	%rbp, 8(%rax)
	movq	(%rsp), %rbx
	movq	8(%rsp), %rbp
	movq	16(%rsp), %r12
	addq	$24, %rsp
	ret
	.p2align 4,,7
.L268:
	call	ExpandStack
	movl	stack_index(%rip), %eax
	jmp	.L264
.L267:
	call	InitStack
	movl	stack_index(%rip), %eax
	jmp	.L264
.LFE58:
	.size	StackPush, .-StackPush
	.p2align 4,,15
.globl ValueInstr
	.type	ValueInstr, @function
ValueInstr:
.LFB52:
	subq	$104, %rsp
.LCFI116:
	movq	112(%rsp), %rax
	movq	%rax, (%rsp)
	movq	120(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	128(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	Obj
	leaq	32(%rsp), %rdi
	movq	%rax, %rsi
	call	VariableValue
	movq	32(%rsp), %rcx
	movq	40(%rsp), %rdx
	movq	48(%rsp), %rax
	movq	%rcx, 64(%rsp)
	movq	%rdx, 72(%rsp)
	movq	%rax, 80(%rsp)
	movq	%rcx, 112(%rsp)
	movq	%rdx, 120(%rsp)
	movq	%rax, 128(%rsp)
	addq	$104, %rsp
	jmp	StackPush
.LFE52:
	.size	ValueInstr, .-ValueInstr
	.p2align 4,,15
.globl PushInstr
	.type	PushInstr, @function
PushInstr:
.LFB49:
	jmp	StackPush
.LFE49:
	.size	PushInstr, .-PushInstr
	.p2align 4,,15
.globl CreateNativeFunction
	.type	CreateNativeFunction, @function
CreateNativeFunction:
.LFB32:
	movq	%rbx, -24(%rsp)
.LCFI117:
	movq	%r12, -16(%rsp)
.LCFI118:
	movq	%rsi, %rbx
	movq	%r13, -8(%rsp)
.LCFI119:
	subq	$40, %rsp
.LCFI120:
	movl	%edi, %r12d
	movq	%rdx, %r13
	movl	%edi, (%rsp)
	movl	$40, %edi
	movq	%rsi, 8(%rsp)
	call	VyMalloc
	movq	%rbx, 8(%rax)
	movl	%r12d, (%rax)
	movq	%r13, 16(%rax)
	movl	$1, 24(%rax)
	movq	16(%rsp), %rbx
	movq	24(%rsp), %r12
	movq	32(%rsp), %r13
	addq	$40, %rsp
	ret
.LFE32:
	.size	CreateNativeFunction, .-CreateNativeFunction
	.p2align 4,,15
.globl CreateFunction
	.type	CreateFunction, @function
CreateFunction:
.LFB31:
	movq	%rbx, -24(%rsp)
.LCFI121:
	movq	%r12, -16(%rsp)
.LCFI122:
	movq	%rsi, %rbx
	movq	%r13, -8(%rsp)
.LCFI123:
	subq	$40, %rsp
.LCFI124:
	movl	%edi, %r12d
	movq	%rdx, %r13
	movl	%edi, (%rsp)
	movl	$40, %edi
	movq	%rsi, 8(%rsp)
	call	VyMalloc
	movq	%rbx, 8(%rax)
	movl	%r12d, (%rax)
	movq	%r13, 16(%rax)
	movl	$0, 24(%rax)
	movq	16(%rsp), %rbx
	movq	24(%rsp), %r12
	movq	32(%rsp), %r13
	addq	$40, %rsp
	ret
.LFE31:
	.size	CreateFunction, .-CreateFunction
	.p2align 4,,15
.globl CreateSymbol_NoObj
	.type	CreateSymbol_NoObj, @function
CreateSymbol_NoObj:
.LFB27:
	movq	%rbp, -8(%rsp)
.LCFI125:
	movq	%rbx, -16(%rsp)
.LCFI126:
	subq	$24, %rsp
.LCFI127:
	cmpq	$0, symbol_hash(%rip)
	movq	%rdi, %rbp
	je	.L284
.L278:
	movq	symbol_hash(%rip), %rdi
	movq	%rbp, %rsi
	call	g_hash_table_lookup
	testq	%rax, %rax
	movq	%rax, %rbx
	je	.L285
.L282:
	movq	%rbx, %rax
	movq	16(%rsp), %rbp
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	ret
	.p2align 4,,7
.L285:
	movl	$8, %edi
	call	VyMalloc
	movq	%rbp, %rdi
	movq	%rax, %rbx
	call	strdup
	movq	symbol_hash(%rip), %rdi
	movq	%rax, (%rbx)
	movq	%rbx, %rdx
	movq	%rbp, %rsi
	call	g_hash_table_insert
	jmp	.L282
.L284:
	movl	$g_str_equal, %esi
	movl	$g_str_hash, %edi
	call	g_hash_table_new
	movq	%rax, symbol_hash(%rip)
	jmp	.L278
.LFE27:
	.size	CreateSymbol_NoObj, .-CreateSymbol_NoObj
	.section	.rodata.str1.1
.LC14:
	.string	"cons"
.LC15:
	.string	"string"
.LC16:
	.string	"symbol"
.LC17:
	.string	"float"
.LC18:
	.string	"int"
.LC19:
	.string	"function"
	.text
	.p2align 4,,15
.globl CreateTypes
	.type	CreateTypes, @function
CreateTypes:
.LFB85:
	subq	$24, %rsp
.LCFI128:
	xorl	%esi, %esi
	xorl	%edi, %edi
	call	CreateType
	movl	%eax, (%rsp)
	movq	(%rsp), %rax
	movl	$.LC14, %edi
	movq	%rdx, 8(%rsp)
	movq	%rdx, TypeNone+8(%rip)
	movq	%rax, TypeNone(%rip)
	call	CreateSymbol_NoObj
	movl	$48, %edi
	movq	%rax, %rsi
	call	CreateType
	movl	%eax, (%rsp)
	movq	(%rsp), %rax
	movl	$.LC15, %edi
	movq	%rdx, 8(%rsp)
	movq	%rdx, TypeCons+8(%rip)
	movq	%rax, TypeCons(%rip)
	call	CreateSymbol_NoObj
	movl	$8, %edi
	movq	%rax, %rsi
	call	CreateType
	movl	%eax, (%rsp)
	movq	(%rsp), %rax
	movl	$.LC16, %edi
	movq	%rdx, 8(%rsp)
	movq	%rdx, TypeString+8(%rip)
	movq	%rax, TypeString(%rip)
	call	CreateSymbol_NoObj
	movl	$8, %edi
	movq	%rax, %rsi
	call	CreateType
	movl	%eax, (%rsp)
	movq	(%rsp), %rax
	movl	$.LC17, %edi
	movq	%rdx, 8(%rsp)
	movq	%rdx, TypeSymbol+8(%rip)
	movq	%rax, TypeSymbol(%rip)
	call	CreateSymbol_NoObj
	movl	$8, %edi
	movq	%rax, %rsi
	call	CreateType
	movl	%eax, (%rsp)
	movq	(%rsp), %rax
	movl	$.LC18, %edi
	movq	%rdx, 8(%rsp)
	movq	%rdx, TypeFloat+8(%rip)
	movq	%rax, TypeFloat(%rip)
	call	CreateSymbol_NoObj
	movl	$4, %edi
	movq	%rax, %rsi
	call	CreateType
	movl	%eax, (%rsp)
	movq	(%rsp), %rax
	movl	$.LC19, %edi
	movq	%rdx, 8(%rsp)
	movq	%rdx, TypeInt+8(%rip)
	movq	%rax, TypeInt(%rip)
	call	CreateSymbol_NoObj
	movl	$40, %edi
	movq	%rax, %rsi
	call	CreateType
	movl	%eax, (%rsp)
	movq	(%rsp), %rax
	movq	%rdx, TypeFunction+8(%rip)
	movq	%rax, TypeFunction(%rip)
	addq	$24, %rsp
	ret
.LFE85:
	.size	CreateTypes, .-CreateTypes
	.p2align 4,,15
.globl CreateSymbol
	.type	CreateSymbol, @function
CreateSymbol:
.LFB28:
	pushq	%rbx
.LCFI129:
	movq	%rdi, %rbx
	movq	%rsi, %rdi
	call	CreateSymbol_NoObj
	movl	TypeSymbol(%rip), %edx
	movq	TypeSymbol+8(%rip), %rcx
	movq	%rax, %rsi
	movq	%rbx, %rdi
	call	WrapObj
	movq	%rbx, %rax
	popq	%rbx
	ret
.LFE28:
	.size	CreateSymbol, .-CreateSymbol
	.section	.rodata.str1.1
.LC20:
	.string	"false"
	.text
	.p2align 4,,15
.globl FalseObj
	.type	FalseObj, @function
FalseObj:
.LFB106:
	pushq	%rbx
.LCFI130:
	movl	$.LC20, %esi
	movq	%rdi, %rbx
	call	CreateSymbol
	movq	%rbx, %rax
	popq	%rbx
	ret
.LFE106:
	.size	FalseObj, .-FalseObj
	.section	.rodata.str1.1
.LC21:
	.string	"true"
	.text
	.p2align 4,,15
.globl TrueObj
	.type	TrueObj, @function
TrueObj:
.LFB105:
	pushq	%rbx
.LCFI131:
	movl	$.LC21, %esi
	movq	%rdi, %rbx
	call	CreateSymbol
	movq	%rbx, %rax
	popq	%rbx
	ret
.LFE105:
	.size	TrueObj, .-TrueObj
	.p2align 4,,15
.globl EqFun
	.type	EqFun, @function
EqFun:
.LFB112:
	pushq	%r15
.LCFI132:
	movl	%edx, %r15d
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
	subq	$168, %rsp
.LCFI138:
	movq	(%rsi), %r14
	movq	8(%rsi), %r13
	movq	16(%rsi), %r12
	cmpl	$1, %edx
	movq	%rdi, 56(%rsp)
	movq	%r14, 128(%rsp)
	movq	%r13, 136(%rsp)
	movq	%r12, 144(%rsp)
	jle	.L295
	leaq	24(%rsi), %rbx
	movl	$1, %ebp
	jmp	.L297
	.p2align 4,,7
.L298:
	addl	$1, %ebp
	addq	$24, %rbx
	cmpl	%r15d, %ebp
	je	.L295
.L297:
	movq	(%rbx), %rax
	movq	%rax, 24(%rsp)
	movq	8(%rbx), %rax
	movq	%rax, 32(%rsp)
	movq	16(%rbx), %rax
	movq	%r14, (%rsp)
	movq	%r13, 8(%rsp)
	movq	%r12, 16(%rsp)
	movq	%rax, 40(%rsp)
	call	ObjEq
	testl	%eax, %eax
	jne	.L298
	leaq	96(%rsp), %rdi
	call	FalseObj
	movq	96(%rsp), %rax
	movq	56(%rsp), %rdx
	movq	%rax, (%rdx)
	movq	104(%rsp), %rax
	movq	%rax, 8(%rdx)
	movq	112(%rsp), %rax
	movq	%rax, 16(%rdx)
	jmp	.L294
.L295:
	leaq	64(%rsp), %rdi
	call	TrueObj
	movq	64(%rsp), %rax
	movq	56(%rsp), %rdx
	movq	%rax, (%rdx)
	movq	72(%rsp), %rax
	movq	%rax, 8(%rdx)
	movq	80(%rsp), %rax
	movq	%rax, 16(%rdx)
.L294:
	movq	56(%rsp), %rax
	addq	$168, %rsp
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
.LFE112:
	.size	EqFun, .-EqFun
	.p2align 4,,15
.globl NumEqFun
	.type	NumEqFun, @function
NumEqFun:
.LFB97:
	pushq	%r15
.LCFI139:
	movl	%edx, %r15d
	pushq	%r14
.LCFI140:
	pushq	%r13
.LCFI141:
	pushq	%r12
.LCFI142:
	pushq	%rbp
.LCFI143:
	pushq	%rbx
.LCFI144:
	subq	$168, %rsp
.LCFI145:
	movq	(%rsi), %r14
	movq	8(%rsi), %r13
	movq	16(%rsi), %r12
	cmpl	$1, %edx
	movq	%rdi, 56(%rsp)
	movq	%r14, 128(%rsp)
	movq	%r13, 136(%rsp)
	movq	%r12, 144(%rsp)
	jle	.L304
	leaq	24(%rsi), %rbx
	movl	$1, %ebp
	jmp	.L306
	.p2align 4,,7
.L307:
	addl	$1, %ebp
	addq	$24, %rbx
	cmpl	%r15d, %ebp
	je	.L304
.L306:
	movq	(%rbx), %rax
	movq	%rax, 24(%rsp)
	movq	8(%rbx), %rax
	movq	%rax, 32(%rsp)
	movq	16(%rbx), %rax
	movq	%r14, (%rsp)
	movq	%r13, 8(%rsp)
	movq	%r12, 16(%rsp)
	movq	%rax, 40(%rsp)
	call	NumEq
	testl	%eax, %eax
	jne	.L307
	leaq	96(%rsp), %rdi
	call	FalseObj
	movq	96(%rsp), %rax
	movq	56(%rsp), %rdx
	movq	%rax, (%rdx)
	movq	104(%rsp), %rax
	movq	%rax, 8(%rdx)
	movq	112(%rsp), %rax
	movq	%rax, 16(%rdx)
	jmp	.L303
.L304:
	leaq	64(%rsp), %rdi
	call	TrueObj
	movq	64(%rsp), %rax
	movq	56(%rsp), %rdx
	movq	%rax, (%rdx)
	movq	72(%rsp), %rax
	movq	%rax, 8(%rdx)
	movq	80(%rsp), %rax
	movq	%rax, 16(%rdx)
.L303:
	movq	56(%rsp), %rax
	addq	$168, %rsp
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
.LFE97:
	.size	NumEqFun, .-NumEqFun
	.p2align 4,,15
.globl IsFloat
	.type	IsFloat, @function
IsFloat:
.LFB91:
	pushq	%rbx
.LCFI146:
	movq	%rdi, %rbx
	subq	$64, %rsp
.LCFI147:
	movq	(%rsi), %rax
	movq	TypeFloat+8(%rip), %rdx
	movl	TypeFloat(%rip), %edi
	movq	%rax, (%rsp)
	movq	8(%rsi), %rax
	movq	%rax, 8(%rsp)
	movq	16(%rsi), %rax
	movq	%rdx, %rsi
	movq	%rax, 16(%rsp)
	call	IsType
	testl	%eax, %eax
	je	.L313
	movq	%rbx, %rdi
	call	TrueObj
	movq	%rbx, %rax
	addq	$64, %rsp
	popq	%rbx
	ret
	.p2align 4,,7
.L313:
	leaq	32(%rsp), %rdi
	call	FalseObj
	movq	32(%rsp), %rax
	movq	%rax, (%rbx)
	movq	40(%rsp), %rax
	movq	%rax, 8(%rbx)
	movq	48(%rsp), %rax
	movq	%rax, 16(%rbx)
	movq	%rbx, %rax
	addq	$64, %rsp
	popq	%rbx
	ret
.LFE91:
	.size	IsFloat, .-IsFloat
	.p2align 4,,15
.globl IsInt
	.type	IsInt, @function
IsInt:
.LFB90:
	pushq	%rbx
.LCFI148:
	movq	%rdi, %rbx
	subq	$64, %rsp
.LCFI149:
	movq	(%rsi), %rax
	movq	TypeInt+8(%rip), %rdx
	movl	TypeInt(%rip), %edi
	movq	%rax, (%rsp)
	movq	8(%rsi), %rax
	movq	%rax, 8(%rsp)
	movq	16(%rsi), %rax
	movq	%rdx, %rsi
	movq	%rax, 16(%rsp)
	call	IsType
	testl	%eax, %eax
	je	.L318
	movq	%rbx, %rdi
	call	TrueObj
	movq	%rbx, %rax
	addq	$64, %rsp
	popq	%rbx
	ret
	.p2align 4,,7
.L318:
	leaq	32(%rsp), %rdi
	call	FalseObj
	movq	32(%rsp), %rax
	movq	%rax, (%rbx)
	movq	40(%rsp), %rax
	movq	%rax, 8(%rbx)
	movq	48(%rsp), %rax
	movq	%rax, 16(%rbx)
	movq	%rbx, %rax
	addq	$64, %rsp
	popq	%rbx
	ret
.LFE90:
	.size	IsInt, .-IsInt
	.section	.rodata.str1.1
.LC22:
	.string	"fn"
.LC23:
	.string	"if"
.LC24:
	.string	"setvar"
.LC25:
	.string	"while"
.LC26:
	.string	"nil"
.LC27:
	.string	"quote"
	.text
	.p2align 4,,15
.globl CreateSymbols
	.type	CreateSymbols, @function
CreateSymbols:
.LFB86:
	subq	$232, %rsp
.LCFI150:
	movl	$.LC22, %esi
	leaq	192(%rsp), %rdi
	call	CreateSymbol
	movq	192(%rsp), %rax
	leaq	160(%rsp), %rdi
	movl	$.LC20, %esi
	movq	%rax, SymbolFn(%rip)
	movq	200(%rsp), %rax
	movq	%rax, SymbolFn+8(%rip)
	movq	208(%rsp), %rax
	movq	%rax, SymbolFn+16(%rip)
	call	CreateSymbol
	movq	160(%rsp), %rax
	leaq	128(%rsp), %rdi
	movl	$.LC23, %esi
	movq	%rax, SymbolFalse(%rip)
	movq	168(%rsp), %rax
	movq	%rax, SymbolFalse+8(%rip)
	movq	176(%rsp), %rax
	movq	%rax, SymbolFalse+16(%rip)
	call	CreateSymbol
	movq	128(%rsp), %rax
	leaq	96(%rsp), %rdi
	movl	$.LC24, %esi
	movq	%rax, SymbolIf(%rip)
	movq	136(%rsp), %rax
	movq	%rax, SymbolIf+8(%rip)
	movq	144(%rsp), %rax
	movq	%rax, SymbolIf+16(%rip)
	call	CreateSymbol
	movq	96(%rsp), %rax
	leaq	64(%rsp), %rdi
	movl	$.LC25, %esi
	movq	%rax, SymbolSetvar(%rip)
	movq	104(%rsp), %rax
	movq	%rax, SymbolSetvar+8(%rip)
	movq	112(%rsp), %rax
	movq	%rax, SymbolSetvar+16(%rip)
	call	CreateSymbol
	movq	64(%rsp), %rax
	leaq	32(%rsp), %rdi
	movl	$.LC26, %esi
	movq	%rax, SymbolWhile(%rip)
	movq	72(%rsp), %rax
	movq	%rax, SymbolWhile+8(%rip)
	movq	80(%rsp), %rax
	movq	%rax, SymbolWhile+16(%rip)
	call	CreateSymbol
	movq	32(%rsp), %rax
	movq	%rsp, %rdi
	movl	$.LC27, %esi
	movq	%rax, SymbolNil(%rip)
	movq	40(%rsp), %rax
	movq	%rax, SymbolNil+8(%rip)
	movq	48(%rsp), %rax
	movq	%rax, SymbolNil+16(%rip)
	call	CreateSymbol
	movq	(%rsp), %rax
	movq	%rax, SymbolQuote(%rip)
	movq	8(%rsp), %rax
	movq	%rax, SymbolQuote+8(%rip)
	movq	16(%rsp), %rax
	movq	%rax, SymbolQuote+16(%rip)
	addq	$232, %rsp
	ret
.LFE86:
	.size	CreateSymbols, .-CreateSymbols
	.p2align 4,,15
.globl IsNil
	.type	IsNil, @function
IsNil:
.LFB23:
	movq	%rbx, -48(%rsp)
.LCFI151:
	movq	%rbp, -40(%rsp)
.LCFI152:
	movq	%r12, -32(%rsp)
.LCFI153:
	movq	%r13, -24(%rsp)
.LCFI154:
	movq	%r14, -16(%rsp)
.LCFI155:
	movq	%r15, -8(%rsp)
.LCFI156:
	subq	$184, %rsp
.LCFI157:
	movq	192(%rsp), %r12
	movl	TypeCons(%rip), %edi
	movq	TypeCons+8(%rip), %rsi
	movq	200(%rsp), %rbp
	movq	208(%rsp), %rbx
	movq	%r12, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%rbx, 16(%rsp)
	call	IsType
	testl	%eax, %eax
	je	.L325
	leaq	96(%rsp), %rdi
	movq	%r12, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%rbx, 16(%rsp)
	call	Car
	movq	96(%rsp), %rax
	movq	%rax, (%rsp)
	movq	104(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	112(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	IsNone
	xorl	%edx, %edx
	testl	%eax, %eax
	jne	.L333
	.p2align 4,,7
.L329:
	movq	136(%rsp), %rbx
	movq	144(%rsp), %rbp
	movl	%edx, %eax
	movq	152(%rsp), %r12
	movq	160(%rsp), %r13
	movq	168(%rsp), %r14
	movq	176(%rsp), %r15
	addq	$184, %rsp
	ret
	.p2align 4,,7
.L325:
	movq	nil_symbol(%rip), %r15
	movq	nil_symbol+8(%rip), %r14
	movq	nil_symbol+16(%rip), %r13
	movq	%r15, (%rsp)
	movq	%r14, 8(%rsp)
	movq	%r13, 16(%rsp)
	call	IsNone
	testl	%eax, %eax
	jne	.L334
.L330:
	movq	%rbx, 16(%rsp)
	movq	%r12, (%rsp)
	movq	%rbp, 8(%rsp)
	call	Obj
	movq	%rax, %rbx
	movq	%r15, (%rsp)
	movq	%r14, 8(%rsp)
	movq	%r13, 16(%rsp)
	call	Obj
	xorl	%edx, %edx
	cmpq	%rax, %rbx
	sete	%dl
	jmp	.L329
	.p2align 4,,7
.L333:
	movq	192(%rsp), %rax
	leaq	64(%rsp), %rdi
	movq	%rax, (%rsp)
	movq	200(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	208(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	Cdr
	movq	64(%rsp), %rax
	movq	%rax, (%rsp)
	movq	72(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	80(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	IsNone
	xorl	%edx, %edx
	testl	%eax, %eax
	setne	%dl
	jmp	.L329
	.p2align 4,,7
.L334:
	leaq	32(%rsp), %rdi
	movl	$.LC26, %esi
	call	CreateSymbol
	movq	32(%rsp), %r15
	movq	40(%rsp), %r14
	movq	48(%rsp), %r13
	movq	192(%rsp), %r12
	movq	200(%rsp), %rbp
	movq	208(%rsp), %rbx
	movq	%r15, nil_symbol(%rip)
	movq	%r14, nil_symbol+8(%rip)
	movq	%r13, nil_symbol+16(%rip)
	jmp	.L330
.LFE23:
	.size	IsNil, .-IsNil
	.p2align 4,,15
.globl IsFalse
	.type	IsFalse, @function
IsFalse:
.LFB108:
	subq	$56, %rsp
.LCFI158:
	movq	64(%rsp), %rax
	movq	%rax, (%rsp)
	movq	72(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	80(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	IsNil
	testl	%eax, %eax
	movl	$1, %edx
	jne	.L338
	movq	SymbolFalse(%rip), %rax
	movq	%rax, 24(%rsp)
	movq	SymbolFalse+8(%rip), %rax
	movq	%rax, 32(%rsp)
	movq	SymbolFalse+16(%rip), %rax
	movq	%rax, 40(%rsp)
	movq	64(%rsp), %rax
	movq	%rax, (%rsp)
	movq	72(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	80(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	ObjEq
	xorl	%edx, %edx
	testl	%eax, %eax
	setne	%dl
.L338:
	movl	%edx, %eax
	addq	$56, %rsp
	ret
.LFE108:
	.size	IsFalse, .-IsFalse
	.p2align 4,,15
.globl OrFun
	.type	OrFun, @function
OrFun:
.LFB110:
	pushq	%r14
.LCFI159:
	pushq	%r13
.LCFI160:
	movq	%rdi, %r13
	pushq	%r12
.LCFI161:
	movl	%edx, %r12d
	pushq	%rbp
.LCFI162:
	pushq	%rbx
.LCFI163:
	subq	$96, %rsp
.LCFI164:
	testl	%edx, %edx
	jle	.L341
	leaq	64(%rsp), %r14
	movq	%rsi, %rbx
	xorl	%ebp, %ebp
	jmp	.L343
	.p2align 4,,7
.L344:
	addl	$1, %ebp
	addq	$24, %rbx
	cmpl	%r12d, %ebp
	je	.L341
.L343:
	movq	(%rbx), %rax
	movq	%rax, (%rsp)
	movq	8(%rbx), %rax
	movq	%rax, 8(%rsp)
	movq	16(%rbx), %rax
	movq	%rax, 16(%rsp)
	call	IsFalse
	testl	%eax, %eax
	jne	.L344
	movq	%r14, %rdi
	call	TrueObj
	movq	64(%rsp), %rax
	movq	%rax, (%r13)
	movq	72(%rsp), %rax
	movq	%rax, 8(%r13)
	movq	80(%rsp), %rax
	movq	%rax, 16(%r13)
	jmp	.L340
.L341:
	leaq	32(%rsp), %rdi
	call	FalseObj
	movq	32(%rsp), %rax
	movq	%rax, (%r13)
	movq	40(%rsp), %rax
	movq	%rax, 8(%r13)
	movq	48(%rsp), %rax
	movq	%rax, 16(%r13)
.L340:
	addq	$96, %rsp
	movq	%r13, %rax
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	ret
.LFE110:
	.size	OrFun, .-OrFun
	.p2align 4,,15
.globl IsTrue
	.type	IsTrue, @function
IsTrue:
.LFB107:
	subq	$24, %rsp
.LCFI165:
	movq	32(%rsp), %rax
	movq	%rax, (%rsp)
	movq	40(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	48(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	IsFalse
	testl	%eax, %eax
	sete	%al
	addq	$24, %rsp
	movzbl	%al, %eax
	ret
.LFE107:
	.size	IsTrue, .-IsTrue
	.p2align 4,,15
.globl NotFun
	.type	NotFun, @function
NotFun:
.LFB111:
	pushq	%rbx
.LCFI166:
	movq	%rdi, %rbx
	subq	$96, %rsp
.LCFI167:
	movq	(%rsi), %rax
	movq	%rax, (%rsp)
	movq	8(%rsi), %rax
	movq	%rax, 8(%rsp)
	movq	16(%rsi), %rax
	movq	%rax, 16(%rsp)
	call	IsTrue
	testl	%eax, %eax
	je	.L352
	leaq	64(%rsp), %rdi
	call	FalseObj
	movq	64(%rsp), %rax
	movq	%rax, (%rbx)
	movq	72(%rsp), %rax
	movq	%rax, 8(%rbx)
	movq	80(%rsp), %rax
	movq	%rax, 16(%rbx)
	movq	%rbx, %rax
	addq	$96, %rsp
	popq	%rbx
	ret
	.p2align 4,,7
.L352:
	leaq	32(%rsp), %rdi
	call	TrueObj
	movq	32(%rsp), %rax
	movq	%rax, (%rbx)
	movq	40(%rsp), %rax
	movq	%rax, 8(%rbx)
	movq	48(%rsp), %rax
	movq	%rax, 16(%rbx)
	movq	%rbx, %rax
	addq	$96, %rsp
	popq	%rbx
	ret
.LFE111:
	.size	NotFun, .-NotFun
	.p2align 4,,15
.globl AndFun
	.type	AndFun, @function
AndFun:
.LFB109:
	pushq	%r14
.LCFI168:
	pushq	%r13
.LCFI169:
	movq	%rdi, %r13
	pushq	%r12
.LCFI170:
	movl	%edx, %r12d
	pushq	%rbp
.LCFI171:
	pushq	%rbx
.LCFI172:
	subq	$96, %rsp
.LCFI173:
	testl	%edx, %edx
	jle	.L357
	leaq	64(%rsp), %r14
	movq	%rsi, %rbx
	xorl	%ebp, %ebp
	jmp	.L359
	.p2align 4,,7
.L360:
	addl	$1, %ebp
	addq	$24, %rbx
	cmpl	%r12d, %ebp
	je	.L357
.L359:
	movq	(%rbx), %rax
	movq	%rax, (%rsp)
	movq	8(%rbx), %rax
	movq	%rax, 8(%rsp)
	movq	16(%rbx), %rax
	movq	%rax, 16(%rsp)
	call	IsTrue
	testl	%eax, %eax
	jne	.L360
	movq	%r14, %rdi
	call	FalseObj
	movq	64(%rsp), %rax
	movq	%rax, (%r13)
	movq	72(%rsp), %rax
	movq	%rax, 8(%r13)
	movq	80(%rsp), %rax
	movq	%rax, 16(%r13)
	jmp	.L356
.L357:
	leaq	32(%rsp), %rdi
	call	TrueObj
	movq	32(%rsp), %rax
	movq	%rax, (%r13)
	movq	40(%rsp), %rax
	movq	%rax, 8(%r13)
	movq	48(%rsp), %rax
	movq	%rax, 16(%r13)
.L356:
	addq	$96, %rsp
	movq	%r13, %rax
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	ret
.LFE109:
	.size	AndFun, .-AndFun
	.p2align 4,,15
.globl IfJmpInstr
	.type	IfJmpInstr, @function
IfJmpInstr:
.LFB54:
	pushq	%rbx
.LCFI174:
	movl	%edi, %ebx
	subq	$96, %rsp
.LCFI175:
	leaq	32(%rsp), %rdi
	call	StackPop
	movq	32(%rsp), %rcx
	movq	40(%rsp), %rdx
	movq	48(%rsp), %rax
	movq	%rcx, 64(%rsp)
	movq	%rdx, 72(%rsp)
	movq	%rax, 80(%rsp)
	movq	%rcx, (%rsp)
	movq	%rdx, 8(%rsp)
	movq	%rax, 16(%rsp)
	call	IsTrue
	testl	%eax, %eax
	movl	$-1, %eax
	cmovne	%eax, %ebx
	addq	$96, %rsp
	movl	%ebx, %eax
	popq	%rbx
	ret
.LFE54:
	.size	IfJmpInstr, .-IfJmpInstr
	.section	.rodata.str1.1
.LC28:
	.string	". "
.LC29:
	.string	"\b) "
	.text
	.p2align 4,,15
.globl PrintCons
	.type	PrintCons, @function
PrintCons:
.LFB45:
	pushq	%r15
.LCFI176:
	movq	%rdi, %r15
	pushq	%r14
.LCFI177:
	pushq	%r13
.LCFI178:
	pushq	%r12
.LCFI179:
	pushq	%rbp
.LCFI180:
	pushq	%rbx
.LCFI181:
	movq	%rsi, %rbx
	movq	%rdi, %rsi
	movl	$40, %edi
	subq	$104, %rsp
.LCFI182:
	call	fputc
	movl	TypeCons(%rip), %r14d
	movq	TypeCons+8(%rip), %r13
	jmp	.L370
	.p2align 4,,7
.L377:
	movq	(%rbx), %rax
	movq	%r15, %rdi
	movq	%rax, (%rsp)
	movq	8(%rbx), %rax
	movq	%rax, 8(%rsp)
	movq	16(%rbx), %rax
	movq	%rax, 16(%rsp)
	call	PrintObj
	movq	24(%rbx), %r12
	movl	TypeCons(%rip), %r14d
	movq	TypeCons+8(%rip), %r13
	movq	%r12, 64(%rsp)
	movq	32(%rbx), %rbp
	movl	%r14d, %edi
	movq	%r13, %rsi
	movq	%rbp, 72(%rsp)
	movq	40(%rbx), %rbx
	movq	%r12, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%rbx, 80(%rsp)
	movq	%rbx, 16(%rsp)
	call	IsType
	testl	%eax, %eax
	je	.L373
	movq	%rbx, 16(%rsp)
	movq	%r12, (%rsp)
	movq	%rbp, 8(%rsp)
	call	Obj
	movq	%rax, %rbx
.L370:
	leaq	32(%rsp), %rdi
	movl	%r14d, %edx
	movq	%r13, %rcx
	movq	%rbx, %rsi
	call	WrapObj
	movq	32(%rsp), %rax
	movq	%rax, (%rsp)
	movq	40(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	48(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	IsNil
	testl	%eax, %eax
	je	.L377
.L371:
	addq	$104, %rsp
	movq	%r15, %rcx
	movl	$3, %edx
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	movl	$1, %esi
	movl	$.LC29, %edi
	jmp	fwrite
.L373:
	movq	64(%rsp), %rax
	movq	%rax, (%rsp)
	movq	72(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	80(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	IsNil
	testl	%eax, %eax
	jne	.L371
	movq	%r15, %rcx
	movl	$2, %edx
	movl	$1, %esi
	movl	$.LC28, %edi
	call	fwrite
	movq	64(%rsp), %rax
	movq	%r15, %rdi
	movq	%rax, (%rsp)
	movq	72(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	80(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	PrintObj
	jmp	.L371
.LFE45:
	.size	PrintCons, .-PrintCons
	.section	.rodata.str1.1
.LC30:
	.string	"\"%s\" "
.LC31:
	.string	"%s "
.LC32:
	.string	"%d "
.LC33:
	.string	"%f "
	.section	.rodata.str1.8,"aMS",@progbits,1
	.align 8
.LC34:
	.string	"Unnamed function lambda at (%p)"
	.section	.rodata.str1.1
.LC35:
	.string	"Obj-type-NONE. Error!"
.LC36:
	.string	"Unknown object type."
	.text
	.p2align 4,,15
.globl PrintObj
	.type	PrintObj, @function
PrintObj:
.LFB46:
	movq	%rbx, -32(%rsp)
.LCFI183:
	movq	%rbp, -24(%rsp)
.LCFI184:
	movq	%r12, -16(%rsp)
.LCFI185:
	movq	%r13, -8(%rsp)
.LCFI186:
	subq	$56, %rsp
.LCFI187:
	movq	%rdi, %r13
	movq	TypeString+8(%rip), %rsi
	movl	TypeString(%rip), %edi
	movq	64(%rsp), %rbx
	movq	72(%rsp), %rbp
	movq	80(%rsp), %r12
	movq	%rbx, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%r12, 16(%rsp)
	call	IsType
	testl	%eax, %eax
	je	.L379
	movq	%rbx, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%r12, 16(%rsp)
	call	Obj
	movq	(%rax), %rdx
	movl	$.LC30, %esi
.L395:
	movq	%r13, %rdi
	movq	24(%rsp), %rbx
	movq	32(%rsp), %rbp
	movq	40(%rsp), %r12
	movq	48(%rsp), %r13
	xorl	%eax, %eax
	addq	$56, %rsp
	jmp	fprintf
	.p2align 4,,7
.L379:
	movl	TypeSymbol(%rip), %edi
	movq	TypeSymbol+8(%rip), %rsi
	movq	%rbx, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%r12, 16(%rsp)
	call	IsType
	testl	%eax, %eax
	jne	.L396
	movl	TypeInt(%rip), %edi
	movq	TypeInt+8(%rip), %rsi
	movq	%rbx, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%r12, 16(%rsp)
	call	IsType
	testl	%eax, %eax
	jne	.L397
	movl	TypeFloat(%rip), %edi
	movq	TypeFloat+8(%rip), %rsi
	movq	%rbx, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%r12, 16(%rsp)
	call	IsType
	testl	%eax, %eax
	je	.L385
	movq	%rbx, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%r12, 16(%rsp)
	call	Obj
	movsd	(%rax), %xmm0
	movq	%r13, %rdi
	movq	24(%rsp), %rbx
	movq	32(%rsp), %rbp
	movl	$.LC33, %esi
	movq	40(%rsp), %r12
	movq	48(%rsp), %r13
	movl	$1, %eax
	addq	$56, %rsp
	jmp	fprintf
	.p2align 4,,7
.L396:
	movq	%rbx, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%r12, 16(%rsp)
	call	Obj
	movq	(%rax), %rdx
	movl	$.LC31, %esi
	jmp	.L395
	.p2align 4,,7
.L397:
	movq	%rbx, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%r12, 16(%rsp)
	call	Obj
	movl	(%rax), %edx
	movq	%r13, %rdi
	movq	24(%rsp), %rbx
	movq	32(%rsp), %rbp
	movq	40(%rsp), %r12
	movl	$.LC32, %esi
	movq	48(%rsp), %r13
	xorl	%eax, %eax
	addq	$56, %rsp
	jmp	fprintf
.L385:
	movl	TypeCons(%rip), %edi
	movq	TypeCons+8(%rip), %rsi
	movq	%rbx, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%r12, 16(%rsp)
	call	IsType
	testl	%eax, %eax
	jne	.L398
	movl	TypeFunction(%rip), %edi
	movq	TypeFunction+8(%rip), %rsi
	movq	%rbx, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%r12, 16(%rsp)
	call	IsType
	testl	%eax, %eax
	je	.L389
	movq	%rbx, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%r12, 16(%rsp)
	call	Obj
	movl	$.LC34, %esi
	movq	%rax, %rdx
	jmp	.L395
.L398:
	movq	%rbx, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%r12, 16(%rsp)
	call	Obj
	movq	%r13, %rdi
	movq	24(%rsp), %rbx
	movq	32(%rsp), %rbp
	movq	40(%rsp), %r12
	movq	48(%rsp), %r13
	movq	%rax, %rsi
	addq	$56, %rsp
	jmp	PrintCons
.L389:
	movl	TypeNone(%rip), %edi
	movq	TypeNone+8(%rip), %rsi
	movq	%rbx, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%r12, 16(%rsp)
	call	IsType
	testl	%eax, %eax
	je	.L391
	movl	$.LC35, %edi
	movq	%r13, %rcx
	movl	$21, %edx
	movl	$1, %esi
	call	fwrite
	xorl	%edi, %edi
	call	exit
.L391:
	movl	$.LC36, %edi
	call	puts
	xorl	%edi, %edi
	call	exit
.LFE46:
	.size	PrintObj, .-PrintObj
	.p2align 4,,15
.globl PrintObjFun
	.type	PrintObjFun, @function
PrintObjFun:
.LFB88:
	movq	%rbx, -16(%rsp)
.LCFI188:
	movq	%r12, -8(%rsp)
.LCFI189:
	subq	$40, %rsp
.LCFI190:
	movq	(%rsi), %rax
	movq	%rdi, %r12
	movq	stdout(%rip), %rdi
	movq	%rsi, %rbx
	movq	%rax, (%rsp)
	movq	8(%rsi), %rax
	movq	%rax, 8(%rsp)
	movq	16(%rsi), %rax
	movq	%rax, 16(%rsp)
	call	PrintObj
	movl	$32, %edi
	call	putchar
	movq	stdout(%rip), %rdi
	call	fflush
	movq	%rbx, %rsi
	movq	%r12, %rdi
	movl	$24, %edx
	call	memmove
	movq	%r12, %rax
	movq	24(%rsp), %rbx
	movq	32(%rsp), %r12
	addq	$40, %rsp
	ret
.LFE88:
	.size	PrintObjFun, .-PrintObjFun
	.section	.rodata.str1.1
.LC37:
	.string	"?"
.LC38:
	.string	".."
	.text
	.p2align 4,,15
.globl CountParams
	.type	CountParams, @function
CountParams:
.LFB34:
	pushq	%r14
.LCFI191:
	pushq	%r13
.LCFI192:
	pushq	%r12
.LCFI193:
	pushq	%rbp
.LCFI194:
	pushq	%rbx
.LCFI195:
	xorl	%ebx, %ebx
	subq	$176, %rsp
.LCFI196:
	leaq	144(%rsp), %r12
	leaq	112(%rsp), %rbp
	leaq	80(%rsp), %r14
	leaq	48(%rsp), %r13
	jmp	.L402
	.p2align 4,,7
.L405:
	movq	224(%rsp), %rax
	leaq	224(%rsp), %rdi
	movq	%rax, (%rsp)
	movq	232(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	240(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	Cdr
.L402:
	movq	224(%rsp), %rax
	movq	%rax, (%rsp)
	movq	232(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	240(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	IsNil
	testl	%eax, %eax
	jne	.L403
	movl	$.LC37, %esi
	movq	%r12, %rdi
	call	CreateSymbol
	movq	224(%rsp), %rax
	movq	%rbp, %rdi
	movq	%rax, (%rsp)
	movq	232(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	240(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	Car
	movq	144(%rsp), %rax
	movq	%rax, 24(%rsp)
	movq	152(%rsp), %rax
	movq	%rax, 32(%rsp)
	movq	160(%rsp), %rax
	movq	%rax, 40(%rsp)
	movq	112(%rsp), %rax
	movq	%rax, (%rsp)
	movq	120(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	128(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	ObjEq
	testl	%eax, %eax
	jne	.L405
	movl	$.LC38, %esi
	movq	%r14, %rdi
	call	CreateSymbol
	movq	224(%rsp), %rax
	movq	%r13, %rdi
	movq	%rax, (%rsp)
	movq	232(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	240(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	Car
	movq	80(%rsp), %rax
	movq	%rax, 24(%rsp)
	movq	88(%rsp), %rax
	movq	%rax, 32(%rsp)
	movq	96(%rsp), %rax
	movq	%rax, 40(%rsp)
	movq	48(%rsp), %rax
	movq	%rax, (%rsp)
	movq	56(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	64(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	ObjEq
	cmpl	$1, %eax
	adcl	$0, %ebx
	jmp	.L405
.L403:
	addq	$176, %rsp
	movl	%ebx, %eax
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	ret
.LFE34:
	.size	CountParams, .-CountParams
	.p2align 4,,15
.globl ListLen
	.type	ListLen, @function
ListLen:
.LFB25:
	subq	$72, %rsp
.LCFI197:
	movq	80(%rsp), %rax
	movq	%rax, (%rsp)
	movq	88(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	96(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	IsNil
	xorl	%edx, %edx
	testl	%eax, %eax
	jne	.L412
	movq	80(%rsp), %rax
	leaq	32(%rsp), %rdi
	movq	%rax, (%rsp)
	movq	88(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	96(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	Cdr
	movq	32(%rsp), %rax
	movq	%rax, (%rsp)
	movq	40(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	48(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	ListLen
	leal	1(%rax), %edx
.L412:
	movl	%edx, %eax
	addq	$72, %rsp
	ret
.LFE25:
	.size	ListLen, .-ListLen
	.p2align 4,,15
.globl GetListLen
	.type	GetListLen, @function
GetListLen:
.LFB104:
	pushq	%rbx
.LCFI198:
	movq	%rdi, %rbx
	subq	$32, %rsp
.LCFI199:
	movq	(%rsi), %rax
	movq	%rax, (%rsp)
	movq	8(%rsi), %rax
	movq	%rax, 8(%rsp)
	movq	16(%rsi), %rax
	movq	%rax, 16(%rsp)
	call	ListLen
	movq	%rbx, %rdi
	movl	%eax, %esi
	call	CreateInt
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	ret
.LFE104:
	.size	GetListLen, .-GetListLen
	.p2align 4,,15
.globl ParseArgList
	.type	ParseArgList, @function
ParseArgList:
.LFB35:
	pushq	%r15
.LCFI200:
	pushq	%r14
.LCFI201:
	pushq	%r13
.LCFI202:
	pushq	%r12
.LCFI203:
	pushq	%rbp
.LCFI204:
	pushq	%rbx
.LCFI205:
	subq	$376, %rsp
.LCFI206:
	movq	432(%rsp), %rax
	movq	%rax, (%rsp)
	movq	440(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	448(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	CountParams
	movslq	%eax,%rdi
	movl	%eax, 68(%rsp)
	leaq	(%rdi,%rdi,4), %rdi
	salq	$3, %rdi
	call	VyMalloc
	leaq	288(%rsp), %rdi
	movl	$.LC37, %esi
	movq	%rax, 72(%rsp)
	call	CreateSymbol
	movq	288(%rsp), %rax
	leaq	224(%rsp), %rdi
	movl	$.LC38, %esi
	movq	%rax, 320(%rsp)
	movq	296(%rsp), %rax
	movq	%rax, 328(%rsp)
	movq	304(%rsp), %rax
	movq	%rax, 336(%rsp)
	call	CreateSymbol
	movq	224(%rsp), %rax
	movq	%rax, 256(%rsp)
	movq	232(%rsp), %rax
	movq	%rax, 264(%rsp)
	movq	240(%rsp), %rax
	movq	%rax, 272(%rsp)
	movq	432(%rsp), %rax
	movq	%rax, (%rsp)
	movq	440(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	448(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	ListLen
	testl	%eax, %eax
	movl	%eax, 92(%rsp)
	jle	.L417
	movq	432(%rsp), %rax
	xorl	%r15d, %r15d
	movq	%rax, (%rsp)
	movq	440(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	448(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	Obj
	movl	$0, 80(%rsp)
	movq	%rax, 56(%rsp)
	movl	$0, 84(%rsp)
	movl	$0, 88(%rsp)
	jmp	.L419
	.p2align 4,,7
.L427:
	movl	$1, 80(%rsp)
.L422:
	addl	$1, %r15d
	cmpl	92(%rsp), %r15d
	je	.L417
.L419:
	movq	56(%rsp), %rsi
	leaq	160(%rsp), %rdi
	movl	%r15d, %edx
	call	ListGet
	movq	320(%rsp), %rax
	movq	160(%rsp), %r14
	movq	168(%rsp), %r13
	movq	176(%rsp), %rbp
	movq	%rax, 24(%rsp)
	movq	328(%rsp), %rax
	movq	%r14, 192(%rsp)
	movq	%r13, 200(%rsp)
	movq	%rbp, 208(%rsp)
	movq	%r14, (%rsp)
	movq	%rax, 32(%rsp)
	movq	336(%rsp), %rax
	movq	%r13, 8(%rsp)
	movq	%rbp, 16(%rsp)
	movq	%rax, 40(%rsp)
	call	ObjEq
	testl	%eax, %eax
	jne	.L427
	movq	256(%rsp), %rax
	movq	%r14, (%rsp)
	movq	%r13, 8(%rsp)
	movq	%rbp, 16(%rsp)
	movq	%rax, 24(%rsp)
	movq	264(%rsp), %rax
	movq	%rax, 32(%rsp)
	movq	272(%rsp), %rax
	movq	%rax, 40(%rsp)
	call	ObjEq
	testl	%eax, %eax
	je	.L423
	addl	$1, %r15d
	cmpl	92(%rsp), %r15d
	movl	$1, 80(%rsp)
	movl	$1, 84(%rsp)
	jne	.L419
	.p2align 4,,7
.L417:
	movl	68(%rsp), %eax
	movq	72(%rsp), %rdx
	addq	$376, %rsp
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
.L423:
	movslq	88(%rsp),%rbx
	movl	84(%rsp), %edx
	leaq	112(%rsp), %rdi
	movl	80(%rsp), %esi
	movq	%r14, (%rsp)
	movq	%r13, 8(%rsp)
	movq	%rbp, 16(%rsp)
	call	ParseParam
	movq	112(%rsp), %rax
	movq	72(%rsp), %rdx
	leaq	(%rbx,%rbx,4), %rbx
	addl	$1, 88(%rsp)
	leaq	0(,%rbx,8), %r12
	movq	%rax, (%rdx,%rbx,8)
	movq	120(%rsp), %rax
	movq	%rax, 8(%r12,%rdx)
	movq	128(%rsp), %rax
	movq	%rax, 16(%r12,%rdx)
	movq	136(%rsp), %rax
	movq	%rax, 24(%r12,%rdx)
	movq	144(%rsp), %rax
	movq	%rax, 32(%r12,%rdx)
	jmp	.L422
.LFE35:
	.size	ParseArgList, .-ParseArgList
	.p2align 4,,15
.globl Nil
	.type	Nil, @function
Nil:
.LFB19:
	pushq	%rbp
.LCFI207:
	movq	%rdi, %rbp
	pushq	%rbx
.LCFI208:
	subq	$72, %rsp
.LCFI209:
	movq	nil_symbol+8(%rip), %rax
	movq	nil_symbol(%rip), %rbx
	movq	%rax, 8(%rsp)
	movq	nil_symbol+16(%rip), %rax
	movq	%rbx, (%rsp)
	movq	%rax, 16(%rsp)
	call	IsNone
	testl	%eax, %eax
	je	.L429
	leaq	32(%rsp), %rdi
	movl	$.LC26, %esi
	call	CreateSymbol
	movq	40(%rsp), %rax
	movq	32(%rsp), %rbx
	movq	%rax, nil_symbol+8(%rip)
	movq	48(%rsp), %rax
	movq	%rbx, nil_symbol(%rip)
	movq	%rax, nil_symbol+16(%rip)
.L429:
	movq	nil_symbol+8(%rip), %rax
	movq	%rbx, (%rbp)
	movq	%rax, 8(%rbp)
	movq	nil_symbol+16(%rip), %rax
	movq	%rax, 16(%rbp)
	addq	$72, %rsp
	movq	%rbp, %rax
	popq	%rbx
	popq	%rbp
	ret
.LFE19:
	.size	Nil, .-Nil
	.p2align 4,,15
.globl CompileExpr
	.type	CompileExpr, @function
CompileExpr:
.LFB72:
	pushq	%r15
.LCFI210:
	movq	%rdi, %r15
	pushq	%r14
.LCFI211:
	pushq	%r13
.LCFI212:
	pushq	%r12
.LCFI213:
	pushq	%rbp
.LCFI214:
	pushq	%rbx
.LCFI215:
	subq	$1752, %rsp
.LCFI216:
	movl	TypeString(%rip), %edi
	movq	TypeString+8(%rip), %rsi
	movl	1808(%rsp), %eax
	movq	1824(%rsp), %r14
	movq	1816(%rsp), %r13
	movl	%eax, 1712(%rsp)
	movq	1712(%rsp), %r12
	movl	%eax, 72(%rsp)
	movq	%r14, 1728(%rsp)
	movq	%r13, 1720(%rsp)
	movq	%r13, 8(%rsp)
	movq	%r12, (%rsp)
	movq	%r14, 16(%rsp)
	call	IsType
	testl	%eax, %eax
	je	.L433
	movl	72(%rsp), %ecx
	movl	$20, 432(%rsp)
	movq	432(%rsp), %rdx
	movq	%r14, 456(%rsp)
	movq	%r13, 448(%rsp)
	movl	%ecx, 440(%rsp)
	movq	440(%rsp), %rax
	movq	%rdx, 880(%rsp)
.L466:
	movq	%r15, %rdi
	movq	%rax, 888(%rsp)
	movq	%r13, 896(%rsp)
	movq	%r14, 904(%rsp)
	movq	%rdx, (%rsp)
	movq	%rax, 8(%rsp)
	movq	%r13, 16(%rsp)
	movq	%r14, 24(%rsp)
	call	EmitInstruction
	movq	1712(%rsp), %r12
.L435:
	movl	TypeSymbol(%rip), %edi
	movq	TypeSymbol+8(%rip), %rsi
	movq	1720(%rsp), %rbp
	movq	1728(%rsp), %rbx
	movq	%r12, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%rbx, 16(%rsp)
	call	IsType
	testl	%eax, %eax
	jne	.L469
.L439:
	movl	TypeCons(%rip), %edi
	movq	TypeCons+8(%rip), %rsi
	movq	%r12, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%rbx, 16(%rsp)
	call	IsType
	testl	%eax, %eax
	jne	.L470
.L441:
	addq	$1752, %rsp
	movq	%r15, %rax
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
	.p2align 4,,7
.L433:
	movl	TypeFloat(%rip), %edi
	movq	TypeFloat+8(%rip), %rsi
	movq	%r12, (%rsp)
	movq	%r13, 8(%rsp)
	movq	%r14, 16(%rsp)
	call	IsType
	testl	%eax, %eax
	jne	.L471
	movl	TypeInt(%rip), %edi
	movq	TypeInt+8(%rip), %rsi
	movq	%r12, (%rsp)
	movq	%r13, 8(%rsp)
	movq	%r14, 16(%rsp)
	call	IsType
	testl	%eax, %eax
	je	.L435
	movl	72(%rsp), %ecx
	movl	$20, 368(%rsp)
	movq	368(%rsp), %rdx
	movq	%r14, 392(%rsp)
	movq	%r13, 384(%rsp)
	movq	%r13, 960(%rsp)
	movl	%ecx, 376(%rsp)
	movq	376(%rsp), %rax
	movq	%rdx, 944(%rsp)
	movq	%r14, 968(%rsp)
	movq	%rdx, 880(%rsp)
	movq	%rax, 952(%rsp)
	jmp	.L466
	.p2align 4,,7
.L471:
	movl	72(%rsp), %eax
	movl	$20, 400(%rsp)
	movq	400(%rsp), %rdx
	movq	%r14, 424(%rsp)
	movq	%r13, 416(%rsp)
	movq	%r13, 928(%rsp)
	movl	%eax, 408(%rsp)
	movq	408(%rsp), %rax
	movq	%rdx, 912(%rsp)
	movq	%r14, 936(%rsp)
	movq	%rdx, 880(%rsp)
	movq	%rax, 920(%rsp)
	jmp	.L466
	.p2align 4,,7
.L470:
	movq	%r12, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%rbx, 16(%rsp)
	call	Obj
	leaq	1040(%rsp), %rdi
	movq	%rax, %r13
	movq	%r12, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%rbx, 16(%rsp)
	call	Car
	movq	1040(%rsp), %rax
	movl	TypeSymbol(%rip), %edi
	movq	TypeSymbol+8(%rip), %rsi
	movq	%rax, (%rsp)
	movq	1048(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	1056(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	IsType
	testl	%eax, %eax
	je	.L443
	movq	1712(%rsp), %rax
	leaq	1648(%rsp), %rdi
	movq	%rax, (%rsp)
	movq	1720(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	1728(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	Car
	movq	SymbolQuote(%rip), %rax
	movq	1648(%rsp), %r12
	movq	1656(%rsp), %rbp
	movq	1664(%rsp), %rbx
	movq	%rax, 24(%rsp)
	movq	SymbolQuote+8(%rip), %rax
	movq	%r12, 1680(%rsp)
	movq	%rbp, 1688(%rsp)
	movq	%rbx, 1696(%rsp)
	movq	%r12, (%rsp)
	movq	%rax, 32(%rsp)
	movq	SymbolQuote+16(%rip), %rax
	movq	%rbp, 8(%rsp)
	movq	%rbx, 16(%rsp)
	movq	%rax, 40(%rsp)
	call	ObjEq
	testl	%eax, %eax
	jne	.L472
	movq	SymbolSetvar(%rip), %rax
	movq	%r12, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%rbx, 16(%rsp)
	movq	%rax, 24(%rsp)
	movq	SymbolSetvar+8(%rip), %rax
	movq	%rax, 32(%rsp)
	movq	SymbolSetvar+16(%rip), %rax
	movq	%rax, 40(%rsp)
	call	ObjEq
	testl	%eax, %eax
	je	.L447
	leaq	1520(%rsp), %rdi
	movl	$1, %edx
	movq	%r13, %rsi
	call	ListGet
	movq	1520(%rsp), %rax
	leaq	1488(%rsp), %rdi
	movl	$2, %edx
	movq	%r13, %rsi
	movq	%rax, 1552(%rsp)
	movq	1528(%rsp), %rax
	movq	%rax, 1560(%rsp)
	movq	1536(%rsp), %rax
	movq	%rax, 1568(%rsp)
	call	ListGet
	movq	1488(%rsp), %rax
	movq	%r15, %rdi
	movq	%rax, (%rsp)
	movq	1496(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	1504(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	CompileExpr
	movq	1552(%rsp), %rax
	movq	%r15, %rdi
	movq	%rax, (%rsp)
	movq	1560(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	1568(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	CompileExpr
	movl	$0, 280(%rsp)
	movl	$70, 272(%rsp)
	movq	280(%rsp), %rcx
	movq	272(%rsp), %rsi
	movq	288(%rsp), %rdx
	movq	296(%rsp), %rax
	movq	%rsi, 784(%rsp)
	movq	%rcx, 792(%rsp)
	movq	%rdx, 800(%rsp)
	movq	%rax, 808(%rsp)
	jmp	.L468
	.p2align 4,,7
.L469:
	movl	72(%rsp), %eax
	movl	$60, 336(%rsp)
	movq	%r15, %rdi
	movq	336(%rsp), %rdx
	movq	%r14, 360(%rsp)
	movq	%r13, 352(%rsp)
	movq	%r13, 864(%rsp)
	movl	%eax, 344(%rsp)
	movq	344(%rsp), %rax
	movq	%rdx, 848(%rsp)
	movq	%r14, 872(%rsp)
	movq	%rdx, (%rsp)
	movq	%r13, 16(%rsp)
	movq	%rax, 856(%rsp)
	movq	%rax, 8(%rsp)
	movq	%r14, 24(%rsp)
	call	EmitInstruction
	movq	1712(%rsp), %r12
	movq	1720(%rsp), %rbp
	movq	1728(%rsp), %rbx
	jmp	.L439
	.p2align 4,,7
.L443:
	movq	1712(%rsp), %rax
	movq	%rax, (%rsp)
	movq	1720(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	1728(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	ListLen
	leal	-1(%rax), %r12d
	testl	%r12d, %r12d
	jle	.L460
	leaq	1008(%rsp), %rbp
	movl	%r12d, %ebx
	.p2align 4,,7
.L462:
	movl	%ebx, %edx
	movq	%r13, %rsi
	movq	%rbp, %rdi
	call	ListGet
	movq	1008(%rsp), %rax
	movq	%r15, %rdi
	movq	%rax, (%rsp)
	movq	1016(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	1024(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	CompileExpr
	subl	$1, %ebx
	movq	%rax, %r15
	jne	.L462
.L460:
	leaq	976(%rsp), %rdi
	xorl	%edx, %edx
	movq	%r13, %rsi
	call	ListGet
	movq	976(%rsp), %rax
	movq	%r15, %rdi
	movq	%rax, (%rsp)
	movq	984(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	992(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	CompileExpr
	movl	%r12d, 88(%rsp)
	movq	%rax, %r15
	movl	$30, 80(%rsp)
	movq	88(%rsp), %rcx
	movq	80(%rsp), %rsi
	movq	96(%rsp), %rdx
	movq	104(%rsp), %rax
	movq	%rsi, 464(%rsp)
	movq	%rcx, 472(%rsp)
	movq	%rdx, 480(%rsp)
	movq	%rax, 488(%rsp)
.L468:
	movq	%r15, %rdi
	movq	%rsi, (%rsp)
	movq	%rcx, 8(%rsp)
	movq	%rdx, 16(%rsp)
	movq	%rax, 24(%rsp)
	call	EmitInstruction
	jmp	.L441
.L472:
	movq	1712(%rsp), %rax
	leaq	1616(%rsp), %rdi
	movq	%rax, (%rsp)
	movq	1720(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	1728(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	Cdr
	movq	1616(%rsp), %rax
	leaq	1584(%rsp), %rdi
	movq	%rax, (%rsp)
	movq	1624(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	1632(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	Car
	movl	1584(%rsp), %eax
	movq	1592(%rsp), %rdx
	movq	1600(%rsp), %rcx
	movl	$20, 304(%rsp)
	movq	304(%rsp), %rsi
	movl	%eax, 312(%rsp)
	movq	312(%rsp), %rax
	movq	%rcx, 328(%rsp)
	movq	%rdx, 320(%rsp)
	movq	%rsi, 816(%rsp)
	movq	%rdx, 832(%rsp)
	movq	%rax, 824(%rsp)
	movq	%rcx, 840(%rsp)
.L467:
	movq	%r15, %rdi
	movq	%rsi, (%rsp)
	movq	%rax, 8(%rsp)
	movq	%rdx, 16(%rsp)
	movq	%rcx, 24(%rsp)
	call	EmitInstruction
	jmp	.L441
.L447:
	movq	SymbolFn(%rip), %rax
	movq	%r12, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%rbx, 16(%rsp)
	movq	%rax, 24(%rsp)
	movq	SymbolFn+8(%rip), %rax
	movq	%rax, 32(%rsp)
	movq	SymbolFn+16(%rip), %rax
	movq	%rax, 40(%rsp)
	call	ObjEq
	testl	%eax, %eax
	jne	.L473
	movq	SymbolIf(%rip), %rax
	movq	%r12, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%rbx, 16(%rsp)
	movq	%rax, 24(%rsp)
	movq	SymbolIf+8(%rip), %rax
	movq	%rax, 32(%rsp)
	movq	SymbolIf+16(%rip), %rax
	movq	%rax, 40(%rsp)
	call	ObjEq
	testl	%eax, %eax
	je	.L451
	movq	1712(%rsp), %rax
	movq	%rax, (%rsp)
	movq	1720(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	1728(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	ListLen
	leaq	1264(%rsp), %rdi
	movl	$1, %edx
	movq	%r13, %rsi
	movl	%eax, %r12d
	call	ListGet
	movq	1264(%rsp), %rax
	movq	%r15, %rdi
	movq	%rax, (%rsp)
	movq	1272(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	1280(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	CompileExpr
	movl	$40, 688(%rsp)
	movq	%rax, %rbx
	movq	688(%rsp), %rax
	movl	$0, 696(%rsp)
	movq	%rbx, %rdi
	movq	%rax, (%rsp)
	movq	696(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	704(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	712(%rsp), %rax
	movq	%rax, 24(%rsp)
	call	EmitInstruction
	movl	4(%rbx), %ecx
	leaq	1232(%rsp), %rdi
	movl	$2, %edx
	movq	%r13, %rsi
	movq	8(%rbx), %r14
	movl	%ecx, 76(%rsp)
	call	ListGet
	movq	1232(%rsp), %rax
	movq	%rbx, %rdi
	movq	%rax, (%rsp)
	movq	1240(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	1248(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	CompileExpr
	movl	$50, 656(%rsp)
	movq	%rax, %r15
	movq	656(%rsp), %rax
	movl	$0, 664(%rsp)
	movq	%r15, %rdi
	movq	%rax, (%rsp)
	movq	664(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	672(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	680(%rsp), %rax
	movq	%rax, 24(%rsp)
	call	EmitInstruction
	cmpl	$4, %r12d
	movq	8(%r15), %rbp
	movl	4(%r15), %ebx
	je	.L474
	leaq	1168(%rsp), %rdi
	call	Nil
	movl	1168(%rsp), %eax
	movq	1176(%rsp), %rdx
	movq	%r15, %rdi
	movq	1184(%rsp), %rcx
	movl	$20, 176(%rsp)
	movq	176(%rsp), %rsi
	movl	%eax, 184(%rsp)
	movq	184(%rsp), %rax
	movq	%rcx, 200(%rsp)
	movq	%rdx, 192(%rsp)
	movq	%rsi, 624(%rsp)
	movq	%rdx, 640(%rsp)
	movq	%rax, 632(%rsp)
	movq	%rcx, 648(%rsp)
	movq	%rsi, (%rsp)
	movq	%rax, 8(%rsp)
	movq	%rdx, 16(%rsp)
	movq	%rcx, 24(%rsp)
	call	EmitInstruction
.L455:
	movl	4(%r15), %edx
	movslq	%ebx,%rax
	salq	$5, %rax
	movl	%edx, -24(%rax,%rbp)
	movslq	76(%rsp),%rax
	salq	$5, %rax
	movl	%ebx, -24(%rax,%r14)
	jmp	.L441
.L473:
	leaq	1424(%rsp), %rdi
	movq	%r13, %rsi
	movl	$1, %edx
	call	ListGet
	movq	1424(%rsp), %rax
	leaq	1360(%rsp), %rdi
	movq	%rax, 1456(%rsp)
	movq	1432(%rsp), %rax
	movq	%rax, 1464(%rsp)
	movq	1440(%rsp), %rax
	movq	%rax, 1472(%rsp)
	movq	1712(%rsp), %rax
	movq	%rax, (%rsp)
	movq	1720(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	1728(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	Cdr
	movq	1360(%rsp), %rax
	leaq	1328(%rsp), %rdi
	movq	%rax, (%rsp)
	movq	1368(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	1376(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	Cdr
	movq	1344(%rsp), %rax
	movq	1328(%rsp), %rcx
	leaq	1296(%rsp), %rdi
	movq	1336(%rsp), %rdx
	movq	%rax, 1408(%rsp)
	movq	%rax, 40(%rsp)
	movq	1456(%rsp), %rax
	movq	%rcx, 1392(%rsp)
	movq	%rdx, 1400(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 32(%rsp)
	movq	%rax, (%rsp)
	movq	1464(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	1472(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	CompileFunctionObj
	movl	1296(%rsp), %eax
	movq	1304(%rsp), %rdx
	movq	%r15, %rdi
	movq	1312(%rsp), %rcx
	movl	$20, 240(%rsp)
	movq	240(%rsp), %rsi
	movl	%eax, 248(%rsp)
	movq	248(%rsp), %rax
	movq	%rcx, 264(%rsp)
	movq	%rdx, 256(%rsp)
	movq	%rsi, 752(%rsp)
	movq	%rdx, 768(%rsp)
	movq	%rax, 760(%rsp)
	movq	%rcx, 776(%rsp)
	movq	%rsi, (%rsp)
	movq	%rax, 8(%rsp)
	movq	%rdx, 16(%rsp)
	movq	%rcx, 24(%rsp)
	call	EmitInstruction
	movl	$0, 216(%rsp)
	movl	$80, 208(%rsp)
	movq	216(%rsp), %rcx
	movq	208(%rsp), %rsi
	movq	224(%rsp), %rdx
	movq	232(%rsp), %rax
	movq	%rsi, 720(%rsp)
	movq	%rcx, 728(%rsp)
	movq	%rdx, 736(%rsp)
	movq	%rax, 744(%rsp)
	jmp	.L468
.L451:
	movq	SymbolWhile(%rip), %rax
	movq	%r12, (%rsp)
	movq	%rbp, 8(%rsp)
	movq	%rbx, 16(%rsp)
	movq	%rax, 24(%rsp)
	movq	SymbolWhile+8(%rip), %rax
	movq	%rax, 32(%rsp)
	movq	SymbolWhile+16(%rip), %rax
	movq	%rax, 40(%rsp)
	call	ObjEq
	testl	%eax, %eax
	je	.L443
	leaq	1136(%rsp), %rdi
	movl	$1, %edx
	movq	%r13, %rsi
	movl	4(%r15), %r14d
	call	ListGet
	movq	1136(%rsp), %rax
	movq	%r15, %rdi
	movq	%rax, (%rsp)
	movq	1144(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	1152(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	CompileExpr
	movl	$40, 592(%rsp)
	movq	%rax, %r15
	movq	592(%rsp), %rax
	movl	$0, 600(%rsp)
	movq	%r15, %rdi
	movq	%rax, (%rsp)
	movq	600(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	608(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	616(%rsp), %rax
	movq	%rax, 24(%rsp)
	call	EmitInstruction
	movq	8(%r15), %rax
	movl	4(%r15), %ecx
	movq	%rax, 56(%rsp)
	movq	1712(%rsp), %rax
	movl	%ecx, 68(%rsp)
	movq	%rax, (%rsp)
	movq	1720(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	1728(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	ListLen
	cmpl	$2, %eax
	movl	%eax, %ebp
	jle	.L457
	leaq	1104(%rsp), %r12
	movl	$2, %ebx
.L459:
	movl	%ebx, %edx
	movq	%r13, %rsi
	movq	%r12, %rdi
	call	ListGet
	movq	1104(%rsp), %rax
	movq	%r15, %rdi
	addl	$1, %ebx
	movq	%rax, (%rsp)
	movq	1112(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	1120(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	CompileExpr
	movq	160(%rsp), %rdx
	movq	%rax, %r15
	movl	$0, 152(%rsp)
	movq	168(%rsp), %rax
	movq	152(%rsp), %rcx
	movq	%r15, %rdi
	movl	$10, 144(%rsp)
	movq	144(%rsp), %rsi
	movq	%rdx, 576(%rsp)
	movq	%rdx, 16(%rsp)
	movq	%rcx, 568(%rsp)
	movq	%rax, 584(%rsp)
	movq	%rsi, 560(%rsp)
	movq	%rsi, (%rsp)
	movq	%rcx, 8(%rsp)
	movq	%rax, 24(%rsp)
	call	EmitInstruction
	cmpl	%ebp, %ebx
	jne	.L459
.L457:
	movl	$50, 528(%rsp)
	movq	528(%rsp), %rax
	movq	%r15, %rdi
	movl	%r14d, 536(%rsp)
	movq	%rax, (%rsp)
	movq	536(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	544(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	552(%rsp), %rax
	movq	%rax, 24(%rsp)
	call	EmitInstruction
	movslq	68(%rsp),%rax
	movl	4(%r15), %edx
	leaq	1072(%rsp), %rdi
	movq	56(%rsp), %rcx
	salq	$5, %rax
	movl	%edx, -24(%rax,%rcx)
	call	Nil
	movl	1072(%rsp), %eax
	movq	1080(%rsp), %rdx
	movq	1088(%rsp), %rcx
	movl	$20, 112(%rsp)
	movq	112(%rsp), %rsi
	movl	%eax, 120(%rsp)
	movq	120(%rsp), %rax
	movq	%rcx, 136(%rsp)
	movq	%rdx, 128(%rsp)
	movq	%rsi, 496(%rsp)
	movq	%rdx, 512(%rsp)
	movq	%rax, 504(%rsp)
	movq	%rcx, 520(%rsp)
	jmp	.L467
.L474:
	leaq	1200(%rsp), %rdi
	movl	$3, %edx
	movq	%r13, %rsi
	call	ListGet
	movq	1200(%rsp), %rax
	movq	%r15, %rdi
	movq	%rax, (%rsp)
	movq	1208(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	1216(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	CompileExpr
	movq	%rax, %r15
	jmp	.L455
.LFE72:
	.size	CompileExpr, .-CompileExpr
	.p2align 4,,15
.globl CompileFunctionObj
	.type	CompileFunctionObj, @function
CompileFunctionObj:
.LFB73:
	pushq	%r15
.LCFI217:
	movq	%rdi, %r15
	pushq	%r14
.LCFI218:
	pushq	%r13
.LCFI219:
	pushq	%r12
.LCFI220:
	pushq	%rbp
.LCFI221:
	pushq	%rbx
.LCFI222:
	subq	$200, %rsp
.LCFI223:
	call	CreateBytecode
	movq	280(%rsp), %r13
	movq	296(%rsp), %rbx
	movq	%rax, %rbp
	movq	288(%rsp), %r12
	movq	%r13, (%rsp)
	movq	%rbx, 16(%rsp)
	movq	%r12, 8(%rsp)
	call	Obj
	movq	%r13, (%rsp)
	movq	%rbx, 16(%rsp)
	movq	%rax, %r14
	movq	%r12, 8(%rsp)
	movq	%rbp, %rbx
	call	ListLen
	testl	%eax, %eax
	movl	%eax, %r13d
	jle	.L479
	leaq	112(%rsp), %rbp
	xorl	%r12d, %r12d
.L478:
	movl	%r12d, %edx
	movq	%r14, %rsi
	movq	%rbp, %rdi
	call	ListGet
	movq	112(%rsp), %rcx
	movq	120(%rsp), %rdx
	movq	%rbx, %rdi
	movq	128(%rsp), %rax
	addl	$1, %r12d
	movq	%rcx, 144(%rsp)
	movq	%rdx, 152(%rsp)
	movq	%rax, 160(%rsp)
	movq	%rcx, (%rsp)
	movq	%rdx, 8(%rsp)
	movq	%rax, 16(%rsp)
	call	CompileExpr
	cmpl	%r13d, %r12d
	movq	%rax, %rbx
	je	.L479
	movq	64(%rsp), %rdx
	movq	72(%rsp), %rax
	movq	%rbx, %rdi
	movl	$0, 56(%rsp)
	movl	$10, 48(%rsp)
	movq	56(%rsp), %rcx
	movq	48(%rsp), %rsi
	movq	%rdx, 96(%rsp)
	movq	%rax, 104(%rsp)
	movq	%rdx, 16(%rsp)
	movq	%rax, 24(%rsp)
	movq	%rsi, 80(%rsp)
	movq	%rcx, 88(%rsp)
	movq	%rsi, (%rsp)
	movq	%rcx, 8(%rsp)
	call	EmitInstruction
	jmp	.L478
	.p2align 4,,7
.L479:
	movq	256(%rsp), %rax
	movq	%rax, (%rsp)
	movq	264(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	272(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	ParseArgList
	movl	%eax, 32(%rsp)
	movq	32(%rsp), %rax
	movq	%rdx, %rsi
	movq	%rdx, 40(%rsp)
	movq	%rdx, 184(%rsp)
	movq	%rbx, %rdx
	movq	%rax, 176(%rsp)
	movl	176(%rsp), %edi
	call	CreateFunction
	movl	TypeFunction(%rip), %edx
	movq	TypeFunction+8(%rip), %rcx
	movq	%rax, %rsi
	movq	%r15, %rdi
	call	WrapObj
	addq	$200, %rsp
	movq	%r15, %rax
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
.LFE73:
	.size	CompileFunctionObj, .-CompileFunctionObj
	.p2align 4,,15
.globl Compile
	.type	Compile, @function
Compile:
.LFB71:
	subq	$8, %rsp
.LCFI224:
	call	CreateBytecode
	addq	$8, %rsp
	movq	%rax, %rdi
	jmp	CompileExpr
.LFE71:
	.size	Compile, .-Compile
	.p2align 4,,15
.globl CreateString
	.type	CreateString, @function
CreateString:
.LFB26:
	movq	%rbx, -24(%rsp)
.LCFI225:
	movq	%r12, -16(%rsp)
.LCFI226:
	movq	%rsi, %rbx
	movq	%r13, -8(%rsp)
.LCFI227:
	subq	$24, %rsp
.LCFI228:
	movq	%rdi, %r13
	movl	$8, %edi
	call	VyMalloc
	movq	%rbx, %rdi
	movq	%rax, %r12
	call	strdup
	movl	TypeString(%rip), %edx
	movq	TypeString+8(%rip), %rcx
	movq	%r12, %rsi
	movq	%rax, (%r12)
	movq	%r13, %rdi
	call	WrapObj
	movq	%r13, %rax
	movq	(%rsp), %rbx
	movq	8(%rsp), %r12
	movq	16(%rsp), %r13
	addq	$24, %rsp
	ret
.LFE26:
	.size	CreateString, .-CreateString
	.p2align 4,,15
.globl Cons
	.type	Cons, @function
Cons:
.LFB20:
	movq	%rbx, -48(%rsp)
.LCFI229:
	movq	%rbp, -40(%rsp)
.LCFI230:
	movq	%r12, -32(%rsp)
.LCFI231:
	movq	%r13, -24(%rsp)
.LCFI232:
	movq	%r14, -16(%rsp)
.LCFI233:
	movq	%r15, -8(%rsp)
.LCFI234:
	subq	$56, %rsp
.LCFI235:
	movq	72(%rsp), %r12
	movl	64(%rsp), %r13d
	movq	96(%rsp), %r15
	movl	88(%rsp), %ebp
	movq	80(%rsp), %rbx
	movq	104(%rsp), %r14
	movq	%rdi, (%rsp)
	movl	$48, %edi
	call	VyMalloc
	movq	(%rsp), %rdi
	movq	%r12, 8(%rax)
	movq	%rax, %rsi
	movl	%r13d, (%rax)
	movq	%r15, 32(%rax)
	movl	%ebp, 24(%rax)
	movl	TypeCons(%rip), %edx
	movq	TypeCons+8(%rip), %rcx
	movq	%rbx, 16(%rax)
	movq	%r14, 40(%rax)
	call	WrapObj
	movq	(%rsp), %rax
	movq	8(%rsp), %rbx
	movq	16(%rsp), %rbp
	movq	24(%rsp), %r12
	movq	32(%rsp), %r13
	movq	40(%rsp), %r14
	movq	48(%rsp), %r15
	addq	$56, %rsp
	ret
.LFE20:
	.size	Cons, .-Cons
	.p2align 4,,15
.globl MakeList
	.type	MakeList, @function
MakeList:
.LFB101:
	pushq	%r13
.LCFI236:
	movl	%edx, %r13d
	pushq	%r12
.LCFI237:
	pushq	%rbp
.LCFI238:
	movq	%rdi, %rbp
	pushq	%rbx
.LCFI239:
	movq	%rsi, %rbx
	subq	$88, %rsp
.LCFI240:
	leaq	48(%rsp), %rdi
	call	Nil
	movq	48(%rsp), %rax
	movq	%rax, (%rbp)
	movq	56(%rsp), %rax
	movq	%rax, 8(%rbp)
	movq	64(%rsp), %rax
	movq	%rax, 16(%rbp)
	movl	%r13d, %eax
	subl	$1, %eax
	js	.L488
	cltq
	xorl	%r12d, %r12d
	leaq	(%rax,%rax,2), %rax
	leaq	(%rbx,%rax,8), %rbx
	.p2align 4,,7
.L491:
	movq	(%rbp), %rax
	movq	%rbp, %rdi
	addl	$1, %r12d
	movq	%rax, 24(%rsp)
	movq	8(%rbp), %rax
	movq	%rax, 32(%rsp)
	movq	16(%rbp), %rax
	movq	%rax, 40(%rsp)
	movq	(%rbx), %rax
	movq	%rax, (%rsp)
	movq	8(%rbx), %rax
	movq	%rax, 8(%rsp)
	movq	16(%rbx), %rax
	subq	$24, %rbx
	movq	%rax, 16(%rsp)
	call	Cons
	cmpl	%r13d, %r12d
	jne	.L491
.L488:
	addq	$88, %rsp
	movq	%rbp, %rax
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	ret
.LFE101:
	.size	MakeList, .-MakeList
	.p2align 4,,15
.globl MakeCons
	.type	MakeCons, @function
MakeCons:
.LFB100:
	pushq	%rbx
.LCFI241:
	movq	%rdi, %rbx
	subq	$48, %rsp
.LCFI242:
	movq	24(%rsi), %rax
	movq	%rax, 24(%rsp)
	movq	32(%rsi), %rax
	movq	%rax, 32(%rsp)
	movq	40(%rsi), %rax
	movq	%rax, 40(%rsp)
	movq	(%rsi), %rax
	movq	%rax, (%rsp)
	movq	8(%rsi), %rax
	movq	%rax, 8(%rsp)
	movq	16(%rsi), %rax
	movq	%rax, 16(%rsp)
	call	Cons
	movq	%rbx, %rax
	addq	$48, %rsp
	popq	%rbx
	ret
.LFE100:
	.size	MakeCons, .-MakeCons
	.p2align 4,,15
.globl BindArguments
	.type	BindArguments, @function
BindArguments:
.LFB36:
	pushq	%r15
.LCFI243:
	pushq	%r14
.LCFI244:
	xorl	%r14d, %r14d
	pushq	%r13
.LCFI245:
	movq	%rdx, %r13
	pushq	%r12
.LCFI246:
	movl	%ecx, %r12d
	pushq	%rbp
.LCFI247:
	pushq	%rbx
.LCFI248:
	subq	$280, %rsp
.LCFI249:
	testl	%ecx, %ecx
	movl	%edi, 68(%rsp)
	movq	%rsi, 56(%rsp)
	movl	%edi, 112(%rsp)
	movq	%rsi, 120(%rsp)
	jg	.L500
	jmp	.L499
	.p2align 4,,7
.L519:
	leaq	(%rdx,%rdx,2), %rax
	movq	%r15, %rdi
	addl	$1, %r14d
	leaq	0(,%rax,8), %rdx
	movq	(%r13,%rax,8), %rax
	movq	%rax, (%rsp)
	movq	8(%rdx,%r13), %rax
	movq	%rax, 8(%rsp)
	movq	16(%rdx,%r13), %rax
	movq	%rax, 16(%rsp)
	call	VariableBind
	cmpl	%r14d, %r12d
	jle	.L499
.L500:
	movslq	%r14d,%rdx
	movq	56(%rsp), %rcx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rcx,%rax,8), %rax
	movq	8(%rax), %r15
	movl	4(%rax), %eax
	testl	%eax, %eax
	je	.L519
	leaq	208(%rsp), %rdi
	leal	-1(%r12), %ebp
	call	Nil
	movq	208(%rsp), %rax
	cmpl	%r14d, %ebp
	movq	%rax, 240(%rsp)
	movq	216(%rsp), %rax
	movq	%rax, 248(%rsp)
	movq	224(%rsp), %rax
	movq	%rax, 256(%rsp)
	jl	.L504
	movslq	%ebp,%rax
	leaq	(%rax,%rax,2), %rax
	leaq	(%r13,%rax,8), %rbx
	.p2align 4,,7
.L506:
	movq	240(%rsp), %rax
	leaq	240(%rsp), %rdi
	subl	$1, %ebp
	movq	%rax, 24(%rsp)
	movq	248(%rsp), %rax
	movq	%rax, 32(%rsp)
	movq	256(%rsp), %rax
	movq	%rax, 40(%rsp)
	movq	(%rbx), %rax
	movq	%rax, (%rsp)
	movq	8(%rbx), %rax
	movq	%rax, 8(%rsp)
	movq	16(%rbx), %rax
	subq	$24, %rbx
	movq	%rax, 16(%rsp)
	call	Cons
	cmpl	%r14d, %ebp
	jge	.L506
.L504:
	movq	240(%rsp), %rax
	movl	%r12d, %r14d
	movq	%r15, %rdi
	addl	$1, %r14d
	movq	%rax, (%rsp)
	movq	248(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	256(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	VariableBind
	cmpl	%r14d, %r12d
	jg	.L500
.L499:
	cmpl	%r14d, 68(%rsp)
	jle	.L515
	movslq	%r14d,%rax
	movq	56(%rsp), %rdx
	xorl	%r15d, %r15d
	leaq	(%rax,%rax,4), %rax
	leaq	32(%rdx,%rax,8), %rbx
	jmp	.L509
	.p2align 4,,7
.L520:
	leaq	176(%rsp), %rdi
	call	Nil
	movq	176(%rsp), %rax
	movq	72(%rsp), %rdi
	movq	%rax, (%rsp)
	movq	184(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	192(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	VariableBind
.L512:
	movl	68(%rsp), %eax
	addl	$1, %r15d
	addq	$40, %rbx
	subl	%r14d, %eax
	cmpl	%eax, %r15d
	je	.L515
.L509:
	movl	-28(%rbx), %eax
	movq	-24(%rbx), %rcx
	movq	(%rbx), %r12
	movq	-8(%rbx), %rbp
	movl	-16(%rbx), %r13d
	testl	%eax, %eax
	movq	%rcx, 72(%rsp)
	jne	.L520
	movl	%r13d, 144(%rsp)
	movq	144(%rsp), %rax
	movq	%r12, 160(%rsp)
	movq	%rbp, 152(%rsp)
	movq	%rbp, 8(%rsp)
	movq	%r12, 16(%rsp)
	movq	%rax, (%rsp)
	call	IsNone
	testl	%eax, %eax
	jne	.L521
.L513:
	movl	%r13d, 144(%rsp)
	movq	72(%rsp), %rdi
	movq	144(%rsp), %rax
	movq	%r12, 160(%rsp)
	movq	%rbp, 152(%rsp)
	movq	%rbp, 8(%rsp)
	movq	%r12, 16(%rsp)
	movq	%rax, (%rsp)
	call	VariableBind
	jmp	.L512
.L521:
	leaq	80(%rsp), %rdi
	call	Nil
	movq	80(%rsp), %rax
	movq	88(%rsp), %rdx
	movq	%rax, 144(%rsp)
	movq	96(%rsp), %rax
	movq	%rdx, %rbp
	movl	144(%rsp), %r13d
	movq	%rdx, 152(%rsp)
	movq	%rax, %r12
	movq	%rax, 160(%rsp)
	jmp	.L513
.L515:
	addq	$280, %rsp
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
.LFE36:
	.size	BindArguments, .-BindArguments
	.p2align 4,,15
.globl CallInstr
	.type	CallInstr, @function
CallInstr:
.LFB55:
	pushq	%rbp
.LCFI250:
	movq	%rsp, %rbp
.LCFI251:
	pushq	%r15
.LCFI252:
	pushq	%r14
.LCFI253:
	pushq	%r13
.LCFI254:
	movl	%edi, %r13d
	leaq	-112(%rbp), %rdi
	pushq	%r12
.LCFI255:
	pushq	%rbx
.LCFI256:
	subq	$280, %rsp
.LCFI257:
	call	StackPop
	movq	-112(%rbp), %rcx
	movq	-104(%rbp), %rdx
	movq	-96(%rbp), %rax
	movq	%rcx, -80(%rbp)
	movq	%rdx, -72(%rbp)
	movq	%rax, -64(%rbp)
	movq	%rcx, (%rsp)
	movq	%rdx, 8(%rsp)
	movq	%rax, 16(%rsp)
	call	Obj
	movq	%rax, -280(%rbp)
	movslq	%r13d,%rax
	leaq	(%rax,%rax,2), %rax
	leaq	30(,%rax,8), %rax
	andq	$-16, %rax
	subq	%rax, %rsp
	leaq	39(%rsp), %r15
	andq	$-16, %r15
	testl	%r13d, %r13d
	movq	%r15, %rbx
	jle	.L523
	leaq	-272(%rbp), %r14
	xorl	%r12d, %r12d
	.p2align 4,,7
.L525:
	movq	%r14, %rdi
	addl	$1, %r12d
	call	StackPop
	movq	-272(%rbp), %rax
	movq	%rax, (%rbx)
	movq	-264(%rbp), %rax
	movq	%rax, 8(%rbx)
	movq	-256(%rbp), %rax
	movq	%rax, 16(%rbx)
	addq	$24, %rbx
	cmpl	%r13d, %r12d
	jne	.L525
.L523:
	movq	-280(%rbp), %rax
	movl	24(%rax), %eax
	testl	%eax, %eax
	jne	.L526
	call	CurrentScope
	movq	%rax, %r12
	movq	-280(%rbp), %rax
	movq	32(%rax), %rdi
	call	CreateScope
	movq	%rax, %rdi
	movq	%rax, %rbx
	call	EnterScope
	movq	-280(%rbp), %rax
	movl	%r13d, %ecx
	movq	%r15, %rdx
	movl	(%rax), %edi
	movq	8(%rax), %rsi
	call	BindArguments
	movq	-280(%rbp), %rax
	leaq	-176(%rbp), %rdi
	movq	16(%rax), %rsi
	call	EvalBytecode
	movq	-176(%rbp), %rax
	movq	%rbx, %rdi
	movq	%rax, -144(%rbp)
	movq	-168(%rbp), %rax
	movq	%rax, -136(%rbp)
	movq	-160(%rbp), %rax
	movq	%rax, -128(%rbp)
	call	DeleteScope
	movq	%r12, %rdi
	call	EnterScope
	movq	-144(%rbp), %rax
	movq	%rax, (%rsp)
	movq	-136(%rbp), %rax
	movq	%rax, 8(%rsp)
	movq	-128(%rbp), %rax
	movq	%rax, 16(%rsp)
	call	StackPush
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	leave
	ret
.L526:
	movq	-280(%rbp), %rax
	leaq	-240(%rbp), %rdi
	movl	%r13d, %edx
	movq	%r15, %rsi
	call	*16(%rax)
	movq	-240(%rbp), %rcx
	movq	-232(%rbp), %rdx
	movq	-224(%rbp), %rax
	movq	%rcx, -208(%rbp)
	movq	%rdx, -200(%rbp)
	movq	%rax, -192(%rbp)
	movq	%rcx, (%rsp)
	movq	%rdx, 8(%rsp)
	movq	%rax, 16(%rsp)
	call	StackPush
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	leave
	ret
.LFE55:
	.size	CallInstr, .-CallInstr
	.section	.rodata.str1.8
	.align 8
.LC39:
	.string	"Unknown opcode type (%d), I'm confused.\n"
	.text
	.p2align 4,,15
.globl EvalBytecode
	.type	EvalBytecode, @function
EvalBytecode:
.LFB48:
	movq	%rbx, -32(%rsp)
.LCFI258:
	movq	%r13, -8(%rsp)
.LCFI259:
	movq	%rsi, %rbx
	movq	%rbp, -24(%rsp)
.LCFI260:
	movq	%r12, -16(%rsp)
.LCFI261:
	subq	$104, %rsp
.LCFI262:
	movl	4(%rsi), %esi
	movq	%rdi, %r13
	testl	%esi, %esi
	jle	.L532
	movl	$-1, %r12d
	xorl	%ebp, %ebp
.L534:
	testl	%r12d, %r12d
	js	.L535
	cmpl	%esi, %r12d
	jge	.L532
	movl	%r12d, %ebp
	movl	$-1, %r12d
.L535:
	movq	8(%rbx), %rcx
	movslq	%ebp,%rdx
	salq	$5, %rdx
	movq	(%rdx,%rcx), %rax
	movq	%rax, 32(%rsp)
	movq	8(%rdx,%rcx), %rax
	movq	%rax, 40(%rsp)
	movq	16(%rdx,%rcx), %rax
	movq	%rax, 48(%rsp)
	movq	24(%rdx,%rcx), %rax
	movl	32(%rsp), %edx
	movq	%rax, 56(%rsp)
	leal	-10(%rdx), %eax
	cmpl	$70, %eax
	ja	.L538
	mov	%eax, %eax
	jmp	*.L547(,%rax,8)
	.section	.rodata
	.align 8
	.align 4
.L547:
	.quad	.L539
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L540
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L541
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L542
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L543
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L544
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L545
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L538
	.quad	.L546
	.text
	.p2align 4,,7
.L538:
	movq	stderr(%rip), %rdi
	movl	$.LC39, %esi
	xorl	%eax, %eax
	call	fprintf
	xorl	%edi, %edi
	call	exit
.L539:
	call	PopInstr
	movl	4(%rbx), %esi
	.p2align 4,,7
.L548:
	addl	$1, %ebp
	cmpl	%ebp, %esi
	jg	.L534
.L532:
	movq	%r13, %rdi
	call	StackPop
	movq	%r13, %rax
	movq	72(%rsp), %rbx
	movq	80(%rsp), %rbp
	movq	88(%rsp), %r12
	movq	96(%rsp), %r13
	addq	$104, %rsp
	ret
.L546:
	call	FuncInstr
	movl	4(%rbx), %esi
	jmp	.L548
.L545:
	.p2align 4,,6
	call	BindInstr
	movl	4(%rbx), %esi
	.p2align 4,,4
	jmp	.L548
.L544:
	movq	40(%rsp), %rax
	movq	%rax, (%rsp)
	movq	48(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	56(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	ValueInstr
	movl	4(%rbx), %esi
	jmp	.L548
.L543:
	movl	40(%rsp), %r12d
	jmp	.L548
.L542:
	movl	40(%rsp), %edi
	call	IfJmpInstr
	movl	4(%rbx), %esi
	movl	%eax, %r12d
	jmp	.L548
.L541:
	movl	40(%rsp), %edi
	call	CallInstr
	movl	4(%rbx), %esi
	jmp	.L548
.L540:
	movq	40(%rsp), %rax
	movq	%rax, (%rsp)
	movq	48(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	56(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	PushInstr
	movl	4(%rbx), %esi
	jmp	.L548
.LFE48:
	.size	EvalBytecode, .-EvalBytecode
	.p2align 4,,15
.globl Eval
	.type	Eval, @function
Eval:
.LFB47:
	movq	%rbx, -40(%rsp)
.LCFI263:
	movq	%r12, -32(%rsp)
.LCFI264:
	movq	%rdi, %rbx
	movq	%r13, -24(%rsp)
.LCFI265:
	movq	%r14, -16(%rsp)
.LCFI266:
	movq	%r15, -8(%rsp)
.LCFI267:
	subq	$104, %rsp
.LCFI268:
	movq	112(%rsp), %rax
	movq	%rax, (%rsp)
	movq	120(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	128(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	Compile
	leaq	32(%rsp), %rdi
	movq	%rax, %rsi
	movq	%rax, %r15
	call	EvalBytecode
	movq	48(%rsp), %r12
	movq	40(%rsp), %r13
	movq	%r15, %rdi
	movl	32(%rsp), %r14d
	call	FreeBytecode
	movq	%r12, 16(%rbx)
	movq	%r13, 8(%rbx)
	movq	%rbx, %rax
	movl	%r14d, (%rbx)
	movq	72(%rsp), %r12
	movq	64(%rsp), %rbx
	movq	80(%rsp), %r13
	movq	88(%rsp), %r14
	movq	96(%rsp), %r15
	addq	$104, %rsp
	ret
.LFE47:
	.size	Eval, .-Eval
	.p2align 4,,15
.globl ParseList
	.type	ParseList, @function
ParseList:
.LFB16:
	pushq	%rbp
.LCFI269:
	movq	%rdx, %rbp
	pushq	%rbx
.LCFI270:
	movq	%rdi, %rbx
	subq	$248, %rsp
.LCFI271:
	cmpb	$20, (%rsi)
	je	.L558
	leaq	144(%rsp), %rdi
	leaq	232(%rsp), %rdx
	call	Parse
	movq	144(%rsp), %rax
	movq	232(%rsp), %rsi
	leaq	80(%rsp), %rdi
	movq	%rbp, %rdx
	movq	%rax, 176(%rsp)
	movq	152(%rsp), %rax
	movq	%rax, 184(%rsp)
	movq	160(%rsp), %rax
	movq	%rax, 192(%rsp)
	call	ParseList
	movq	96(%rsp), %rax
	movq	80(%rsp), %rcx
	leaq	48(%rsp), %rdi
	movq	88(%rsp), %rdx
	movq	%rax, 128(%rsp)
	movq	%rax, 40(%rsp)
	movq	176(%rsp), %rax
	movq	%rcx, 112(%rsp)
	movq	%rdx, 120(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 32(%rsp)
	movq	%rax, (%rsp)
	movq	184(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	192(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	Cons
	movq	48(%rsp), %rax
	movq	%rax, (%rbx)
	movq	56(%rsp), %rax
	movq	%rax, 8(%rbx)
	movq	64(%rsp), %rax
	movq	%rax, 16(%rbx)
	movq	%rbx, %rax
	addq	$248, %rsp
	popq	%rbx
	popq	%rbp
	ret
	.p2align 4,,7
.L558:
	movq	24(%rsi), %rax
	leaq	208(%rsp), %rdi
	movq	%rax, (%rdx)
	call	Nil
	movq	208(%rsp), %rax
	movq	%rax, (%rbx)
	movq	216(%rsp), %rax
	movq	%rax, 8(%rbx)
	movq	224(%rsp), %rax
	movq	%rax, 16(%rbx)
	movq	%rbx, %rax
	addq	$248, %rsp
	popq	%rbx
	popq	%rbp
	ret
.LFE16:
	.size	ParseList, .-ParseList
	.section	.rodata.str1.1
.LC40:
	.string	"Error: No code in the file.\n"
.LC41:
	.string	"Error: Unexpected token type\n"
	.text
	.p2align 4,,15
.globl Parse
	.type	Parse, @function
Parse:
.LFB18:
	pushq	%rbx
.LCFI272:
	movq	%rdi, %rbx
	subq	$192, %rsp
.LCFI273:
	movzbl	(%rsi), %eax
	movq	8(%rsi), %rcx
	subl	$10, %eax
	cmpb	$50, %al
	ja	.L560
	movzbl	%al, %eax
	jmp	*.L568(,%rax,8)
	.section	.rodata
	.align 8
	.align 4
.L568:
	.quad	.L561
	.quad	.L560
	.quad	.L560
	.quad	.L560
	.quad	.L560
	.quad	.L560
	.quad	.L560
	.quad	.L560
	.quad	.L560
	.quad	.L560
	.quad	.L560
	.quad	.L560
	.quad	.L560
	.quad	.L560
	.quad	.L560
	.quad	.L562
	.quad	.L563
	.quad	.L560
	.quad	.L560
	.quad	.L560
	.quad	.L564
	.quad	.L560
	.quad	.L560
	.quad	.L560
	.quad	.L560
	.quad	.L560
	.quad	.L560
	.quad	.L560
	.quad	.L560
	.quad	.L560
	.quad	.L565
	.quad	.L560
	.quad	.L560
	.quad	.L560
	.quad	.L560
	.quad	.L560
	.quad	.L560
	.quad	.L560
	.quad	.L560
	.quad	.L560
	.quad	.L566
	.quad	.L560
	.quad	.L560
	.quad	.L560
	.quad	.L560
	.quad	.L560
	.quad	.L560
	.quad	.L560
	.quad	.L560
	.quad	.L560
	.quad	.L567
	.text
	.p2align 4,,7
.L560:
	movq	stderr(%rip), %rcx
	movl	$.LC41, %edi
	movl	$29, %edx
	movl	$1, %esi
	call	fwrite
	xorl	%edi, %edi
	call	exit
.L561:
	movq	24(%rsi), %rsi
	leaq	160(%rsp), %rdi
	call	ParseList
	movq	160(%rsp), %rax
	movq	%rax, (%rbx)
	movq	168(%rsp), %rax
	movq	%rax, 8(%rbx)
	movq	176(%rsp), %rax
	movq	%rax, 16(%rbx)
	.p2align 4,,7
.L559:
	movq	%rbx, %rax
	addq	$192, %rsp
	popq	%rbx
	ret
.L567:
	movq	stderr(%rip), %rcx
	movl	$.LC40, %edi
	movl	$28, %edx
	movl	$1, %esi
	call	fwrite
	xorl	%edi, %edi
	call	exit
.L566:
	movq	24(%rsi), %rax
	leaq	64(%rsp), %rdi
	movq	%rcx, %rsi
	movq	%rax, (%rdx)
	call	CreateString
	movq	64(%rsp), %rax
	movq	%rax, (%rbx)
	movq	72(%rsp), %rax
	movq	%rax, 8(%rbx)
	movq	80(%rsp), %rax
	movq	%rax, 16(%rbx)
	jmp	.L559
.L565:
	movq	24(%rsi), %rsi
	leaq	128(%rsp), %rdi
	call	ParseQuoted
	movq	128(%rsp), %rax
	movq	%rax, (%rbx)
	movq	136(%rsp), %rax
	movq	%rax, 8(%rbx)
	movq	144(%rsp), %rax
	movq	%rax, 16(%rbx)
	jmp	.L559
.L564:
	movq	24(%rsi), %rax
	leaq	96(%rsp), %rdi
	movq	%rcx, %rsi
	movq	%rax, (%rdx)
	call	CreateSymbol
	movq	96(%rsp), %rax
	movq	%rax, (%rbx)
	movq	104(%rsp), %rax
	movq	%rax, 8(%rbx)
	movq	112(%rsp), %rax
	movq	%rax, 16(%rbx)
	jmp	.L559
.L563:
	movq	24(%rsi), %rax
	movq	%rsp, %rdi
	movq	%rcx, %rsi
	movq	%rax, (%rdx)
	call	CreateIntFromStr
	movq	(%rsp), %rax
	movq	%rax, (%rbx)
	movq	8(%rsp), %rax
	movq	%rax, 8(%rbx)
	movq	16(%rsp), %rax
	movq	%rax, 16(%rbx)
	jmp	.L559
.L562:
	movq	24(%rsi), %rax
	leaq	32(%rsp), %rdi
	movq	%rcx, %rsi
	movq	%rax, (%rdx)
	call	CreateFloatFromStr
	movq	32(%rsp), %rax
	movq	%rax, (%rbx)
	movq	40(%rsp), %rax
	movq	%rax, 8(%rbx)
	movq	48(%rsp), %rax
	movq	%rax, 16(%rbx)
	jmp	.L559
.LFE18:
	.size	Parse, .-Parse
	.p2align 4,,15
.globl ParseQuoted
	.type	ParseQuoted, @function
ParseQuoted:
.LFB17:
	pushq	%r13
.LCFI274:
	movq	%rdi, %r13
	pushq	%r12
.LCFI275:
	movq	%rsi, %r12
	pushq	%rbx
.LCFI276:
	movq	%rdx, %rbx
	subq	$176, %rsp
.LCFI277:
	movq	%rsi, (%rbx)
	leaq	144(%rsp), %rdi
	call	Nil
	leaq	112(%rsp), %rdi
	movq	%rbx, %rdx
	movq	%r12, %rsi
	call	Parse
	movq	144(%rsp), %rax
	leaq	80(%rsp), %rdi
	movq	%rax, 24(%rsp)
	movq	152(%rsp), %rax
	movq	%rax, 32(%rsp)
	movq	160(%rsp), %rax
	movq	%rax, 40(%rsp)
	movq	112(%rsp), %rax
	movq	%rax, (%rsp)
	movq	120(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	128(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	Cons
	leaq	48(%rsp), %rdi
	movl	$.LC27, %esi
	call	CreateSymbol
	movq	80(%rsp), %rax
	movq	%r13, %rdi
	movq	%rax, 24(%rsp)
	movq	88(%rsp), %rax
	movq	%rax, 32(%rsp)
	movq	96(%rsp), %rax
	movq	%rax, 40(%rsp)
	movq	48(%rsp), %rax
	movq	%rax, (%rsp)
	movq	56(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	64(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	Cons
	addq	$176, %rsp
	movq	%r13, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	ret
.LFE17:
	.size	ParseQuoted, .-ParseQuoted
	.p2align 4,,15
.globl AppendToken
	.type	AppendToken, @function
AppendToken:
.LFB5:
	movq	%rbx, -24(%rsp)
.LCFI278:
	movq	%rbp, -16(%rsp)
.LCFI279:
	movq	%rdi, %rbx
	movq	%r12, -8(%rsp)
.LCFI280:
	subq	$40, %rsp
.LCFI281:
	movq	%rdx, %rbp
	movl	%esi, (%rsp)
	movq	%rdx, 8(%rsp)
	cmpb	$60, (%rdi)
	movzbl	(%rsp), %r12d
	je	.L578
	movl	$32, %edi
	call	VyMalloc
	movq	%rax, %rdi
	movl	16(%rbx), %eax
	movq	$0, 24(%rdi)
	movq	%rbp, 8(%rdi)
	movq	%rdi, 24(%rbx)
	movb	%r12b, (%rdi)
	movq	%rdi, %rbx
	addl	$1, %eax
	movl	%eax, 16(%rdi)
.L576:
	movq	%rbx, %rax
	movq	24(%rsp), %rbp
	movq	16(%rsp), %rbx
	movq	32(%rsp), %r12
	addq	$40, %rsp
	ret
	.p2align 4,,7
.L578:
	movq	%rdx, 8(%rbx)
	movb	%r12b, (%rdi)
	jmp	.L576
.LFE5:
	.size	AppendToken, .-AppendToken
	.p2align 4,,15
.globl CreateTokenList
	.type	CreateTokenList, @function
CreateTokenList:
.LFB4:
	subq	$8, %rsp
.LCFI282:
	movl	$32, %edi
	call	VyMalloc
	movl	$0, 16(%rax)
	movq	$0, 8(%rax)
	movb	$60, (%rax)
	movq	$0, 24(%rax)
	addq	$8, %rsp
	ret
.LFE4:
	.size	CreateTokenList, .-CreateTokenList
	.p2align 4,,15
.globl FindTokenType
	.type	FindTokenType, @function
FindTokenType:
.LFB8:
	pushq	%rbp
.LCFI283:
	pushq	%rbx
.LCFI284:
	movq	%rdi, %rbx
	subq	$24, %rsp
.LCFI285:
	leaq	16(%rsp), %rsi
	call	strtod
	movq	16(%rsp), %rax
	movsd	%xmm0, (%rsp)
	movl	$30, %edx
	movq	(%rsp), %rbp
	cmpb	$0, (%rax)
	je	.L594
	addq	$24, %rsp
	movl	%edx, %eax
	popq	%rbx
	popq	%rbp
	ret
	.p2align 4,,7
.L594:
	movq	%rbx, %rdi
	call	atoi
	cvtsi2sd	%eax, %xmm0
	movq	%rbp, (%rsp)
	movsd	(%rsp), %xmm1
	ucomisd	%xmm0, %xmm1
	je	.L595
.L585:
	addq	$24, %rsp
	movl	$25, %edx
	popq	%rbx
	popq	%rbp
	movl	%edx, %eax
	ret
.L595:
	jp	.L585
	movq	%rbx, %rdi
	call	strlen
	xorl	%edx, %edx
	movq	%rax, %rcx
	.p2align 4,,2
	jmp	.L588
	.p2align 4,,7
.L596:
	movzbl	(%rdx,%rbx), %eax
	addq	$1, %rdx
	cmpb	$46, %al
	je	.L585
.L588:
	cmpq	%rcx, %rdx
	jne	.L596
	addq	$24, %rsp
	movl	$26, %edx
	popq	%rbx
	popq	%rbp
	movl	%edx, %eax
	ret
.LFE8:
	.size	FindTokenType, .-FindTokenType
	.p2align 4,,15
.globl LexFile
	.type	LexFile, @function
LexFile:
.LFB9:
	pushq	%r15
.LCFI286:
	pushq	%r14
.LCFI287:
	pushq	%r13
.LCFI288:
	pushq	%r12
.LCFI289:
	pushq	%rbp
.LCFI290:
	movq	%rdi, %rbp
	pushq	%rbx
.LCFI291:
	subq	$40, %rsp
.LCFI292:
	call	CreateTokenList
	movq	%rbp, %rdi
	movq	%rax, 8(%rsp)
	call	fgetc
	movq	%rbp, %rdi
	movl	%eax, %ebx
	movq	8(%rsp), %r15
	call	feof
	testl	%eax, %eax
	jne	.L599
	.p2align 4,,7
.L647:
	leal	-34(%rbx), %eax
	cmpb	$25, %al
	ja	.L601
	movzbl	%al, %eax
	.p2align 4,,3
	jmp	*.L607(,%rax,8)
	.section	.rodata
	.align 8
	.align 4
.L607:
	.quad	.L602
	.quad	.L601
	.quad	.L601
	.quad	.L601
	.quad	.L601
	.quad	.L603
	.quad	.L604
	.quad	.L605
	.quad	.L601
	.quad	.L601
	.quad	.L601
	.quad	.L601
	.quad	.L601
	.quad	.L601
	.quad	.L601
	.quad	.L601
	.quad	.L601
	.quad	.L601
	.quad	.L601
	.quad	.L601
	.quad	.L601
	.quad	.L601
	.quad	.L601
	.quad	.L601
	.quad	.L601
	.quad	.L637
	.text
	.p2align 4,,7
.L637:
	movq	%rbp, %rdi
	call	fgetc
	cmpb	$10, %al
	.p2align 4,,2
	jne	.L637
	.p2align 4,,7
.L608:
	movq	%rbp, %rdi
	call	fgetc
	movl	%eax, %ebx
.L649:
	movq	%rbp, %rdi
	call	feof
	testl	%eax, %eax
	je	.L647
.L599:
	movq	8(%rsp), %rax
	addq	$40, %rsp
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
	.p2align 4,,7
.L601:
	cmpb	$32, %bl
	je	.L608
	cmpb	$9, %bl
	je	.L608
	cmpb	$10, %bl
	.p2align 4,,5
	je	.L608
	cmpb	$41, %bl
	.p2align 4,,5
	je	.L618
	cmpb	$39, %bl
	.p2align 4,,5
	je	.L618
	xorl	%r12d, %r12d
	cmpb	$40, %bl
	.p2align 4,,3
	jne	.L631
	.p2align 4,,5
	jmp	.L618
	.p2align 4,,7
.L633:
	movq	%rbp, %rdi
	addl	$1, %r12d
	call	fgetc
	cmpb	$41, %al
	.p2align 4,,2
	je	.L644
	cmpb	$39, %al
	je	.L644
	cmpb	$40, %al
	.p2align 4,,5
	je	.L644
	cmpb	$32, %al
	.p2align 4,,5
	je	.L644
	cmpb	$9, %al
	.p2align 4,,5
	je	.L644
	cmpb	$10, %al
	.p2align 4,,5
	je	.L644
.L631:
	movq	%rbp, %rdi
	.p2align 4,,5
	call	feof
	testl	%eax, %eax
	.p2align 4,,2
	je	.L633
.L644:
	leal	1(%r12), %eax
	movslq	%eax,%rdi
	movl	%r12d, %eax
	movslq	%r12d,%r12
	notl	%eax
	movq	%r12, %r13
	movslq	%eax,%r14
.L624:
	call	VyMalloc
	movl	$1, %edx
	movq	%rax, %rbx
	movq	%r14, %rsi
	movq	%rbp, %rdi
	call	fseek
	movq	%rbp, %rcx
	movq	%r12, %rdx
	movl	$1, %esi
	movq	%rbx, %rdi
	call	fread
	movb	$0, (%rbx,%r13)
	movq	%rbx, %rdi
	call	FindTokenType
	movb	%al, 16(%rsp)
	jmp	.L646
.L602:
	movq	%rbp, %rdi
	xorl	%r12d, %r12d
	call	fgetc
	movl	$34, %edx
	movl	%eax, %ebx
	jmp	.L609
	.p2align 4,,7
.L648:
	movq	%rbp, %rdi
	addl	$1, %r12d
	call	fgetc
	movl	%ebx, %edx
	movl	%eax, %ebx
.L609:
	cmpb	$34, %bl
	jne	.L610
	cmpb	$92, %dl
	jne	.L612
.L610:
	movq	%rbp, %rdi
	call	feof
	testl	%eax, %eax
	.p2align 4,,2
	je	.L648
.L612:
	leal	1(%r12), %edi
	movslq	%edi,%rdi
	call	VyMalloc
	movl	%r12d, %esi
	movq	%rax, %rbx
	movl	$1, %edx
	notl	%esi
	movq	%rbp, %rdi
	movslq	%r12d,%r12
	movslq	%esi,%rsi
	call	fseek
	movq	%rbp, %rcx
	movq	%r12, %rdx
	movl	$1, %esi
	movq	%rbx, %rdi
	call	fread
	movb	$0, (%r12,%rbx)
	movq	%rbp, %rdi
	call	fgetc
	movb	$50, 16(%rsp)
	.p2align 4,,7
.L646:
	movl	16(%rsp), %esi
	movq	%rbx, %rdx
	movq	%r15, %rdi
	movq	%rbx, 24(%rsp)
	call	AppendToken
	movq	%rbp, %rdi
	movq	%rax, %r15
	call	fgetc
	movl	%eax, %ebx
	jmp	.L649
.L604:
	movb	$10, 16(%rsp)
.L645:
	movl	16(%rsp), %esi
	movq	%r15, %rdi
	xorl	%edx, %edx
	movq	$0, 24(%rsp)
	call	AppendToken
	movq	%rbp, %rdi
	movq	%rax, %r15
	call	fgetc
	movl	%eax, %ebx
	jmp	.L649
.L603:
	movb	$40, 16(%rsp)
	jmp	.L645
.L605:
	movb	$20, 16(%rsp)
	.p2align 4,,3
	jmp	.L645
.L618:
	movl	$1, %edi
	movq	$-1, %r14
	xorl	%r12d, %r12d
	xorl	%r13d, %r13d
	jmp	.L624
.LFE9:
	.size	LexFile, .-LexFile
	.section	.rodata.str1.1
.LC42:
	.string	"r"
	.text
	.p2align 4,,15
.globl LexString
	.type	LexString, @function
LexString:
.LFB10:
	movq	%rbx, -16(%rsp)
.LCFI293:
	movq	%r12, -8(%rsp)
.LCFI294:
	movq	%rdi, %rbx
	subq	$24, %rsp
.LCFI295:
	call	strlen
	movl	$.LC42, %edx
	movq	%rax, %rsi
	movq	%rbx, %rdi
	xorl	%eax, %eax
	call	fmemopen
	movslq	%eax,%rbx
	movq	%rbx, %rdi
	call	LexFile
	movq	%rbx, %rdi
	movq	%rax, %r12
	call	fclose
	movq	%r12, %rax
	movq	8(%rsp), %rbx
	movq	16(%rsp), %r12
	addq	$24, %rsp
	ret
.LFE10:
	.size	LexString, .-LexString
	.p2align 4,,15
.globl NewFunction
	.type	NewFunction, @function
NewFunction:
.LFB80:
	pushq	%r13
.LCFI296:
	movq	%rdi, %rax
	pushq	%r12
.LCFI297:
	movq	%rdx, %r12
	pushq	%rbx
.LCFI298:
	movq	%rsi, %rbx
	movq	%rax, %rsi
	subq	$256, %rsp
.LCFI299:
	leaq	176(%rsp), %rdi
	call	CreateSymbol
	movq	176(%rsp), %rax
	movq	%rax, (%rsp)
	movq	184(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	192(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	Obj
	movq	%rbx, %rdi
	movq	%rax, %r13
	call	LexString
	leaq	112(%rsp), %rdi
	leaq	248(%rsp), %rdx
	movq	%rax, %rsi
	movq	%rax, %rbx
	call	Parse
	movq	120(%rsp), %rdx
	movq	112(%rsp), %rcx
	movq	128(%rsp), %rax
	movq	%rcx, 144(%rsp)
	movq	%rcx, (%rsp)
	movq	%rdx, 152(%rsp)
	movq	%rdx, 8(%rsp)
	movq	%rax, 160(%rsp)
	movq	%rax, 16(%rsp)
	call	ParseArgList
	movl	%eax, 32(%rsp)
	movq	32(%rsp), %rax
	movq	%rbx, %rdi
	movq	%rdx, 40(%rsp)
	movq	%rdx, 216(%rsp)
	movq	%rdx, 232(%rsp)
	movq	%rax, 208(%rsp)
	movq	%rax, 224(%rsp)
	call	FreeTokens
	movl	224(%rsp), %edi
	movq	232(%rsp), %rsi
	movq	%r12, %rdx
	call	CreateNativeFunction
	movl	TypeFunction(%rip), %edx
	movq	TypeFunction+8(%rip), %rcx
	leaq	48(%rsp), %rdi
	movq	%rax, %rsi
	call	WrapObj
	movq	48(%rsp), %rcx
	movq	56(%rsp), %rdx
	movq	%r13, %rdi
	movq	64(%rsp), %rax
	movq	%rcx, 80(%rsp)
	movq	%rdx, 88(%rsp)
	movq	%rax, 96(%rsp)
	movq	%rcx, (%rsp)
	movq	%rdx, 8(%rsp)
	movq	%rax, 16(%rsp)
	call	VariableBind
	addq	$256, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	ret
.LFE80:
	.size	NewFunction, .-NewFunction
	.section	.rodata.str1.1
.LC43:
	.string	"(... vals)"
.LC44:
	.string	"and"
.LC45:
	.string	"or"
.LC46:
	.string	"(val)"
.LC47:
	.string	"not"
.LC48:
	.string	"(.. vals)"
.LC49:
	.string	"is"
	.text
	.p2align 4,,15
.globl LoadBool
	.type	LoadBool, @function
LoadBool:
.LFB113:
	subq	$8, %rsp
.LCFI300:
	movl	$AndFun, %edx
	movl	$.LC43, %esi
	movl	$.LC44, %edi
	call	NewFunction
	movl	$OrFun, %edx
	movl	$.LC43, %esi
	movl	$.LC45, %edi
	call	NewFunction
	movl	$NotFun, %edx
	movl	$.LC46, %esi
	movl	$.LC47, %edi
	call	NewFunction
	movl	$EqFun, %edx
	movl	$.LC48, %esi
	movl	$.LC49, %edi
	addq	$8, %rsp
	jmp	NewFunction
.LFE113:
	.size	LoadBool, .-LoadBool
	.section	.rodata.str1.1
.LC50:
	.string	"(x y)"
.LC51:
	.string	"pair"
.LC52:
	.string	"(... all)"
.LC53:
	.string	"list"
.LC54:
	.string	"(lst)"
.LC55:
	.string	"first"
.LC56:
	.string	"rest"
.LC57:
	.string	"length"
	.text
	.p2align 4,,15
.globl LoadList
	.type	LoadList, @function
LoadList:
.LFB99:
	subq	$8, %rsp
.LCFI301:
	movl	$MakeCons, %edx
	movl	$.LC50, %esi
	movl	$.LC51, %edi
	call	NewFunction
	movl	$MakeList, %edx
	movl	$.LC52, %esi
	movl	$.LC53, %edi
	call	NewFunction
	movl	$GetCar, %edx
	movl	$.LC54, %esi
	movl	$.LC55, %edi
	call	NewFunction
	movl	$GetCdr, %edx
	movl	$.LC54, %esi
	movl	$.LC56, %edi
	call	NewFunction
	movl	$GetListLen, %edx
	movl	$.LC54, %esi
	movl	$.LC57, %edi
	addq	$8, %rsp
	jmp	NewFunction
.LFE99:
	.size	LoadList, .-LoadList
	.section	.rodata.str1.1
.LC58:
	.string	"(x)"
.LC59:
	.string	"int?"
.LC60:
	.string	"float?"
.LC61:
	.string	"+"
.LC62:
	.string	"-"
.LC63:
	.string	"*"
.LC64:
	.string	"(a b)"
.LC65:
	.string	"/"
.LC66:
	.string	"(a b .. vals)"
.LC67:
	.string	"="
	.text
	.p2align 4,,15
.globl LoadMath
	.type	LoadMath, @function
LoadMath:
.LFB98:
	subq	$8, %rsp
.LCFI302:
	movl	$IsInt, %edx
	movl	$.LC58, %esi
	movl	$.LC59, %edi
	call	NewFunction
	movl	$IsFloat, %edx
	movl	$.LC58, %esi
	movl	$.LC60, %edi
	call	NewFunction
	movl	$AddFun, %edx
	movl	$.LC43, %esi
	movl	$.LC61, %edi
	call	NewFunction
	movl	$SubFun, %edx
	movl	$.LC43, %esi
	movl	$.LC62, %edi
	call	NewFunction
	movl	$MulFun, %edx
	movl	$.LC43, %esi
	movl	$.LC63, %edi
	call	NewFunction
	movl	$DivFun, %edx
	movl	$.LC64, %esi
	movl	$.LC65, %edi
	call	NewFunction
	movl	$NumEqFun, %edx
	movl	$.LC66, %esi
	movl	$.LC67, %edi
	addq	$8, %rsp
	jmp	NewFunction
.LFE98:
	.size	LoadMath, .-LoadMath
	.section	.rodata.str1.1
.LC68:
	.string	"(a)"
.LC69:
	.string	"print"
	.text
	.p2align 4,,15
.globl LoadIO
	.type	LoadIO, @function
LoadIO:
.LFB89:
	movl	$PrintObjFun, %edx
	movl	$.LC68, %esi
	movl	$.LC69, %edi
	jmp	NewFunction
.LFE89:
	.size	LoadIO, .-LoadIO
	.p2align 4,,15
.globl LoadCoreLibrary
	.type	LoadCoreLibrary, @function
LoadCoreLibrary:
.LFB87:
	subq	$8, %rsp
.LCFI303:
	call	CreateTypes
	call	CreateSymbols
	call	LoadMath
	.p2align 4,,5
	call	LoadBool
	.p2align 4,,5
	call	LoadList
	addq	$8, %rsp
	jmp	LoadIO
.LFE87:
	.size	LoadCoreLibrary, .-LoadCoreLibrary
	.section	.rodata.str1.1
.LC70:
	.string	"test/test.vy"
.LC71:
	.string	"|\nevaluates to"
.LC72:
	.string	"\n"
	.text
	.p2align 4,,15
.globl main
	.type	main, @function
main:
.LFB14:
	pushq	%r13
.LCFI304:
	pushq	%r12
.LCFI305:
	pushq	%rbp
.LCFI306:
	pushq	%rbx
.LCFI307:
	subq	$168, %rsp
.LCFI308:
	call	LoadCoreLibrary
	movl	$.LC42, %esi
	movl	$.LC70, %edi
	call	fopen
	movq	%rax, %rdi
	call	LexFile
	testq	%rax, %rax
	movq	%rax, %rbx
	movq	%rax, 152(%rsp)
	je	.L665
	leaq	96(%rsp), %r13
	leaq	152(%rsp), %r12
	leaq	32(%rsp), %rbp
	movq	%rax, %rsi
	.p2align 4,,7
.L667:
	movq	%r12, %rdx
	movq	%r13, %rdi
	call	Parse
	movq	96(%rsp), %rax
	movl	$124, %edi
	movq	%rax, 128(%rsp)
	movq	104(%rsp), %rax
	movq	%rax, 136(%rsp)
	movq	112(%rsp), %rax
	movq	%rax, 144(%rsp)
	call	putchar
	movq	128(%rsp), %rax
	movq	stdout(%rip), %rdi
	movq	%rax, (%rsp)
	movq	136(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	144(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	PrintObj
	movl	$.LC71, %edi
	call	puts
	movq	128(%rsp), %rax
	movq	%rbp, %rdi
	movq	%rax, (%rsp)
	movq	136(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	144(%rsp), %rax
	movq	%rax, 16(%rsp)
	call	Eval
	movq	32(%rsp), %rcx
	movq	40(%rsp), %rdx
	movq	48(%rsp), %rax
	movq	stdout(%rip), %rdi
	movq	%rcx, 64(%rsp)
	movq	%rdx, 72(%rsp)
	movq	%rax, 80(%rsp)
	movq	%rcx, (%rsp)
	movq	%rdx, 8(%rsp)
	movq	%rax, 16(%rsp)
	call	PrintObj
	movl	$.LC72, %edi
	call	puts
	movq	152(%rsp), %rsi
	testq	%rsi, %rsi
	jne	.L667
.L665:
	movq	%rbx, %rdi
	call	FreeTokens
	call	FinishRuntime
	addq	$168, %rsp
	xorl	%eax, %eax
	popq	%rbx
	popq	%rbp
	popq	%r12
	popq	%r13
	ret
.LFE14:
	.size	main, .-main
.globl nil_symbol
	.bss
	.align 16
	.type	nil_symbol, @object
	.size	nil_symbol, 24
nil_symbol:
	.zero	24
.globl symbol_hash
	.align 8
	.type	symbol_hash, @object
	.size	symbol_hash, 8
symbol_hash:
	.zero	8
.globl stack
	.align 8
	.type	stack, @object
	.size	stack, 8
stack:
	.zero	8
.globl stack_size
	.data
	.align 4
	.type	stack_size, @object
	.size	stack_size, 4
stack_size:
	.long	100
.globl stack_index
	.bss
	.align 4
	.type	stack_index, @object
	.size	stack_index, 4
stack_index:
	.zero	4
.globl current_scope
	.align 8
	.type	current_scope, @object
	.size	current_scope, 8
current_scope:
	.zero	8
	.comm	TypeCons,16,16
	.comm	TypeString,16,16
	.comm	TypeSymbol,16,16
	.comm	TypeFloat,16,16
	.comm	TypeInt,16,16
	.comm	TypeFunction,16,16
	.comm	TypeNone,16,16
	.comm	SymbolFalse,24,16
	.comm	SymbolIf,24,16
	.comm	SymbolSetvar,24,16
	.comm	SymbolWhile,24,16
	.comm	SymbolNil,24,16
	.comm	SymbolQuote,24,16
	.comm	SymbolFn,24,16
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
	.long	.LFB2
	.long	.LFE2-.LFB2
	.uleb128 0x0
	.align 8
.LEFDE1:
.LSFDE3:
	.long	.LEFDE3-.LASFDE3
.LASFDE3:
	.long	.LASFDE3-.Lframe1
	.long	.LFB3
	.long	.LFE3-.LFB3
	.uleb128 0x0
	.align 8
.LEFDE3:
.LSFDE5:
	.long	.LEFDE5-.LASFDE5
.LASFDE5:
	.long	.LASFDE5-.Lframe1
	.long	.LFB29
	.long	.LFE29-.LFB29
	.uleb128 0x0
	.align 8
.LEFDE5:
.LSFDE7:
	.long	.LEFDE7-.LASFDE7
.LASFDE7:
	.long	.LASFDE7-.Lframe1
	.long	.LFB37
	.long	.LFE37-.LFB37
	.uleb128 0x0
	.align 8
.LEFDE7:
.LSFDE9:
	.long	.LEFDE9-.LASFDE9
.LASFDE9:
	.long	.LASFDE9-.Lframe1
	.long	.LFB38
	.long	.LFE38-.LFB38
	.uleb128 0x0
	.align 8
.LEFDE9:
.LSFDE11:
	.long	.LEFDE11-.LASFDE11
.LASFDE11:
	.long	.LASFDE11-.Lframe1
	.long	.LFB24
	.long	.LFE24-.LFB24
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI2-.LFB24
	.byte	0xe
	.uleb128 0x58
	.byte	0x86
	.uleb128 0x2
	.byte	0x83
	.uleb128 0x3
	.align 8
.LEFDE11:
.LSFDE13:
	.long	.LEFDE13-.LASFDE13
.LASFDE13:
	.long	.LASFDE13-.Lframe1
	.long	.LFB22
	.long	.LFE22-.LFB22
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI3-.LFB22
	.byte	0xe
	.uleb128 0x10
	.byte	0x83
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI4-.LCFI3
	.byte	0xe
	.uleb128 0x28
	.align 8
.LEFDE13:
.LSFDE15:
	.long	.LEFDE15-.LASFDE15
.LASFDE15:
	.long	.LASFDE15-.Lframe1
	.long	.LFB21
	.long	.LFE21-.LFB21
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI5-.LFB21
	.byte	0xe
	.uleb128 0x10
	.byte	0x83
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI6-.LCFI5
	.byte	0xe
	.uleb128 0x28
	.align 8
.LEFDE15:
.LSFDE17:
	.long	.LEFDE17-.LASFDE17
.LASFDE17:
	.long	.LASFDE17-.Lframe1
	.long	.LFB39
	.long	.LFE39-.LFB39
	.uleb128 0x0
	.align 8
.LEFDE17:
.LSFDE19:
	.long	.LEFDE19-.LASFDE19
.LASFDE19:
	.long	.LASFDE19-.Lframe1
	.long	.LFB40
	.long	.LFE40-.LFB40
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI7-.LFB40
	.byte	0xe
	.uleb128 0x10
	.byte	0x83
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI8-.LCFI7
	.byte	0xe
	.uleb128 0x60
	.align 8
.LEFDE19:
.LSFDE21:
	.long	.LEFDE21-.LASFDE21
.LASFDE21:
	.long	.LASFDE21-.Lframe1
	.long	.LFB41
	.long	.LFE41-.LFB41
	.uleb128 0x0
	.align 8
.LEFDE21:
.LSFDE23:
	.long	.LEFDE23-.LASFDE23
.LASFDE23:
	.long	.LASFDE23-.Lframe1
	.long	.LFB42
	.long	.LFE42-.LFB42
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI15-.LFB42
	.byte	0xe
	.uleb128 0xa0
	.byte	0x8f
	.uleb128 0x2
	.byte	0x8e
	.uleb128 0x3
	.byte	0x8d
	.uleb128 0x4
	.byte	0x8c
	.uleb128 0x5
	.byte	0x86
	.uleb128 0x6
	.byte	0x83
	.uleb128 0x7
	.align 8
.LEFDE23:
.LSFDE25:
	.long	.LEFDE25-.LASFDE25
.LASFDE25:
	.long	.LASFDE25-.Lframe1
	.long	.LFB43
	.long	.LFE43-.LFB43
	.uleb128 0x0
	.align 8
.LEFDE25:
.LSFDE27:
	.long	.LEFDE27-.LASFDE27
.LASFDE27:
	.long	.LASFDE27-.Lframe1
	.long	.LFB33
	.long	.LFE33-.LFB33
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI17-.LFB33
	.byte	0x8d
	.uleb128 0x4
	.byte	0x86
	.uleb128 0x6
	.byte	0x4
	.long	.LCFI22-.LCFI17
	.byte	0xe
	.uleb128 0xe0
	.byte	0x8e
	.uleb128 0x3
	.byte	0x8c
	.uleb128 0x5
	.byte	0x83
	.uleb128 0x7
	.byte	0x8f
	.uleb128 0x2
	.align 8
.LEFDE27:
.LSFDE29:
	.long	.LEFDE29-.LASFDE29
.LASFDE29:
	.long	.LASFDE29-.Lframe1
	.long	.LFB44
	.long	.LFE44-.LFB44
	.uleb128 0x0
	.align 8
.LEFDE29:
.LSFDE31:
	.long	.LEFDE31-.LASFDE31
.LASFDE31:
	.long	.LASFDE31-.Lframe1
	.long	.LFB59
	.long	.LFE59-.LFB59
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI24-.LFB59
	.byte	0x8c
	.uleb128 0x5
	.byte	0x83
	.uleb128 0x6
	.byte	0x4
	.long	.LCFI28-.LCFI24
	.byte	0xe
	.uleb128 0x50
	.byte	0x8f
	.uleb128 0x2
	.byte	0x8e
	.uleb128 0x3
	.byte	0x8d
	.uleb128 0x4
	.align 8
.LEFDE31:
.LSFDE33:
	.long	.LEFDE33-.LASFDE33
.LASFDE33:
	.long	.LASFDE33-.Lframe1
	.long	.LFB50
	.long	.LFE50-.LFB50
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI29-.LFB50
	.byte	0xe
	.uleb128 0x30
	.align 8
.LEFDE33:
.LSFDE35:
	.long	.LEFDE35-.LASFDE35
.LASFDE35:
	.long	.LASFDE35-.Lframe1
	.long	.LFB60
	.long	.LFE60-.LFB60
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI30-.LFB60
	.byte	0xe
	.uleb128 0x10
	.byte	0x83
	.uleb128 0x2
	.align 8
.LEFDE35:
.LSFDE37:
	.long	.LEFDE37-.LASFDE37
.LASFDE37:
	.long	.LASFDE37-.Lframe1
	.long	.LFB65
	.long	.LFE65-.LFB65
	.uleb128 0x0
	.align 8
.LEFDE37:
.LSFDE39:
	.long	.LEFDE39-.LASFDE39
.LASFDE39:
	.long	.LASFDE39-.Lframe1
	.long	.LFB66
	.long	.LFE66-.LFB66
	.uleb128 0x0
	.align 8
.LEFDE39:
.LSFDE41:
	.long	.LEFDE41-.LASFDE41
.LASFDE41:
	.long	.LASFDE41-.Lframe1
	.long	.LFB67
	.long	.LFE67-.LFB67
	.uleb128 0x0
	.align 8
.LEFDE41:
.LSFDE43:
	.long	.LEFDE43-.LASFDE43
.LASFDE43:
	.long	.LASFDE43-.Lframe1
	.long	.LFB68
	.long	.LFE68-.LFB68
	.uleb128 0x0
	.align 8
.LEFDE43:
.LSFDE45:
	.long	.LEFDE45-.LASFDE45
.LASFDE45:
	.long	.LASFDE45-.Lframe1
	.long	.LFB69
	.long	.LFE69-.LFB69
	.uleb128 0x0
	.align 8
.LEFDE45:
.LSFDE47:
	.long	.LEFDE47-.LASFDE47
.LASFDE47:
	.long	.LASFDE47-.Lframe1
	.long	.LFB70
	.long	.LFE70-.LFB70
	.uleb128 0x0
	.align 8
.LEFDE47:
.LSFDE49:
	.long	.LEFDE49-.LASFDE49
.LASFDE49:
	.long	.LASFDE49-.Lframe1
	.long	.LFB78
	.long	.LFE78-.LFB78
	.uleb128 0x0
	.align 8
.LEFDE49:
.LSFDE51:
	.long	.LEFDE51-.LASFDE51
.LASFDE51:
	.long	.LASFDE51-.Lframe1
	.long	.LFB96
	.long	.LFE96-.LFB96
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI37-.LFB96
	.byte	0xe
	.uleb128 0x60
	.byte	0x8e
	.uleb128 0x3
	.byte	0x8d
	.uleb128 0x4
	.byte	0x8f
	.uleb128 0x2
	.byte	0x8c
	.uleb128 0x5
	.byte	0x86
	.uleb128 0x6
	.byte	0x83
	.uleb128 0x7
	.align 8
.LEFDE51:
.LSFDE53:
	.long	.LEFDE53-.LASFDE53
.LASFDE53:
	.long	.LASFDE53-.Lframe1
	.long	.LFB102
	.long	.LFE102-.LFB102
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI38-.LFB102
	.byte	0xe
	.uleb128 0x10
	.byte	0x83
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI39-.LCFI38
	.byte	0xe
	.uleb128 0x28
	.align 8
.LEFDE53:
.LSFDE55:
	.long	.LEFDE55-.LASFDE55
.LASFDE55:
	.long	.LASFDE55-.Lframe1
	.long	.LFB103
	.long	.LFE103-.LFB103
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI40-.LFB103
	.byte	0xe
	.uleb128 0x10
	.byte	0x83
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI41-.LCFI40
	.byte	0xe
	.uleb128 0x28
	.align 8
.LEFDE55:
.LSFDE57:
	.long	.LEFDE57-.LASFDE57
.LASFDE57:
	.long	.LASFDE57-.Lframe1
	.long	.LFB6
	.long	.LFE6-.LFB6
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI42-.LFB6
	.byte	0xe
	.uleb128 0x10
	.byte	0x86
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI43-.LCFI42
	.byte	0xe
	.uleb128 0x18
	.byte	0x83
	.uleb128 0x3
	.byte	0x4
	.long	.LCFI44-.LCFI43
	.byte	0xe
	.uleb128 0x20
	.align 8
.LEFDE57:
.LSFDE59:
	.long	.LEFDE59-.LASFDE59
.LASFDE59:
	.long	.LASFDE59-.Lframe1
	.long	.LFB30
	.long	.LFE30-.LFB30
	.uleb128 0x0
	.align 8
.LEFDE59:
.LSFDE61:
	.long	.LEFDE61-.LASFDE61
.LASFDE61:
	.long	.LASFDE61-.Lframe1
	.long	.LFB15
	.long	.LFE15-.LFB15
	.uleb128 0x0
	.align 8
.LEFDE61:
.LSFDE63:
	.long	.LEFDE63-.LASFDE63
.LASFDE63:
	.long	.LASFDE63-.Lframe1
	.long	.LFB13
	.long	.LFE13-.LFB13
	.uleb128 0x0
	.align 8
.LEFDE63:
.LSFDE65:
	.long	.LEFDE65-.LASFDE65
.LASFDE65:
	.long	.LASFDE65-.Lframe1
	.long	.LFB79
	.long	.LFE79-.LFB79
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI45-.LFB79
	.byte	0xe
	.uleb128 0x10
	.byte	0x83
	.uleb128 0x2
	.align 8
.LEFDE65:
.LSFDE67:
	.long	.LEFDE67-.LASFDE67
.LASFDE67:
	.long	.LASFDE67-.Lframe1
	.long	.LFB62
	.long	.LFE62-.LFB62
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI46-.LFB62
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
	.long	.LFB7
	.long	.LFE7-.LFB7
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI47-.LFB7
	.byte	0xe
	.uleb128 0x10
	.byte	0x86
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI48-.LCFI47
	.byte	0xe
	.uleb128 0x18
	.byte	0x4
	.long	.LCFI49-.LCFI48
	.byte	0xe
	.uleb128 0x20
	.byte	0x83
	.uleb128 0x3
	.align 8
.LEFDE69:
.LSFDE71:
	.long	.LEFDE71-.LASFDE71
.LASFDE71:
	.long	.LASFDE71-.Lframe1
	.long	.LFB12
	.long	.LFE12-.LFB12
	.uleb128 0x0
	.align 8
.LEFDE71:
.LSFDE73:
	.long	.LEFDE73-.LASFDE73
.LASFDE73:
	.long	.LASFDE73-.Lframe1
	.long	.LFB63
	.long	.LFE63-.LFB63
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI50-.LFB63
	.byte	0xe
	.uleb128 0x10
	.byte	0x83
	.uleb128 0x2
	.align 8
.LEFDE73:
.LSFDE75:
	.long	.LEFDE75-.LASFDE75
.LASFDE75:
	.long	.LASFDE75-.Lframe1
	.long	.LFB64
	.long	.LFE64-.LFB64
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI53-.LFB64
	.byte	0xe
	.uleb128 0x20
	.byte	0x86
	.uleb128 0x2
	.byte	0x83
	.uleb128 0x3
	.align 8
.LEFDE75:
.LSFDE77:
	.long	.LEFDE77-.LASFDE77
.LASFDE77:
	.long	.LASFDE77-.Lframe1
	.long	.LFB57
	.long	.LFE57-.LFB57
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI54-.LFB57
	.byte	0xe
	.uleb128 0x10
	.align 8
.LEFDE77:
.LSFDE79:
	.long	.LEFDE79-.LASFDE79
.LASFDE79:
	.long	.LASFDE79-.Lframe1
	.long	.LFB11
	.long	.LFE11-.LFB11
	.uleb128 0x0
	.align 8
.LEFDE79:
.LSFDE81:
	.long	.LEFDE81-.LASFDE81
.LASFDE81:
	.long	.LASFDE81-.Lframe1
	.long	.LFB82
	.long	.LFE82-.LFB82
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI57-.LFB82
	.byte	0xe
	.uleb128 0x20
	.byte	0x8c
	.uleb128 0x2
	.byte	0x83
	.uleb128 0x3
	.align 8
.LEFDE81:
.LSFDE83:
	.long	.LEFDE83-.LASFDE83
.LASFDE83:
	.long	.LASFDE83-.Lframe1
	.long	.LFB84
	.long	.LFE84-.LFB84
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI58-.LFB84
	.byte	0xe
	.uleb128 0x10
	.byte	0x83
	.uleb128 0x2
	.align 8
.LEFDE83:
.LSFDE85:
	.long	.LEFDE85-.LASFDE85
.LASFDE85:
	.long	.LASFDE85-.Lframe1
	.long	.LFB81
	.long	.LFE81-.LFB81
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI61-.LFB81
	.byte	0xe
	.uleb128 0x20
	.byte	0x8c
	.uleb128 0x2
	.byte	0x83
	.uleb128 0x3
	.align 8
.LEFDE85:
.LSFDE87:
	.long	.LEFDE87-.LASFDE87
.LASFDE87:
	.long	.LASFDE87-.Lframe1
	.long	.LFB95
	.long	.LFE95-.LFB95
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI63-.LFB95
	.byte	0x86
	.uleb128 0x6
	.byte	0x83
	.uleb128 0x7
	.byte	0x4
	.long	.LCFI68-.LCFI63
	.byte	0xe
	.uleb128 0x70
	.byte	0x8f
	.uleb128 0x2
	.byte	0x8e
	.uleb128 0x3
	.byte	0x8d
	.uleb128 0x4
	.byte	0x8c
	.uleb128 0x5
	.align 8
.LEFDE87:
.LSFDE89:
	.long	.LEFDE89-.LASFDE89
.LASFDE89:
	.long	.LASFDE89-.Lframe1
	.long	.LFB94
	.long	.LFE94-.LFB94
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI69-.LFB94
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.long	.LCFI70-.LCFI69
	.byte	0xe
	.uleb128 0x18
	.byte	0x4
	.long	.LCFI71-.LCFI70
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.long	.LCFI72-.LCFI71
	.byte	0xe
	.uleb128 0x28
	.byte	0x4
	.long	.LCFI73-.LCFI72
	.byte	0xe
	.uleb128 0x30
	.byte	0x4
	.long	.LCFI74-.LCFI73
	.byte	0xe
	.uleb128 0x38
	.byte	0x4
	.long	.LCFI75-.LCFI74
	.byte	0xe
	.uleb128 0xd0
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
.LEFDE89:
.LSFDE91:
	.long	.LEFDE91-.LASFDE91
.LASFDE91:
	.long	.LASFDE91-.Lframe1
	.long	.LFB93
	.long	.LFE93-.LFB93
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI76-.LFB93
	.byte	0xe
	.uleb128 0x10
	.byte	0x8f
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI77-.LCFI76
	.byte	0xe
	.uleb128 0x18
	.byte	0x4
	.long	.LCFI78-.LCFI77
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.long	.LCFI79-.LCFI78
	.byte	0xe
	.uleb128 0x28
	.byte	0x8c
	.uleb128 0x5
	.byte	0x8d
	.uleb128 0x4
	.byte	0x8e
	.uleb128 0x3
	.byte	0x4
	.long	.LCFI80-.LCFI79
	.byte	0xe
	.uleb128 0x30
	.byte	0x4
	.long	.LCFI81-.LCFI80
	.byte	0xe
	.uleb128 0x38
	.byte	0x4
	.long	.LCFI82-.LCFI81
	.byte	0xe
	.uleb128 0x100
	.byte	0x83
	.uleb128 0x7
	.byte	0x86
	.uleb128 0x6
	.align 8
.LEFDE91:
.LSFDE93:
	.long	.LEFDE93-.LASFDE93
.LASFDE93:
	.long	.LASFDE93-.Lframe1
	.long	.LFB92
	.long	.LFE92-.LFB92
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI83-.LFB92
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.long	.LCFI84-.LCFI83
	.byte	0xe
	.uleb128 0x18
	.byte	0x4
	.long	.LCFI85-.LCFI84
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.long	.LCFI86-.LCFI85
	.byte	0xe
	.uleb128 0x28
	.byte	0x4
	.long	.LCFI87-.LCFI86
	.byte	0xe
	.uleb128 0x30
	.byte	0x4
	.long	.LCFI88-.LCFI87
	.byte	0xe
	.uleb128 0x38
	.byte	0x4
	.long	.LCFI89-.LCFI88
	.byte	0xe
	.uleb128 0xd0
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
.LEFDE93:
.LSFDE95:
	.long	.LEFDE95-.LASFDE95
.LASFDE95:
	.long	.LASFDE95-.Lframe1
	.long	.LFB83
	.long	.LFE83-.LFB83
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI90-.LFB83
	.byte	0xe
	.uleb128 0x10
	.byte	0x83
	.uleb128 0x2
	.align 8
.LEFDE95:
.LSFDE97:
	.long	.LEFDE97-.LASFDE97
.LASFDE97:
	.long	.LASFDE97-.Lframe1
	.long	.LFB74
	.long	.LFE74-.LFB74
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI91-.LFB74
	.byte	0xe
	.uleb128 0x10
	.byte	0x8c
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI92-.LCFI91
	.byte	0xe
	.uleb128 0x18
	.byte	0x4
	.long	.LCFI93-.LCFI92
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
	.long	.LFB75
	.long	.LFE75-.LFB75
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI94-.LFB75
	.byte	0xe
	.uleb128 0x10
	.align 8
.LEFDE99:
.LSFDE101:
	.long	.LEFDE101-.LASFDE101
.LASFDE101:
	.long	.LASFDE101-.Lframe1
	.long	.LFB77
	.long	.LFE77-.LFB77
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI95-.LFB77
	.byte	0xe
	.uleb128 0x10
	.byte	0x8c
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI96-.LCFI95
	.byte	0xe
	.uleb128 0x18
	.byte	0x4
	.long	.LCFI97-.LCFI96
	.byte	0xe
	.uleb128 0x70
	.byte	0x83
	.uleb128 0x3
	.align 8
.LEFDE101:
.LSFDE103:
	.long	.LEFDE103-.LASFDE103
.LASFDE103:
	.long	.LASFDE103-.Lframe1
	.long	.LFB51
	.long	.LFE51-.LFB51
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI101-.LFB51
	.byte	0xe
	.uleb128 0xc0
	.byte	0x8d
	.uleb128 0x2
	.byte	0x8c
	.uleb128 0x3
	.byte	0x83
	.uleb128 0x4
	.align 8
.LEFDE103:
.LSFDE105:
	.long	.LEFDE105-.LASFDE105
.LASFDE105:
	.long	.LASFDE105-.Lframe1
	.long	.LFB76
	.long	.LFE76-.LFB76
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI102-.LFB76
	.byte	0xe
	.uleb128 0x10
	.byte	0x8e
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI103-.LCFI102
	.byte	0xe
	.uleb128 0x18
	.byte	0x8d
	.uleb128 0x3
	.byte	0x4
	.long	.LCFI104-.LCFI103
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.long	.LCFI105-.LCFI104
	.byte	0xe
	.uleb128 0x28
	.byte	0x4
	.long	.LCFI106-.LCFI105
	.byte	0xe
	.uleb128 0x30
	.byte	0x4
	.long	.LCFI107-.LCFI106
	.byte	0xe
	.uleb128 0x60
	.byte	0x83
	.uleb128 0x6
	.byte	0x86
	.uleb128 0x5
	.byte	0x8c
	.uleb128 0x4
	.align 8
.LEFDE105:
.LSFDE107:
	.long	.LEFDE107-.LASFDE107
.LASFDE107:
	.long	.LASFDE107-.Lframe1
	.long	.LFB53
	.long	.LFE53-.LFB53
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI108-.LFB53
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.long	.LCFI109-.LCFI108
	.byte	0xe
	.uleb128 0x70
	.byte	0x83
	.uleb128 0x2
	.align 8
.LEFDE107:
.LSFDE109:
	.long	.LEFDE109-.LASFDE109
.LASFDE109:
	.long	.LASFDE109-.Lframe1
	.long	.LFB61
	.long	.LFE61-.LFB61
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI110-.LFB61
	.byte	0xe
	.uleb128 0x10
	.byte	0x83
	.uleb128 0x2
	.align 8
.LEFDE109:
.LSFDE111:
	.long	.LEFDE111-.LASFDE111
.LASFDE111:
	.long	.LASFDE111-.Lframe1
	.long	.LFB56
	.long	.LFE56-.LFB56
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI111-.LFB56
	.byte	0xe
	.uleb128 0x10
	.align 8
.LEFDE111:
.LSFDE113:
	.long	.LEFDE113-.LASFDE113
.LASFDE113:
	.long	.LASFDE113-.Lframe1
	.long	.LFB58
	.long	.LFE58-.LFB58
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI115-.LFB58
	.byte	0xe
	.uleb128 0x20
	.byte	0x8c
	.uleb128 0x2
	.byte	0x86
	.uleb128 0x3
	.byte	0x83
	.uleb128 0x4
	.align 8
.LEFDE113:
.LSFDE115:
	.long	.LEFDE115-.LASFDE115
.LASFDE115:
	.long	.LASFDE115-.Lframe1
	.long	.LFB52
	.long	.LFE52-.LFB52
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI116-.LFB52
	.byte	0xe
	.uleb128 0x70
	.align 8
.LEFDE115:
.LSFDE117:
	.long	.LEFDE117-.LASFDE117
.LASFDE117:
	.long	.LASFDE117-.Lframe1
	.long	.LFB49
	.long	.LFE49-.LFB49
	.uleb128 0x0
	.align 8
.LEFDE117:
.LSFDE119:
	.long	.LEFDE119-.LASFDE119
.LASFDE119:
	.long	.LASFDE119-.Lframe1
	.long	.LFB32
	.long	.LFE32-.LFB32
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI118-.LFB32
	.byte	0x8c
	.uleb128 0x3
	.byte	0x83
	.uleb128 0x4
	.byte	0x4
	.long	.LCFI120-.LCFI118
	.byte	0xe
	.uleb128 0x30
	.byte	0x8d
	.uleb128 0x2
	.align 8
.LEFDE119:
.LSFDE121:
	.long	.LEFDE121-.LASFDE121
.LASFDE121:
	.long	.LASFDE121-.Lframe1
	.long	.LFB31
	.long	.LFE31-.LFB31
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI122-.LFB31
	.byte	0x8c
	.uleb128 0x3
	.byte	0x83
	.uleb128 0x4
	.byte	0x4
	.long	.LCFI124-.LCFI122
	.byte	0xe
	.uleb128 0x30
	.byte	0x8d
	.uleb128 0x2
	.align 8
.LEFDE121:
.LSFDE123:
	.long	.LEFDE123-.LASFDE123
.LASFDE123:
	.long	.LASFDE123-.Lframe1
	.long	.LFB27
	.long	.LFE27-.LFB27
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI127-.LFB27
	.byte	0xe
	.uleb128 0x20
	.byte	0x83
	.uleb128 0x3
	.byte	0x86
	.uleb128 0x2
	.align 8
.LEFDE123:
.LSFDE125:
	.long	.LEFDE125-.LASFDE125
.LASFDE125:
	.long	.LASFDE125-.Lframe1
	.long	.LFB85
	.long	.LFE85-.LFB85
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI128-.LFB85
	.byte	0xe
	.uleb128 0x20
	.align 8
.LEFDE125:
.LSFDE127:
	.long	.LEFDE127-.LASFDE127
.LASFDE127:
	.long	.LASFDE127-.Lframe1
	.long	.LFB28
	.long	.LFE28-.LFB28
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI129-.LFB28
	.byte	0xe
	.uleb128 0x10
	.byte	0x83
	.uleb128 0x2
	.align 8
.LEFDE127:
.LSFDE129:
	.long	.LEFDE129-.LASFDE129
.LASFDE129:
	.long	.LASFDE129-.Lframe1
	.long	.LFB106
	.long	.LFE106-.LFB106
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI130-.LFB106
	.byte	0xe
	.uleb128 0x10
	.byte	0x83
	.uleb128 0x2
	.align 8
.LEFDE129:
.LSFDE131:
	.long	.LEFDE131-.LASFDE131
.LASFDE131:
	.long	.LASFDE131-.Lframe1
	.long	.LFB105
	.long	.LFE105-.LFB105
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI131-.LFB105
	.byte	0xe
	.uleb128 0x10
	.byte	0x83
	.uleb128 0x2
	.align 8
.LEFDE131:
.LSFDE133:
	.long	.LEFDE133-.LASFDE133
.LASFDE133:
	.long	.LASFDE133-.Lframe1
	.long	.LFB112
	.long	.LFE112-.LFB112
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI132-.LFB112
	.byte	0xe
	.uleb128 0x10
	.byte	0x8f
	.uleb128 0x2
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
	.byte	0x4
	.long	.LCFI138-.LCFI137
	.byte	0xe
	.uleb128 0xe0
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
	.align 8
.LEFDE133:
.LSFDE135:
	.long	.LEFDE135-.LASFDE135
.LASFDE135:
	.long	.LASFDE135-.Lframe1
	.long	.LFB97
	.long	.LFE97-.LFB97
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI139-.LFB97
	.byte	0xe
	.uleb128 0x10
	.byte	0x8f
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI140-.LCFI139
	.byte	0xe
	.uleb128 0x18
	.byte	0x4
	.long	.LCFI141-.LCFI140
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.long	.LCFI142-.LCFI141
	.byte	0xe
	.uleb128 0x28
	.byte	0x4
	.long	.LCFI143-.LCFI142
	.byte	0xe
	.uleb128 0x30
	.byte	0x4
	.long	.LCFI144-.LCFI143
	.byte	0xe
	.uleb128 0x38
	.byte	0x4
	.long	.LCFI145-.LCFI144
	.byte	0xe
	.uleb128 0xe0
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
	.align 8
.LEFDE135:
.LSFDE137:
	.long	.LEFDE137-.LASFDE137
.LASFDE137:
	.long	.LASFDE137-.Lframe1
	.long	.LFB91
	.long	.LFE91-.LFB91
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI146-.LFB91
	.byte	0xe
	.uleb128 0x10
	.byte	0x83
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI147-.LCFI146
	.byte	0xe
	.uleb128 0x50
	.align 8
.LEFDE137:
.LSFDE139:
	.long	.LEFDE139-.LASFDE139
.LASFDE139:
	.long	.LASFDE139-.Lframe1
	.long	.LFB90
	.long	.LFE90-.LFB90
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI148-.LFB90
	.byte	0xe
	.uleb128 0x10
	.byte	0x83
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI149-.LCFI148
	.byte	0xe
	.uleb128 0x50
	.align 8
.LEFDE139:
.LSFDE141:
	.long	.LEFDE141-.LASFDE141
.LASFDE141:
	.long	.LASFDE141-.Lframe1
	.long	.LFB86
	.long	.LFE86-.LFB86
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI150-.LFB86
	.byte	0xe
	.uleb128 0xf0
	.align 8
.LEFDE141:
.LSFDE143:
	.long	.LEFDE143-.LASFDE143
.LASFDE143:
	.long	.LASFDE143-.Lframe1
	.long	.LFB23
	.long	.LFE23-.LFB23
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI157-.LFB23
	.byte	0xe
	.uleb128 0xc0
	.byte	0x8f
	.uleb128 0x2
	.byte	0x8e
	.uleb128 0x3
	.byte	0x8d
	.uleb128 0x4
	.byte	0x8c
	.uleb128 0x5
	.byte	0x86
	.uleb128 0x6
	.byte	0x83
	.uleb128 0x7
	.align 8
.LEFDE143:
.LSFDE145:
	.long	.LEFDE145-.LASFDE145
.LASFDE145:
	.long	.LASFDE145-.Lframe1
	.long	.LFB108
	.long	.LFE108-.LFB108
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI158-.LFB108
	.byte	0xe
	.uleb128 0x40
	.align 8
.LEFDE145:
.LSFDE147:
	.long	.LEFDE147-.LASFDE147
.LASFDE147:
	.long	.LASFDE147-.Lframe1
	.long	.LFB110
	.long	.LFE110-.LFB110
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI159-.LFB110
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.long	.LCFI160-.LCFI159
	.byte	0xe
	.uleb128 0x18
	.byte	0x8d
	.uleb128 0x3
	.byte	0x8e
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI161-.LCFI160
	.byte	0xe
	.uleb128 0x20
	.byte	0x8c
	.uleb128 0x4
	.byte	0x4
	.long	.LCFI162-.LCFI161
	.byte	0xe
	.uleb128 0x28
	.byte	0x4
	.long	.LCFI163-.LCFI162
	.byte	0xe
	.uleb128 0x30
	.byte	0x4
	.long	.LCFI164-.LCFI163
	.byte	0xe
	.uleb128 0x90
	.byte	0x83
	.uleb128 0x6
	.byte	0x86
	.uleb128 0x5
	.align 8
.LEFDE147:
.LSFDE149:
	.long	.LEFDE149-.LASFDE149
.LASFDE149:
	.long	.LASFDE149-.Lframe1
	.long	.LFB107
	.long	.LFE107-.LFB107
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI165-.LFB107
	.byte	0xe
	.uleb128 0x20
	.align 8
.LEFDE149:
.LSFDE151:
	.long	.LEFDE151-.LASFDE151
.LASFDE151:
	.long	.LASFDE151-.Lframe1
	.long	.LFB111
	.long	.LFE111-.LFB111
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI166-.LFB111
	.byte	0xe
	.uleb128 0x10
	.byte	0x83
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI167-.LCFI166
	.byte	0xe
	.uleb128 0x70
	.align 8
.LEFDE151:
.LSFDE153:
	.long	.LEFDE153-.LASFDE153
.LASFDE153:
	.long	.LASFDE153-.Lframe1
	.long	.LFB109
	.long	.LFE109-.LFB109
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI168-.LFB109
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.long	.LCFI169-.LCFI168
	.byte	0xe
	.uleb128 0x18
	.byte	0x8d
	.uleb128 0x3
	.byte	0x8e
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI170-.LCFI169
	.byte	0xe
	.uleb128 0x20
	.byte	0x8c
	.uleb128 0x4
	.byte	0x4
	.long	.LCFI171-.LCFI170
	.byte	0xe
	.uleb128 0x28
	.byte	0x4
	.long	.LCFI172-.LCFI171
	.byte	0xe
	.uleb128 0x30
	.byte	0x4
	.long	.LCFI173-.LCFI172
	.byte	0xe
	.uleb128 0x90
	.byte	0x83
	.uleb128 0x6
	.byte	0x86
	.uleb128 0x5
	.align 8
.LEFDE153:
.LSFDE155:
	.long	.LEFDE155-.LASFDE155
.LASFDE155:
	.long	.LASFDE155-.Lframe1
	.long	.LFB54
	.long	.LFE54-.LFB54
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI174-.LFB54
	.byte	0xe
	.uleb128 0x10
	.byte	0x83
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI175-.LCFI174
	.byte	0xe
	.uleb128 0x70
	.align 8
.LEFDE155:
.LSFDE157:
	.long	.LEFDE157-.LASFDE157
.LASFDE157:
	.long	.LASFDE157-.Lframe1
	.long	.LFB45
	.long	.LFE45-.LFB45
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI176-.LFB45
	.byte	0xe
	.uleb128 0x10
	.byte	0x8f
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI177-.LCFI176
	.byte	0xe
	.uleb128 0x18
	.byte	0x4
	.long	.LCFI178-.LCFI177
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.long	.LCFI179-.LCFI178
	.byte	0xe
	.uleb128 0x28
	.byte	0x4
	.long	.LCFI180-.LCFI179
	.byte	0xe
	.uleb128 0x30
	.byte	0x4
	.long	.LCFI181-.LCFI180
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
	.byte	0x4
	.long	.LCFI182-.LCFI181
	.byte	0xe
	.uleb128 0xa0
	.align 8
.LEFDE157:
.LSFDE159:
	.long	.LEFDE159-.LASFDE159
.LASFDE159:
	.long	.LASFDE159-.Lframe1
	.long	.LFB46
	.long	.LFE46-.LFB46
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI187-.LFB46
	.byte	0xe
	.uleb128 0x40
	.byte	0x8d
	.uleb128 0x2
	.byte	0x8c
	.uleb128 0x3
	.byte	0x86
	.uleb128 0x4
	.byte	0x83
	.uleb128 0x5
	.align 8
.LEFDE159:
.LSFDE161:
	.long	.LEFDE161-.LASFDE161
.LASFDE161:
	.long	.LASFDE161-.Lframe1
	.long	.LFB88
	.long	.LFE88-.LFB88
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI190-.LFB88
	.byte	0xe
	.uleb128 0x30
	.byte	0x8c
	.uleb128 0x2
	.byte	0x83
	.uleb128 0x3
	.align 8
.LEFDE161:
.LSFDE163:
	.long	.LEFDE163-.LASFDE163
.LASFDE163:
	.long	.LASFDE163-.Lframe1
	.long	.LFB34
	.long	.LFE34-.LFB34
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI191-.LFB34
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.long	.LCFI192-.LCFI191
	.byte	0xe
	.uleb128 0x18
	.byte	0x4
	.long	.LCFI193-.LCFI192
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.long	.LCFI194-.LCFI193
	.byte	0xe
	.uleb128 0x28
	.byte	0x4
	.long	.LCFI195-.LCFI194
	.byte	0xe
	.uleb128 0x30
	.byte	0x83
	.uleb128 0x6
	.byte	0x86
	.uleb128 0x5
	.byte	0x8c
	.uleb128 0x4
	.byte	0x8d
	.uleb128 0x3
	.byte	0x8e
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI196-.LCFI195
	.byte	0xe
	.uleb128 0xe0
	.align 8
.LEFDE163:
.LSFDE165:
	.long	.LEFDE165-.LASFDE165
.LASFDE165:
	.long	.LASFDE165-.Lframe1
	.long	.LFB25
	.long	.LFE25-.LFB25
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI197-.LFB25
	.byte	0xe
	.uleb128 0x50
	.align 8
.LEFDE165:
.LSFDE167:
	.long	.LEFDE167-.LASFDE167
.LASFDE167:
	.long	.LASFDE167-.Lframe1
	.long	.LFB104
	.long	.LFE104-.LFB104
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI198-.LFB104
	.byte	0xe
	.uleb128 0x10
	.byte	0x83
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI199-.LCFI198
	.byte	0xe
	.uleb128 0x30
	.align 8
.LEFDE167:
.LSFDE169:
	.long	.LEFDE169-.LASFDE169
.LASFDE169:
	.long	.LASFDE169-.Lframe1
	.long	.LFB35
	.long	.LFE35-.LFB35
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI200-.LFB35
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.long	.LCFI201-.LCFI200
	.byte	0xe
	.uleb128 0x18
	.byte	0x4
	.long	.LCFI202-.LCFI201
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.long	.LCFI203-.LCFI202
	.byte	0xe
	.uleb128 0x28
	.byte	0x4
	.long	.LCFI204-.LCFI203
	.byte	0xe
	.uleb128 0x30
	.byte	0x4
	.long	.LCFI205-.LCFI204
	.byte	0xe
	.uleb128 0x38
	.byte	0x4
	.long	.LCFI206-.LCFI205
	.byte	0xe
	.uleb128 0x1b0
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
.LEFDE169:
.LSFDE171:
	.long	.LEFDE171-.LASFDE171
.LASFDE171:
	.long	.LASFDE171-.Lframe1
	.long	.LFB19
	.long	.LFE19-.LFB19
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI207-.LFB19
	.byte	0xe
	.uleb128 0x10
	.byte	0x86
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI208-.LCFI207
	.byte	0xe
	.uleb128 0x18
	.byte	0x4
	.long	.LCFI209-.LCFI208
	.byte	0xe
	.uleb128 0x60
	.byte	0x83
	.uleb128 0x3
	.align 8
.LEFDE171:
.LSFDE173:
	.long	.LEFDE173-.LASFDE173
.LASFDE173:
	.long	.LASFDE173-.Lframe1
	.long	.LFB72
	.long	.LFE72-.LFB72
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI210-.LFB72
	.byte	0xe
	.uleb128 0x10
	.byte	0x8f
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI211-.LCFI210
	.byte	0xe
	.uleb128 0x18
	.byte	0x4
	.long	.LCFI212-.LCFI211
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.long	.LCFI213-.LCFI212
	.byte	0xe
	.uleb128 0x28
	.byte	0x4
	.long	.LCFI214-.LCFI213
	.byte	0xe
	.uleb128 0x30
	.byte	0x4
	.long	.LCFI215-.LCFI214
	.byte	0xe
	.uleb128 0x38
	.byte	0x4
	.long	.LCFI216-.LCFI215
	.byte	0xe
	.uleb128 0x710
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
	.align 8
.LEFDE173:
.LSFDE175:
	.long	.LEFDE175-.LASFDE175
.LASFDE175:
	.long	.LASFDE175-.Lframe1
	.long	.LFB73
	.long	.LFE73-.LFB73
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI217-.LFB73
	.byte	0xe
	.uleb128 0x10
	.byte	0x8f
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI218-.LCFI217
	.byte	0xe
	.uleb128 0x18
	.byte	0x4
	.long	.LCFI219-.LCFI218
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.long	.LCFI220-.LCFI219
	.byte	0xe
	.uleb128 0x28
	.byte	0x4
	.long	.LCFI221-.LCFI220
	.byte	0xe
	.uleb128 0x30
	.byte	0x4
	.long	.LCFI222-.LCFI221
	.byte	0xe
	.uleb128 0x38
	.byte	0x4
	.long	.LCFI223-.LCFI222
	.byte	0xe
	.uleb128 0x100
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
	.align 8
.LEFDE175:
.LSFDE177:
	.long	.LEFDE177-.LASFDE177
.LASFDE177:
	.long	.LASFDE177-.Lframe1
	.long	.LFB71
	.long	.LFE71-.LFB71
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI224-.LFB71
	.byte	0xe
	.uleb128 0x10
	.align 8
.LEFDE177:
.LSFDE179:
	.long	.LEFDE179-.LASFDE179
.LASFDE179:
	.long	.LASFDE179-.Lframe1
	.long	.LFB26
	.long	.LFE26-.LFB26
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI226-.LFB26
	.byte	0x8c
	.uleb128 0x3
	.byte	0x83
	.uleb128 0x4
	.byte	0x4
	.long	.LCFI228-.LCFI226
	.byte	0xe
	.uleb128 0x20
	.byte	0x8d
	.uleb128 0x2
	.align 8
.LEFDE179:
.LSFDE181:
	.long	.LEFDE181-.LASFDE181
.LASFDE181:
	.long	.LASFDE181-.Lframe1
	.long	.LFB20
	.long	.LFE20-.LFB20
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI235-.LFB20
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
	.byte	0x86
	.uleb128 0x6
	.byte	0x83
	.uleb128 0x7
	.align 8
.LEFDE181:
.LSFDE183:
	.long	.LEFDE183-.LASFDE183
.LASFDE183:
	.long	.LASFDE183-.Lframe1
	.long	.LFB101
	.long	.LFE101-.LFB101
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI236-.LFB101
	.byte	0xe
	.uleb128 0x10
	.byte	0x8d
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI237-.LCFI236
	.byte	0xe
	.uleb128 0x18
	.byte	0x4
	.long	.LCFI238-.LCFI237
	.byte	0xe
	.uleb128 0x20
	.byte	0x86
	.uleb128 0x4
	.byte	0x8c
	.uleb128 0x3
	.byte	0x4
	.long	.LCFI239-.LCFI238
	.byte	0xe
	.uleb128 0x28
	.byte	0x83
	.uleb128 0x5
	.byte	0x4
	.long	.LCFI240-.LCFI239
	.byte	0xe
	.uleb128 0x80
	.align 8
.LEFDE183:
.LSFDE185:
	.long	.LEFDE185-.LASFDE185
.LASFDE185:
	.long	.LASFDE185-.Lframe1
	.long	.LFB100
	.long	.LFE100-.LFB100
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI241-.LFB100
	.byte	0xe
	.uleb128 0x10
	.byte	0x83
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI242-.LCFI241
	.byte	0xe
	.uleb128 0x40
	.align 8
.LEFDE185:
.LSFDE187:
	.long	.LEFDE187-.LASFDE187
.LASFDE187:
	.long	.LASFDE187-.Lframe1
	.long	.LFB36
	.long	.LFE36-.LFB36
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI243-.LFB36
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.long	.LCFI244-.LCFI243
	.byte	0xe
	.uleb128 0x18
	.byte	0x8e
	.uleb128 0x3
	.byte	0x8f
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI245-.LCFI244
	.byte	0xe
	.uleb128 0x20
	.byte	0x8d
	.uleb128 0x4
	.byte	0x4
	.long	.LCFI246-.LCFI245
	.byte	0xe
	.uleb128 0x28
	.byte	0x8c
	.uleb128 0x5
	.byte	0x4
	.long	.LCFI247-.LCFI246
	.byte	0xe
	.uleb128 0x30
	.byte	0x4
	.long	.LCFI248-.LCFI247
	.byte	0xe
	.uleb128 0x38
	.byte	0x4
	.long	.LCFI249-.LCFI248
	.byte	0xe
	.uleb128 0x150
	.byte	0x83
	.uleb128 0x7
	.byte	0x86
	.uleb128 0x6
	.align 8
.LEFDE187:
.LSFDE189:
	.long	.LEFDE189-.LASFDE189
.LASFDE189:
	.long	.LASFDE189-.Lframe1
	.long	.LFB55
	.long	.LFE55-.LFB55
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI250-.LFB55
	.byte	0xe
	.uleb128 0x10
	.byte	0x86
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI251-.LCFI250
	.byte	0xd
	.uleb128 0x6
	.byte	0x4
	.long	.LCFI254-.LCFI251
	.byte	0x8d
	.uleb128 0x5
	.byte	0x8e
	.uleb128 0x4
	.byte	0x8f
	.uleb128 0x3
	.byte	0x4
	.long	.LCFI257-.LCFI254
	.byte	0x83
	.uleb128 0x7
	.byte	0x8c
	.uleb128 0x6
	.align 8
.LEFDE189:
.LSFDE191:
	.long	.LEFDE191-.LASFDE191
.LASFDE191:
	.long	.LASFDE191-.Lframe1
	.long	.LFB48
	.long	.LFE48-.LFB48
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI259-.LFB48
	.byte	0x8d
	.uleb128 0x2
	.byte	0x83
	.uleb128 0x5
	.byte	0x4
	.long	.LCFI262-.LCFI259
	.byte	0xe
	.uleb128 0x70
	.byte	0x8c
	.uleb128 0x3
	.byte	0x86
	.uleb128 0x4
	.align 8
.LEFDE191:
.LSFDE193:
	.long	.LEFDE193-.LASFDE193
.LASFDE193:
	.long	.LASFDE193-.Lframe1
	.long	.LFB47
	.long	.LFE47-.LFB47
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI264-.LFB47
	.byte	0x8c
	.uleb128 0x5
	.byte	0x83
	.uleb128 0x6
	.byte	0x4
	.long	.LCFI268-.LCFI264
	.byte	0xe
	.uleb128 0x70
	.byte	0x8f
	.uleb128 0x2
	.byte	0x8e
	.uleb128 0x3
	.byte	0x8d
	.uleb128 0x4
	.align 8
.LEFDE193:
.LSFDE195:
	.long	.LEFDE195-.LASFDE195
.LASFDE195:
	.long	.LASFDE195-.Lframe1
	.long	.LFB16
	.long	.LFE16-.LFB16
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI269-.LFB16
	.byte	0xe
	.uleb128 0x10
	.byte	0x86
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI270-.LCFI269
	.byte	0xe
	.uleb128 0x18
	.byte	0x83
	.uleb128 0x3
	.byte	0x4
	.long	.LCFI271-.LCFI270
	.byte	0xe
	.uleb128 0x110
	.align 8
.LEFDE195:
.LSFDE197:
	.long	.LEFDE197-.LASFDE197
.LASFDE197:
	.long	.LASFDE197-.Lframe1
	.long	.LFB18
	.long	.LFE18-.LFB18
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI272-.LFB18
	.byte	0xe
	.uleb128 0x10
	.byte	0x83
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI273-.LCFI272
	.byte	0xe
	.uleb128 0xd0
	.align 8
.LEFDE197:
.LSFDE199:
	.long	.LEFDE199-.LASFDE199
.LASFDE199:
	.long	.LASFDE199-.Lframe1
	.long	.LFB17
	.long	.LFE17-.LFB17
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI274-.LFB17
	.byte	0xe
	.uleb128 0x10
	.byte	0x8d
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI275-.LCFI274
	.byte	0xe
	.uleb128 0x18
	.byte	0x8c
	.uleb128 0x3
	.byte	0x4
	.long	.LCFI276-.LCFI275
	.byte	0xe
	.uleb128 0x20
	.byte	0x83
	.uleb128 0x4
	.byte	0x4
	.long	.LCFI277-.LCFI276
	.byte	0xe
	.uleb128 0xd0
	.align 8
.LEFDE199:
.LSFDE201:
	.long	.LEFDE201-.LASFDE201
.LASFDE201:
	.long	.LASFDE201-.Lframe1
	.long	.LFB5
	.long	.LFE5-.LFB5
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI279-.LFB5
	.byte	0x86
	.uleb128 0x3
	.byte	0x83
	.uleb128 0x4
	.byte	0x4
	.long	.LCFI281-.LCFI279
	.byte	0xe
	.uleb128 0x30
	.byte	0x8c
	.uleb128 0x2
	.align 8
.LEFDE201:
.LSFDE203:
	.long	.LEFDE203-.LASFDE203
.LASFDE203:
	.long	.LASFDE203-.Lframe1
	.long	.LFB4
	.long	.LFE4-.LFB4
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI282-.LFB4
	.byte	0xe
	.uleb128 0x10
	.align 8
.LEFDE203:
.LSFDE205:
	.long	.LEFDE205-.LASFDE205
.LASFDE205:
	.long	.LASFDE205-.Lframe1
	.long	.LFB8
	.long	.LFE8-.LFB8
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI283-.LFB8
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.long	.LCFI284-.LCFI283
	.byte	0xe
	.uleb128 0x18
	.byte	0x83
	.uleb128 0x3
	.byte	0x86
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI285-.LCFI284
	.byte	0xe
	.uleb128 0x30
	.align 8
.LEFDE205:
.LSFDE207:
	.long	.LEFDE207-.LASFDE207
.LASFDE207:
	.long	.LASFDE207-.Lframe1
	.long	.LFB9
	.long	.LFE9-.LFB9
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI286-.LFB9
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.long	.LCFI287-.LCFI286
	.byte	0xe
	.uleb128 0x18
	.byte	0x4
	.long	.LCFI288-.LCFI287
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.long	.LCFI289-.LCFI288
	.byte	0xe
	.uleb128 0x28
	.byte	0x4
	.long	.LCFI290-.LCFI289
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
	.long	.LCFI291-.LCFI290
	.byte	0xe
	.uleb128 0x38
	.byte	0x4
	.long	.LCFI292-.LCFI291
	.byte	0xe
	.uleb128 0x60
	.byte	0x83
	.uleb128 0x7
	.align 8
.LEFDE207:
.LSFDE209:
	.long	.LEFDE209-.LASFDE209
.LASFDE209:
	.long	.LASFDE209-.Lframe1
	.long	.LFB10
	.long	.LFE10-.LFB10
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI294-.LFB10
	.byte	0x8c
	.uleb128 0x2
	.byte	0x83
	.uleb128 0x3
	.byte	0x4
	.long	.LCFI295-.LCFI294
	.byte	0xe
	.uleb128 0x20
	.align 8
.LEFDE209:
.LSFDE211:
	.long	.LEFDE211-.LASFDE211
.LASFDE211:
	.long	.LASFDE211-.Lframe1
	.long	.LFB80
	.long	.LFE80-.LFB80
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI296-.LFB80
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.long	.LCFI297-.LCFI296
	.byte	0xe
	.uleb128 0x18
	.byte	0x8c
	.uleb128 0x3
	.byte	0x8d
	.uleb128 0x2
	.byte	0x4
	.long	.LCFI298-.LCFI297
	.byte	0xe
	.uleb128 0x20
	.byte	0x83
	.uleb128 0x4
	.byte	0x4
	.long	.LCFI299-.LCFI298
	.byte	0xe
	.uleb128 0x120
	.align 8
.LEFDE211:
.LSFDE213:
	.long	.LEFDE213-.LASFDE213
.LASFDE213:
	.long	.LASFDE213-.Lframe1
	.long	.LFB113
	.long	.LFE113-.LFB113
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI300-.LFB113
	.byte	0xe
	.uleb128 0x10
	.align 8
.LEFDE213:
.LSFDE215:
	.long	.LEFDE215-.LASFDE215
.LASFDE215:
	.long	.LASFDE215-.Lframe1
	.long	.LFB99
	.long	.LFE99-.LFB99
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI301-.LFB99
	.byte	0xe
	.uleb128 0x10
	.align 8
.LEFDE215:
.LSFDE217:
	.long	.LEFDE217-.LASFDE217
.LASFDE217:
	.long	.LASFDE217-.Lframe1
	.long	.LFB98
	.long	.LFE98-.LFB98
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI302-.LFB98
	.byte	0xe
	.uleb128 0x10
	.align 8
.LEFDE217:
.LSFDE219:
	.long	.LEFDE219-.LASFDE219
.LASFDE219:
	.long	.LASFDE219-.Lframe1
	.long	.LFB89
	.long	.LFE89-.LFB89
	.uleb128 0x0
	.align 8
.LEFDE219:
.LSFDE221:
	.long	.LEFDE221-.LASFDE221
.LASFDE221:
	.long	.LASFDE221-.Lframe1
	.long	.LFB87
	.long	.LFE87-.LFB87
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI303-.LFB87
	.byte	0xe
	.uleb128 0x10
	.align 8
.LEFDE221:
.LSFDE223:
	.long	.LEFDE223-.LASFDE223
.LASFDE223:
	.long	.LASFDE223-.Lframe1
	.long	.LFB14
	.long	.LFE14-.LFB14
	.uleb128 0x0
	.byte	0x4
	.long	.LCFI304-.LFB14
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.long	.LCFI305-.LCFI304
	.byte	0xe
	.uleb128 0x18
	.byte	0x4
	.long	.LCFI306-.LCFI305
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.long	.LCFI307-.LCFI306
	.byte	0xe
	.uleb128 0x28
	.byte	0x4
	.long	.LCFI308-.LCFI307
	.byte	0xe
	.uleb128 0xd0
	.byte	0x83
	.uleb128 0x5
	.byte	0x86
	.uleb128 0x4
	.byte	0x8c
	.uleb128 0x3
	.byte	0x8d
	.uleb128 0x2
	.align 8
.LEFDE223:
	.ident	"GCC: (GNU) 4.1.2 20071124 (Red Hat 4.1.2-42)"
	.section	.note.GNU-stack,"",@progbits
