	.text
	.file	"SOS"
	.globl	main                    # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbx
	.cfi_def_cfa_offset 16
	subq	$96, %rsp
	.cfi_def_cfa_offset 112
	.cfi_offset %rbx, -16
	movl	$5, 92(%rsp)
	leaq	.Lfmt(%rip), %rdi
	movl	$5, %esi
	xorl	%eax, %eax
	callq	printf@PLT
	movl	$8, %edi
	callq	malloc@PLT
	movabsq	$-4654920573356972442, %rcx # imm = 0xBF666666BF666666
	movq	%rcx, (%rax)
	movq	%rax, 80(%rsp)
	movl	$8, %edi
	callq	malloc@PLT
	movabsq	$-4669332091305564570, %rcx # imm = 0xBF333333BF666666
	movq	%rcx, (%rax)
	movq	%rax, 72(%rsp)
	movl	$8, %edi
	callq	malloc@PLT
	movabsq	$-4669332091308920013, %rcx # imm = 0xBF333333BF333333
	movq	%rcx, (%rax)
	movq	%rax, 64(%rsp)
	movl	$8, %edi
	callq	malloc@PLT
	movabsq	$-4654920573360327885, %rcx # imm = 0xBF666666BF333333
	movq	%rcx, (%rax)
	movq	%rax, 56(%rsp)
	movl	$32, %edi
	callq	malloc@PLT
	movq	%rax, %rbx
	movq	80(%rsp), %rax
	movq	%rax, (%rbx)
	movq	72(%rsp), %rax
	movq	%rax, 8(%rbx)
	movq	64(%rsp), %rax
	movq	%rax, 16(%rbx)
	movq	56(%rsp), %rax
	movq	%rax, 24(%rbx)
	movl	$16, %edi
	callq	malloc@PLT
	movq	%rbx, (%rax)
	movl	$4, 8(%rax)
	movq	%rax, 48(%rsp)
	movl	$16, %edi
	callq	malloc@PLT
	movq	$1132396544, (%rax)     # imm = 0x437F0000
	movabsq	$4561245703459831808, %rbx # imm = 0x3F4CCCCD00000000
	movq	%rbx, 8(%rax)
	movq	%rax, 40(%rsp)
	movl	$16, %edi
	callq	malloc@PLT
	movabsq	$4863606122583425024, %rcx # imm = 0x437F000000000000
	movq	%rcx, (%rax)
	movq	%rbx, 8(%rax)
	movq	%rax, 32(%rsp)
	movl	$16, %edi
	callq	malloc@PLT
	movq	$0, (%rax)
	movabsq	$4561245704592228352, %rcx # imm = 0x3F4CCCCD437F0000
	movq	%rcx, 8(%rax)
	movq	%rax, 24(%rsp)
	movl	$16, %edi
	callq	malloc@PLT
	movabsq	$4812096202965778432, %rcx # imm = 0x42C8000042C80000
	movq	%rcx, (%rax)
	movq	%rbx, 8(%rax)
	movq	%rax, 16(%rsp)
	movl	$32, %edi
	callq	malloc@PLT
	movq	%rax, %rbx
	movq	40(%rsp), %rax
	movq	%rax, (%rbx)
	movq	32(%rsp), %rax
	movq	%rax, 8(%rbx)
	movq	24(%rsp), %rax
	movq	%rax, 16(%rbx)
	movq	16(%rsp), %rax
	movq	%rax, 24(%rbx)
	movl	$16, %edi
	callq	malloc@PLT
	movq	%rbx, (%rax)
	movl	$4, 8(%rax)
	movq	%rax, 8(%rsp)
	movl	$12, %edi
	callq	malloc@PLT
	movabsq	$1717986919200, %rcx    # imm = 0x19000000320
	movq	%rcx, (%rax)
	movl	$2, 8(%rax)
	movq	%rax, (%rsp)
	movq	%rax, %rdi
	callq	startCanvas@PLT
	movq	48(%rsp), %rdi
	movq	8(%rsp), %rsi
	xorl	%edx, %edx
	movl	$1, %ecx
	callq	drawShape@PLT
	movq	(%rsp), %rdi
	callq	endCanvas@PLT
	xorl	%eax, %eax
	addq	$96, %rsp
	.cfi_def_cfa_offset 16
	popq	%rbx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.globl	copy_point              # -- Begin function copy_point
	.p2align	4, 0x90
	.type	copy_point,@function
copy_point:                             # @copy_point
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	%rdi, (%rsp)
	callq	__copy2@PLT
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	copy_point, .Lfunc_end1-copy_point
	.cfi_endproc
                                        # -- End function
	.globl	__copy2                 # -- Begin function __copy2
	.p2align	4, 0x90
	.type	__copy2,@function
__copy2:                                # @__copy2
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbx
	.cfi_def_cfa_offset 16
	subq	$16, %rsp
	.cfi_def_cfa_offset 32
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	%rdi, 8(%rsp)
	movl	$8, %edi
	callq	malloc@PLT
	movss	(%rbx), %xmm0           # xmm0 = mem[0],zero,zero,zero
	movss	%xmm0, (%rax)
	movss	4(%rbx), %xmm0          # xmm0 = mem[0],zero,zero,zero
	movss	%xmm0, 4(%rax)
	addq	$16, %rsp
	.cfi_def_cfa_offset 16
	popq	%rbx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end2:
	.size	__copy2, .Lfunc_end2-__copy2
	.cfi_endproc
                                        # -- End function
	.globl	free_point              # -- Begin function free_point
	.p2align	4, 0x90
	.type	free_point,@function
free_point:                             # @free_point
	.cfi_startproc
# %bb.0:                                # %entry
	movq	%rdi, -8(%rsp)
	jmp	free@PLT                # TAILCALL
.Lfunc_end3:
	.size	free_point, .Lfunc_end3-free_point
	.cfi_endproc
                                        # -- End function
	.globl	copy_path               # -- Begin function copy_path
	.p2align	4, 0x90
	.type	copy_path,@function
copy_path:                              # @copy_path
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%r15
	.cfi_def_cfa_offset 16
	pushq	%r14
	.cfi_def_cfa_offset 24
	pushq	%r12
	.cfi_def_cfa_offset 32
	pushq	%rbx
	.cfi_def_cfa_offset 40
	subq	$24, %rsp
	.cfi_def_cfa_offset 64
	.cfi_offset %rbx, -40
	.cfi_offset %r12, -32
	.cfi_offset %r14, -24
	.cfi_offset %r15, -16
	movq	%rdi, 16(%rsp)
	movl	8(%rdi), %r14d
	movq	(%rdi), %r15
	leal	(,%r14,8), %edi
	callq	malloc@PLT
	movq	%rax, %r12
	movl	$0, 12(%rsp)
	.p2align	4, 0x90
.LBB4_1:                                # %loop
                                        # =>This Inner Loop Header: Depth=1
	movslq	12(%rsp), %rbx
	movq	(%r15,%rbx,8), %rdi
	callq	copy_point@PLT
	movq	%rax, (%r12,%rbx,8)
	leal	1(%rbx), %eax
	movl	%eax, 12(%rsp)
	cmpl	%r14d, %eax
	jl	.LBB4_1
# %bb.2:                                # %continue
	movl	$16, %edi
	callq	malloc@PLT
	movq	%r12, (%rax)
	movl	%r14d, 8(%rax)
	addq	$24, %rsp
	.cfi_def_cfa_offset 40
	popq	%rbx
	.cfi_def_cfa_offset 32
	popq	%r12
	.cfi_def_cfa_offset 24
	popq	%r14
	.cfi_def_cfa_offset 16
	popq	%r15
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end4:
	.size	copy_path, .Lfunc_end4-copy_path
	.cfi_endproc
                                        # -- End function
	.globl	free_path               # -- Begin function free_path
	.p2align	4, 0x90
	.type	free_path,@function
free_path:                              # @free_path
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	pushq	%r14
	.cfi_def_cfa_offset 24
	pushq	%rbx
	.cfi_def_cfa_offset 32
	subq	$16, %rsp
	.cfi_def_cfa_offset 48
	.cfi_offset %rbx, -32
	.cfi_offset %r14, -24
	.cfi_offset %rbp, -16
	movq	%rdi, 8(%rsp)
	movl	8(%rdi), %r14d
	movq	(%rdi), %rbx
	movl	$0, 4(%rsp)
	.p2align	4, 0x90
.LBB5_1:                                # %loop
                                        # =>This Inner Loop Header: Depth=1
	movslq	4(%rsp), %rbp
	movq	(%rbx,%rbp,8), %rdi
	callq	free_point@PLT
	leal	1(%rbp), %eax
	movl	%eax, 4(%rsp)
	cmpl	%r14d, %eax
	jl	.LBB5_1
# %bb.2:                                # %continue
	addq	$16, %rsp
	.cfi_def_cfa_offset 32
	popq	%rbx
	.cfi_def_cfa_offset 24
	popq	%r14
	.cfi_def_cfa_offset 16
	popq	%rbp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end5:
	.size	free_path, .Lfunc_end5-free_path
	.cfi_endproc
                                        # -- End function
	.globl	appendhelp_copyin       # -- Begin function appendhelp_copyin
	.p2align	4, 0x90
	.type	appendhelp_copyin,@function
appendhelp_copyin:                      # @appendhelp_copyin
	.cfi_startproc
# %bb.0:                                # %entry
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movq	%rdi, 16(%rsp)
	movq	%rsi, 8(%rsp)
	movl	%edx, 4(%rsp)
	cmpl	8(%rdi), %edx
	jge	.LBB6_2
# %bb.1:                                # %then
	movq	16(%rsp), %rax
	movq	(%rax), %rax
	movslq	4(%rsp), %rcx
	movq	8(%rsp), %rdx
	leal	1(%rcx), %esi
	movq	(%rdx), %rdx
	movslq	%esi, %rsi
	movq	(%rdx,%rsi,8), %rdx
	movq	%rdx, (%rax,%rcx,8)
	movq	16(%rsp), %rdi
	movq	8(%rsp), %rsi
	movl	4(%rsp), %edx
	incl	%edx
	callq	appendhelp_copyin@PLT
.LBB6_2:                                # %merge
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end6:
	.size	appendhelp_copyin, .Lfunc_end6-appendhelp_copyin
	.cfi_endproc
                                        # -- End function
	.globl	appendhelp_tail         # -- Begin function appendhelp_tail
	.p2align	4, 0x90
	.type	appendhelp_tail,@function
appendhelp_tail:                        # @appendhelp_tail
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	pushq	%r15
	.cfi_def_cfa_offset 24
	pushq	%r14
	.cfi_def_cfa_offset 32
	pushq	%rbx
	.cfi_def_cfa_offset 40
	subq	$24, %rsp
	.cfi_def_cfa_offset 64
	.cfi_offset %rbx, -40
	.cfi_offset %r14, -32
	.cfi_offset %r15, -24
	.cfi_offset %rbp, -16
	movq	%rdi, 16(%rsp)
	movl	8(%rdi), %ebx
	leal	-1(%rbx), %r15d
	movl	$8, %edi
	callq	malloc@PLT
	movq	%rax, %r14
	movl	$8, %edi
	callq	malloc@PLT
	movq	$0, (%rax)
	movq	%rax, (%r14)
	movl	$16, %edi
	callq	malloc@PLT
	movq	%r14, (%rax)
	movl	$1, 8(%rax)
	movl	$1, %ebp
	leal	-8(,%rbx,8), %edi
	callq	malloc@PLT
	movq	%rax, %rbx
	movl	$0, 4(%rsp)
	movl	$0, (%rsp)
	.p2align	4, 0x90
.LBB7_2:                                # %inner
                                        # =>This Inner Loop Header: Depth=1
	movslq	(%rsp), %rax
	movslq	4(%rsp), %rcx
	movq	(%r14,%rax,8), %rdx
	movq	%rdx, (%rbx,%rcx,8)
	incl	%ecx
	movl	%ecx, 4(%rsp)
	incl	%eax
	movl	%eax, (%rsp)
	cmpl	%ebp, %eax
	jl	.LBB7_2
# %bb.1:                                # %loop
                                        #   in Loop: Header=BB7_2 Depth=1
	movl	4(%rsp), %eax
	movl	$0, (%rsp)
	cmpl	%r15d, %eax
	jl	.LBB7_2
# %bb.3:                                # %continue
	movl	$16, %edi
	callq	malloc@PLT
	movq	%rbx, (%rax)
	movl	%r15d, 8(%rax)
	movq	%rax, 8(%rsp)
	movq	16(%rsp), %rsi
	movq	%rax, %rdi
	xorl	%edx, %edx
	callq	appendhelp_copyin@PLT
	movq	8(%rsp), %rax
	addq	$24, %rsp
	.cfi_def_cfa_offset 40
	popq	%rbx
	.cfi_def_cfa_offset 32
	popq	%r14
	.cfi_def_cfa_offset 24
	popq	%r15
	.cfi_def_cfa_offset 16
	popq	%rbp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end7:
	.size	appendhelp_tail, .Lfunc_end7-appendhelp_tail
	.cfi_endproc
                                        # -- End function
	.globl	append                  # -- Begin function append
	.p2align	4, 0x90
	.type	append,@function
append:                                 # @append
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$72, %rsp
	.cfi_offset %rbx, -56
	.cfi_offset %r12, -48
	.cfi_offset %r13, -40
	.cfi_offset %r14, -32
	.cfi_offset %r15, -24
	movq	%rdi, -56(%rbp)
	movq	%rsi, -48(%rbp)
	cmpl	$0, 8(%rdi)
	je	.LBB8_4
# %bb.1:                                # %else
	movq	-48(%rbp), %rax
	movq	%rsp, %rbx
	addq	$-16, %rbx
	movq	%rbx, %rsp
	cmpl	$0, 8(%rax)
	je	.LBB8_5
# %bb.2:                                # %else12
	movq	%rbx, -104(%rbp)        # 8-byte Spill
	movq	%rsp, %r14
	leaq	-16(%r14), %rax
	movq	%rax, -96(%rbp)         # 8-byte Spill
	movq	%rax, %rsp
	movq	-56(%rbp), %rax
	movl	8(%rax), %ecx
	decl	%ecx
	movq	(%rax), %rax
	movslq	%ecx, %rcx
	movq	(%rax,%rcx,8), %rdi
	movq	-48(%rbp), %rax
	movq	(%rax), %rax
	movq	(%rax), %rsi
	callq	__eqf2@PLT
	andb	$1, %al
	movb	%al, -16(%r14)
	movq	%rsp, %r15
	addq	$-16, %r15
	movq	%r15, %rsp
	movq	%rsp, %rbx
	addq	$-16, %rbx
	movq	%rbx, %rsp
	cmpb	$0, -16(%r14)
	je	.LBB8_6
# %bb.3:                                # %then29
	movq	-48(%rbp), %rdi
	callq	appendhelp_tail@PLT
	jmp	.LBB8_7
.LBB8_4:                                # %then
	movq	-48(%rbp), %rdi
	callq	copy_path@PLT
	jmp	.LBB8_16
.LBB8_5:                                # %then11
	movq	-56(%rbp), %rdi
	callq	copy_path@PLT
	jmp	.LBB8_14
.LBB8_6:                                # %else30
	movq	-48(%rbp), %rax
.LBB8_7:                                # %merge28
	movq	%rax, (%rbx)
	movq	(%rbx), %rax
	movq	%rax, (%r15)
	movq	%rsp, %rax
	addq	$-16, %rax
	movq	%rax, -88(%rbp)         # 8-byte Spill
	movq	%rax, %rsp
	movq	-56(%rbp), %rax
	movq	%r15, -72(%rbp)         # 8-byte Spill
	movq	(%r15), %rcx
	movl	8(%rax), %r14d
	movl	8(%rcx), %r15d
	leal	(%r14,%r15), %edx
	movq	(%rax), %r13
	movq	(%rcx), %r12
	movq	%rdx, -80(%rbp)         # 8-byte Spill
	leal	(,%rdx,8), %edi
	callq	malloc@PLT
	movq	%rax, %rbx
	movq	%rsp, %rcx
	leaq	-16(%rcx), %rax
	movq	%rax, %rsp
	movl	$0, -16(%rcx)
	movq	%rsp, %rdx
	leaq	-16(%rdx), %rcx
	movq	%rcx, %rsp
	movl	$0, -16(%rdx)
	.p2align	4, 0x90
.LBB8_8:                                # %loop1
                                        # =>This Inner Loop Header: Depth=1
	movslq	(%rcx), %rdx
	movslq	(%rax), %rsi
	movq	(%r13,%rdx,8), %rdi
	movq	%rdi, (%rbx,%rsi,8)
	incl	%esi
	movl	%esi, (%rax)
	incl	%edx
	movl	%edx, (%rcx)
	cmpl	%r14d, %edx
	jl	.LBB8_8
# %bb.9:                                # %inbtw
	movl	$0, (%rcx)
	.p2align	4, 0x90
.LBB8_10:                               # %loop2
                                        # =>This Inner Loop Header: Depth=1
	movslq	(%rcx), %rdx
	movslq	(%rax), %rsi
	movq	(%r12,%rdx,8), %rdi
	movq	%rdi, (%rbx,%rsi,8)
	incl	%esi
	movl	%esi, (%rax)
	incl	%edx
	movl	%edx, (%rcx)
	cmpl	%r15d, %edx
	jl	.LBB8_10
# %bb.11:                               # %contb
	movl	$16, %edi
	callq	malloc@PLT
	movq	%rbx, (%rax)
	movq	-80(%rbp), %rcx         # 8-byte Reload
	movl	%ecx, 8(%rax)
	movq	-88(%rbp), %rbx         # 8-byte Reload
	movq	%rax, (%rbx)
	movq	-96(%rbp), %rax         # 8-byte Reload
	cmpb	$0, (%rax)
	je	.LBB8_13
# %bb.12:                               # %then58
	movq	-72(%rbp), %rax         # 8-byte Reload
	movq	(%rax), %rdi
	callq	free_path@PLT
.LBB8_13:                               # %merge57
	movq	(%rbx), %rax
	movq	-104(%rbp), %rbx        # 8-byte Reload
.LBB8_14:                               # %merge10
	movq	%rax, (%rbx)
	movq	(%rbx), %rax
.LBB8_16:                               # %merge
	movq	%rax, -64(%rbp)
	movq	-64(%rbp), %rax
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end8:
	.size	append, .Lfunc_end8-append
	.cfi_endproc
                                        # -- End function
	.globl	__eqf2                  # -- Begin function __eqf2
	.p2align	4, 0x90
	.type	__eqf2,@function
__eqf2:                                 # @__eqf2
	.cfi_startproc
# %bb.0:                                # %entry
	movq	%rdi, -8(%rsp)
	movq	%rsi, -16(%rsp)
	movb	$1, -17(%rsp)
	movss	(%rdi), %xmm0           # xmm0 = mem[0],zero,zero,zero
	ucomiss	(%rsi), %xmm0
	setnp	%al
	sete	%cl
	andb	%al, %cl
	movb	%cl, -17(%rsp)
	movss	4(%rdi), %xmm0          # xmm0 = mem[0],zero,zero,zero
	ucomiss	4(%rsi), %xmm0
	setnp	%dl
	sete	%al
	andb	%dl, %al
	andb	%cl, %al
	movb	%al, -17(%rsp)
	retq
.Lfunc_end9:
	.size	__eqf2, .Lfunc_end9-__eqf2
	.cfi_endproc
                                        # -- End function
	.globl	reversedhelp            # -- Begin function reversedhelp
	.p2align	4, 0x90
	.type	reversedhelp,@function
reversedhelp:                           # @reversedhelp
	.cfi_startproc
# %bb.0:                                # %entry
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movq	%rdi, 16(%rsp)
	movq	%rsi, 8(%rsp)
	movl	%edx, 4(%rsp)
	cmpl	8(%rdi), %edx
	jge	.LBB10_2
# %bb.1:                                # %then
	movq	16(%rsp), %rax
	movslq	4(%rsp), %rcx
	movq	(%rax), %rdx
	movq	(%rdx,%rcx,8), %rdx
	movq	8(%rsp), %rsi
	notl	%ecx
	addl	8(%rax), %ecx
	movq	(%rsi), %rax
	movslq	%ecx, %rcx
	movq	(%rax,%rcx,8), %rax
	movss	(%rax), %xmm0           # xmm0 = mem[0],zero,zero,zero
	movss	%xmm0, (%rdx)
	movq	16(%rsp), %rax
	movslq	4(%rsp), %rcx
	movq	(%rax), %rdx
	movq	(%rdx,%rcx,8), %rdx
	movq	8(%rsp), %rsi
	notl	%ecx
	addl	8(%rax), %ecx
	movq	(%rsi), %rax
	movslq	%ecx, %rcx
	movq	(%rax,%rcx,8), %rax
	movss	4(%rax), %xmm0          # xmm0 = mem[0],zero,zero,zero
	movss	%xmm0, 4(%rdx)
	movq	16(%rsp), %rdi
	movq	8(%rsp), %rsi
	movl	4(%rsp), %edx
	incl	%edx
	callq	reversedhelp@PLT
.LBB10_2:                               # %merge
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end10:
	.size	reversedhelp, .Lfunc_end10-reversedhelp
	.cfi_endproc
                                        # -- End function
	.globl	reversed                # -- Begin function reversed
	.p2align	4, 0x90
	.type	reversed,@function
reversed:                               # @reversed
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	pushq	%r15
	.cfi_def_cfa_offset 24
	pushq	%r14
	.cfi_def_cfa_offset 32
	pushq	%rbx
	.cfi_def_cfa_offset 40
	subq	$24, %rsp
	.cfi_def_cfa_offset 64
	.cfi_offset %rbx, -40
	.cfi_offset %r14, -32
	.cfi_offset %r15, -24
	.cfi_offset %rbp, -16
	movq	%rdi, 16(%rsp)
	movl	8(%rdi), %r15d
	movl	$8, %edi
	callq	malloc@PLT
	movq	%rax, %r14
	movl	$8, %edi
	callq	malloc@PLT
	movq	$0, (%rax)
	movq	%rax, (%r14)
	movl	$16, %edi
	callq	malloc@PLT
	movq	%r14, (%rax)
	movl	$1, 8(%rax)
	movl	$1, %ebp
	leal	(,%r15,8), %edi
	callq	malloc@PLT
	movq	%rax, %rbx
	movl	$0, 4(%rsp)
	movl	$0, (%rsp)
	.p2align	4, 0x90
.LBB11_2:                               # %inner
                                        # =>This Inner Loop Header: Depth=1
	movslq	(%rsp), %rax
	movslq	4(%rsp), %rcx
	movq	(%r14,%rax,8), %rdx
	movq	%rdx, (%rbx,%rcx,8)
	incl	%ecx
	movl	%ecx, 4(%rsp)
	incl	%eax
	movl	%eax, (%rsp)
	cmpl	%ebp, %eax
	jl	.LBB11_2
# %bb.1:                                # %loop
                                        #   in Loop: Header=BB11_2 Depth=1
	movl	4(%rsp), %eax
	movl	$0, (%rsp)
	cmpl	%r15d, %eax
	jl	.LBB11_2
# %bb.3:                                # %continue
	movl	$16, %edi
	callq	malloc@PLT
	movq	%rbx, (%rax)
	movl	%r15d, 8(%rax)
	movq	%rax, 8(%rsp)
	movq	16(%rsp), %rsi
	movq	%rax, %rdi
	xorl	%edx, %edx
	callq	reversedhelp@PLT
	movq	8(%rsp), %rax
	addq	$24, %rsp
	.cfi_def_cfa_offset 40
	popq	%rbx
	.cfi_def_cfa_offset 32
	popq	%r14
	.cfi_def_cfa_offset 24
	popq	%r15
	.cfi_def_cfa_offset 16
	popq	%rbp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end11:
	.size	reversed, .Lfunc_end11-reversed
	.cfi_endproc
                                        # -- End function
	.globl	reversehelp             # -- Begin function reversehelp
	.p2align	4, 0x90
	.type	reversehelp,@function
reversehelp:                            # @reversehelp
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -16(%rbp)
	movl	%esi, -4(%rbp)
	movl	8(%rdi), %eax
	movl	%eax, %ecx
	shrl	$31, %ecx
	addl	%eax, %ecx
	sarl	%ecx
	cmpl	%ecx, %esi
	jge	.LBB12_2
# %bb.1:                                # %then
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movq	-16(%rbp), %rcx
	movslq	-4(%rbp), %rdx
	movq	(%rcx), %rcx
	movq	(%rcx,%rdx,8), %rcx
	movq	%rcx, -16(%rax)
	movq	-16(%rbp), %rcx
	movq	(%rcx), %rdx
	movslq	-4(%rbp), %rsi
	movl	%esi, %edi
	notl	%edi
	addl	8(%rcx), %edi
	movslq	%edi, %rcx
	movq	(%rdx,%rcx,8), %rcx
	movq	%rcx, (%rdx,%rsi,8)
	movq	-16(%rbp), %rcx
	movq	(%rcx), %rdx
	movl	-4(%rbp), %esi
	notl	%esi
	addl	8(%rcx), %esi
	movq	-16(%rax), %rax
	movslq	%esi, %rcx
	movq	%rax, (%rdx,%rcx,8)
	movq	-16(%rbp), %rdi
	movl	-4(%rbp), %esi
	incl	%esi
	callq	reversehelp@PLT
.LBB12_2:                               # %merge
	movq	%rbp, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end12:
	.size	reversehelp, .Lfunc_end12-reversehelp
	.cfi_endproc
                                        # -- End function
	.globl	reverse                 # -- Begin function reverse
	.p2align	4, 0x90
	.type	reverse,@function
reverse:                                # @reverse
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	%rdi, (%rsp)
	xorl	%esi, %esi
	callq	reversehelp@PLT
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end13:
	.size	reverse, .Lfunc_end13-reverse
	.cfi_endproc
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2               # -- Begin function floor
.LCPI14_0:
	.long	3212836864              # float -1
	.text
	.globl	floor
	.p2align	4, 0x90
	.type	floor,@function
floor:                                  # @floor
	.cfi_startproc
# %bb.0:                                # %entry
	movss	%xmm0, -4(%rsp)
	cvttss2si	%xmm0, %eax
	movl	%eax, -8(%rsp)
	cvtsi2ss	%eax, %xmm1
	movss	%xmm1, -12(%rsp)
	ucomiss	%xmm1, %xmm0
	jb	.LBB14_2
# %bb.1:                                # %then
	movss	-12(%rsp), %xmm0        # xmm0 = mem[0],zero,zero,zero
	jmp	.LBB14_3
.LBB14_2:                               # %else
	movss	-12(%rsp), %xmm0        # xmm0 = mem[0],zero,zero,zero
	addss	.LCPI14_0(%rip), %xmm0
.LBB14_3:                               # %merge
	movss	%xmm0, -16(%rsp)
	movss	-16(%rsp), %xmm0        # xmm0 = mem[0],zero,zero,zero
	retq
.Lfunc_end14:
	.size	floor, .Lfunc_end14-floor
	.cfi_endproc
                                        # -- End function
	.section	.rodata.cst16,"aM",@progbits,16
	.p2align	4               # -- Begin function ceil
.LCPI15_0:
	.long	2147483648              # float -0
	.long	2147483648              # float -0
	.long	2147483648              # float -0
	.long	2147483648              # float -0
	.text
	.globl	ceil
	.p2align	4, 0x90
	.type	ceil,@function
ceil:                                   # @ceil
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movss	%xmm0, 4(%rsp)
	xorps	.LCPI15_0(%rip), %xmm0
	callq	floor@PLT
	xorps	.LCPI15_0(%rip), %xmm0
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end15:
	.size	ceil, .Lfunc_end15-ceil
	.cfi_endproc
                                        # -- End function
	.globl	frac                    # -- Begin function frac
	.p2align	4, 0x90
	.type	frac,@function
frac:                                   # @frac
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movss	%xmm0, (%rsp)           # 4-byte Spill
	movss	%xmm0, 4(%rsp)
	callq	floor@PLT
	movss	(%rsp), %xmm1           # 4-byte Reload
                                        # xmm1 = mem[0],zero,zero,zero
	subss	%xmm0, %xmm1
	movaps	%xmm1, %xmm0
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end16:
	.size	frac, .Lfunc_end16-frac
	.cfi_endproc
                                        # -- End function
	.globl	max                     # -- Begin function max
	.p2align	4, 0x90
	.type	max,@function
max:                                    # @max
	.cfi_startproc
# %bb.0:                                # %entry
	movss	%xmm0, -4(%rsp)
	movss	%xmm1, -8(%rsp)
	ucomiss	%xmm0, %xmm1
	jbe	.LBB17_2
# %bb.1:                                # %then
	movss	-8(%rsp), %xmm0         # xmm0 = mem[0],zero,zero,zero
	jmp	.LBB17_3
.LBB17_2:                               # %else
	movss	-4(%rsp), %xmm0         # xmm0 = mem[0],zero,zero,zero
.LBB17_3:                               # %merge
	movss	%xmm0, -12(%rsp)
	movss	-12(%rsp), %xmm0        # xmm0 = mem[0],zero,zero,zero
	retq
.Lfunc_end17:
	.size	max, .Lfunc_end17-max
	.cfi_endproc
                                        # -- End function
	.globl	min                     # -- Begin function min
	.p2align	4, 0x90
	.type	min,@function
min:                                    # @min
	.cfi_startproc
# %bb.0:                                # %entry
	movss	%xmm0, -4(%rsp)
	movss	%xmm1, -8(%rsp)
	ucomiss	%xmm0, %xmm1
	jbe	.LBB18_2
# %bb.1:                                # %then
	movss	-4(%rsp), %xmm0         # xmm0 = mem[0],zero,zero,zero
	jmp	.LBB18_3
.LBB18_2:                               # %else
	movss	-8(%rsp), %xmm0         # xmm0 = mem[0],zero,zero,zero
.LBB18_3:                               # %merge
	movss	%xmm0, -12(%rsp)
	movss	-12(%rsp), %xmm0        # xmm0 = mem[0],zero,zero,zero
	retq
.Lfunc_end18:
	.size	min, .Lfunc_end18-min
	.cfi_endproc
                                        # -- End function
	.globl	clamp                   # -- Begin function clamp
	.p2align	4, 0x90
	.type	clamp,@function
clamp:                                  # @clamp
	.cfi_startproc
# %bb.0:                                # %entry
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movss	%xmm2, 8(%rsp)          # 4-byte Spill
	movss	%xmm0, 20(%rsp)
	movss	%xmm1, 16(%rsp)
	movss	%xmm2, 12(%rsp)
	callq	max@PLT
	movaps	%xmm0, %xmm1
	movss	8(%rsp), %xmm0          # 4-byte Reload
                                        # xmm0 = mem[0],zero,zero,zero
	callq	min@PLT
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end19:
	.size	clamp, .Lfunc_end19-clamp
	.cfi_endproc
                                        # -- End function
	.section	.rodata.cst16,"aM",@progbits,16
	.p2align	4               # -- Begin function abs
.LCPI20_0:
	.long	2147483648              # float -0
	.long	2147483648              # float -0
	.long	2147483648              # float -0
	.long	2147483648              # float -0
	.text
	.globl	abs
	.p2align	4, 0x90
	.type	abs,@function
abs:                                    # @abs
	.cfi_startproc
# %bb.0:                                # %entry
	movss	%xmm0, -4(%rsp)
	xorps	%xmm1, %xmm1
	ucomiss	%xmm0, %xmm1
	jbe	.LBB20_2
# %bb.1:                                # %then
	movss	-4(%rsp), %xmm0         # xmm0 = mem[0],zero,zero,zero
	xorps	.LCPI20_0(%rip), %xmm0
	jmp	.LBB20_3
.LBB20_2:                               # %else
	movss	-4(%rsp), %xmm0         # xmm0 = mem[0],zero,zero,zero
.LBB20_3:                               # %merge
	movss	%xmm0, -8(%rsp)
	movss	-8(%rsp), %xmm0         # xmm0 = mem[0],zero,zero,zero
	retq
.Lfunc_end20:
	.size	abs, .Lfunc_end20-abs
	.cfi_endproc
                                        # -- End function
	.globl	modf                    # -- Begin function modf
	.p2align	4, 0x90
	.type	modf,@function
modf:                                   # @modf
	.cfi_startproc
# %bb.0:                                # %entry
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movss	%xmm1, 12(%rsp)         # 4-byte Spill
	movss	%xmm0, 20(%rsp)
	movss	%xmm1, 16(%rsp)
	divss	%xmm1, %xmm0
	callq	frac@PLT
	mulss	12(%rsp), %xmm0         # 4-byte Folded Reload
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end21:
	.size	modf, .Lfunc_end21-modf
	.cfi_endproc
                                        # -- End function
	.globl	sin                     # -- Begin function sin
	.p2align	4, 0x90
	.type	sin,@function
sin:                                    # @sin
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movss	%xmm0, 4(%rsp)
	callq	sinf@PLT
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end22:
	.size	sin, .Lfunc_end22-sin
	.cfi_endproc
                                        # -- End function
	.globl	cos                     # -- Begin function cos
	.p2align	4, 0x90
	.type	cos,@function
cos:                                    # @cos
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movss	%xmm0, 4(%rsp)
	callq	cosf@PLT
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end23:
	.size	cos, .Lfunc_end23-cos
	.cfi_endproc
                                        # -- End function
	.globl	tan                     # -- Begin function tan
	.p2align	4, 0x90
	.type	tan,@function
tan:                                    # @tan
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movss	%xmm0, 4(%rsp)
	callq	tanf@PLT
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end24:
	.size	tan, .Lfunc_end24-tan
	.cfi_endproc
                                        # -- End function
	.globl	asin                    # -- Begin function asin
	.p2align	4, 0x90
	.type	asin,@function
asin:                                   # @asin
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movss	%xmm0, 4(%rsp)
	callq	asinf@PLT
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end25:
	.size	asin, .Lfunc_end25-asin
	.cfi_endproc
                                        # -- End function
	.globl	acos                    # -- Begin function acos
	.p2align	4, 0x90
	.type	acos,@function
acos:                                   # @acos
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movss	%xmm0, 4(%rsp)
	callq	acosf@PLT
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end26:
	.size	acos, .Lfunc_end26-acos
	.cfi_endproc
                                        # -- End function
	.globl	atan                    # -- Begin function atan
	.p2align	4, 0x90
	.type	atan,@function
atan:                                   # @atan
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movss	%xmm0, 4(%rsp)
	callq	atanf@PLT
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end27:
	.size	atan, .Lfunc_end27-atan
	.cfi_endproc
                                        # -- End function
	.globl	sqrt                    # -- Begin function sqrt
	.p2align	4, 0x90
	.type	sqrt,@function
sqrt:                                   # @sqrt
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movss	%xmm0, 4(%rsp)
	xorps	%xmm1, %xmm1
	ucomiss	%xmm1, %xmm0
	jb	.LBB28_2
# %bb.1:
	sqrtss	%xmm0, %xmm0
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.LBB28_2:                               # %call.sqrt
	.cfi_def_cfa_offset 16
	callq	sqrtf@PLT
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end28:
	.size	sqrt, .Lfunc_end28-sqrt
	.cfi_endproc
                                        # -- End function
	.globl	toradians               # -- Begin function toradians
	.p2align	4, 0x90
	.type	toradians,@function
toradians:                              # @toradians
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movss	%xmm0, 4(%rsp)
	callq	toradians_f@PLT
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end29:
	.size	toradians, .Lfunc_end29-toradians
	.cfi_endproc
                                        # -- End function
	.globl	rgb                     # -- Begin function rgb
	.p2align	4, 0x90
	.type	rgb,@function
rgb:                                    # @rgb
	.cfi_startproc
# %bb.0:                                # %entry
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movss	%xmm0, 20(%rsp)
	movss	%xmm1, 16(%rsp)
	movss	%xmm2, 12(%rsp)
	movl	$16, %edi
	callq	malloc@PLT
	movss	20(%rsp), %xmm0         # xmm0 = mem[0],zero,zero,zero
	movss	%xmm0, (%rax)
	movss	16(%rsp), %xmm0         # xmm0 = mem[0],zero,zero,zero
	movss	%xmm0, 4(%rax)
	movss	12(%rsp), %xmm0         # xmm0 = mem[0],zero,zero,zero
	movss	%xmm0, 8(%rax)
	movl	$1065353216, 12(%rax)   # imm = 0x3F800000
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end30:
	.size	rgb, .Lfunc_end30-rgb
	.cfi_endproc
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2               # -- Begin function hsv
.LCPI31_0:
	.long	1086324736              # float 6
.LCPI31_1:
	.long	1073741824              # float 2
.LCPI31_2:
	.long	1065353216              # float 1
.LCPI31_3:
	.long	1077936128              # float 3
.LCPI31_4:
	.long	1082130432              # float 4
.LCPI31_5:
	.long	1084227584              # float 5
	.text
	.globl	hsv
	.p2align	4, 0x90
	.type	hsv,@function
hsv:                                    # @hsv
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r12
	pushq	%rbx
	subq	$48, %rsp
	.cfi_offset %rbx, -48
	.cfi_offset %r12, -40
	.cfi_offset %r14, -32
	.cfi_offset %r15, -24
	movss	%xmm0, -60(%rbp)
	movss	%xmm1, -80(%rbp)
	movss	%xmm2, -36(%rbp)
	mulss	%xmm1, %xmm2
	movss	%xmm2, -52(%rbp)
	mulss	.LCPI31_0(%rip), %xmm0
	movss	.LCPI31_1(%rip), %xmm1  # xmm1 = mem[0],zero,zero,zero
	callq	modf@PLT
	movss	%xmm0, -76(%rbp)
	movss	-52(%rbp), %xmm1        # xmm1 = mem[0],zero,zero,zero
	movss	%xmm1, -56(%rbp)        # 4-byte Spill
	subss	.LCPI31_2(%rip), %xmm0
	callq	abs@PLT
	movss	.LCPI31_2(%rip), %xmm2  # xmm2 = mem[0],zero,zero,zero
	movaps	%xmm2, %xmm1
	subss	%xmm0, %xmm1
	mulss	-56(%rbp), %xmm1        # 4-byte Folded Reload
	movss	%xmm1, -44(%rbp)
	movss	-36(%rbp), %xmm0        # xmm0 = mem[0],zero,zero,zero
	subss	-52(%rbp), %xmm0
	movss	%xmm0, -40(%rbp)
	movss	.LCPI31_0(%rip), %xmm0  # xmm0 = mem[0],zero,zero,zero
	mulss	-60(%rbp), %xmm0
	movss	%xmm0, -48(%rbp)
	ucomiss	%xmm0, %xmm2
	jbe	.LBB31_2
# %bb.1:                                # %then
	movss	-36(%rbp), %xmm0        # xmm0 = mem[0],zero,zero,zero
	movss	-40(%rbp), %xmm2        # xmm2 = mem[0],zero,zero,zero
	movss	-44(%rbp), %xmm1        # xmm1 = mem[0],zero,zero,zero
	addss	%xmm2, %xmm1
	callq	rgb@PLT
	jmp	.LBB31_17
.LBB31_2:                               # %else
	movq	%rsp, %rbx
	addq	$-16, %rbx
	movq	%rbx, %rsp
	movss	.LCPI31_1(%rip), %xmm0  # xmm0 = mem[0],zero,zero,zero
	ucomiss	-48(%rbp), %xmm0
	jbe	.LBB31_4
# %bb.3:                                # %then31
	movss	-40(%rbp), %xmm2        # xmm2 = mem[0],zero,zero,zero
	movss	-44(%rbp), %xmm0        # xmm0 = mem[0],zero,zero,zero
	addss	%xmm2, %xmm0
	movss	-36(%rbp), %xmm1        # xmm1 = mem[0],zero,zero,zero
	callq	rgb@PLT
	jmp	.LBB31_15
.LBB31_4:                               # %else32
	movq	%rsp, %r14
	addq	$-16, %r14
	movq	%r14, %rsp
	movss	.LCPI31_3(%rip), %xmm0  # xmm0 = mem[0],zero,zero,zero
	ucomiss	-48(%rbp), %xmm0
	jbe	.LBB31_6
# %bb.5:                                # %then43
	movss	-40(%rbp), %xmm0        # xmm0 = mem[0],zero,zero,zero
	movss	-36(%rbp), %xmm1        # xmm1 = mem[0],zero,zero,zero
	movss	-44(%rbp), %xmm2        # xmm2 = mem[0],zero,zero,zero
	addss	%xmm0, %xmm2
	callq	rgb@PLT
	jmp	.LBB31_14
.LBB31_6:                               # %else44
	movq	%rsp, %r15
	addq	$-16, %r15
	movq	%r15, %rsp
	movss	.LCPI31_4(%rip), %xmm0  # xmm0 = mem[0],zero,zero,zero
	ucomiss	-48(%rbp), %xmm0
	jbe	.LBB31_8
# %bb.7:                                # %then55
	movss	-40(%rbp), %xmm0        # xmm0 = mem[0],zero,zero,zero
	movss	-44(%rbp), %xmm1        # xmm1 = mem[0],zero,zero,zero
	addss	%xmm0, %xmm1
	movss	-36(%rbp), %xmm2        # xmm2 = mem[0],zero,zero,zero
	callq	rgb@PLT
	jmp	.LBB31_12
.LBB31_8:                               # %else56
	movq	%rsp, %r12
	addq	$-16, %r12
	movq	%r12, %rsp
	movss	.LCPI31_5(%rip), %xmm0  # xmm0 = mem[0],zero,zero,zero
	ucomiss	-48(%rbp), %xmm0
	jbe	.LBB31_10
# %bb.9:                                # %then67
	movss	-40(%rbp), %xmm1        # xmm1 = mem[0],zero,zero,zero
	movss	-44(%rbp), %xmm0        # xmm0 = mem[0],zero,zero,zero
	addss	%xmm1, %xmm0
	movss	-36(%rbp), %xmm2        # xmm2 = mem[0],zero,zero,zero
	jmp	.LBB31_11
.LBB31_10:                              # %else68
	movss	-36(%rbp), %xmm0        # xmm0 = mem[0],zero,zero,zero
	movss	-40(%rbp), %xmm1        # xmm1 = mem[0],zero,zero,zero
	movss	-44(%rbp), %xmm2        # xmm2 = mem[0],zero,zero,zero
	addss	%xmm1, %xmm2
.LBB31_11:                              # %merge66
	callq	rgb@PLT
	movq	%rax, (%r12)
	movq	(%r12), %rax
.LBB31_12:                              # %merge54
	movq	%rax, (%r15)
	movq	(%r15), %rax
.LBB31_14:                              # %merge42
	movq	%rax, (%r14)
	movq	(%r14), %rax
.LBB31_15:                              # %merge30
	movq	%rax, (%rbx)
	movq	(%rbx), %rax
.LBB31_17:                              # %merge
	movq	%rax, -72(%rbp)
	movq	-72(%rbp), %rax
	leaq	-32(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end31:
	.size	hsv, .Lfunc_end31-hsv
	.cfi_endproc
                                        # -- End function
	.globl	startCanvas             # -- Begin function startCanvas
	.p2align	4, 0x90
	.type	startCanvas,@function
startCanvas:                            # @startCanvas
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	%rdi, (%rsp)
	movl	(%rdi), %eax
	movl	4(%rdi), %esi
	movl	%eax, %edi
	callq	gl_startRendering@PLT
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end32:
	.size	startCanvas, .Lfunc_end32-startCanvas
	.cfi_endproc
                                        # -- End function
	.globl	cvoid                   # -- Begin function cvoid
	.p2align	4, 0x90
	.type	cvoid,@function
cvoid:                                  # @cvoid
	.cfi_startproc
# %bb.0:                                # %entry
	retq
.Lfunc_end33:
	.size	cvoid, .Lfunc_end33-cvoid
	.cfi_endproc
                                        # -- End function
	.globl	drawHelper              # -- Begin function drawHelper
	.p2align	4, 0x90
	.type	drawHelper,@function
drawHelper:                             # @drawHelper
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -40(%rbp)
	movq	%rsi, -24(%rbp)
	movl	%edx, -44(%rbp)
	movl	%ecx, -4(%rbp)
	movq	%r8, -32(%rbp)
	movq	%r9, -16(%rbp)
	cmpl	%edx, %ecx
	jl	.LBB34_2
# %bb.1:                                # %then
	callq	cvoid@PLT
	jmp	.LBB34_3
.LBB34_2:                               # %else
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movq	-40(%rbp), %rcx
	movslq	-4(%rbp), %rdx
	movq	(%rcx), %rcx
	movq	(%rcx,%rdx,8), %rcx
	movss	(%rcx), %xmm0           # xmm0 = mem[0],zero,zero,zero
	movss	%xmm0, -16(%rax)
	movq	%rsp, %rcx
	leaq	-16(%rcx), %rsp
	movq	-40(%rbp), %rdx
	movslq	-4(%rbp), %rsi
	movq	(%rdx), %rdx
	movq	(%rdx,%rsi,8), %rdx
	movss	4(%rdx), %xmm0          # xmm0 = mem[0],zero,zero,zero
	movss	%xmm0, -16(%rcx)
	movq	-32(%rbp), %rdx
	movq	(%rdx), %rdx
	movl	-4(%rbp), %esi
	addl	%esi, %esi
	movss	-16(%rax), %xmm0        # xmm0 = mem[0],zero,zero,zero
	movslq	%esi, %rax
	movss	%xmm0, (%rdx,%rax,4)
	movq	-32(%rbp), %rax
	movq	(%rax), %rax
	movl	-4(%rbp), %edx
	leal	1(%rdx,%rdx), %edx
	movss	-16(%rcx), %xmm0        # xmm0 = mem[0],zero,zero,zero
	movslq	%edx, %rcx
	movss	%xmm0, (%rax,%rcx,4)
	movq	%rsp, %r8
	leaq	-16(%r8), %rsp
	movq	-24(%rbp), %rcx
	movslq	-4(%rbp), %rdx
	movq	(%rcx), %rcx
	movq	(%rcx,%rdx,8), %rcx
	movss	(%rcx), %xmm0           # xmm0 = mem[0],zero,zero,zero
	movss	%xmm0, -16(%r8)
	movq	%rsp, %rcx
	leaq	-16(%rcx), %rsp
	movq	-24(%rbp), %rdx
	movslq	-4(%rbp), %rsi
	movq	(%rdx), %rdx
	movq	(%rdx,%rsi,8), %rdx
	movss	4(%rdx), %xmm0          # xmm0 = mem[0],zero,zero,zero
	movss	%xmm0, -16(%rcx)
	movq	%rsp, %rdx
	leaq	-16(%rdx), %rsp
	movq	-24(%rbp), %rsi
	movslq	-4(%rbp), %rdi
	movq	(%rsi), %rsi
	movq	(%rsi,%rdi,8), %rsi
	movss	8(%rsi), %xmm0          # xmm0 = mem[0],zero,zero,zero
	movss	%xmm0, -16(%rdx)
	movq	%rsp, %rsi
	leaq	-16(%rsi), %rsp
	movq	-24(%rbp), %rdi
	movslq	-4(%rbp), %rax
	movq	(%rdi), %rdi
	movq	(%rdi,%rax,8), %rax
	movss	12(%rax), %xmm0         # xmm0 = mem[0],zero,zero,zero
	movss	%xmm0, -16(%rsi)
	movq	-16(%rbp), %rax
	movq	(%rax), %rax
	movl	-4(%rbp), %edi
	shll	$2, %edi
	movss	-16(%r8), %xmm0         # xmm0 = mem[0],zero,zero,zero
	movslq	%edi, %rdi
	movss	%xmm0, (%rax,%rdi,4)
	movq	-16(%rbp), %rax
	movq	(%rax), %rax
	movl	-4(%rbp), %edi
	leal	1(,%rdi,4), %edi
	movss	-16(%rcx), %xmm0        # xmm0 = mem[0],zero,zero,zero
	movslq	%edi, %rcx
	movss	%xmm0, (%rax,%rcx,4)
	movq	-16(%rbp), %rax
	movq	(%rax), %rax
	movl	-4(%rbp), %ecx
	leal	2(,%rcx,4), %ecx
	movss	-16(%rdx), %xmm0        # xmm0 = mem[0],zero,zero,zero
	movslq	%ecx, %rcx
	movss	%xmm0, (%rax,%rcx,4)
	movq	-16(%rbp), %rax
	movq	(%rax), %rax
	movl	-4(%rbp), %ecx
	leal	3(,%rcx,4), %ecx
	movss	-16(%rsi), %xmm0        # xmm0 = mem[0],zero,zero,zero
	movslq	%ecx, %rcx
	movss	%xmm0, (%rax,%rcx,4)
	movq	-40(%rbp), %rdi
	movq	-24(%rbp), %rsi
	movl	-44(%rbp), %edx
	movl	-4(%rbp), %ecx
	incl	%ecx
	movq	-32(%rbp), %r8
	movq	-16(%rbp), %r9
	callq	drawHelper@PLT
.LBB34_3:                               # %merge
	movq	%rbp, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end34:
	.size	drawHelper, .Lfunc_end34-drawHelper
	.cfi_endproc
                                        # -- End function
	.globl	drawPoints              # -- Begin function drawPoints
	.p2align	4, 0x90
	.type	drawPoints,@function
drawPoints:                             # @drawPoints
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$40, %rsp
	.cfi_offset %rbx, -56
	.cfi_offset %r12, -48
	.cfi_offset %r13, -40
	.cfi_offset %r14, -32
	.cfi_offset %r15, -24
	movq	%rdi, -80(%rbp)
	movq	%rsi, -72(%rbp)
	movl	8(%rdi), %ebx
	movl	%ebx, -52(%rbp)
	movl	$4, %edi
	callq	malloc@PLT
	movq	%rax, %r14
	movl	$0, (%rax)
	movl	$16, %edi
	callq	malloc@PLT
	movq	%r14, (%rax)
	movl	$1, 8(%rax)
	movl	$1, %r12d
	leal	(%rbx,%rbx), %r15d
	shll	$3, %ebx
	movl	%ebx, %edi
	callq	malloc@PLT
	movq	%rax, %rbx
	movl	$0, -48(%rbp)
	movl	$0, -44(%rbp)
	.p2align	4, 0x90
.LBB35_2:                               # %inner
                                        # =>This Inner Loop Header: Depth=1
	movslq	-44(%rbp), %rax
	movslq	-48(%rbp), %rcx
	movss	(%r14,%rax,4), %xmm0    # xmm0 = mem[0],zero,zero,zero
	movss	%xmm0, (%rbx,%rcx,4)
	incl	%ecx
	movl	%ecx, -48(%rbp)
	incl	%eax
	movl	%eax, -44(%rbp)
	cmpl	%r12d, %eax
	jl	.LBB35_2
# %bb.1:                                # %loop
                                        #   in Loop: Header=BB35_2 Depth=1
	movl	-48(%rbp), %eax
	movl	$0, -44(%rbp)
	cmpl	%r15d, %eax
	jl	.LBB35_2
# %bb.3:                                # %continue
	movl	$16, %edi
	callq	malloc@PLT
	movq	%rbx, (%rax)
	movl	%r15d, 8(%rax)
	movq	%rax, -64(%rbp)
	movq	%rsp, %r15
	addq	$-16, %r15
	movq	%r15, %rsp
	movl	-52(%rbp), %ebx
	movl	$4, %edi
	callq	malloc@PLT
	movq	%rax, %r14
	movl	$0, (%rax)
	movl	$16, %edi
	callq	malloc@PLT
	movq	%r14, (%rax)
	movl	$1, 8(%rax)
	movl	$1, %r13d
	leal	(,%rbx,4), %r12d
	shll	$4, %ebx
	movl	%ebx, %edi
	callq	malloc@PLT
	movq	%rax, %rbx
	movq	%rsp, %rcx
	leaq	-16(%rcx), %rax
	movq	%rax, %rsp
	movl	$0, -16(%rcx)
	movq	%rsp, %rdx
	leaq	-16(%rdx), %rcx
	movq	%rcx, %rsp
	movl	$0, -16(%rdx)
	.p2align	4, 0x90
.LBB35_5:                               # %inner44
                                        # =>This Inner Loop Header: Depth=1
	movslq	(%rcx), %rdx
	movslq	(%rax), %rsi
	movss	(%r14,%rdx,4), %xmm0    # xmm0 = mem[0],zero,zero,zero
	movss	%xmm0, (%rbx,%rsi,4)
	incl	%esi
	movl	%esi, (%rax)
	incl	%edx
	movl	%edx, (%rcx)
	cmpl	%r13d, %edx
	jl	.LBB35_5
# %bb.4:                                # %loop43
                                        #   in Loop: Header=BB35_5 Depth=1
	movl	(%rax), %edx
	movl	$0, (%rcx)
	cmpl	%r12d, %edx
	jl	.LBB35_5
# %bb.6:                                # %continue45
	movl	$16, %edi
	callq	malloc@PLT
	movq	%rbx, (%rax)
	movl	%r12d, 8(%rax)
	movq	%rax, (%r15)
	movq	-80(%rbp), %rdi
	movq	-72(%rbp), %rsi
	movl	-52(%rbp), %edx
	movq	-64(%rbp), %r8
	xorl	%ecx, %ecx
	movq	%rax, %r9
	callq	drawHelper@PLT
	movq	-64(%rbp), %rdi
	movq	(%r15), %rsi
	movl	$2, %edx
	callq	gl_drawPoint@PLT
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end35:
	.size	drawPoints, .Lfunc_end35-drawPoints
	.cfi_endproc
                                        # -- End function
	.globl	drawPath                # -- Begin function drawPath
	.p2align	4, 0x90
	.type	drawPath,@function
drawPath:                               # @drawPath
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$40, %rsp
	.cfi_offset %rbx, -56
	.cfi_offset %r12, -48
	.cfi_offset %r13, -40
	.cfi_offset %r14, -32
	.cfi_offset %r15, -24
	movq	%rdi, -80(%rbp)
	movq	%rsi, -72(%rbp)
	movl	%edx, -56(%rbp)
	movl	8(%rdi), %ebx
	movl	%ebx, -52(%rbp)
	movl	$4, %edi
	callq	malloc@PLT
	movq	%rax, %r14
	movl	$0, (%rax)
	movl	$16, %edi
	callq	malloc@PLT
	movq	%r14, (%rax)
	movl	$1, 8(%rax)
	movl	$1, %r12d
	leal	(%rbx,%rbx), %r15d
	shll	$3, %ebx
	movl	%ebx, %edi
	callq	malloc@PLT
	movq	%rax, %rbx
	movl	$0, -48(%rbp)
	movl	$0, -44(%rbp)
	.p2align	4, 0x90
.LBB36_2:                               # %inner
                                        # =>This Inner Loop Header: Depth=1
	movslq	-44(%rbp), %rax
	movslq	-48(%rbp), %rcx
	movss	(%r14,%rax,4), %xmm0    # xmm0 = mem[0],zero,zero,zero
	movss	%xmm0, (%rbx,%rcx,4)
	incl	%ecx
	movl	%ecx, -48(%rbp)
	incl	%eax
	movl	%eax, -44(%rbp)
	cmpl	%r12d, %eax
	jl	.LBB36_2
# %bb.1:                                # %loop
                                        #   in Loop: Header=BB36_2 Depth=1
	movl	-48(%rbp), %eax
	movl	$0, -44(%rbp)
	cmpl	%r15d, %eax
	jl	.LBB36_2
# %bb.3:                                # %continue
	movl	$16, %edi
	callq	malloc@PLT
	movq	%rbx, (%rax)
	movl	%r15d, 8(%rax)
	movq	%rax, -64(%rbp)
	movq	%rsp, %r15
	addq	$-16, %r15
	movq	%r15, %rsp
	movl	-52(%rbp), %ebx
	movl	$4, %edi
	callq	malloc@PLT
	movq	%rax, %r14
	movl	$0, (%rax)
	movl	$16, %edi
	callq	malloc@PLT
	movq	%r14, (%rax)
	movl	$1, 8(%rax)
	movl	$1, %r13d
	leal	(,%rbx,4), %r12d
	shll	$4, %ebx
	movl	%ebx, %edi
	callq	malloc@PLT
	movq	%rax, %rbx
	movq	%rsp, %rcx
	leaq	-16(%rcx), %rax
	movq	%rax, %rsp
	movl	$0, -16(%rcx)
	movq	%rsp, %rdx
	leaq	-16(%rdx), %rcx
	movq	%rcx, %rsp
	movl	$0, -16(%rdx)
	.p2align	4, 0x90
.LBB36_5:                               # %inner45
                                        # =>This Inner Loop Header: Depth=1
	movslq	(%rcx), %rdx
	movslq	(%rax), %rsi
	movss	(%r14,%rdx,4), %xmm0    # xmm0 = mem[0],zero,zero,zero
	movss	%xmm0, (%rbx,%rsi,4)
	incl	%esi
	movl	%esi, (%rax)
	incl	%edx
	movl	%edx, (%rcx)
	cmpl	%r13d, %edx
	jl	.LBB36_5
# %bb.4:                                # %loop44
                                        #   in Loop: Header=BB36_5 Depth=1
	movl	(%rax), %edx
	movl	$0, (%rcx)
	cmpl	%r12d, %edx
	jl	.LBB36_5
# %bb.6:                                # %continue46
	movl	$16, %edi
	callq	malloc@PLT
	movq	%rbx, (%rax)
	movl	%r12d, 8(%rax)
	movq	%rax, (%r15)
	movq	-80(%rbp), %rdi
	movq	-72(%rbp), %rsi
	movl	-52(%rbp), %edx
	movq	-64(%rbp), %r8
	xorl	%ecx, %ecx
	movq	%rax, %r9
	callq	drawHelper@PLT
	movq	-64(%rbp), %rdi
	movq	(%r15), %rsi
	movl	-56(%rbp), %edx
	callq	gl_drawCurve@PLT
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end36:
	.size	drawPath, .Lfunc_end36-drawPath
	.cfi_endproc
                                        # -- End function
	.globl	drawShape               # -- Begin function drawShape
	.p2align	4, 0x90
	.type	drawShape,@function
drawShape:                              # @drawShape
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$56, %rsp
	.cfi_offset %rbx, -56
	.cfi_offset %r12, -48
	.cfi_offset %r13, -40
	.cfi_offset %r14, -32
	.cfi_offset %r15, -24
	movq	%rdi, -88(%rbp)
	movq	%rsi, -80(%rbp)
	movl	%edx, -60(%rbp)
	movl	%ecx, -56(%rbp)
	movl	8(%rdi), %ebx
	movl	%ebx, -52(%rbp)
	movl	$4, %edi
	callq	malloc@PLT
	movq	%rax, %r14
	movl	$0, (%rax)
	movl	$16, %edi
	callq	malloc@PLT
	movq	%r14, (%rax)
	movl	$1, 8(%rax)
	movl	$1, %r12d
	leal	(%rbx,%rbx), %r15d
	shll	$3, %ebx
	movl	%ebx, %edi
	callq	malloc@PLT
	movq	%rax, %rbx
	movl	$0, -48(%rbp)
	movl	$0, -44(%rbp)
	.p2align	4, 0x90
.LBB37_2:                               # %inner
                                        # =>This Inner Loop Header: Depth=1
	movslq	-44(%rbp), %rax
	movslq	-48(%rbp), %rcx
	movss	(%r14,%rax,4), %xmm0    # xmm0 = mem[0],zero,zero,zero
	movss	%xmm0, (%rbx,%rcx,4)
	incl	%ecx
	movl	%ecx, -48(%rbp)
	incl	%eax
	movl	%eax, -44(%rbp)
	cmpl	%r12d, %eax
	jl	.LBB37_2
# %bb.1:                                # %loop
                                        #   in Loop: Header=BB37_2 Depth=1
	movl	-48(%rbp), %eax
	movl	$0, -44(%rbp)
	cmpl	%r15d, %eax
	jl	.LBB37_2
# %bb.3:                                # %continue
	movl	$16, %edi
	callq	malloc@PLT
	movq	%rbx, (%rax)
	movl	%r15d, 8(%rax)
	movq	%rax, -72(%rbp)
	movq	%rsp, %r15
	addq	$-16, %r15
	movq	%r15, %rsp
	movl	-52(%rbp), %ebx
	movl	$4, %edi
	callq	malloc@PLT
	movq	%rax, %r14
	movl	$0, (%rax)
	movl	$16, %edi
	callq	malloc@PLT
	movq	%r14, (%rax)
	movl	$1, 8(%rax)
	movl	$1, %r13d
	leal	(,%rbx,4), %r12d
	shll	$4, %ebx
	movl	%ebx, %edi
	callq	malloc@PLT
	movq	%rax, %rbx
	movq	%rsp, %rcx
	leaq	-16(%rcx), %rax
	movq	%rax, %rsp
	movl	$0, -16(%rcx)
	movq	%rsp, %rdx
	leaq	-16(%rdx), %rcx
	movq	%rcx, %rsp
	movl	$0, -16(%rdx)
	.p2align	4, 0x90
.LBB37_5:                               # %inner46
                                        # =>This Inner Loop Header: Depth=1
	movslq	(%rcx), %rdx
	movslq	(%rax), %rsi
	movss	(%r14,%rdx,4), %xmm0    # xmm0 = mem[0],zero,zero,zero
	movss	%xmm0, (%rbx,%rsi,4)
	incl	%esi
	movl	%esi, (%rax)
	incl	%edx
	movl	%edx, (%rcx)
	cmpl	%r13d, %edx
	jl	.LBB37_5
# %bb.4:                                # %loop45
                                        #   in Loop: Header=BB37_5 Depth=1
	movl	(%rax), %edx
	movl	$0, (%rcx)
	cmpl	%r12d, %edx
	jl	.LBB37_5
# %bb.6:                                # %continue47
	movl	$16, %edi
	callq	malloc@PLT
	movq	%rbx, (%rax)
	movl	%r12d, 8(%rax)
	movq	%rax, (%r15)
	movq	-88(%rbp), %rdi
	movq	-80(%rbp), %rsi
	movl	-52(%rbp), %edx
	movq	-72(%rbp), %r8
	xorl	%ecx, %ecx
	movq	%rax, %r9
	callq	drawHelper@PLT
	movq	-72(%rbp), %rdi
	movq	(%r15), %rsi
	movl	-60(%rbp), %edx
	movl	-56(%rbp), %ecx
	callq	gl_drawShape@PLT
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end37:
	.size	drawShape, .Lfunc_end37-drawShape
	.cfi_endproc
                                        # -- End function
	.globl	endCanvas               # -- Begin function endCanvas
	.p2align	4, 0x90
	.type	endCanvas,@function
endCanvas:                              # @endCanvas
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	%rdi, (%rsp)
	movl	(%rdi), %eax
	movl	4(%rdi), %esi
	movl	8(%rdi), %edx
	movl	%eax, %edi
	callq	gl_endRendering@PLT
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end38:
	.size	endCanvas, .Lfunc_end38-endCanvas
	.cfi_endproc
                                        # -- End function
	.type	.Lfmt,@object           # @fmt
	.section	.rodata.str1.1,"aMS",@progbits,1
.Lfmt:
	.asciz	"%d\n"
	.size	.Lfmt, 4

	.section	".note.GNU-stack","",@progbits
