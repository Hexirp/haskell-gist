[1 of 1] Compiling Main             ( Unsafe.hs, Unsafe.o )

==================== Asm code ====================
.section .data
.align 8
.align 1
.globl Main.bool_var1_closure
Main.bool_var1_closure:
	.quad	GHC.Types.True_con_info



==================== Asm code ====================
.section .rdata
.align 1
.align 1
.globl Main.$trModule4_bytes
Main.$trModule4_bytes:
	.asciz "main"



==================== Asm code ====================
.section .data
.align 8
.align 1
.globl Main.$trModule3_closure
Main.$trModule3_closure:
	.quad	GHC.Types.TrNameS_con_info
	.quad	Main.$trModule4_bytes



==================== Asm code ====================
.section .rdata
.align 1
.align 1
.globl Main.$trModule2_bytes
Main.$trModule2_bytes:
	.asciz "Main"



==================== Asm code ====================
.section .data
.align 8
.align 1
.globl Main.$trModule1_closure
Main.$trModule1_closure:
	.quad	GHC.Types.TrNameS_con_info
	.quad	Main.$trModule2_bytes



==================== Asm code ====================
.section .data
.align 8
.align 1
.globl Main.$trModule_closure
Main.$trModule_closure:
	.quad	GHC.Types.Module_con_info
	.quad	Main.$trModule3_closure+1
	.quad	Main.$trModule1_closure+1
	.quad	3



==================== Asm code ====================
.section .data
.align 8
.align 1
.globl Main.main2_closure
Main.main2_closure:
	.quad	Main.main2_info
	.quad	0
	.quad	0
	.quad	0



==================== Asm code ====================
.section .text
.align 8
.align 8
	.quad	0
	.quad	21
.globl Main.main2_info
Main.main2_info:
_c2B6:
	leaq -24(%rbp),%rax
	cmpq %r15,%rax
	jb _c2Be
_c2Bf:
	subq $8,%rsp
	movq %r13,%rcx
	movq %rbx,%rdx
	subq $32,%rsp
	xorl %eax,%eax
	call newCAF
	addq $40,%rsp
	testq %rax,%rax
	je _c2B4
_c2B3:
	movq $stg_bh_upd_frame_info,-16(%rbp)
	movq %rax,-8(%rbp)
	movq Main.bool_var1_closure+9,%rax
	movq $block_c2Bb_info,-24(%rbp)
	movl $GHC.Types.[]_closure+1,%edi
	movq %rax,%rsi
	xorl %r14d,%r14d
	addq $-24,%rbp
	jmp GHC.Show.$wshowSignedInt_info
_c2Bj:
	movq $24,904(%r13)
	jmp stg_gc_pp
.align 8
	.quad	0
	.quad	30
block_c2Bb_info:
_c2Bb:
	addq $24,%r12
	cmpq 856(%r13),%r12
	ja _c2Bj
_c2Bi:
	movq $:_con_info,-16(%r12)
	movq %rbx,-8(%r12)
	movq %r14,(%r12)
	leaq -14(%r12),%rbx
	addq $8,%rbp
	jmp *(%rbp)
_c2Be:
	jmp *-16(%r13)
_c2B4:
	jmp *(%rbx)



==================== Asm code ====================
.section .data
.align 8
.align 1
.globl Main.main1_closure
Main.main1_closure:
	.quad	Main.main1_info
	.quad	0



==================== Asm code ====================
.section .text
.align 8
.align 8
	.long	S2Bv_srt-(Main.main1_info)+0
	.long	0
	.quad	4294967299
	.quad	0
	.quad	30064771086
.globl Main.main1_info
Main.main1_info:
_c2Bs:
	movl $GHC.Types.True_closure+2,%edi
	movl $Main.main2_closure,%esi
	movl $GHC.IO.Handle.FD.stdout_closure,%r14d
	jmp GHC.IO.Handle.Text.hPutStr2_info



==================== Asm code ====================
.section .data
.align 8
.align 1
.globl Main.main_closure
Main.main_closure:
	.quad	Main.main_info
	.quad	0



==================== Asm code ====================
.section .text
.align 8
.align 8
	.long	S2Bv_srt-(Main.main_info)+24
	.long	0
	.quad	4294967299
	.quad	0
	.quad	4294967310
.globl Main.main_info
Main.main_info:
_c2BD:
	jmp Main.main1_info



==================== Asm code ====================
.section .data
.align 8
.align 1
.globl Main.main3_closure
Main.main3_closure:
	.quad	Main.main3_info
	.quad	0



==================== Asm code ====================
.section .text
.align 8
.align 8
	.long	S2Bv_srt-(Main.main3_info)+24
	.long	0
	.quad	4294967299
	.quad	0
	.quad	12884901902
.globl Main.main3_info
Main.main3_info:
_c2BN:
	movl $Main.main1_closure+1,%r14d
	jmp GHC.TopHandler.runMainIO1_info



==================== Asm code ====================
.section .data
.align 8
.align 1
.globl :Main.main_closure
:Main.main_closure:
	.quad	:Main.main_info
	.quad	0



==================== Asm code ====================
.section .text
.align 8
.align 8
	.long	S2Bv_srt-(:Main.main_info)+40
	.long	0
	.quad	4294967299
	.quad	0
	.quad	4294967310
.globl :Main.main_info
:Main.main_info:
_c2BX:
	jmp Main.main3_info



==================== Asm code ====================
.section .rdata$rel.ro
.align 8
.align 1
S2Bv_srt:
	.quad	GHC.IO.Handle.Text.hPutStr2_closure
	.quad	GHC.IO.Handle.FD.stdout_closure
	.quad	Main.main2_closure
	.quad	Main.main1_closure
	.quad	GHC.TopHandler.runMainIO1_closure
	.quad	Main.main3_closure


Linking Unsafe.exe ...
