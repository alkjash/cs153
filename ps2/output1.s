	.text
	.align	2
	.globl main
main:
	li	$3, 0xC
	la	$4, T1
	sw	$3, 0($4)
	li	$3, 0x4
	la	$4, T1
	lw	$4, 0($4)
	add	$3, $4, $3
	seq	$2, $3, $3
	jr	$31


	.data
	.align 0
T1:	.word 0

