	.text
	.align	2
	.globl main
main:
	li	$2, 0xC
	la	$3, T1
	sw	$2, 0($3)
	li	$2, 0x4
	la	$3, T1
	lw	$3, 0($3)
	sub	$2, $3, $2
	jr	$31
	li	$2, 0x0


	.data
	.align 0
T1:	.word 0

