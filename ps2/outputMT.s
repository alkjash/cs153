	.text
	.align	2
	.globl main
main:
	la	$3, MOOi
	lw	$3, 0($3)
	la	$4, T11
	sw	$3, 0($4)
	li	$3, 0x0
	la	$4, T11
	lw	$4, 0($4)
	slt	$3, $4, $3
	beq	$3, $0, L1
	li	$3, 0x0
	la	$4, MOOj
	sw	$3, 0($4)
	j L5
L6:
	li	$3, 0xC
	la	$4, T10
	sw	$3, 0($4)
	la	$3, MOOj
	lw	$3, 0($3)
	la	$4, T10
	lw	$4, 0($4)
	mul	$3, $4, $3
	la	$4, T9
	sw	$3, 0($4)
	li	$3, 0x1
	la	$4, T9
	lw	$4, 0($4)
	add	$3, $4, $3
	la	$4, MOOk
	sw	$3, 0($4)
	la	$3, MOOj
	lw	$3, 0($3)
	la	$4, T8
	sw	$3, 0($4)
	li	$3, 0x1
	la	$4, T8
	lw	$4, 0($4)
	sub	$3, $4, $3
	la	$4, MOOj
	sw	$3, 0($4)
L5:
	la	$3, MOOj
	lw	$3, 0($3)
	la	$4, T7
	sw	$3, 0($4)
	la	$3, MOOi
	lw	$3, 0($3)
	la	$4, T7
	lw	$4, 0($4)
	sgt	$3, $4, $3
	bne	$3, $0, L6
	li	$3, 0x0
	j L2
L1:
	li	$3, 0xC
	la	$4, T4
	sw	$3, 0($4)
	la	$3, MOOi
	lw	$3, 0($3)
	la	$4, T4
	lw	$4, 0($4)
	mul	$3, $4, $3
	la	$4, T3
	sw	$3, 0($4)
	li	$3, 0x1
	la	$4, T3
	lw	$4, 0($4)
	add	$3, $4, $3
	la	$4, MOOk
	sw	$3, 0($4)
	li	$3, 0x0
L2:
	li	$3, 0x0


	.data
	.align 0
MOOi:	.word 0
MOOj:	.word 0
MOOk:	.word 0
T10:	.word 0
T11:	.word 0
T3:	.word 0
T4:	.word 0
T7:	.word 0
T8:	.word 0
T9:	.word 0

