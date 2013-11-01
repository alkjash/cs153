	.text
	.align	2
	.globl main
	jal main
FUNCf:
	add	$3, $30, $0
	add	$30, $29, $0
	li	$4, 0x14
	sub	$29, $29, $4
	addi	$29, $29, 0xFFFFFFFC
	sw	$3, 0($29)
	addi	$29, $29, 0xFFFFFFFC
	sw	$31, 0($29)
	addi	$3, $30, 0xFFFFFFEC
	lw	$3, 0($3)
	addi	$29, $29, 0xFFFFFFFC
	sw	$3, 0($29)
	addi	$3, $30, 0xFFFFFFEC
	lw	$3, 0($3)
	lw	$4, 0($29)
	addi	$29, $29, 0x4
	mul	$3, $4, $3
	addi	$29, $29, 0xFFFFFFFC
	sw	$3, 0($29)
	addi	$3, $30, 0xFFFFFFF0
	lw	$3, 0($3)
	addi	$29, $29, 0xFFFFFFFC
	sw	$3, 0($29)
	addi	$3, $30, 0xFFFFFFF0
	lw	$3, 0($3)
	lw	$4, 0($29)
	addi	$29, $29, 0x4
	mul	$3, $4, $3
	lw	$4, 0($29)
	addi	$29, $29, 0x4
	add	$3, $4, $3
	addi	$29, $29, 0xFFFFFFFC
	sw	$3, 0($29)
	addi	$3, $30, 0xFFFFFFF4
	lw	$3, 0($3)
	addi	$29, $29, 0xFFFFFFFC
	sw	$3, 0($29)
	addi	$3, $30, 0xFFFFFFF4
	lw	$3, 0($3)
	lw	$4, 0($29)
	addi	$29, $29, 0x4
	mul	$3, $4, $3
	lw	$4, 0($29)
	addi	$29, $29, 0x4
	add	$3, $4, $3
	addi	$29, $29, 0xFFFFFFFC
	sw	$3, 0($29)
	addi	$3, $30, 0xFFFFFFF8
	lw	$3, 0($3)
	addi	$29, $29, 0xFFFFFFFC
	sw	$3, 0($29)
	addi	$3, $30, 0xFFFFFFF8
	lw	$3, 0($3)
	lw	$4, 0($29)
	addi	$29, $29, 0x4
	mul	$3, $4, $3
	lw	$4, 0($29)
	addi	$29, $29, 0x4
	add	$3, $4, $3
	addi	$29, $29, 0xFFFFFFFC
	sw	$3, 0($29)
	addi	$3, $30, 0xFFFFFFFC
	lw	$3, 0($3)
	addi	$29, $29, 0xFFFFFFFC
	sw	$3, 0($29)
	addi	$3, $30, 0xFFFFFFFC
	lw	$3, 0($3)
	lw	$4, 0($29)
	addi	$29, $29, 0x4
	mul	$3, $4, $3
	lw	$4, 0($29)
	addi	$29, $29, 0x4
	add	$3, $4, $3
	add	$2, $3, $0
	add	$16, $3, $0
	j L1
L1:
	addi	$3, $30, 0xFFFFFFE4
	lw	$31, 0($3)
	add	$29, $30, $0
	addi	$3, $30, 0xFFFFFFE8
	lw	$30, 0($3)
	jr	$31
main:
	add	$3, $30, $0
	add	$30, $29, $0
	li	$4, 0x0
	sub	$29, $29, $4
	addi	$29, $29, 0xFFFFFFFC
	sw	$3, 0($29)
	addi	$29, $29, 0xFFFFFFFC
	sw	$31, 0($29)
	li	$3, 0x1
	addi	$29, $29, 0xFFFFFFFC
	sw	$3, 0($29)
	li	$3, 0x2
	addi	$29, $29, 0xFFFFFFFC
	sw	$3, 0($29)
	li	$3, 0x3
	addi	$29, $29, 0xFFFFFFFC
	sw	$3, 0($29)
	li	$3, 0x4
	addi	$29, $29, 0xFFFFFFFC
	sw	$3, 0($29)
	li	$3, 0x5
	addi	$29, $29, 0xFFFFFFFC
	sw	$3, 0($29)
	addi	$29, $29, 0x14
	jal FUNCf
	addi	$3, $2, 0x0
	addi	$29, $29, 0xFFFFFFFC
	sw	$3, 0($29)
	li	$3, 0x6
	addi	$29, $29, 0xFFFFFFFC
	sw	$3, 0($29)
	li	$3, 0x7
	addi	$29, $29, 0xFFFFFFFC
	sw	$3, 0($29)
	li	$3, 0x8
	addi	$29, $29, 0xFFFFFFFC
	sw	$3, 0($29)
	li	$3, 0x9
	addi	$29, $29, 0xFFFFFFFC
	sw	$3, 0($29)
	li	$3, 0xA
	addi	$29, $29, 0xFFFFFFFC
	sw	$3, 0($29)
	addi	$29, $29, 0x14
	jal FUNCf
	addi	$3, $2, 0x0
	lw	$4, 0($29)
	addi	$29, $29, 0x4
	add	$3, $4, $3
	add	$2, $3, $0
	add	$16, $3, $0
	j L2
L2:
	addi	$3, $30, 0xFFFFFFF8
	lw	$31, 0($3)
	add	$29, $30, $0
	addi	$3, $30, 0xFFFFFFFC
	lw	$30, 0($3)
	jr	$31


	.data
	.align 0

#
# below here is the print debugging support code
#
	
.data
_spaceString: .asciiz " "
_newlineString: .asciiz "\n"

.text
.globl printInt     # int reg -> unit
.globl printSpace   # unit    -> unit
.globl printNewline # unit    -> unit

printInt: # int reg->unit
	                  # The syscall takes its argument in $a0
   add $t0, $v0, $zero    # since this function does not return anything, it should probably preserve $v0
   li $v0, 1              # print_int syscall
   syscall
   add $v0, $t0, $zero    # restore $v0 
jr $ra


printSpace: # unit->unit
add $t0, $v0, $zero
la $a0, _spaceString      # address of string to print
li $v0, 4                 # system call code for print_str
syscall                   # print the string
add $v0, $t0, $zero
jr $ra

printNewline: # unit->unit
add $t0, $v0, $zero
la $a0, _newlineString    # address of string to print
li $v0, 4                 # system call code for print_str
syscall                   # print the string
add $v0, $t0, $zero
jr $ra
