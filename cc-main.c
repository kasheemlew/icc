#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <string.h>

int token;
char *src, *old_src;
int poolsize;
int line;

// memory segments
int *text; // memory segment for storing code
int *old_text;
int *stack; // memory segment for handling the states of function calls
char *data; // combine memory segments data(storing initilized data) & bss(storing un-initilized data)

// VM registers
int *pc, *bp, *sp, ax, cycle;

// instructions set
enum {LEA, IMM, JMP, CALL, JZ, JNZ, ENT, ADJ, LEV, LI, LC, SI, SC, PUSH, OR, XOR, AND, EQ, NE, LT, GT, LE, GE, SHL, SHR, ADD, SUB, MUL, DIV, MOD, OPEN, READ, CLOS, PRTF, MALC, MSET, MCMP, EXIT};

void next() {
    token = *src++;
    return;
}

void expression(int level) {
}

void program() {
    // main entrance for parser
    next();
    while(token > 0) {
        printf("token is: %c\n", token);
        next();
    }
}

int eval() {
    // entrance for virtual machine; to intercept target instruction
    int op, *tmp;
    while (1) {
        if (op == IMM) { ax = *pc++; } // load immediate value to AX
        else if (op == LC) { ax = *(char *)ax; } // load character to AX, address in AX
        else if (op == LI) { ax = *(int *)ax; } // load integer to AX, address in AX
        else if (op == SC) { *(char *)*sp++ = ax; } // save character to address, value in AX, address on stack
        else if (op == SI) { *(int *)*sp++ = ax; } // save integer to address, value in AX, address on stack
        else if (op == PUSH) { *--sp = ax; } // push the value of AX onto the stack
        else if (op == JMP) { pc = (int *)*pc; } // jump to the address, PC points to the next instruction to be executed

        // conditional jump
        else if (op == JZ) { pc = ax ? pc+1 : (int *)*pc; } // jump if AX is 0
        else if (op == JNZ) { pc = ax ? (int *)*pc : pc + 1; } // jump if AX is not 0
        else if (op == CALL) { *--sp = (int)(pc + 1); pc = (int *)*pc; } // call subroutine
        // else if (op == RET) { pc = (int *)*sp++; } // return from subroutine. to be replaced by LEV
        else if (op == ENT) { *--sp = (int)bp; bp = sp; sp = sp - *pc++; } // make new calling frame
        else if (op == ADJ) { sp = sp + *pc++; } // remove arguments from frame
        else if (op == LEV) { sp = bp; bp = (int *)*sp++; pc = (int *)*sp++; } // restore call frame
        else if (op == LEA) { ax = (int)(bp + *pc++); } // load address for arguments in sub function

        // mathematical instructions
        // two arguments are stored in AX and top of stack
        else if (op == OR)  ax = *sp++ | ax;
        else if (op == XOR) ax = *sp++ ^ ax;
        else if (op == AND) ax = *sp++ & ax;
        else if (op == EQ)  ax = *sp++ == ax;
        else if (op == NE)  ax = *sp++ != ax;
        else if (op == LT)  ax = *sp++ < ax;
        else if (op == LE)  ax = *sp++ <= ax;
        else if (op == GT)  ax = *sp++ >  ax;
        else if (op == GE)  ax = *sp++ >= ax;
        else if (op == SHL) ax = *sp++ << ax;
        else if (op == SHR) ax = *sp++ >> ax;
        else if (op == ADD) ax = *sp++ + ax;
        else if (op == SUB) ax = *sp++ - ax;
        else if (op == MUL) ax = *sp++ * ax;
        else if (op == DIV) ax = *sp++ / ax;
        else if (op == MOD) ax = *sp++ % ax;

        // built-in functions
        else if (op == EXIT) { printf("exit(%d)", *sp); return *sp;}
        else if (op == OPEN) { ax = open((char *)sp[1], sp[0]); }
        else if (op == CLOS) { ax = close(*sp);}
        else if (op == READ) { ax = read(sp[2], (char *)sp[1], *sp); }
        else if (op == PRTF) { tmp = sp + pc[1]; ax = printf((char *)tmp[-1], tmp[-2], tmp[-3], tmp[-4], tmp[-5], tmp[-6]); }
        else if (op == MALC) { ax = (int)malloc(*sp);}
        else if (op == MSET) { ax = (int)memset((char *)sp[2], sp[1], *sp);}
        else if (op == MCMP) { ax = memcmp((char *)sp[2], (char *)sp[1], *sp);}

        // error handling
        else {
            printf("unknown instruction: %d\n", op);
            return -1;
        }
    }
    return 0;
}

int main(int argc, char *argv) {
    int i, fd;
    argc--; argv++;

    poolsize = 256 * 1024 // arbitrary
    line = 1;

    if ((fd = open(*argv, 0)) < 0) {
        printf("could not open (%s)\n", *argv);
        return -1;
    }

    if (!(src = old_src = malloc(poolsize))) {
        printf("could not malloc (%d) for souce area\n", poolsize);
    }

    src[i] = 0; // EOF character
    close(fd);

    // allocate memory for VM
    if (!(text = old_text = malloc(poolsize))) {
        printf("could not malloc (%d) for text area\n", poolsize);
        return -1;
    }
    if (!(data = malloc(poolsize))) {
        printf("could not malloc (%d) for data area\n", poolsize);
        return -1;
    }
    if (!(stack = malloc(poolsize))) {
        printf("could not malloc (%d) for stack area\n", poolsize);
        return -1;
    }
    memset(text, 0, poolsize);
    memset(data, 0, poolsize);
    memset(stack, 0, poolsize);

    // VM registers initialization
    bp = sp = (int *)((int)stack + poolsize);
    ax = 0;

    program();
    return eval();
}