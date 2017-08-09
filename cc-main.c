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

// Tokens & classes, ordered by priority
enum {
    Num = 128, Fun, Sys, Glo, Loc, Id,
    Char, Else, Enum, If, Int, Return, Sizeof, While,
    Assign, Cond, Lor, Lan, Or, Xor, And, Eq, Ne, Lt, Gt, Le, Ge, Shl, Shr, Add, Sub, Mul, Div, Mod, Inc, Dec, Brak
};

// types of variable/function
enum {CHAR, INT, PTR};
int *idmain; // `main` function

// Symbol Table
struct identifier {
    int token; // token type of an identifier
    int hash; // hash value of the name of the identifier
    char * name; // name of identifier
    int class; // global, local or constants
    int type; // int, char or pointer.
    int value; // value of the variable the identifier points to

    // store global variables if blocked by local ones
    int Bclass;
    int Btype;
    int Bvalue;
}

int token_val; // value of current token
int *current_id; // current parsed ID
int *symbols; // symbol table

// identifier fields
enum {Token, Hash, Name, Type, Class, Value, BType, BClass, BValue, IdSize};

void next() {
    char *last_pos;
    int hash;
    while(token = *src) {
        ++src;

        // parse token
        if (token == '\n') {
            ++line;
        }
        // skip macros, not support
        else if (token == '#') {
            while(*src!=0 && *src!='\n') {
                src++;
            }
        }
        else if ((token >= 'a' && token <= 'z') || (token >= 'A' && token <= 'Z') || (token == '_')) {
            last_pos = src - 1;
            hash = token;
            while((*src >= 'a' && *src <='z') || (*src >= 'A' && *src <= 'Z') || (*src >= '0' && *src <= '9') || (*src == '_')) {
                hash = hash * 147 + *src;
                src++;
            }
            // look for existing identifier
            current_id = symbols;
            while (current_id[Token]) {
                if (current_id[Hash] == hash && !memcmp((char *)current_id[Name], last_pos, src - lastpos)) {
                    token = current_id[Token];
                    return;
                }
                current_id = current_id + IdSize;
            }
            // store new ID
            current_id[Name] = (int)last_pos;
            current_id[Hash] = hash;
            token = current_id[Token] = Id;
            return;
        }
        else if (token >= '0' && token <= '9') {
            // parse number, dec(123) hex(0x123) oct(017)
            token_val = token - '0';
            if (token_val > 0) {
                // dec, starts with [1-9]
                while (*src >= '0' && *src <= '9') {
                    token_val = token_val*10 + *src++ - '0';
                }
            } else {
                // starts with number 0
                if (*src == 'x' || *src == 'X') {
                    //hex
                    token = *++src;
                    while ((token >= '0' && token <= '9') || (token >= 'a' && token <= 'f') || (token >= 'A' && token <= 'F')) {
                        token_val = token_val * 16 + (token & 15) + (token >= 'A' ? 9 : 0);
                        token = *++src;
                    }
                } else {
                    // oct
                    while (*src >= '0' && *src <= '7') {
                        token_val = token_val*8 + *src++ - '0';
                    }
                }
            }
            token = Num;
            return;
        }
        else if (token == '"' || token == '\'') {
            // parse string literal, currently, the only supported escape
            // character is '\n', store the string literal into data segment.
            last_pos = data;
            while (*src != 0 && *src != token) {
                token_val = *src++;
                if (token_val == '\\') {
                    // escape character
                    token_val = *src++;
                    if (token_val == 'n') {
                        token_val = '\n';
                    }
                }
                if (token == '"') {
                    *data++ = token_val;
                }
            }

            src++;
            // if it is a single character, return Num token
            if (token == '"') {
                token_val = (int)last_pos;
            } else {
                token = Num;
            }
            return;
        }
        else if (token == '/') { // only support '//'
            if (*src == '/') {
                // skip comments
                while (*src != 0 && *src != '\n') {
                    ++src;
                }
            } else {
                // divide operator
                token = Div;
                return;
            }
        }
        else if (token == '=') {
            // parse '==' and '='
            if (*src == '=') {
                src ++;
                token = Eq;
            } else {
                token = Assign;
            }
            return;
        }
        else if (token == '+') {
            // parse '+' and '++'
            if (*src == '+') {
                src ++;
                token = Inc;
            } else {
                token = Add;
            }
            return;
        }
        else if (token == '-') {
            // parse '-' and '--'
            if (*src == '-') {
                src ++;
                token = Dec;
            } else {
                token = Sub;
            }
            return;
        }
        else if (token == '!') {
            // parse '!='
            if (*src == '=') {
                src++;
                token = Ne;
            }
            return;
        }
        else if (token == '<') {
            // parse '<=', '<<' or '<'
            if (*src == '=') {
                src ++;
                token = Le;
            } else if (*src == '<') {
                src ++;
                token = Shl;
            } else {
                token = Lt;
            }
            return;
        }
        else if (token == '>') {
            // parse '>=', '>>' or '>'
            if (*src == '=') {
                src ++;
                token = Ge;
            } else if (*src == '>') {
                src ++;
                token = Shr;
            } else {
                token = Gt;
            }
            return;
        }
        else if (token == '|') {
            // parse '|' or '||'
            if (*src == '|') {
                src ++;
                token = Lor;
            } else {
                token = Or;
            }
            return;
        }
        else if (token == '&') {
            // parse '&' and '&&'
            if (*src == '&') {
                src ++;
                token = Lan;
            } else {
                token = And;
            }
            return;
        }
        else if (token == '^') {
            token = Xor;
            return;
        }
        else if (token == '%') {
            token = Mod;
            return;
        }
        else if (token == '*') {
            token = Mul;
            return;
        }
        else if (token == '[') {
            token = Brak;
            return;
        }
        else if (token == '?') {
            token = Cond;
            return;
        }
        else if (token == '~' || token == ';' || token == '{' || token == '}' || token == '(' || token == ')' || token == ']' || token == ',' || token == ':') {
            // directly return the character as token;
            return;
        }
    }
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

int main(int argc, char *argv[]) {
    int i, fd;
    argc--; argv++;

    poolsize = 256 * 1024; // arbitrary
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

    src = "char else enum if int return sizeof while "
          "open read close printf malloc memset memcmp exit void main";
    
    i = Char;
    while (i <= While) {
        next();
        current_id[Token] = i++;
    }
    i = OPEN;
    while(i <= EXIT) {
        next();
        current_id[Class] = Sys;
        current_id[Type] = INT;
        current_id[Value] = i++;
    }

    next(); current_id[Token] = Char; // Type void
    next(); idmain = current_id; // track main
    
    program();
    return eval();
}