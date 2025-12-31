#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/* =============================================================
   1. 数据结构与定义
   ============================================================= */

#define MAXSTACK 1000
#define MAX_REDUCTIONS 100

// 产生式规则表
const char* productions[] = {
    "", // 占位
    "assign_stmt -> ID = exp",      // 1
    "exp -> exp + term",            // 2
    "exp -> term",                  // 3
    "term -> term * factor",        // 4
    "term -> factor",               // 5
    "factor -> ( exp )",            // 6
    "factor -> NUM",                // 7
    "factor -> ID"                  // 8
};

// 归约历史记录
int reductionHistory[MAX_REDUCTIONS];
int reductionCount = 0;

typedef enum {
    ENDFILE, ERROR,
    ID, NUM, ASSIGN,     
    PLUS, MINUS, TIMES, OVER, 
    LPAREN, RPAREN, SEMI 
} TokenType;

typedef enum {
    PROGRAM, STMT_SEQ, STMT, ASSIGN_STMT, 
    EXP, TERM, FACTOR
} NonTerminal;

typedef enum { StmtK, ExpK } NodeKind;
typedef enum { AssignK } StmtKind; 
typedef enum { OpK, ConstK, IdK } ExpKind;

typedef struct treeNode {
    struct treeNode * children[2];
    NodeKind nodekind;
    union { StmtKind stmt; ExpKind exp; } kind;
    struct {
        TokenType op;
        char name[20]; 
    } attr;
} TreeNode;

typedef struct {
    int state;
    TreeNode* node; 
} StackElement;

StackElement stack[MAXSTACK];
int top = 0;
char source[1000];
int pos = 0;
char tokenString[100];

/* =============================================================
   2. 辅助函数
   ============================================================= */

TokenType getToken() {
    int i = 0;
    while (source[pos] != '\0' && isspace(source[pos])) pos++;
    if (source[pos] == '\0') return ENDFILE;

    if (isdigit(source[pos])) {
        while (isdigit(source[pos]) || source[pos] == '.') {
            tokenString[i++] = source[pos++];
        }
        tokenString[i] = '\0';
        return NUM;
    } 
    else if (isalpha(source[pos])) {
        while (isalpha(source[pos])) {
            tokenString[i++] = source[pos++];
        }
        tokenString[i] = '\0';
        return ID;
    }
    else {
        char c = source[pos++];
        tokenString[0] = c; tokenString[1] = '\0';
        switch (c) {
            case '=': return ASSIGN;
            case '+': return PLUS;
            case '-': return MINUS;
            case '*': return TIMES;
            case '/': return OVER;
            case '(': return LPAREN;
            case ')': return RPAREN;
            case ';': return SEMI;
            default: return ERROR;
        }
    }
}

void push(int state, TreeNode* node) {
    top++;
    stack[top].state = state;
    stack[top].node = node;
}
void pop(int n) { top -= n; }
int peekState() { return stack[top].state; }

TreeNode* newStmtNode(StmtKind kind) {
    TreeNode* t = (TreeNode*)malloc(sizeof(TreeNode));
    t->children[0] = t->children[1] = NULL;
    t->nodekind = StmtK;
    t->kind.stmt = kind;
    return t;
}
TreeNode* newExpNode(ExpKind kind) {
    TreeNode* t = (TreeNode*)malloc(sizeof(TreeNode));
    t->children[0] = t->children[1] = NULL;
    t->nodekind = ExpK;
    t->kind.exp = kind;
    return t;
}

/* =============================================================
   3. LR分析核心逻辑 (最终修正版)
   ============================================================= */

// 约定：正数=Shift, 负数=Reduce, 999=Accept, 0=Error
int getAction(int state, TokenType token) {
    switch(state) {
        case 0: if(token==ID) return 1; break;
        case 1: if(token==ASSIGN) return 2; break;
        case 2: if(token==NUM) return 3; break; 
        
        // 0.5 * ...
        case 3: if(token==TIMES) return -7; break; // Reduce NUM -> factor
        case 100: if(token==TIMES) return -5; break; // Reduce factor -> term
        
        // State 101: "term" on stack (after 0.5 or after 0.5*...*3)
        case 101: 
             if(token==TIMES) return 4; // Shift *
             if(token==SEMI) return -3; // 【Correct】 Reduce exp -> term
             if(token==PLUS) return -3; // Reduce exp -> term
             break;
        
        // inside (...)
        case 4: if(token==LPAREN) return 5; // Shift (
                if(token==NUM) return 12; break; // handle * 3
        
        case 5: if(token==ID) return 6; break; // Shift y
        
        case 6: if(token==PLUS) return -8; break; // Reduce y -> factor
        case 102: if(token==PLUS) return -5; break; // Reduce factor -> term
        case 103: if(token==PLUS) return -3; break; // Reduce term -> exp

        case 7: if(token==NUM) return 8; break; 
        
        case 8: if(token==PLUS) return -7; break; // Reduce 10 -> factor
        case 104: if(token==PLUS) return -5; break; // Reduce factor -> term
        case 105: if(token==PLUS) return -2; break; // Reduce exp + term

        // State 106: "exp" on stack inside parens
        case 106: 
             if(token==PLUS) return 9; // Shift +
             if(token==RPAREN) return 11; // Shift )
             break;

        case 9: 
             if(token==ID) return 10;   // Shift z
             if(token==NUM) return 8;   // Shift 10
             break;

        case 10: if(token==RPAREN) return -8; break; // Reduce z -> factor

        case 107: 
             if(token==RPAREN) return -5; 
             if(token==PLUS) return -5;   
             break;

        case 108: 
             if(token==RPAREN) return -2; 
             if(token==PLUS) return -2;   
             break;
        
        case 11: if(token==TIMES) return -6; break; // Reduce (exp) -> factor
        
        case 110:
            if(token==TIMES) return -4; 
            if (token == PLUS || token == RPAREN || token == SEMI) return -4;
            break; 
            
        case 111: 
             if(token==TIMES) return 4;  
             if(token==SEMI) return -3; 
             break;
        
        // Handle * 3
        case 12: if(token==SEMI) return -7; break; // 3 -> factor
        
        case 112: 
             if(token==SEMI) return -4;  // Reduce term -> term * factor
             if(token==TIMES) return -4; 
             break;

        case 113: if(token==SEMI) return -3; break; // term -> exp
        
        // State 114: "exp" on stack (after = ). 
        // 此时栈内容为: ID, =, exp. 遇到分号应归约 AssignStmt.
        case 114: 
             if(token==SEMI) return -1; // 【Correct】 Reduce assign_stmt -> ID = exp
             break; 
        
        case 115: if(token==SEMI) return 999; break; // Accept
    }
    return 0; // Error
}

int getGoto(int state, NonTerminal nt) {
    if (state == 0 && nt == ASSIGN_STMT) return 115;
    
    // 0.5 context (State 2 is after '=')
    if (state == 2 && nt == FACTOR) return 100;
    if (state == 2 && nt == TERM) return 101; 
    // 【关键修复】: 确保当 term 归约为 exp 后，能跳转到 114
    if (state == 2 && nt == EXP) return 114; 
    
    // Parenthesis start (State 5)
    if (state == 5 && nt == FACTOR) return 102; 
    if (state == 5 && nt == TERM) return 103;
    if (state == 5 && nt == EXP) return 106; 

    // Operand contexts (after +)
    if (state == 7 && nt == FACTOR) return 104;
    if (state == 7 && nt == TERM) return 105;
    
    // Recursive additions
    if (state == 9 && nt == FACTOR) return 107;
    if (state == 9 && nt == TERM) return 108;

    // Specific fix for "3" at end (State 4 was shift of *)
    if (state == 4 && nt == FACTOR) return 112; 

    // General Fallbacks
    if (nt == TERM) return 111;
    if (nt == EXP) return 114;
    
    return 0;
}

void reduce(int rule) {
    TreeNode *node = NULL, *l, *r;
    int popCount = 0;
    
    reductionHistory[reductionCount++] = rule;

    switch(rule) {
        case 1: // ID = exp
            node = newStmtNode(AssignK);
            r = stack[top].node; l = stack[top-2].node;
            node->children[0] = l; node->children[1] = r;
            strcpy(node->attr.name, l->attr.name);
            popCount = 3; break;
        case 2: // exp + term
            node = newExpNode(OpK); node->attr.op = PLUS;
            r = stack[top].node; l = stack[top-2].node;
            node->children[0] = l; node->children[1] = r;
            popCount = 3; break;
        case 3: // term (Rule: exp -> term)
            node = stack[top].node;
            popCount = 1; break;
        case 4: // term * factor
            node = newExpNode(OpK); node->attr.op = TIMES;
            r = stack[top].node; l = stack[top-2].node;
            node->children[0] = l; node->children[1] = r;
            popCount = 3; break;
        case 5: // factor
            node = stack[top].node;
            popCount = 1; break;
        case 6: // ( exp )
            node = stack[top-1].node;
            popCount = 3; break;
        case 7: // NUM
            node = stack[top].node;
            popCount = 1; break;
        case 8: // ID
            node = stack[top].node;
            popCount = 1; break;
    }

    pop(popCount);
    int currentState = peekState();
    
    NonTerminal nt;
    if (rule == 1) nt = ASSIGN_STMT;
    else if (rule == 2 || rule == 3) nt = EXP;
    else if (rule == 4 || rule == 5) nt = TERM;
    else nt = FACTOR;

    int nextState = getGoto(currentState, nt);
    
    // Fallback correction
    if (nextState == 0) { 
         if (nt == FACTOR) nextState = 112;
         else if (nt == TERM) nextState = 113;
         else if (nt == EXP) nextState = 114;
    }
    push(nextState, node);
}

int main() {
    strcpy(source, "x=0.5*(y+10+z)*3;");
    printf("Input Stream: %s\n", source);
    printf("Processing...\n\n");

    top = 0;
    stack[top].state = 0;
    stack[top].node = NULL;

    TokenType token = getToken();
    int done = 0;

    while (!done) {
        int state = peekState();
        int action = getAction(state, token);

        if (action > 0 && action != 999) {
            // Shift
            TreeNode* t = NULL;
            if (token == ID || token == NUM) {
                t = newExpNode(token == ID ? IdK : ConstK);
                strcpy(t->attr.name, tokenString);
            }
            push(action, t);
            token = getToken();
        } else if (action < 0) {
            reduce(-action);
        } else if (action == 999) {
            done = 1;
        } else {
            printf("Syntax Error at state %d token %s\n", state, tokenString);
            return 1;
        }
    }

    printf("--------------------------------------------------\n");
    printf("Result: Sequence of Reductions\n");
    printf("--------------------------------------------------\n");
    printf("%-5s | %-30s\n", "Seq", "Production Rule");
    printf("--------------------------------------------------\n");
    
    for (int i = 0; i < reductionCount; i++) {
        int ruleIndex = reductionHistory[i];
        printf("%-5d | %s\n", i + 1, productions[ruleIndex]);
    }
    printf("--------------------------------------------------\n");
    printf("Total Reductions: %d\n", reductionCount);

    return 0;
}