#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

// ==========================================
// 1. 定义数据结构
// ==========================================

// Token 类型定义
typedef enum {
    // 关键字
    IF, THEN, ELSE, END, REPEAT, UNTIL, READ, WRITE,
    // 类别
    ID, NUM,
    // 运算符
    PLUS, MINUS, TIMES, OVER,    // + - * /
    ASSIGN,                      // :=
    EQ, LT, GT, LTE, GTE, NEQ,   // = < > <= >= <>
    // 界符
    LPAREN, RPAREN, SEMI,        // ( ) ;
    // 结束与错误
    ENDFILE, ERROR
} TokenType;

// DFA 状态定义
typedef enum {
    START,
    IN_ID,
    IN_NUM,
    IN_ASSIGN,   // 读到了 :
    IN_LESS,     // 读到了 <
    IN_GREATER,  // 读到了 >
    IN_COMMENT,  // 读到了 {
    DONE         // 完成一个Token识别
} StateType;

// 全局变量
FILE *source;      // 输入源文件指针
FILE *outputFile;  // 输出结果文件指针
char tokenString[100]; 
int lineNo = 1;

// 关键字查找表
struct {
    char *str;
    TokenType tok;
} keywords[] = {
    {"if", IF}, {"then", THEN}, {"else", ELSE}, {"end", END},
    {"repeat", REPEAT}, {"until", UNTIL}, {"read", READ}, {"write", WRITE},
    {NULL, 0}
};

// ==========================================
// 2. 辅助函数
// ==========================================

// 查表判断是否为关键字
TokenType lookup(char *s) {
    for (int i = 0; keywords[i].str != NULL; i++) {
        if (strcmp(s, keywords[i].str) == 0)
            return keywords[i].tok;
    }
    return ID;
}

char getNextChar() {
    return fgetc(source);
}

void ungetNextChar(char c) {
    ungetc(c, source);
}


// 使用 sprintf 先格式化，然后同时写入屏幕和文件
void printToken(TokenType token, const char *tokenString) {
    char buffer[256]; // 临时缓冲区，用于存放一行输出信息

    switch (token) {
        case IF: case THEN: case ELSE: case END: 
        case REPEAT: case UNTIL: case READ: case WRITE:
            sprintf(buffer, "KEYWORD: %s\n", tokenString); break;
        case ASSIGN: sprintf(buffer, "ASSIGN: :=\n"); break;
        case LT: sprintf(buffer, "RELOP: <\n"); break;
        case GT: sprintf(buffer, "RELOP: >\n"); break;
        case EQ: sprintf(buffer, "RELOP: =\n"); break;
        case NEQ: sprintf(buffer, "RELOP: <>\n"); break;
        case LTE: sprintf(buffer, "RELOP: <=\n"); break;
        case GTE: sprintf(buffer, "RELOP: >=\n"); break;
        case LPAREN: sprintf(buffer, "SEMI: (\n"); break;
        case RPAREN: sprintf(buffer, "SEMI: )\n"); break;
        case SEMI: sprintf(buffer, "SEMI: ;\n"); break;
        case PLUS: sprintf(buffer, "OP: +\n"); break;
        case MINUS: sprintf(buffer, "OP: -\n"); break;
        case TIMES: sprintf(buffer, "OP: *\n"); break;
        case OVER: sprintf(buffer, "OP: /\n"); break;
        case NUM: sprintf(buffer, "NUM: %s\n", tokenString); break;
        case ID: sprintf(buffer, "ID: %s\n", tokenString); break;
        case ENDFILE: sprintf(buffer, "EOF\n"); break;
        case ERROR: sprintf(buffer, "ERROR: Unexpected character '%s' at line %d\n", tokenString, lineNo); break;
        default: sprintf(buffer, "UNKNOWN TOKEN\n"); break;
    }

    // 1. 输出到屏幕
    printf("%s", buffer);
    
    // 2. 输出到文件 (如果文件打开成功)
    if (outputFile != NULL) {
        fprintf(outputFile, "%s", buffer);
    }
}

// ==========================================
// 3. 核心算法：DFA 驱动
// ==========================================
TokenType getToken() {
    int tokenStringIndex = 0;
    TokenType currentToken;
    StateType state = START;
    int save; 

    while (state != DONE) {
        char c = getNextChar();
        save = 1;

        switch (state) {
            case START:
                if (isdigit(c)) state = IN_NUM;
                else if (isalpha(c)) state = IN_ID;
                else if (c == ':') state = IN_ASSIGN;
                else if (c == '<') state = IN_LESS;
                else if (c == '>') state = IN_GREATER;
                else if (c == ' ' || c == '\t' || c == '\r') save = 0;
                else if (c == '\n') { save = 0; lineNo++; }
                else if (c == '{') { save = 0; state = IN_COMMENT; }
                else {
                    state = DONE;
                    switch (c) {
                        case EOF: save = 0; currentToken = ENDFILE; break;
                        case '=': currentToken = EQ; break;
                        case '+': currentToken = PLUS; break;
                        case '-': currentToken = MINUS; break;
                        case '*': currentToken = TIMES; break;
                        case '/': currentToken = OVER; break;
                        case '(': currentToken = LPAREN; break;
                        case ')': currentToken = RPAREN; break;
                        case ';': currentToken = SEMI; break;
                        default: currentToken = ERROR; break;
                    }
                }
                break;

            case IN_COMMENT:
                save = 0;
                if (c == '}') state = START;
                else if (c == '\n') lineNo++;
                else if (c == EOF) { state = DONE; currentToken = ENDFILE; }
                break;

            case IN_ASSIGN:
                state = DONE;
                if (c == '=') currentToken = ASSIGN;
                else { ungetNextChar(c); save = 0; currentToken = ERROR; }
                break;
            
            case IN_LESS:
                state = DONE;
                if (c == '=') currentToken = LTE;
                else if (c == '>') currentToken = NEQ;
                else { ungetNextChar(c); save = 0; currentToken = LT; }
                break;

            case IN_GREATER:
                state = DONE;
                if (c == '=') currentToken = GTE;
                else { ungetNextChar(c); save = 0; currentToken = GT; }
                break;

            case IN_NUM:
                if (!isdigit(c) && c != '.') {
                    ungetNextChar(c); save = 0; state = DONE; currentToken = NUM;
                }
                break;

            case IN_ID:
                if (!isalnum(c)) {
                    ungetNextChar(c); save = 0; state = DONE; currentToken = ID;
                }
                break;

            case DONE: default:
                state = DONE; currentToken = ERROR; break;
        }

        if ((save) && (tokenStringIndex < 99)) tokenString[tokenStringIndex++] = c;
        if (state == DONE) {
            tokenString[tokenStringIndex] = '\0';
            if (currentToken == ID) currentToken = lookup(tokenString);
        }
    }
    return currentToken;
}

// ==========================================
// 4. 主程序
// ==========================================
int main() {
    // 1. 生成测试用的输入文件
    FILE *fp = fopen("test_code.txt", "w");
    if (fp) {
        fprintf(fp, "read x;\n");
        fprintf(fp, "if 0 < x then\n");
        fprintf(fp, "  fact := 1;\n");
        fprintf(fp, "  { Comment Ignored }\n");
        fprintf(fp, "  repeat fact := fact * x; until x = 0\n");
        fprintf(fp, "end");
        fclose(fp);
    }

    // 2. 打开输入文件
    source = fopen("test_code.txt", "r");
    if (source == NULL) {
        printf("Error: Could not open source file.\n");
        return 1;
    }

    // 3. 【修改】打开输出文件
    outputFile = fopen("output.txt", "w");
    if (outputFile == NULL) {
        printf("Error: Could not create output file.\n");
        // 即使输出文件创建失败，我们仍然继续运行，只是只输出到屏幕
    }

    printf("Analysis started. Results will be saved to 'output.txt'.\n");
    printf("=======================================================\n");

    TokenType token;
    while ((token = getToken()) != ENDFILE) {
        printToken(token, tokenString);
    }

    // 4. 清理资源
    fclose(source);
    if (outputFile != NULL) {
        fclose(outputFile);
        printf("=======================================================\n");
        printf("Analysis finished. Check 'output.txt' for results.\n");
    }

    return 0;
}