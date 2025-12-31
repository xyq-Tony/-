

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h> // 用于可变参数的日志输出

/* =========================================
   1. 定义与全局变量
   ========================================= */

// 定义所有可能的 Token 类型
typedef enum {
    TOK_ID,         // 标识符 (x, y, count)
    TOK_NUM,        // 数字 (10, 0.5)
    TOK_ASSIGN,     // =
    TOK_SEMI,       // ;
    TOK_LPAREN,     // (
    TOK_RPAREN,     // )
    TOK_LBRACE,     // {
    TOK_RBRACE,     // }
    TOK_PLUS,       // +
    TOK_MULT,       // *
    TOK_IF,         // if 关键字
    TOK_ELSE,       // else 关键字
    TOK_WHILE,      // while 关键字
    TOK_EOF,        // 输入结束
    TOK_ERROR       // 词法错误
} TokenType;

// Token 结构体
typedef struct {
    TokenType type;
    char str[100]; // 存储 Token 的原始字符串值
} Token;

// 全局变量
char inputBuffer[2048]; // 输入字符流缓冲区
int pos = 0;            // 当前字符流读取位置
Token currentToken;     // 当前正在分析的 Token
FILE *fileOut = NULL;   // 输出文件指针

/* =========================================
   2. 函数声明
   ========================================= */

// 工具函数
void logOutput(const char *format, ...); // 同时输出到屏幕和文件
void getNextToken();                     // 词法分析器 (Scanner)
void match(TokenType expected);          // 匹配终结符
void error(const char *msg);             // 报错处理

// 语法分析函数 (对应每个非终结符)
void parseProgram();    // 起始符号
void parseStmt();       // 语句 (Statement)
void parseBlock();      // 代码块 (Block)
void parseStmts();      // 语句列表
void parseAssignStmt(); // 赋值语句
void parseIfStmt();     // 条件语句
void parseElsePart();   // else 部分 (提取左因子后)
void parseWhileStmt();  // 循环语句

// 表达式分析 (消除左递归后的文法)
void parseE();  // E -> T E'
void parseE_(); // E' -> + T E' | epsilon
void parseT();  // T -> F T'
void parseT_(); // T' -> * F T' | epsilon
void parseF();  // F -> ( E ) | id | num

/* =========================================
   3. 主函数 (入口)
   ========================================= */
int main() {
    // 1. 打开输出文件
    fileOut = fopen("output.txt", "w");
    if (fileOut == NULL) {
        printf("Error: Could not create output.txt\n");
        return 1;
    }

    // 2. 准备输入数据 (模拟字符流)
    // 你可以在这里修改测试用例
    // 测试用例涵盖: 赋值, 算术运算, 括号, 循环结构
    strcpy(inputBuffer, "x = 0.5 * (y + 10 + z) * 3; while(x) { x = x + 1; }");
    
    logOutput("Lab2 LL(1) Syntax Parser\n");
    logOutput("----------------------------------------\n");
    logOutput("Input Stream: %s\n", inputBuffer);
    logOutput("----------------------------------------\n");
    logOutput("Sequence of Derivations (Syntax Tree Construction):\n\n");

    // 3. 初始化并启动分析
    pos = 0;
    getNextToken(); // 预读第一个 Token
    parseProgram(); // 进入递归下降分析

    // 4. 结束检查
    if (currentToken.type == TOK_EOF) {
        logOutput("\n----------------------------------------\n");
        logOutput("Result: SUCCESS. Syntax tree built successfully.\n");
        logOutput("Output saved to 'output.txt'.\n");
    } else {
        error("Unexpected characters at the end of input.");
    }

    fclose(fileOut);
    return 0;
}

/* =========================================
   4. 工具函数实现
   ========================================= */

// 双路输出日志函数 (屏幕 + 文件)
void logOutput(const char *format, ...) {
    va_list args;
    
    // 输出到控制台
    va_start(args, format);
    vprintf(format, args);
    va_end(args);

    // 输出到文件
    if (fileOut != NULL) {
        va_start(args, format);
        vfprintf(fileOut, format, args);
        va_end(args);
    }
}

// 报错函数
void error(const char *msg) {
    logOutput("\n[Syntax Error] %s (Current Token: '%s')\n", msg, currentToken.str);
    if (fileOut) fclose(fileOut);
    exit(1);
}

// 终结符匹配函数
void match(TokenType expected) {
    if (currentToken.type == expected) {
        getNextToken(); // 匹配成功，读取下一个 Token
    } else {
        char errBuf[100];
        sprintf(errBuf, "Expected token type %d but found '%s'", expected, currentToken.str);
        error(errBuf);
    }
}

/* =========================================
   5. 词法分析器 (Lexer)
   ========================================= */
void getNextToken() {
    // 1. 跳过空白字符 (空格, Tab, 换行)
    while (inputBuffer[pos] == ' ' || inputBuffer[pos] == '\t' || 
           inputBuffer[pos] == '\n' || inputBuffer[pos] == '\r') {
        pos++;
    }

    // 2. 判断字符串结束
    if (inputBuffer[pos] == '\0') {
        currentToken.type = TOK_EOF;
        strcpy(currentToken.str, "EOF");
        return;
    }

    char c = inputBuffer[pos];

    // 3. 识别标识符 (Identifier) 或 关键字 (Keywords)
    if (isalpha(c) || c == '_') {
        int i = 0;
        while (isalnum(inputBuffer[pos]) || inputBuffer[pos] == '_') {
            currentToken.str[i++] = inputBuffer[pos++];
        }
        currentToken.str[i] = '\0';

        // 检查是否是保留字
        if (strcmp(currentToken.str, "if") == 0) currentToken.type = TOK_IF;
        else if (strcmp(currentToken.str, "else") == 0) currentToken.type = TOK_ELSE;
        else if (strcmp(currentToken.str, "while") == 0) currentToken.type = TOK_WHILE;
        else currentToken.type = TOK_ID;
        return;
    }

    // 4. 识别数字 (Numbers, 支持小数)
    if (isdigit(c)) {
        int i = 0;
        while (isdigit(inputBuffer[pos]) || inputBuffer[pos] == '.') {
            currentToken.str[i++] = inputBuffer[pos++];
        }
        currentToken.str[i] = '\0';
        currentToken.type = TOK_NUM;
        return;
    }

    // 5. 识别运算符和界符
    // 每次处理完后 pos++ 移动指针
    currentToken.str[0] = c;
    currentToken.str[1] = '\0';
    pos++;

    switch(c) {
        case '=': currentToken.type = TOK_ASSIGN; break;
        case ';': currentToken.type = TOK_SEMI; break;
        case '+': currentToken.type = TOK_PLUS; break;
        case '*': currentToken.type = TOK_MULT; break;
        case '(': currentToken.type = TOK_LPAREN; break;
        case ')': currentToken.type = TOK_RPAREN; break;
        case '{': currentToken.type = TOK_LBRACE; break;
        case '}': currentToken.type = TOK_RBRACE; break;
        default:  currentToken.type = TOK_ERROR; break;
    }
}

/* =========================================
   6. 语法分析器 (LL(1) Logic)
   ========================================= */

// Grammar: Program -> Stmts
void parseProgram() {
    logOutput("Program -> Stmts\n");
    parseStmts(); // 解析语句列表
}

// Grammar: Stmts -> Stmt Stmts | epsilon
// 解释: 如果当前 token 属于 First(Stmt)，则解析 Stmt；否则如果是 '}' 或 EOF，则推导为空。
void parseStmts() {
    // First(Stmt) = { id, if, while, { }
    if (currentToken.type == TOK_ID || currentToken.type == TOK_IF || 
        currentToken.type == TOK_WHILE || currentToken.type == TOK_LBRACE) {
        logOutput("Stmts -> Stmt Stmts\n");
        parseStmt();
        parseStmts();
    } else {
        // Follow(Stmts) = { '}', EOF }
        logOutput("Stmts -> epsilon\n");
    }
}

// Grammar: Stmt -> Block | AssignStmt | IfStmt | WhileStmt
void parseStmt() {
    if (currentToken.type == TOK_LBRACE) {
        logOutput("Stmt -> Block\n");
        parseBlock();
    } 
    else if (currentToken.type == TOK_ID) {
        logOutput("Stmt -> AssignStmt\n");
        parseAssignStmt();
    }
    else if (currentToken.type == TOK_IF) {
        logOutput("Stmt -> IfStmt\n");
        parseIfStmt();
    }
    else if (currentToken.type == TOK_WHILE) {
        logOutput("Stmt -> WhileStmt\n");
        parseWhileStmt();
    }
    else {
        error("Expected start of a statement (id, if, while, {)");
    }
}

// Grammar: Block -> { Stmts }
void parseBlock() {
    logOutput("Block -> { Stmts }\n");
    match(TOK_LBRACE);
    parseStmts();
    match(TOK_RBRACE);
}

// Grammar: AssignStmt -> id = E ;
void parseAssignStmt() {
    logOutput("AssignStmt -> id = E ;\n");
    match(TOK_ID);     // 匹配 id
    match(TOK_ASSIGN); // 匹配 =
    parseE();          // 解析表达式
    match(TOK_SEMI);   // 匹配 ;
}

// Grammar: WhileStmt -> while ( E ) Stmt
void parseWhileStmt() {
    logOutput("WhileStmt -> while ( E ) Stmt\n");
    match(TOK_WHILE);
    match(TOK_LPAREN);
    parseE();
    match(TOK_RPAREN);
    parseStmt(); // 循环体
}

// Grammar: IfStmt -> if ( E ) Stmt ElsePart
// 注意: 提取左因子，处理 else
void parseIfStmt() {
    logOutput("IfStmt -> if ( E ) Stmt ElsePart\n");
    match(TOK_IF);
    match(TOK_LPAREN);
    parseE();
    match(TOK_RPAREN);
    parseStmt();     // if 的主体
    parseElsePart(); // 处理可能的 else
}

// Grammar: ElsePart -> else Stmt | epsilon
void parseElsePart() {
    if (currentToken.type == TOK_ELSE) {
        logOutput("ElsePart -> else Stmt\n");
        match(TOK_ELSE);
        parseStmt();
    } else {
        // 如果不是 else，推导为空 (epsilon)
        logOutput("ElsePart -> epsilon\n");
    }
}

/* --- 算术表达式处理 (消除左递归) --- */

// Grammar: E -> T E'
void parseE() {
    logOutput("E -> T E'\n");
    parseT();
    parseE_();
}

// Grammar: E' -> + T E' | epsilon
void parseE_() {
    if (currentToken.type == TOK_PLUS) {
        logOutput("E' -> + T E'\n");
        match(TOK_PLUS);
        parseT();
        parseE_();
    } else {
        // Follow(E') 包括 ), ;
        logOutput("E' -> epsilon\n");
    }
}

// Grammar: T -> F T'
void parseT() {
    logOutput("T -> F T'\n");
    parseF();
    parseT_();
}

// Grammar: T' -> * F T' | epsilon
void parseT_() {
    if (currentToken.type == TOK_MULT) {
        logOutput("T' -> * F T'\n");
        match(TOK_MULT);
        parseF();
        parseT_();
    } else {
        // Follow(T') 包括 +, ), ;
        logOutput("T' -> epsilon\n");
    }
}

// Grammar: F -> ( E ) | id | num
void parseF() {
    if (currentToken.type == TOK_LPAREN) {
        logOutput("F -> ( E )\n");
        match(TOK_LPAREN);
        parseE();
        match(TOK_RPAREN);
    } else if (currentToken.type == TOK_ID) {
        logOutput("F -> id (%s)\n", currentToken.str);
        match(TOK_ID);
    } else if (currentToken.type == TOK_NUM) {
        logOutput("F -> num (%s)\n", currentToken.str);
        match(TOK_NUM);
    } else {
        error("Invalid Factor. Expected '(', identifier, or number.");
    }
}