/*	Definition section */
%{
    #include "common.h" //Extern variables that communicate with lex
    // #define YYDEBUG 1
    // int yydebug = 1;

    extern int yylineno;
    extern int yylex();
    extern FILE *yyin;

    void yyerror()
    {
        printf("error:%d:" , yylineno);
    }

    /* Symbol table function - you can add new function if needed. */
    static void create_symbol();
    static void insert_symbol();
    static void lookup_symbol();
    static void dump_symbol();
    struct table {
        struct table *next;
        int level;
        char *name;
        char *type;
        int address;
        int linenum;
        char *eletype;
    };
    struct table *table , *head , *find;
    int nowlevel , nowaddr , levelnum; //levelnum means how many element in this level
    struct table *get();
    int getlevelnum();
    char *printtype;
    char *changetype;
    char *s1 , *s2 , *datatype;
    FILE *fp;
    void invalidoper(char *op)
    {
        if(strcmp(s1 , s2))
        {
            yyerror();
            printf(" invalid operation: %s (mismatched types %s and %s)\n", op , s1 , s2);
        }
    }
    int temp;
    int HAS_ERROR = 0;
    struct Stack {
        char *name;//store variable name
        char *type;//store variable type
        int intnum;//store integer number or boolean
        float floatnum;//store floating number
        char *content;//store string
        int status;//-1 means pop out
    };
    struct Stack stack[100];
    int current;//current stack
    int findstack(char *name);//find variable and load which stack position
    char *jump = "label";//for jump label name
    int label;  // distinguish jump label number(for some jump and if instruction)
    char *leftvar;//store left variable name
    void printassign();
%} 

%error-verbose

/* Use variable or self-defined structure to represent
 * nonterminal and token type
 */
%union {
    int i_val;
    float f_val;
    char *s_val;
    /* ... */
}

/* Token without return */
%token VAR
%token INT FLOAT BOOL STRING TRUE FALSE
%token INC DEC
%token GEQ LEQ EQL NEQ
%token ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN QUO_ASSIGN REM_ASSIGN
%token LAND LOR
%token LINE PRINT PRINTLN IF ELSE FOR 
%token ID

/* Token with return, which need to sepcify type */
%token <i_val> INT_LIT
%token <f_val> FLOAT_LIT
%token <s_val> STRING_LIT

/* Nonterminal with return, which need to sepcify type */
%type <s_val> Type TypeName ArrayType

/* Yacc will start at this nonterminal */
%start Program
%left LOR
%left LAND
%left '<' '>' GEQ LEQ EQL NEQ
%left '+' '-'
%left '*' '/' '%'
%left '[' ']'

/* Grammar section */
%%
Program
    : StatementList { datatype = ""; }
;

StatementList
    : StatementList { datatype = ""; } Statement
    | Statement 
;

Statement
    : SimpleStmt LINE
    | Block LINE
    | PrintStmt LINE
    | Dcls LINE
    | LINE
    | IfStmts LINE
    | ForStmt LINE
;
 
SimpleStmt
    : Assignment { datatype = ""; }
    | Expr
    | IncDec
;
Dcls 
    : Dcl Dcls
    |
;
Dcl
    : VAR IDdcl Type 
    {
        stack[current].name = table->name;
        stack[current].type = table->type;
        if(!strcmp(table->type,"int32") || !strcmp(table->type , "bool"))
        {
            fprintf(fp , "ldc 0\nistore %d\n" , current);
            stack[current].intnum = yylval.i_val;
        }
        else if(!strcmp(table->type , "float32"))
        {
            fprintf(fp , "ldc 0.0\nfstore %d\n" , current);
            stack[current].floatnum = yylval.f_val;
        }
        else if(!strcmp(table->type , "string"))
        {
            fprintf(fp , "ldc \"\"\nastore %d\n" , current);
            stack[current].content = yylval.s_val;
        }
        current = current + 1;
        find = get(table->name);
        if(find == NULL)
        {
            insert_symbol(table->name);
            table->next = malloc(sizeof(struct table));
            table = table->next;
            table->next = NULL;
        }
        else //find != NULL mean this VAR is redeclared
        {
            yyerror();
            printf(" %s redeclared in this block. previous declaration at line %d\n" ,
            find->name , find->linenum);
        }
    }
    | VAR IDdcl Type '=' Expr 
    { 
        stack[current].name = table->name;
        stack[current].type = table->type;
        if(!strcmp(table->type,"int32") || !strcmp(table->type , "bool"))
        {

fprintf(fp , "istore %d\n" , current);
            stack[current].intnum = yylval.i_val;
        }
        else if(!strcmp(table->type , "float32"))
        {
            fprintf(fp , "fstore %d\n" , current);
            stack[current].floatnum = yylval.f_val;
        }
        else if(!strcmp(table->type , "string"))
        {
            fprintf(fp , "astore %d\n" , current);
            stack[current].content = yylval.s_val;
        }
        current = current + 1;
        insert_symbol(table->name);
        table->next = malloc(sizeof(struct table));
        table = table->next;
        table->next = NULL;
    }
;
IDdcl
    : ID 
    { 
            table->name = yylval.s_val;
    }
;
IfStmts
    : IfStmt IfStmts
    |
;
IfStmt
    : IF Condition Block
    | IF Condition Block ELSE IfStmt
    | IF Condition Block ELSE Block
;
Condition
    : Expr 
    { 
    if (strcmp(printtype , "bool"))
    {
        printf("error:%d: non-bool (type %s) used as for condition\n" , yylineno+1 , printtype);
    }
    }
        
;
ForStmt
    : FOR Condition Block
    | FOR ForClause Block
;
ForClause
    : Init ';' Condition ';' Post
;
Init
    : SimpleStmt
;
Post
    : SimpleStmt
;
Type
    : TypeName
    | ArrayType
;
TypeName
    : INT    { $$ = strdup("int32"); table->eletype = "-"; table->type = "int32"; }
    | FLOAT  { $$ = strdup("float32"); table->eletype = "-"; table->type = "float32"; }
    | STRING { $$ = strdup("string"); table->eletype = "-"; table->type = "string"; }
    | BOOL   { $$ = strdup("bool"); table->eletype = "-"; table->type = "bool"; }
;
ArrayType
    : '[' Expr ']' Type { table->eletype = table->type; table->type = "array"; }
;

Expr
    : UnaryExpr
    | Expr '+'{ s1 = printtype; } Expr 
    { s2 = printtype; invalidoper("ADD"); //printf("ADD\n"); }  
        if(!strcmp(s1 , "int32") && !strcmp(s2 , "int32")) fprintf(fp , "iadd\n");
        else if(!strcmp(s1 , "float32") && !strcmp(s2 , "float32")) fprintf(fp , "fadd\n");
    }
    | Expr '-'{ s1 = printtype; } Expr 
    { s2 = printtype; invalidoper("SUB"); //printf("SUB\n"); }
        if(!strcmp(s1 , "int32") && !strcmp(s2 , "int32")) fprintf(fp , "isub\n");
        else if(!strcmp(s1 , "float32") && !strcmp(s2 , "float32")) fprintf(fp , "fsub\n");
    }
    | Expr '*'{ s1 = printtype; } Expr 
    { s2 = printtype; invalidoper("MUL"); //printf("MUL\n"); }
        if(!strcmp(s1 , "int32") && !strcmp(s2 , "int32")) fprintf(fp , "imul\n");
        else if(!strcmp(s1 , "float32") && !strcmp(s2 , "float32")) fprintf(fp , "fmul\n");
    }
    | Expr '/'{ s1 = printtype; } Expr 
    { s2 = printtype; invalidoper("QUO"); //printf("QUO\n"); }
        if(!strcmp(s1 , "int32") && !strcmp(s2 , "int32")) fprintf(fp , "idiv\n");
        else if(!strcmp(s1 , "float32") && !strcmp(s2 , "float32")) fprintf(fp , "fdiv\n");
    }
    | Expr '%'{ s1 = printtype; } Expr 
    { s2 = printtype; 
      if (!strcmp(s1 , "float32") || !strcmp(s2 , "float32"))
      {
        yyerror();
        printf(" invalid operation: (operator REM not defined on float32)\n");
      }
//      printf("REM\n"); }
        if(!strcmp(s1 , "int32") && !strcmp(s2 , "int32")) fprintf(fp , "irem\n");
        else if(!strcmp(s1 , "float32") && !strcmp(s2 , "float32")) fprintf(fp , "frem\n");
    }
    | Expr EQL Expr { //printf("EQL\n"); 
        printtype = "bool";
//        fprintf(fp , "ifeq %s%d_0\n" , jump , label);
    }
    | Expr '<' Expr { printf("LSS\n"); printtype = "bool";}
    | Expr '>' Expr { //printf("GTR\n"); 
        if(!strcmp(printtype , "int32"))
            fprintf(fp , "isub\n");
        else if (!strcmp(printtype , "float32"))
            fprintf(fp , "fcmpl\n");
        fprintf(fp , "ifgt %s%d_0\n" , jump , label);
        fprintf(fp , "iconst_0\ngoto %s%d_1\n" , jump , label);
        fprintf(fp , "%s%d_0:\n" , jump , label);
        fprintf(fp , "iconst_1\n%s%d_1:\n" , jump , label);
        label = label + 1;
        printtype = "bool";
    }
    | Expr NEQ Expr { printf("NEQ\n"); printtype = "bool";}
    | Expr GEQ Expr { printf("GEQ\n"); printtype = "bool";}
    | Expr LEQ Expr { printf("LEQ\n"); printtype = "bool";}
    | Expr LOR { s1 = printtype; } Expr
    { s2 = printtype;
      if (strcmp("bool" , s1)){ yyerror(); 
        printf(" invalid operation: (operator LOR not defined on %s)\n" , s1);}
      else if (strcmp("bool" , s2)){ yyerror();
        printf(" invalid operation: (operator LOR not defined on %s)\n" , s2);}
      fprintf(fp , "ior\n");
      printf("LOR\n"); }
    | Expr LAND { s1 = printtype; } Expr
    { s2 = printtype; 
      if (strcmp("bool" , s1)){ yyerror();
        printf(" invalid operation: (operator LAND not defined on %s)\n" , s1);}
      else if (strcmp("bool" , s2)){ yyerror();
        printf(" invalid operation: (operator LAND not defined on %s)\n" , s2);}
      fprintf(fp , "iand\n");
      printf("LAND\n"); }
;
UnaryExpr
    : PrimaryExpr
    | '+' UnaryExpr { /*printf("POS\n");*/ }
    | '-' UnaryExpr { 
        if(!strcmp(printtype , "int32"))
            fprintf(fp , "ineg\n");
        else if (!strcmp(printtype , "float32"))
            fprintf(fp , "fneg\n");
    }
    | '!' UnaryExpr {
        fprintf(fp , "iconst_1\n");
        fprintf(fp , "ixor\n");
    }
;

PrimaryExpr 
    : Operand 
    | IndexExpr
    | Conversion
;
Operand
    : Literal
    | ID {
            find = get(yylval.s_val);
            if(find != NULL){
                {
                    printf("IDENT (name=%s, address=%d)\n", yylval.s_val , find->address);
                    if(!strcmp(find->type , "int32"))
                    {
                        printtype = "int32";
                        fprintf(fp , "iload %d\n" , findstack(yylval.s_val));
                    }
                    else if (!strcmp(find->type , "float32"))
                    {
                        printtype = "float32";
                        fprintf(fp , "fload %d\n" , findstack(yylval.s_val));
                    }
                    else if (!strcmp(find->type , "string"))
                    {
                        printtype = "string";
                        fprintf(fp , "aload %d\n" , findstack(yylval.s_val));
                    }
                    else if (!strcmp(find->type , "bool"))
                    {
                        printtype = "bool";
                        fprintf(fp , "iload %d\n" , findstack(yylval.s_val));
                    }
                }
            }
            else
            {
                printf("error:%d: undefined: %s\n" , yylineno+1 ,yylval.s_val);
            }
         }

    | '(' Expr ')'
;
Literal
    : INT_LIT { //printf("%s %d\n" , "INT_LIT" , yylval.i_val); 
    fprintf(fp , "ldc %d\n" , yylval.i_val);
    printtype = "int32";
    datatype = "LIT";}
    | FLOAT_LIT { //printf("%s %f\n" , "FLOAT_LIT" , yylval.f_val); 
    fprintf(fp , "ldc %.6f\n" , yylval.f_val);
    printtype = "float32";}
    | STRING_LIT { //printf("%s %s\n" , "STRING_LIT" , yylval.s_val); 
    fprintf(fp , "ldc \"%s\"\n" , yylval.s_val);
    printtype = "string";}
    | TRUE { //printf("TRUE\n"); 
    fprintf(fp , "iconst_1\n");
    printtype = "bool";}
    | FALSE { //printf("FALSE\n"); 
    fprintf(fp , "iconst_0\n");
    printtype = "bool";}
;
Assignment
    : LeftExpr '='{ s1 = printtype; } Expr 
    {
        printassign();
        s2 = printtype; if(find != NULL) invalidoper("ASSIGN"); printf("ASSIGN\n");
    }
    | LeftExpr
    {
        if(!strcmp(datatype , "LIT"))
        {
            temp = 1;
        }
        datatype = "";
    }
    ADD_ASSIGN Expr 
    {
        if(!strcmp(printtype , "int32") || !strcmp(printtype , "bool"))
            fprintf(fp , "iload %d\niadd\n" , findstack(leftvar));
        else if(!strcmp(printtype , "float32"))
            fprintf(fp , "fload %d\nfadd\n" , findstack(leftvar));
        printassign();
        if(temp == 1)
            printf("error:%d: cannot assign to int32\n" , yylineno);
        printf("ADD_ASSIGN\n"); 
            
    }
    | LeftExpr SUB_ASSIGN Expr { 
        if(!strcmp(printtype , "int32") || !strcmp(printtype , "bool"))
            fprintf(fp , "iload %d\nswap\nisub\n" , findstack(leftvar));
        else if(!strcmp(printtype , "float32"))
            fprintf(fp , "fload %d\nswap\nfsub\n" , findstack(leftvar));
        printassign();
        printf("SUB_ASSIGN\n"); 
    }
    | LeftExpr MUL_ASSIGN Expr { 
        if(!strcmp(printtype , "int32") || !strcmp(printtype , "bool"))
            fprintf(fp , "iload %d\nimul\n" , findstack(leftvar));
        else if(!strcmp(printtype , "float32"))
            fprintf(fp , "fload %d\nfmul\n" , findstack(leftvar));
        printassign();
        printf("MUL_ASSIGN\n"); 
    }
    | LeftExpr QUO_ASSIGN Expr { 
        if(!strcmp(printtype , "int32") || !strcmp(printtype , "bool"))
            fprintf(fp , "iload %d\nswap\nidiv\n" , findstack(leftvar));
        else if(!strcmp(printtype , "float32"))
            fprintf(fp , "fload %d\nswap\nfdiv\n" , findstack(leftvar));
        printassign();
        printf("QUO_ASSIGN\n"); 
    }
    | LeftExpr REM_ASSIGN Expr { 
        if(!strcmp(printtype , "int32"))
            fprintf(fp , "iload %d\nswap\nirem\n" , findstack(leftvar));
        printassign();
        printf("REM_ASSIGN\n"); 
    }
;
LeftExpr
    : ID { leftvar = yylval.s_val; }
    | IndexExpr { leftvar = yylval.s_val; }
;
IncDec
    : Expr INC { //printf("INC\n"); }
        if(!strcmp(printtype , "int32"))
            fprintf(fp , "ldc 1\niadd\nistore %d\n" , findstack(yylval.s_val));
        else if(!strcmp(printtype , "float32"))
            fprintf(fp , "ldc 1.0\nfadd\nfstore %d\n" , findstack(yylval.s_val));
        }
    | Expr DEC { //printf("DEC\n"); }
        if(!strcmp(printtype , "int32"))
            fprintf(fp , "ldc 1\nisub\nistore %d\n" , findstack(yylval.s_val));
        else if(!strcmp(printtype , "float32"))
            fprintf(fp , "ldc 1.0\nfsub\nfstore %d\n" , findstack(yylval.s_val));
    }
;
PrintStmt
    : PRINT '(' Expr ')' { //printf("%s" , "PRINT"); 
        if(!strcmp(printtype , "bool"))
        {
            fprintf(fp , "ifne %s%d_0\n" , jump , label);
            fprintf(fp , "ldc \"false\"\ngoto %s%d_1\n" , jump , label);
            fprintf(fp , "%s%d_0:\n" , jump , label);
            fprintf(fp , "ldc \"true\"\n");
            fprintf(fp , "%s%d_1:\n" , jump , label);
            label = label + 1;
        }
        fprintf(fp , "getstatic java/lang/System/out Ljava/io/PrintStream;\n");
        fprintf(fp , "swap\n");
        if (!strcmp(printtype , "int32"))
            fprintf(fp , "invokevirtual java/io/PrintStream/print(I)V\n");
        else if (!strcmp(printtype , "float32"))
            fprintf(fp , "invokevirtual java/io/PrintStream/print(F)V\n");
        else if (!strcmp(printtype , "string"))
            fprintf(fp ,"invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\n");
        else if (!strcmp(printtype , "bool"))
            fprintf(fp ,"invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\n");
    }
    | PRINTLN '(' Expr ')' { //printf("%s", "PRINTLN"); 
        if(!strcmp(printtype , "bool"))
        {
            fprintf(fp , "ifne %s%d_0\n" , jump , label);
            fprintf(fp , "ldc \"false\"\ngoto %s%d_1\n" , jump , label);
            fprintf(fp , "%s%d_0:\n" , jump , label);
            fprintf(fp , "ldc \"true\"\n");
            fprintf(fp , "%s%d_1:\n" , jump , label);
            label = label + 1;
        }
        fprintf(fp , "getstatic java/lang/System/out Ljava/io/PrintStream;\n");
        fprintf(fp , "swap\n");
        if (!strcmp(printtype , "int32"))
            fprintf(fp , "invokevirtual java/io/PrintStream/println(I)V\n");
        else if (!strcmp(printtype , "float32"))
            fprintf(fp , "invokevirtual java/io/PrintStream/println(F)V\n");
        else if (!strcmp(printtype , "string"))
            fprintf(fp ,"invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V\n");
        else if (!strcmp(printtype , "bool"))
            fprintf(fp ,"invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V\n");
    }
;
Block
    : '{' { nowlevel = nowlevel + 1;} StatementList '}' { 
                              dump_symbol();
                            }
;
IndexExpr
    : PrimaryExpr { find = get(yylval.s_val); } '[' Expr ']' 
    {
        if(!strcmp(find->eletype , "int32"))
            printtype = "int32";
        else if (!strcmp(find->eletype , "float32"))
            printtype = "float32";
        else if (!strcmp(find->eletype , "string"))
            printtype = "string";
        else if (!strcmp(find->eletype , "bool"))
            printtype = "bool";
    }
;
Conversion
    : Type_Change '(' Expr ')' 
    {
        if (!strcmp(changetype , "I"))
        {
            printf("%s to %s\n" , "F" , changetype);
            printtype = "int32";
        }
        else
        {
            printf("%s to %s\n" , "I" , changetype);
            printtype = "float32";
        }
    }
;
Type_Change
    : INT    { changetype = "I";}
    | FLOAT  { changetype = "F";}
/*    | STRING { $$ = strdup("string"); table->eletype = "-"; table->type = "string"; }
    | BOOL   { $$ = strdup("bool"); table->eletype = "-"; table->type = "bool"; }*/
;
%%

/* C code section */
int main(int argc, char *argv[])
{
    fp = fopen("hw3.j" , "w");
    fprintf(fp , ".source hw3.j\n.class public Main\n");
    fprintf(fp , ".super java/lang/Object\n");
    fprintf(fp , ".method public static main([Ljava/lang/String;)V\n");
    fprintf(fp , ".limit stack 100\n.limit locals 100 ;\n");
    table = malloc(sizeof(struct table));
    head = table;
    find = table;
    nowlevel = 0;
    if (argc == 2) {
        yyin = fopen(argv[1], "r");
    } else {
        yyin = stdin;
    }

    yylineno = 0;
    yyparse();
//    dump_symbol();
	printf("Total lines: %d\n", yylineno);
    fclose(yyin);
    struct table *tmp;
    while(head != NULL)
    {
        tmp = head;
        head = head->next;
        free(tmp);
    }
    fprintf(fp , "return\n.end method\n");
    if(HAS_ERROR) {
        remove("hw3.j");
    }
    return 0;
}

static void create_symbol() {
    nowlevel = nowlevel + 1;
}

static void insert_symbol(char *id) {
    printf("> Insert {%s} into symbol table (scope level: %d)\n", id, nowlevel);
    table->level = nowlevel;
    table->name = id;
    table->address = nowaddr;
    table->linenum = yylineno;
    nowaddr = nowaddr + 1;
}

static void lookup_symbol() {
}

static void dump_symbol() {
    char *namelist[30];
    int number = 0;
    printf("> Dump symbol table (scope level: %d)\n", nowlevel);
    printf("%-10s%-10s%-10s%-10s%-10s%s\n",
           "Index", "Name", "Type", "Address", "Lineno", "Element type");
    if(nowaddr != 0)
    {
        levelnum = getlevelnum();
        find = head;
        for(int i = 0;i < levelnum;i++)
        {
            while(find->level != nowlevel)
                find = find->next;
            printf("%-10d%-10s%-10s%-10d%-10d%s\n",
                i, find->name , find->type, find->address, find->linenum, find->eletype);
            namelist[number] = find->name;
            number = number + 1;
            find->level = -1;
            find = find->next;
        }
        for(int i = 1;i <= number;i++)
            for(int j = 1;j <= current;j++)
                if(!strcmp(namelist[number-i] , stack[current-j].name) && stack[current-j].status != -1)
                {
                    stack[current-j].status = -1;//pop this variable(set it's status to -1
                    break;
                }
    }
    nowlevel = nowlevel - 1;
}
struct table *get(char *name)
{
    find = head;
    struct table *temp;
    temp = NULL;
    for (int i = 0;i < nowaddr;i++)
    {
        if(!strcmp(find->name , name) && find->level >= 0)
            temp = find;
        find = find->next;
    }
    return temp;
}
int getlevelnum()
{
    int count = 0;
    find = head;
    for(int i = 0;i < nowaddr;i++)
    {
        if(find->level == nowlevel)
            count = count + 1;
        if(i != nowaddr)
            find = find->next;
    }
//    printf("%d %d  %d\n" , nowaddr , count , nowaddr);
    return count;
}
int findstack(char *name)
{
    for(int i = 1;i <= current;i++)//from the top of stack to find the variable
        if(!strcmp(name , stack[current-i].name) && stack[current-i].status != -1)
        {
            return current - i;
        }
    return 0;
}
void printassign()
{
    if(!strcmp(printtype , "int32") || !strcmp(printtype , "bool"))
        fprintf(fp , "istore %d\n" , findstack(leftvar));
    else if(!strcmp(printtype , "float32"))
        fprintf(fp , "fstore %d\n" , findstack(leftvar));
    else if(!strcmp(printtype , "string"))
        fprintf(fp , "astore %d\n" , findstack(leftvar));
}
