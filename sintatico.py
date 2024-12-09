import ply.lex as lex
import ply.yacc as yacc

# Lista de tokens
reserved = {
    'if': 'IF',
    'else': 'ELSE',
    'return': 'RETURN',
    'int': 'DATATYPE',
    'float': 'DATATYPE',
    'char': 'DATATYPE',
    'boolean': 'DATATYPE',
}

tokens = [
    'IDENTIFIER', 'INTEGER_VALUE', 'FLOAT_VALUE', 'CHAR_VALUE', 'STRING_VALUE',
    'BOOLEAN_VALUE', 'ASSIGNMENT_OPERATOR', 'SEPARATOR', 'PAREN', 'BRACE',
    'COMMENT', 'ARITHMETIC_OPERATOR', 'PREPROCESSOR', 'LESS_THAN', 'GREATER_THAN',
] + list(reserved.values())

# Regras de expressão regular para tokens simples
t_ASSIGNMENT_OPERATOR = r'='
t_SEPARATOR = r'[;,]'
t_PAREN = r'[()]'
t_BRACE = r'[{}]'
t_ARITHMETIC_OPERATOR = r'[-+*/]'
t_ignore_WHITESPACE = r'\s+'
t_LESS_THAN = r'<'
t_GREATER_THAN = r'>'

# Regras de expressões regulares para tokens complexos
def t_PREPROCESSOR(t):
    r'\#include\s+<[^>]+>'
    t.value = t.value.strip()
    return t

def t_COMMENT(t):
    r'//.*|/\*[\s\S]*?\*/'
    pass

def t_BOOLEAN_VALUE(t):
    r'\btrue\b|\bfalse\b'
    return t

def t_FLOAT_VALUE(t):
    r'\b\d+\.\d+\b'
    return t

def t_INTEGER_VALUE(t):
    r'\b\d+\b'
    return t

def t_CHAR_VALUE(t):
    r'\'([^\\\n]|(\\.))*?\''
    return t

def t_STRING_VALUE(t):
    r'"([^"\\]|\\.)*"'
    return t

def t_IDENTIFIER(t):
    r'\b[A-Za-z_][A-Za-z0-9_]*\b'
    t.type = reserved.get(t.value, 'IDENTIFIER')  # Verifica palavras reservadas
    return t

def t_error(t):
    print(f"Illegal character '{t.value[0]}'")
    t.lexer.skip(1)

lexer = lex.lex()

# Definindo a gramática
def p_program(p):
    '''program : declarations'''
    p[0] = {"type": "Program", "body": p[1]}

def p_declarations(p):
    '''declarations : declarations declaration
                    | empty'''
    if len(p) == 3:
        p[0] = p[1] + [p[2]]
    else:
        p[0] = []

def p_declaration(p):
    '''declaration : variable_declaration
                   | function_declaration
                   | function_call
                   | conditional
                   | return_statement
                   | comment
                   | preprocessor_directive'''
    p[0] = p[1]

def p_variable_declaration(p):
    '''variable_declaration : DATATYPE IDENTIFIER ASSIGNMENT_OPERATOR expression SEPARATOR
                            | DATATYPE IDENTIFIER SEPARATOR'''
    if len(p) == 6:
        p[0] = {"type": "DeclaracaoVariavel", "varType": p[1], "id": p[2], "init": p[4]}
    else:
        p[0] = {"type": "DeclaracaoVariavel", "varType": p[1], "id": p[2], "init": None}

def p_function_declaration(p):
    '''function_declaration : DATATYPE IDENTIFIER PAREN PAREN block'''
    p[0] = {"type": "DeclaracaoFuncao", "funcType": p[1], "id": p[2], "params": [], "body": p[5]}

def p_function_call(p):
    '''function_call : IDENTIFIER PAREN expression_list PAREN SEPARATOR
                     | IDENTIFIER PAREN PAREN SEPARATOR'''
    if len(p) == 6:
        p[0] = {"type": "ChamadaFuncao", "id": p[1], "args": p[3]}
    else:
        p[0] = {"type": "ChamadaFuncao", "id": p[1], "args": []}

def p_expression_list(p):
    '''expression_list : expression
                       | expression_list SEPARATOR expression'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[3]]

def p_block(p):
    '''block : BRACE declarations BRACE'''
    p[0] = {"type": "Bloco", "body": p[2]}

def p_conditional(p):
    '''conditional : IF PAREN expression PAREN block
                   | IF PAREN expression PAREN block ELSE block'''
    if len(p) == 6:
        p[0] = {"type": "Condicional", "test": p[3], "consequence": p[5], "alternative": None}
    else:
        p[0] = {"type": "Condicional", "test": p[3], "consequence": p[5], "alternative": p[7]}

def p_return_statement(p):
    '''return_statement : RETURN expression SEPARATOR
                        | RETURN SEPARATOR'''
    if len(p) == 4:
        p[0] = {"type": "ReturnStatement", "value": p[2]}
    else:
        p[0] = {"type": "ReturnStatement", "value": None}

def p_expression(p):
    '''expression : INTEGER_VALUE
                  | FLOAT_VALUE
                  | CHAR_VALUE
                  | STRING_VALUE
                  | BOOLEAN_VALUE
                  | IDENTIFIER
                  | expression ARITHMETIC_OPERATOR expression
                  | expression LESS_THAN expression
                  | expression GREATER_THAN expression'''
    if len(p) == 2:
        p[0] = {"type": "Expressao", "value": p[1]}
    else:
        p[0] = {
            "type": "ExpressaoBinaria",
            "operator": p[2],
            "left": p[1],
            "right": p[3],
        }

def p_comment(p):
    '''comment : COMMENT'''
    p[0] = {"type": "Comentario", "value": p[1]}

def p_preprocessor_directive(p):
    '''preprocessor_directive : PREPROCESSOR'''
    p[0] = {"type": "PreprocessorDirective", "value": p[1]}

def p_empty(p):
    'empty :'
    pass

def p_error(p):
    if p:
        print(f"Syntax error at '{p.value}'")
    else:
        print("Syntax error at EOF")

parser = yacc.yacc()

# Código fonte de teste
source_code = """
#include <stdio.h>

int main() {
    int a = 10;
    int b = 20;


    if (a < b) {
        printf("a is less than b\\n");
    } else {
        printf("b is greater than a\\n");
    }

    return 0;
}
"""

# Executando o lexer
lexer.input(source_code)
tokens = []
while True:
    tok = lexer.token()
    if not tok:
        break
    tokens.append((tok.type, tok.value))

# Executando o parser
ast = parser.parse(source_code, lexer=lexer)

# Função para imprimir a árvore
def print_tree(node, level=0):
    if isinstance(node, dict):
        print("|   " * level + "+-- " + node["type"])
        for key, value in node.items():
            if key != "type":
                print_tree(value, level + 1)
    elif isinstance(node, list):
        for item in node:
            print_tree(item, level)
    else:
        print("|   " * level + "+-- " + str(node))

print_tree(ast)
