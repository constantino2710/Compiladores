import re
import ply.lex as lex
import ply.yacc as yacc

token_patterns = [
    ('NUM_INT', r'\b\d+\b'),
    ('NUM_DEC', r'\b\d+\.\d+\b'),
    ('ID', r'\b[a-zA-Z_]\w*\b'),
    ('TEXTO', r'"([^"\\]|\\.)*"'),
    ('PALAVRA_RESERVADA', r'\b(int|float|char|boolean|void|if|else|for|while|scanf|println|main|return)\b'),  # Palavras reservadas
    ('COMENTARIO', r'\/\/.*'),
    ('OPERADOR', r'[+\-*/%]|&&|\|\||!|==|!=|>=|<=|>|<|=|\+\+|\-\-'),
    ('SIMBOLO_ESPECIAL', r'[\(\)\[\]\{\},;]'),
    ('INCREMENTO', r'\+\+'),
    ('DECREMENTO', r'\-\-'),
    ('PONTO', r'\.'),
    ('SETA', r'->'),
    ('PONTO_E_VIRGULA', r';'),
    ('VIRGULA', r','),
    ('ASPAS', r'"'),
    ('ASPAS_SIMPLES', r"'"),
    ('COMENTARIO_MULTILINHA', r'/\*[\s\S]*?\*/'),
    ('OPERADOR_COMPARACAO', r'==|!=|>=|<=|>|<'),
    ('E_LOGICO', r'&&'),
    ('OU_LOGICO', r'\|\|'),  # Operador lógico OR
    ('NEGACAO_LOGICA', r'!'),
]

# Função para analisar o código-fonte e identificar os tokens
def lexical_analyzer(codigo):
    tokens = []
    posicao = 0

    while posicao < len(codigo):
        match_found = False
        for token_nome, token_pattern in token_patterns:
            regex = re.compile(token_pattern)
            match = regex.match(codigo, posicao)
            if match:
                valor = match.group(0)
                if token_nome != 'COMENTARIO' and token_nome != 'COMENTARIO_MULTILINHA':  # Ignorar comentários
                    tokens.append((token_nome, valor))
                posicao = match.end()
                match_found = True
                break
        if not match_found:
            # Tentar ignorar o token inválido
            posicao += 1
    return tokens

# Tokens
tokens = [
    'IDENTIFIER', 'INTEGER_VALUE', 'FLOAT_VALUE', 'CHAR_VALUE', 'STRING_VALUE',
    'BOOLEAN_VALUE', 'ASSIGNMENT_OPERATOR', 'SEPARATOR', 'PAREN', 'BRACE', 'COMMENT', 'COMMENT_MULTILINE', 'DATATYPE', 'ARITHMETIC_OPERATOR'
]

# Regras de expressão regular para tokens simples
t_ASSIGNMENT_OPERATOR = r'='
t_SEPARATOR = r'[;,]'
t_PAREN = r'[()]'
t_BRACE = r'[{}]'
t_ARITHMETIC_OPERATOR = r'[-+*/]'
t_ignore_WHITESPACE = r'\s+'

# Expressões regulares -> tokens complexos
def t_COMMENT(t):
    r'//.*'
    pass

def t_COMMENT_MULTILINE(t):
    r'/\*[\s\S]*?\*/'
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
    r'\"([^\\\n]|(\\.))*?\"'
    return t

def t_DATATYPE(t):
    r'\bint\b|\bfloat\b|\bdouble\b|\bchar\b|\bboolean\b'
    t.value = {"type": "DATATYPE", "value": t.value}
    return t

def t_IDENTIFIER(t):
    r'\b[A-Za-z_][A-Za-z0-9_]*\b'
    t.value = {"type": "IDENTIFIER", "value": t.value}
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
                   | comment'''
    p[0] = p[1]

def p_variable_declaration(p):
    '''variable_declaration : DATATYPE IDENTIFIER ASSIGNMENT_OPERATOR expression SEPARATOR
                            | DATATYPE IDENTIFIER SEPARATOR'''
    if len(p) == 6:
        p[0] = {"type": "DeclaracaoVariavel", "varType": p[1]["value"], "id": p[2]["value"], "init": p[4]}
    else:
        p[0] = {"type": "DeclaracaoVariavel", "varType": p[1]["value"], "id": p[2]["value"], "init": None}

def p_function_declaration(p):
    '''function_declaration : DATATYPE IDENTIFIER PAREN PAREN block'''
    p[0] = {"type": "DeclaracaoFuncao", "funcType": p[1]["value"], "id": p[2]["value"], "params": [], "body": p[5]}

def p_block(p):
    '''block : BRACE declarations BRACE'''
    p[0] = {"type": "Bloco", "body": p[2]}

def p_expression(p):
    '''expression : INTEGER_VALUE
                  | FLOAT_VALUE
                  | CHAR_VALUE
                  | STRING_VALUE
                  | BOOLEAN_VALUE
                  | IDENTIFIER
                  | expression ARITHMETIC_OPERATOR expression'''
    if len(p) == 4:
        p[0] = {"type": "Expressao", "left": p[1], "operator": p[2], "right": p[3]}
    else:
        p[0] = {"type": "Expressao", "value": p[1]}

def p_comment(p):
    '''comment : COMMENT
               | COMMENT_MULTILINE'''
    p[0] = {"type": "Comentario", "value": p[1]}

def p_empty(p):
    'empty :'
    pass

def p_error(p):
    if p:
        print(f"Syntax error at '{p.value}'")
    else:
        print("Syntax error: unexpected end of input")

parser = yacc.yacc()

# Adicionar uma classe para
# o Analisador Semântico
class SemanticAnalyzer:
    def __init__(self, ast):
        self.ast = ast
        self.symbol_table = {}  # Tabela de símbolos para armazenar tipos de variáveis

    def analyze(self): #inicia a analise semantica na arvore
        self.visit(self.ast)

    def visit(self, node):
        if isinstance(node, dict):
            method_name = 'visit_' + node.get("type", "")
            visitor = getattr(self, method_name, self.generic_visit)
            return visitor(node)
        elif isinstance(node, list):
            for item in node:
                self.visit(item)
        elif isinstance(node, str):
            return self.visit_string(node)
        elif isinstance(node, int):
            return self.visit_int(node)
        elif isinstance(node, float):
            return self.visit_float(node)
        elif isinstance(node, bool):
            return self.visit_boolean(node)
        else:
            return None

    def generic_visit(self, node):
        for key, value in node.items():
            self.visit(value)

    def visit_DeclaracaoVariavel(self, node):
        var_type = node["varType"]
        var_id = node["id"]
        if var_id in self.symbol_table:
            raise TypeError(f"Variable {var_id} already declared")
        if node["init"] is not None:
            init_type = self.visit(node["init"])
            if var_type != init_type:
                raise TypeError(f"Type mismatch for variable {var_id}: {var_type} != {init_type}")
        self.symbol_table[var_id] = var_type
        return var_type

    def visit_Expressao(self, node):
        if "operator" in node:
            left_type = self.visit(node["left"])
            right_type = self.visit(node["right"])
            operator = node["operator"]

            # Verificar se os tipos são compatíveis com o operador
            if operator in ['+', '-', '*', '/']:
                if left_type != right_type:
                    raise TypeError(f"Type error in binary operation: {left_type} and {right_type}")
                if left_type not in ['int', 'float']:
                    raise TypeError(f"Unsupported operand type(s) for {operator}: '{left_type}'")
                return left_type
            elif operator in ['&&', '||']:
                if left_type != 'boolean' or right_type != 'boolean':
                    raise TypeError(f"Unsupported operand type(s) for {operator}: '{left_type}' and '{right_type}'")
                return 'boolean'
            else:
                # Adicionar suporte para operadores sobrecarregados
                # Aqui você pode adicionar lógica para verificar operadores sobrecarregados
                # para tipos definidos pelo usuário, se necessário
                pass
        else:
            return self.visit(node["value"])

    def visit_string(self, node):
        return 'string'

    def visit_int(self, node):
        return 'int'

    def visit_float(self, node):
        return 'float'

    def visit_boolean(self, node):
        return 'boolean'

    def visit_IDENTIFIER(self, node):
        var_id = node["value"]
        if var_id not in self.symbol_table:
            raise NameError(f"Variable {var_id} not declared")
        return self.symbol_table[var_id]

    def visit_LITERAL(self, node):
        return node["value"]

# Função para imprimir a árvore
def print_tree(node, level=0):
    if isinstance(node, dict):
        if level > 0:
            print("|   " * (level - 1) + "+-- " + node["type"])
        for key, value in node.items():
            if key != "type":
                if isinstance(value, list):
                    for item in value:
                        print_tree(item, level + 1)
                else:
                    print_tree(value, level + 1)
    elif isinstance(node, list):
        for item in node:
            print_tree(item, level)
    else:
        print("|   " * level + "+-- " + str(node))


source_code = """
// Este é um comentário de linha.
int main() {
    int x = "2.4";
}
"""

# Lexer
try:
    tokens = lexical_analyzer(source_code)
    for token in tokens:
        print(token)

    # Executar o parser
    ast = parser.parse(source_code)

    print("\nAST:")
    print_tree(ast)

    # Análise semântica
    analyzer = SemanticAnalyzer(ast)
    analyzer.analyze()

except ValueError as e:
    print("Erro léxico:", e)
except TypeError as e:
    print("Erro semântico:", e)
except NameError as e:
    print("Erro semântico:", e)
