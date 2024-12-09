import re

token_specification = [
    ('INCLUDE',    r'#include\s+<[^>]+>'),
    ('NUM_DEC',    r'\b\d+\.\d+\b'),
    ('NUM_INT',    r'\b\d+\b'),
    ('ID',         r'\b[A-Za-z_]\w*\b'),
    ('TEXTO',      r'"[^"]*"'),
    ('CHAR',       r"'.'"),
    ('RESERVADA',  r'\b(int|float|char|boolean|void|if|else|for|while|scanf|printf|main|return)\b'),
    ('COMENTARIO', r'//.*'),
    ('OPERADOR',   r'==|!=|>=|<=|[+\-*/%=&|<>!?:]'),
    ('DELIMITADOR',r'[()\[\]{},;]'),
    ('NEWLINE',    r'\n'),
    ('ESPACO',     r'[ \t]+'),
    ('ERRO',       r'.')
]

token_regex = '|'.join(f'(?P<{pair[0]}>{pair[1]})' for pair in token_specification)

palavras_reservadas = {'int', 'float', 'char', 'boolean', 'void', 'if', 'else', 'for', 'while', 'scanf', 'printf', 'main', 'return'}

class Token:
    def __init__(self, tipo, valor):
        self.tipo = tipo
        self.valor = valor

    def __str__(self):
        return f'{self.tipo}: {self.valor}'

def lexical_analyzer(source_code):
    tokens = []
    symbol_table = {}
    
    for mo in re.finditer(token_regex, source_code):
        tipo = mo.lastgroup
        valor = mo.group(tipo)
        
        if tipo == 'ESPACO' or tipo == 'NEWLINE':
            continue
        elif tipo == 'ID' and valor not in palavras_reservadas:
            if valor not in symbol_table:
                symbol_table[valor] = len(symbol_table) + 1
            tokens.append(Token(tipo, valor))
        elif tipo == 'ERRO':
            raise ValueError(f'Erro léxico: caractere inesperado {valor}')
        else:
            tokens.append(Token(tipo, valor))
    return tokens, symbol_table

def print_symbol_table(symbol_table):
    print("\nTabela de Símbolos:")
    print("------------------")
    for symbol, symbol_id in symbol_table.items():
        print(f'{symbol_id}: {symbol}')

def main():
    source_code = '''
    #include <stdio.h>
int main() {
    int a = 10, b = 20;
    float c = 30.5;

    if (a < b && b > c) {
        printf("B é maior");
    } else if (b == c) {
        printf("B é igual a C");
    } else {
        printf("Nenhuma das condições é verdadeira");
    }

    for (int i = 0; i < 5; i++) {
        printf("Valor de i: %d\\n", i);
    }

    return 0;
}
    '''
    tokens, symbol_table = lexical_analyzer(source_code)

    print("Lista de Tokens:")
    print("----------------")
    for token in tokens:
        print(token)
    print_symbol_table(symbol_table)

if __name__ == '__main__':
    main()
