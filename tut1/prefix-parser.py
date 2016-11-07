#!/usr/bin/env python3

import sys


def parse_expression(expression, index=0):
    a = 0
    b = 0
    c = expression[index]
    if c >= '0' and c <= '9':
        return int(c)
    elif c in ['+', '-', '*', '/']:
        a = parse_expression(expression, index + 1)
        b = parse_expression(expression, index + 2)
    else:
        raise Exception()

    if c == '+':
        return a + b
    elif c == '-':
        return a - b
    elif c == '*':
        return a * b
    elif c == '/':
        return a / b


def main(arg):
    result = parse_expression(arg.split(' '))
    print(result)

if __name__ == '__main__':
    main(sys.argv[1])
