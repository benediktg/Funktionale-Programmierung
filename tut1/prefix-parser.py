#!/usr/bin/env python3

import sys


def parse_expression(expression):
    c = expression[0]
    if c == '+':
        return parse_expression() + parse_expression()
    elif c == '-':
        return parse_expression() - parse_expression()
    elif c == '*':
        return parse_expression() * parse_expression()
    elif c == '/':
        return parse_expression() / parse_expression()
    elif c >= '0' or c <= '9':
        return int(c)


def main(args):
    expression = args.split(' ')
    parse_expression(expression)

if __name__ == '__main__':
    main(sys.argv)
