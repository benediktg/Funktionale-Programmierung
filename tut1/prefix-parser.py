#!/usr/bin/env python3

import sys


def parse_expression(expression, index=0):
    a = 0
    b = 0
    c = expression[index]
    index += 1
    if c in "0123456789":
        return (int(c), index)
    elif c in "+-*/":
        a, index = parse_expression(expression, index)
        b, index = parse_expression(expression, index)
    else:
        raise Exception("illegal operator")

    if c == "+":
        return (a + b, index)
    elif c == "-":
        return (a - b, index)
    elif c == "*":
        return (a * b, index)
    elif c == "/":
        return (a // b, index)


def main(arg):
    result, index = parse_expression(arg)
    if (index == len(arg)):
        print(result)
    else:
        raise Exception("too much operands")

if __name__ == "__main__":
    main(sys.argv[1])
