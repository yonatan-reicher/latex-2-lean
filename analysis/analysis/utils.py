""" What do you think this module is about? """

from sys import exit
import traceback as tb

def bad_exit(reason: str | None = None):
    if reason is not None:
        stack = tb.extract_stack()
        stack.pop()
        print(f"Error: {reason}")
        tb.print_list(stack)
        print(f"Error: {reason}")
    return exit(1)

def raise_(e: Exception): raise e

def sorry():
    """ mark unimplemented code """
    return bad_exit('this part of the code has been left unimplemented')
