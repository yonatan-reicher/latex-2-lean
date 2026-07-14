""" What do you think this module is about? """

from sys import exit

def bad_exit(reason: str | None = None):
    if reason is not None:
        print(f"Error: {reason}")
    exit(1)

def raise_(e: Exception): raise e
