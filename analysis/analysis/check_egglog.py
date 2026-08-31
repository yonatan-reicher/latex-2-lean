# Check that the egglog module is importable.

try:
    import egglog
except ModuleNotFoundError as e:
    e.add_note('could it be that you forgot to activate a virtual enviornment or to pip-install egglog?')
    raise
