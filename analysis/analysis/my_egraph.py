from egglog import EGraph

class MyEGraph(EGraph):
    """ Currently, exactly the same as the regular e-graph. Later, will be
    extended with conveniences. """
    def __init__(self, save_egglog_string=True):
        super().__init__(save_egglog_string=save_egglog_string) # type: ignore
