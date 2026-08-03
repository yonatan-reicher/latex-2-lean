from egglog import *
from typing import Callable, Unpack
from analysis.utils import raise_

class MyEGraph(EGraph):
    """ Currently, exactly the same as the regular e-graph. Later, will be
    extended with conveniences. """

    def __init__(self, save_egglog_string=True):
        super().__init__(save_egglog_string=save_egglog_string) # type: ignore

    def relation_elements[*TS](self, relation: Callable[[Unpack[TS]], Unit]) \
    -> list[tuple[Unpack[TS]]]:
        # A relation is a partial function to unit, and the elements are the
        # values with mapped outputs. That means that the keys of the
        # following dictionary are expressions containing the relation applied
        # to it's elements.
        appiled_elements = self.function_values(relation).keys()
        return [
            args if (args := get_callable_args(el, relation)) is not None else
                raise_(Exception(
                    f"'{relation}' had a table that contained '{args}', which "
                    f"is not an application of the relation '{relation}'"
                ))
            for el in appiled_elements
        ]
