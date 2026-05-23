import anywidget
import traitlets
from pathlib import Path

class ElkWidget(anywidget.AnyWidget):
    _esm = Path(__file__).parent / "build/widget/widget.js"
    _css = Path(__file__).parent / "build/widget/index.scss.css"

    graph = traitlets.Any([]).tag(sync=True)

    def __init__(self, graph=None):
        super(ElkWidget, self).__init__()

        if graph is not None:
            self.graph = graph