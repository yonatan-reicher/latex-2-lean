
Some UI components.

## Jupyter Lab Widget for dependency graphs

Can be used to visualize stuff. Not sure what yet. This is a playground.

To use the widget in Jupyter Lab:
 * Install the lab extention "anywidget" (`pip install anywidget`. I noticed that
   this does not work as a global install, at least in Homebrew's sandbox, but it
   works if you run Jupyter in a venv.)
 * `kremlin --esm -o build/widget src/index.ts`.

   A small patch in `elkjs` is needed because of an oversight in Kremlin.
   In `node_modules/elkjs/lib/elk-api.js`, line 209 -
   ```js
   var ELK = require('./elk-api.js')["default"]; /** @kremlin.bundled */
   ```
 * Load it like this:
    ```py
    from widgets import ElkWidget

    ElkWidget({
        'nodes': [ ... ],
        'edges': [ ... ]
    })
    ```
