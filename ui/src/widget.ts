import * as Vue from 'vue';
import { default as App, IApp } from './components/app.vue';
import ELK, { ElkNode } from 'elkjs';

import './index.scss';


interface Model {
    get(name: 'graph'): ElkNode & {nodes?: ElkNode['children']}
}

async function render(model: Model, el: HTMLElement) {

    let app = Vue.createApp(App).mount(el) as IApp;

    const elk = new ELK()

    const graph = {
        id: "root",
        layoutOptions: { 'elk.algorithm': 'layered' },
        ...model.get('graph')
    };
    graph.children ??= graph.nodes ?? [];

    app.layout = await elk.layout(graph);
}


export default {
    render({ model, el }) {
        render(model, el);
    }
}