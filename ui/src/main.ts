import _ from 'lodash';
import * as Vue from 'vue';
import ELK, { ElkNode } from 'elkjs';

import './index.scss';
import App, { IApp } from './components/app.vue';


async function main() {

    let app = Vue.createApp(App).mount('#app') as IApp;

    const elk = new ELK()

    const graph = {
        id: "root",
        layoutOptions: { 'elk.algorithm': 'layered' },
        'children': _.range(10).map(i =>
            ({'id': `n${i}`, 'width': 30, 'height': 30})),
        'edges': [
            {'id': "e0", 'sources': ["n0"], 'targets': ["n1"]},
            {'id': "e1", 'sources': ["n1"], 'targets': ["n2"]},
            {'id': "e2", 'sources': ["n1"], 'targets': ["n3"]},
            {'id': "e3", 'sources': ["n2"], 'targets': ["n4"]},
            {'id': "e4", 'sources': ["n4"], 'targets': ["n5"]},
            {'id': "e5", 'sources': ["n4"], 'targets': ["n6"]},
            {'id': "e6", 'sources': ["n3"], 'targets': ["n7"]},
            {'id': "e7", 'sources': ["n7"], 'targets': ["n8"]},
            {'id': "e8", 'sources': ["n8"], 'targets': ["n9"]},
            {'id': "e9", 'sources': ["n5"], 'targets': ["n6"]},
            {'id': "e10", 'sources': ["n6"], 'targets': ["n9"]},
        ]
    }

    app.layout = await elk.layout(graph);

    Object.assign(window, { app })
}

document.addEventListener('DOMContentLoaded', main);
