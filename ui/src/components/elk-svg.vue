<template lang="pug">
<svg class="elkjs" :width="layout.width" :height="layout.height">
    <rect v-for="node in layout.children" :x="node.x" :y="node.y" :width="node.width" :height="node.height"/>
    <g v-for="edge in layout.edges">
        <path v-for="s in edge.sections" :d="pathForSection(s)">
    </g>
</svg>
</template>

<script lang="ts">
import { Vue, Component, Prop, toNative } from 'vue-facing-decorator';
import type { ElkNode } from 'elkjs';

@Component
class IElkSVG extends Vue {
    @Prop layout: ElkNode

    pt(p: XY) {
        return `${p.x} ${p.y}`
    }

    pathForSection(section: ElkNode['edges'][number]['sections'][number]) {
        let s = section, pt = (p: XY) => this.pt(p);
        if (section.bendPoints)
            return `M ${pt(s.startPoint)} ${[...this.curves(s.bendPoints)].join(' ')} ${pt(s.endPoint)}`;
        else
            return `M ${pt(s.startPoint)} L ${pt(s.endPoint)}`;
    }

    /**
     * Interpret Elk.js bend points (https://stackoverflow.com/a/72577667).
     * Internal points are inserted by computing the midpoints between two
     * consecutive control points.
     * Curves are cubic, except for the last one, which may be quadratic
     * when there is an odd number of bend points.
     */
    *curves(pts: XY[]) {
        for (let idx = 0; idx < pts.length; ) {
            let w = [0,1,2].map(i => pts[idx + i]);
            idx += 2;
            yield w[1] ?
                `C ${this.pt(w[0])}, ${this.pt(w[1])}, ${
                    w[2] ? this.pt(mid(w[1], w[2])) : ''}`
                : `Q ${this.pt(w[0])}, `;
        }
    }
}

type XY = {x: number, y: number};

function mid(pt1: XY, pt2: XY) { return {x: (pt1.x + pt2.x) / 2, y: (pt1.y + pt2.y) / 2}; }

export { IElkSVG }
export default toNative(IElkSVG)
</script>