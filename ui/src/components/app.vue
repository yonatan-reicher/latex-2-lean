<template>
    <div>
        <elk-svg :layout="layout"/>
    </div>
</template>

<script lang="ts">
import { Vue, Component, toNative } from 'vue-facing-decorator';
import ElkSVG from './elk-svg.vue';

@Component({
    components: { 'elk-svg': ElkSVG }
})
class IApp extends Vue {
    layout: any = {children: []}
}

export { IApp }
export default toNative(IApp)

</script>