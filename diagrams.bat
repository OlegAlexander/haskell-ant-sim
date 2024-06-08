calligraphy --stdout-dot  --reverse-dependency-rank --collapse-values --hide-data | dot -Tpng -Gdpi=300 -o diagrams/call_graph.png
calligraphy --stdout-dot  --reverse-dependency-rank --hide-values | dot -Tpng -Gdpi=300 -o diagrams/types.png
calligraphy --stdout-dot  --reverse-dependency-rank --collapse-modules | dot -Tpng -Gdpi=300 -o diagrams/modules.png