cabal clean
cabal build all
calligraphy --stdout-dot  --reverse-dependency-rank --exclude "app*" --collapse-values --hide-data | dot -Tpng -Gdpi=300 -o diagrams/call_graph.png
calligraphy --stdout-dot  --reverse-dependency-rank --exclude "app*" --hide-values | dot -Tpng -Gdpi=300 -o diagrams/types.png
calligraphy --stdout-dot  --reverse-dependency-rank --exclude "app*" --collapse-modules | dot -Tpng -Gdpi=300 -o diagrams/modules.png