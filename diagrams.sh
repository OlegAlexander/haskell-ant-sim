# bash diagrams.sh
find . -name '*.hs' ! -path '*dist-newstyle*' ! -name 'Main.hs' | xargs graphmod --quiet --no-cabal --no-cluster --colors=2 | dot -Tpng -Gdpi=300 -o diagrams/graphmod.png
stack dot | dot -Tpng -Gdpi=300 -o diagrams/stackdot.png
stack dot > diagrams/stackdot.dot