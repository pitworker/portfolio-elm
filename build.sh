rm -r ./dist/*
cp -r ./pub/* ./dist/
elm make ./src/Main.elm --output="dist/main.js"
