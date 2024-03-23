build-idris:
  pack build rpg-idris.ipkg

# build source and copy files to dist folder
build: build-idris
  mkdir -p dist
  cp static/* dist
  cp build/exec/rpg.js dist/rpg.js