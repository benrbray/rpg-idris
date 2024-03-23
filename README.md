# `rpg-idris`

## Dependencies

* Install `idris2`
* Install [`idris2-pack`](https://github.com/stefan-hoeck/idris2-pack)
* (optional) Install [`just`](https://github.com/casey/just)

## Build

The `justfile` contains a `build` command which compiles the Idris source to JavaScript and collects the relevant output files in the `dist` folder.

```
just build
```

After the build has succeeded, open `dist/index.html` in a browser to view the result.