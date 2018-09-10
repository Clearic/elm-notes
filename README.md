# Notes app written in Elm

This is my Elm learning project. It is not intended to be something serious or useful, I wrote it to just try out and learn Elm.

## Build Instructions

Run the following commands:

```sh
yarn install
yarn run build # create production build
```

Then open `build/index.html` in your browser.

## Additional build commands

```sh
yarn run js:d # build elm in devlopment mode
yarn run js:p # build elm in production mode
yarn run js:w # watch elm files and rebuild whenever they change

yarn run css:d # build css in development mode
yarn run css:p # build css in production mode
yarn run css:w # watch css files and rebuild whenever they change

yarn run build:d # build everything in development mode
```