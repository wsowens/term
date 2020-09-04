# Examples
Hi! Thanks for checking out this project.
I hope that one day, Elm will allow me to package these terminal emulators with websockets, so that you can immediately connect to a websocket server and be on with your day.

If that's what you're interested in, check out [ws-term](https://github.com/wsowens/wsterm).
Some of the [examples in that repo](https://github.com/wsowens/wsterm/tree/master/examples) might also be illustrative.

Otherwise, let's play with an Elm example in this folder.
To compile the example to JavaScript, run:
```sh
elm make Main.elm --output="example.js"
```

With that done, load [example.html](./example.html) in your browser.
Note that [example.html](./example.html) requires [example.css](./example.css) to make everything work properly.
I recommend looking at both of these files, since `Term` does not work without proper css.
Feel free to tweak and remix `example.css` to make your own color schemes.
For instance, here is my attempt at a [solarized colorscheme](https://github.com/wsowens/wsterm/blob/master/examples/solarized.css).
