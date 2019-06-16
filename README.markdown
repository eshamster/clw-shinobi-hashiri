# clw-shinobi-hashiri

A simple jump game with auto scroll like shinobi using [cl-web-2d-game](https://github.com/eshamster/cl-web-2d-game).

## Usage

```lisp
> (ql:quickload :qlot)
> (qlot:quickload :clw-shinobi-hashiri)
> (clw-shinobi-hashiri:start :port <port number>)
```

After starting, you can access to the game by `http://localhost:<port number>` using a web browser.


## Installation

This project depends on liblaries that are not registered in the quicklisp repository. So I recommend to use [qlot](https://github.com/fukamachi/qlot) to install them.

```bash
# Under a directory managed by quicklisp
$ git clone https://github.com/eshamster/clw-shinobi-hashiri.git
$ cd clw-shinobi-hashiri
$ ros install qlot # if you haven't installed
$ qlot install
```

## Operation

## Screenshots

## Author

* eshamster (hamgoostar@gmail.com)

## Copyright

Copyright (c) 2018 eshamster (hamgoostar@gmail.com)

## License

Licensed under the LLGPL License.
