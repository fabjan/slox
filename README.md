# slox

This is a Scala implementation of the Lox interpreter from
http://craftinginterpreters.com/

It's a pretty straightforward port of the Java code, but maybe I'll get to go
back and make it more functional later.

## Dependencies

This project is built with Mill:
https://com-lihaoyi.github.io/mill/mill/Intro_to_Mill.html#_installation

## Running

To start the lox repl run:

```shell
mill slox.run
```

To execute a lox script run:

```shell
mill slox.run scriptname.lox
```

To build a fat jar (deployable without Scala etc.) run:

```shell
./build_jar.sh
```

## License

MIT, see [LICENSE](./LICENSE). Based on the MIT licensed code by munificent in
https://github.com/munificent/craftinginterpreters

## TODO

- [x] chapter 4
- [ ] chapter 5
- [ ] chapter 6
- [ ] chapter 7
- [ ] chapter 8
- [ ] chapter 9
- [ ] chapter 10
- [ ] chapter 11
- [ ] chapter 12
- [ ] chapter 13
