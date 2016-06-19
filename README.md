# Sana Compiler Framework

[![Build Status](https://travis-ci.com/amanjpro/languages-a-la-carte.svg?token=xagHpp4qXzfWp5WxM1j3)](https://travis-ci.com/amanjpro/languages-a-la-carte)

Sana is an extensible compiler framework written in Scala. Sana aims to be both
fully modular in a sense that removing a new language component should be as
easy as adding a new one, in a type-safe manner. Sana implements ``languages à
la carte'' which is proposed by Amanj Sherwany and Nate Nystrom. As proof of
concept, Sana implements the compiler for Java 1.0 among other languages.

## Installation

To play with Sana, first clone the project:
```
git clone github.com/amanjpro/languages-a-la-carte
```

Then, using `sbt` you can build the wanted target. For example to build the
`tiny` language, you do:

```
sbt tiny/compile
```

Please consult the project's [wiki](https://github.com/amanjpro/languages-a-la-carte/wiki)
to know all possible targets.

You can generate a fat jar by running assembly for your target, for example for
`tiny` it will be:

```
sbt tiny/assembly
```

To generate ANTLR grammar, you can use antlr4:antlr4Generate task, for primj it will be:

```
sbt primj/antlr4:antlr4Generate
```

There are two sets of tests, the unit tests can be executed in sbt using:
```
sbt tests
```

And the integration tests can run using:
```
./runtests [recompile] (module name or all)
```


## Usage

Please refer to the [wiki](https://github.com/amanjpro/languages-a-la-carte/wiki)

## Contributing

1. Fork it!
2. Join us on `freenode`: #sanacompiler
3. Experiment with the module system of Sana
4. Play with Sana more and implement a language feature on top of an
   existing language.
5. Report bugs
[] (6. Go through [wiki](https://github.com/amanjpro/sana/wiki/Contributing))

## Q/A Forum
[here](https://groups.google.com/d/forum/sana-compiler-framework)


[](## History)


## Credits
see CONTRIBUTERS


## License

```
Copyright (c) <2015-2016>, see CONTRIBUTERS
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the <organization> nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
```
