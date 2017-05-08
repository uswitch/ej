# ej

A tool to convert from EDN to JSON.

## Usage

    $ ej < some.edn > some.json

Note: no command line options (or even filenames) are (currently) accepted.

Output is one record per line.

    $ cat drinks.edn
    {:name "milkshake" :sizes [330 500 1000] :contents {:milk true :alcohol false}}
    {:name "latte" :sizes [125 250] :contents {:milk true :alcohol false}}
    {:name "beer" :sizes [284 568] :contents {:milk false :alcohol true}}
    {:name "white russian" :sizes [100] :contents {:milk true :alcohol true}}
    {:name "water" :sizes [1000] :contents {:milk false :alcohol false}}

    $ ej < drinks.edn
    {"contents":{"alcohol":false,"milk":true},"name":"milkshake","sizes":[330,500,1000]}
    {"contents":{"alcohol":false,"milk":true},"name":"latte","sizes":[125,250]}
    {"contents":{"alcohol":true,"milk":false},"name":"beer","sizes":[284,568]}
    {"contents":{"alcohol":true,"milk":true},"name":"white russian","sizes":[100]}
    {"contents":{"alcohol":false,"milk":false},"name":"water","sizes":[1000]}

### jq

ej works well with [jq](http://stedolan.github.io/jq), a tool for command-line JSON processing.

To get pretty JSON from EDN:

    $ ej < some.edn | jq .

To query EDN from the command line:

    $ cat drinks.edn | ej | jq -r 'select(.contents.milk) | .name'
    milkshake
    latte
    white russian

## Caveats

* EDN tagging information is removed
* Rational numbers are parsed, and become floats in JSON (the EDN spec doesn't mention rationals, but it looks as if it should)
* keywords and symbols as map keys become strings
* other types used as map keys become JSON-encoded strings

## Prerequisites

* The Haskell Stack

    $ curl -sSL https://get.haskellstack.org/ | sh

(Be prepared for a wait)

## Installation

    $ git clone https://github.com/uswitch/ej.git
    $ cd ej
    $ stack setup
    $ stack install

which, if all goes well, will create `~/.local/bin/ej`. It may be a
good idea to put `~/.local/bin` in your `PATH`.

## Motivation

* Needed a fast way to convert streams of EDN to JSON, to get benefit of tools such as jq.
* Clojure takes too long to start for a command line tool.
* There's a Go implementation, but its licence is unclear.
* I'm a Haskell beginner, so I thought this would make a good first (useful) project.
