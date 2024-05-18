# HTMX Filter Tree Aggregations PoC

![Meme](doc/htmx-20yearold-meme.png?raw=true "meme image depicting a crying wojak on the left hiding hiding tears beding a smug mask saying 'lol you are going to use twenty year old technology to build modern websites lol maybe for a toy website good luck w/ that lmao', with django, php, rails, htmx logos on the right saying 'Yes.' and a Common Lisp logo in the lower right saying 'no. sixty years old'")

PoC demonstrating a dynamic filtering form with HTMX for a SIP/Mail-Server applicance.

The backend is implemented in Common Lisp, using the 3rd party systems _cl-who_ (HTML generation), _esrap_ (Packrat parsing), _hunchentoot_ (HTTP server), _cl-sqlite_ (SQL database).

## Installation

1. Install [SBCL](http://www.sbcl.org/) (Other Common Lisp implementations such as Clozure or ECL may work, but are untested).
2. Install the [ocicl](https://github.com/ocicl/ocicl/) package manager.

## Deployment

1. Run `ocicl install` to install all dependencies.

## Starting

Run the following command:

```sh
sbcl --disable-debugger \
     --eval '(require :asdf)' \
     --eval '(pushnew (uiop:getcwd) asdf:*central-registry*)' \
     --eval '(asdf:load-system :xfiltertree-server)' \
     --eval '(hunchentoot:start xfiltertree-server:*acceptor*)' \
     --eval '(sleep #xffffffff)'
```

Then access the server at http://localhost:8080

## Systems

### webstr

Provides `webstr:escape` that hex-escapes strings for safe usage in HTML/CSS.
All non-alphanumeric characters _C_ are translated to `_XX_` where `XX = hex(C)`.

`webstr:unescape` unescapes any string produced by `webstr:escape` so that `(webstr:unescape (webstr:escape) S) ≣ S`.

### fql

Parser & grammar definition for a simple equivalency expression language. `(fql:parse-filter string)` parses _string_ into a structured list.

`table.column=x` is parsed into a structured list of the form `((:eq "table.column" "x"))`.

Subtyping clauses are also supported as a special case. `table[type=t].column=x` is parsed into `((:eq "table.column" "x") (:strict-eq "table.type" "t"))`.

### xfiltertree

Defines the classes `node` and `aggregation` where _aggregation extends node_.

Each node has a string _name_ and a list of _children_ nodes.

An aggregation node additional has an associative list _bin_ which associates bin specification string to _fql_ filter strings.

### xfiltertree-html

_Uses xfiltertree, fql, webstr_.

#### Clause Processing

A clause is a list of pairs of the form `((filter . bin) …)` where _filter_ is a _fql_ filter string and _bin_ is an opaque bin designator. The HTML form values are received by the server as such pairs.

`(xfiltertree-html:parse-filter-clauses clauses)` parses `((filter . bin) …)` (where _filter_ is unescaped) into structured lists using `fql:parse-filter`.

The resulting structured list is converted into a hierarchical tree of the form `((column (bin (operator arg0 arg1 …) …) …) …)` using `(xfiltertree-html:sort-filter-clauses structured-clauses)`.

#### Form Generation

`(xfiltertree-html:htmlize node)` returns a string containing a htmx-enabled hypermedia HTML form given a `xfiltertree:node` instance.

`xfiltertree:aggregation` nodes are transformed into `<fieldset>` nodes containing checkbox inputs, allowing for selection of aggregation bin. One checkbox input is generated for each bin. The `name=…` attribute is set to be equivalent to the _webstr:escape_-ed bin _fql_ filter string, and the `value=…` attribute is set to the bin specification string.

`xfiltertree-html:*form-post*` should be bound to the form submission POST endpoint, and defaults to `/`.

If `xfiltertree-html:*form-update*` is bound to _t_ (or any true value), `xfiltertree-html:htmlize` will generate a DOM update payload, where tags may have an additional `hx-oob-swap` property facilitating htmx-driven updates of bin counts with minimal rerendering.

### xfiltertree-bom

Defines factories for creating `xfiltertree` nodes.

`(xfiltertree-bom:consume-aggregation-tree consume)` is a pull-style function which calls _consume_ without arguments for every required aggregation count.

### xfiltertree-sql

_Uses xfiltertree-bom_.

Generates a filter node tree from a _sqlite3_ database using `xfiltertree-bom`.

Provides `(xfiltertree-sql:query-aggregation-tree hier-clauses)` where _hier-clauses_ is a hierarchical clause tree as produced by `xfiltertree-html:sort-filter-clauses`.

`xfiltertree-sql:*db*` should be bound to the sqlite3 database file (this defaults to `db.sqlite3`).

### xfiltertree-server

_Uses xfiltertree-html, xfiltertree-sql_.

Integrates the systems _xfiltertree-html_, _xfiltertree-sql_ and _hunchentoot_ to provide a filter tree server.

`xfiltertree-server:*acceptor*` is the hunchentoot acceptor, with all routes defined. By default, it's configured to listen on _localhost:8080_.

The server may be interactively started with `(hunchentoot:start xfiltertree-server:*acceptor*)`, and stopped with `(hunchentoot:stop xfiltertree-server:*acceptor*)`.

