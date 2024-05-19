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
     --eval '(pushnew (uiop:getcwd) asdf:*central-registry*)' \
     --eval '(asdf:load-system :xfiltertree-server)' \
     --eval '(hunchentoot:start xfiltertree-server:*acceptor*)' \
     --eval '(sleep #xffffffff)'
```

Then access the server at http://localhost:8080

## Systems

### WEBSTR

Provides `WEBSTR:ESCAPE` that hex-escapes strings for safe usage in HTML/CSS.
All non-alphanumeric characters _C_ are translated to `_XX_` where `XX = hex(C)`.

`WEBSTR:UNESCAPE` unescapes any string produced by `WEBSTR:ESCAPE` so that `(WEBSTR:UNESCAPE (WEBSTR:ESCAPE) S) ≣ S`.

### EQVALG

Equivalence algebra system. Provides `EQUALITY` `MEMBERSHIP` and `COJUNCTION` types to represent terms in AND conjunctions of equalities. `COLUMN` is provided as an abstract variable type representing a table column.

A `COALESCE(X,Y)` operator is provided to combine terms _X_ and _Y_, which can be instances of any of the aforementioned term types.

### FQL

_Uses EQVALG._

Parser & grammar definition for a simple equivalency expression language. `(FQL:PARSE-FILTER (STRING))` parses _STRING_ into an _EQVALG_ term object.

`table.column=x` is parsed into an `EQVALG:EQUALITY` object.

Subtyping clauses are also supported as a special case. `table[type=t].column=x` is parsed into `(EQVALG:CONJUNCTION (EQVALG:EQUALITY (EQVALG:COLUMN table type) t) (EQVALG:EQUALITY (EQVALG:COLUMN table column) x))`.

### XFILTERTREE

Defines the classes `NODE` and `AGGREGATION` where _AGGREGATION extends NODE_.

Each _NODE_ has an _ID_ and a list of _CHILDREN_ nodes.

An aggregation has an additional associative list _BINS_ which is a list of the form `((BIN-ID (AGG-1) (AGG-2) …) …)`. Note that the _CDR_ of `(AGG-N)` is mutable by convention and may be set by processing functions to the aggregation count.

A `DYNAMIC` node extends `AGGREGATION` with _QUERIER_ and _SEARCHER_ properties, which are conventionally set to URIs that implement aggregation querying and DWIM searching for the dynamic node's resource types.

### XFILTERTREE-EQVALG

_Uses XFILTERTREE, EQVALG._

Provides `(XFILTERTREE-EQVALG:CONSTRAIN (TREE CONSTRAINTS)` that mutates _TREE_ by coalescing all its _XFILTERTREE:AGGREGATION_ nodes' aggregation predicates with the given constraints.

Provides `(XFILTERTEE-EQVALG:EXTEND (TREE DYNAMIC))` that mutates _TREE_ by extending each _XFILTERTREE:DYNAMIC_ node within the _TREE_ with aggregation clauses from _DYNAMIC_ that are deemed _compatible_ with it. A _compatible_ aggregation clause has a subject table that matches the _DYNAMIC_ node's ID.

### XFILTERTREE-HTML

_Uses XFILTERTREE, WEBSTR._

HTML/Hypermedia rendering system for _XFILTERTREE_ trees.

`(XFILTERTREE-HTML:HTMLIZE (NODE))` returns a string containing a htmx-enabled hypermedia HTML form given a `XFILTERTREE:NODE` instance.

`XFILTERTREE-HTML:*TRANSLATE*` is a dynamic parameter used to stringify _NODE_ IDs and aggregation bin IDs into a string representation. This is used to facilitate conversion of custom ID types. This defaults to `#'IDENTITY` if not otherwise bound.

`XFILTERTREE:AGGREGATION` nodes are transformed into `<fieldset>` nodes containing checkbox inputs, allowing for selection of aggregation bin. One checkbox input is generated for each bin. The `value=…` HTML attribute is set to be equivalent to the _WEBSTR:ESCAPE_-ed `*TRANSLATE*`-transformed bin ID, and the HTML `name=…` attribute is set to `clause`.

`XFILTERTREE-HTML:*FORM-POST*` should be bound to the form submission POST endpoint, and defaults to `/` if not bound otherwise.

If `XFILTERTREE-HTML:*FORM-UPDATE*` is bound to _T_ (or any generalized true value), `XFILTERTREE-HTML:HTMLIZE` will generate a DOM update payload, where tags may have an additional `hx-oob-swap` property facilitating htmx-driven updates of bin counts with minimal rerendering.

### XFILTERTREE-BOM

_Uses XFILTERTREE, EQVALG._

Defines factories for creating `XFILTERTREE` nodes.

`XFILTERTREE:MAKE-TREE` is a factory that generates a predefined filter tree.

### XFILTERTREE-SQL

_Uses XFILTERTREE, EQVALG-SQL._

Generates a filter node tree from a _sqlite3_ database using `EQVALG-SQL`.

Provides `(XFILTERTREE:COMPUTE-AGGREGATIONS (TREE))` which mutates the _CDR_ of aggregation pairs in all _AGGREGATION_ nodes in tree, setting the aggregation count for each bin by querying an SQL database.

`XFILTERTREE-SQL:*DB*` should be bound to the sqlite3 database file (this defaults to `db.sqlite3`).

### XFILTERTREE-SERVER

_Uses FQL, XFILTERTREE-EQVALG, XFILTERTREE-BOM, XFILTERTREE-HTML, XFILTERTREE-SQL._

Integrates the systems _XFILTERTREE-HTML_, _XFILTERTREE-SQL_ and _HUNCHENTOOT_ to provide a filter tree server.

`XFILTERTREE-SERVER:*ACCEPTOR*` is the hunchentoot acceptor, with all routes defined. By default, it's configured to listen on _localhost:8080_.

The server may be interactively started with `(HUNCHENTOOT:START XFILTERTREE-SERVER:*ACCEPTOR*)`, and stopped with `(HUNCHENTOOT:STOP XFILTERTREE-SERVER:*ACCEPTOR*)`.

