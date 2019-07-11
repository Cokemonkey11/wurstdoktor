A parser for public [wurstlang](https://wurstlang.org) docs.

[Read the published docs here.](https://cokemonkey11.github.io/wurstdoktor/)

# Usage

* `cargo run -- --help`
* `cargo run < file.wurst > out.yaml`
* `find ../WurstStdlib2/ -name '*.wurst' | xargs -I {} bash -c "cargo run < {}"`
* `cargo run -- --sqlite < file.wurst | sqlite3 fromdump.db`
* `cargo run --features "sqlitedb" -- --sqlitedb < file.wurst`


# TODO

- [x] Use [pom](https://crates.io/crates/pom) for parsing
- [x] Parse package-level docs
- [x] Parse public enums
- [x] Parse public classes
- [x] version and publish to crates.io
- [x] Parse public interfaces
- [x] Parse public functions
- [x] Parse document with multiple public elements
- [x] Generate yaml result set
- [x] I/O and help text
- [x] publish parsed stdlib2
- [x] emit sqlite
- [x] emit sqlitedb with feature gate
- [ ] emit json
- [ ] consume published stdlib2 doktor
- [ ] support non-function public class members
- [ ] sample html in lexical order
- [ ] sample html icons could look better
- [ ] sample html don't print "null"
- [ ] sample html indents stuff inside classes
- [ ] sample html better css
- [ ] alt sample html w/o js
- [ ] sample html queryable from IRC
- [ ] sample html link to element
- [ ] publish documentation
