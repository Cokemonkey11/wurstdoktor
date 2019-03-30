A parser for public [wurstlang](https://wurstlang.org) docs.

# Usage

* `cargo run -- --help`
* `cargo run < file.wurst > out.yaml`
* `find ../WurstStdlib2/ -name '*.wurst' | xargs -I {} bash -c "cargo run < {}"`
* `cargo run -- --sqlite < file.wurst | sqlite3 fromdump.db`
* `cargo run --features "sqlitedb" -- --sqlitedb < file.wurst`


# TODO

- [x] Use [pom](https://crates.io/crates/pom) for parsing
- [x] Parse package-level docs
- [ ] Parse public classes
- [ ] version and publish to crates.io
- [ ] Parse public interfaces
- [x] Parse public functions
- [x] Parse document with multiple public elements
- [x] Generate yaml result set
- [x] I/O and help text
- [x] publish parsed stdlib2
- [x] emit sqlite
- [x] emit sqlitedb with feature gate
- [ ] emit json
- [ ] consume published stdlib2 doktor
