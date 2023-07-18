use anyhow::Error;
use pom::parser::*;
use serde::Serialize;
use thiserror::Error;
use clap::{Parser as CParser};

use std::io::Read;

#[derive(Debug, Error)]
#[allow(dead_code)]
enum WurstdoktorError {
    #[error("can't use sqlite without the feature enabled")]
    SqliteUnavailable,

    #[error("couldn't decide which format to write")]
    BadOutputArguments,
}

/// Wurstdoktor consumes wurst code via stdin, and produces structured data for
/// the public documentation found via stdout.
#[derive(CParser, Debug)]
#[command(name = "wurstdoktor")]
struct Opt {
    /// Write the parsed contents as YAML (default).
    #[arg(long = "yaml", group = "outfmt")]
    yaml: bool,

    /// Write the parsed contents as SQL commands.
    #[arg(long = "sqlite", group = "outfmt")]
    sqlite: bool,

    /// Write the parsed contents as a sqlite database (needs sqlitedb feature).
    #[arg(long = "sqlitedb", group = "outfmt")]
    sqlitedb: bool,
}

#[derive(Clone, Serialize, Debug, PartialEq)]
struct WurstFnParam {
    typ: String,
    name: String
}

#[derive(Clone, Serialize, Debug, PartialEq)]
struct WurstClass {
    doc: Option<String>,
    abstract_: bool,
    name: String,
    extends: Option<String>,
    implements: Vec<String>,
    fns: Vec<WurstFunction>
}

#[derive(Clone, Serialize, Debug, PartialEq)]
struct WurstInterface {
    doc: Option<String>,
    name: String,
    fns: Vec<WurstFunction>,
}

#[derive(Clone, Serialize, Debug, PartialEq)]
struct WurstEnum {
    doc: Option<String>,
    name: String,
    variants: Vec<String>,
}

#[derive(Serialize, Debug, PartialEq)]
struct WurstPackage {
    doc: Option<String>,
    name: String,
    classes: Vec<WurstClass>,
    enums: Vec<WurstEnum>,
    free_fns: Vec<WurstFunction>,
    interfaces: Vec<WurstInterface>,
}

#[derive(Clone, Serialize, Debug, PartialEq)]
struct WurstFunction {
    doc: Option<String>,
    static_: bool,
    extensor: Option<String>,
    name: String,
    params: Vec<WurstFnParam>,
    returns: Option<String>
}

#[derive(Serialize, Debug, PartialEq)]
enum WurstDok {
    Package(WurstPackage),
    Class(WurstClass),
    Enum(WurstEnum),
    FreeFunction(WurstFunction),
    Interface(WurstInterface),
    Nothing
}

enum FunctionOrClassTemporary {
    Function(WurstFunction),
    Class(WurstClass),
    Enum(WurstEnum),
    Interface(WurstInterface),
}

fn whitespace<'a>() -> Parser<'a, u8, Vec<u8>> {
    one_of(b" \t\n\r").repeat(1..)
}

fn maybe_whitespace<'a>() -> Parser<'a, u8, Vec<u8>> {
    one_of(b" \t\r\n").repeat(0..)
}

fn maybe_endofline_space<'a>() -> Parser<'a, u8, ()> {
    maybe_sep() * maybe_linebreak()
}

fn sep<'a>() -> Parser<'a, u8, Vec<u8>> {
    one_of(b" \t").repeat(1..)
}

fn maybe_sep<'a>() -> Parser<'a, u8, Vec<u8>> {
    one_of(b" \t").repeat(0..)
}

fn linebreak<'a>() -> Parser<'a, u8, ()> {
    sym(b'\r').opt() * sym(b'\n').discard()
}

fn maybe_linebreak<'a>() -> Parser<'a, u8, ()> {
    one_of(b"\n\r").repeat(0..).discard()
}

fn maybe_emptylines<'a>() -> Parser<'a, u8, ()> {
    (
        one_of(b" \t").repeat(0..) *
        linebreak()
    ).repeat(0..).discard()
}

fn empty<'a>() -> Parser<'a, u8, ()> {
    one_of(b" \t").repeat(0..).discard() - linebreak()
}

fn name<'a>() -> Parser<'a, u8, String> {
    none_of(b" (.,\t\r\n").repeat(1..).convert(String::from_utf8)
}

fn name_generics<'a>() -> Parser<'a, u8, String> {
    (
      none_of(b" (.,\t\r\n<").repeat(1..)
      + (
        sym(b'<')
        + (list(is_a(|u: u8| u8::is_ascii_alphanumeric(&u)).repeat(1..) /* call(name_generics) */, sym(b',') + sym(b' ').repeat(0..)))
        + sym(b'>')
      ).opt()
    ).collect().convert(|st| String::from_utf8(st.to_vec()))
}

/*
fn name_parameterised<'a>() -> Parser<'a, u8, String> {
    is_a(u8::is_ascii_alphanumeric).repeat(1..) + (
        sym(b'<') + (
            call(name_parameterised) |
            whitespace().convert(String::from_utf8) |
            seq(b",").convert(|bytes| String::from_utf8(bytes.to_vec()))
        ).repeat(1..) + sym(b'>')
    ).opt()
}
*/

fn indented<'a>() -> Parser<'a, u8, Vec<u8>> {
    (
        seq(b"\t") |
        seq(b"    ")
    ) * none_of(b"\n\r").repeat(1..) - linebreak()
}

fn cruft<'a>() -> Parser<'a, u8, ()> {
    (
        !class_declaration() *
        !free_function() *
        !eenum() *
        !interface() *
        take(1)
    ).discard()
}

fn class_cruft<'a>() -> Parser<'a, u8, ()> {
    (
        !class_declaration() *
        !class_fn() *
        !eenum() *
        !free_function() *
        // !interface() *
        take(1)
    ).discard()
}

fn interface_cruft<'a>() -> Parser<'a, u8, ()> {
    (
        !class_fn() *
        take(1)
    ).discard()
}

static PRIVATE: &'static [u8] = b"private";

fn class_fn<'a>() -> Parser<'a, u8, WurstFunction> {
    (
        doc() -
        maybe_whitespace() +
        (
            (
                (
                    // Only match non-private functions.
                    !seq(PRIVATE) *
                    seq(b"static ").opt().map(
                        |p| p.map(
                            |s| std::str::from_utf8(
                                s
                            ).expect(
                                "Failed to unwrap a UTF8 string"
                            ).to_string()
                        )
                    ) +
                    (
                        (
                            seq(b"function ") *
                            name_generics()
                        ) | seq(b"construct").map(
                            |s| std::str::from_utf8(s).expect(
                                "Failed to unwrap UTF8"
                            ).to_string()
                        )
                    ) -
                    sym(b'(')
                ) +
                call(function_params) -
                sym(b')') +
                (
                    // Return value.
                    maybe_sep() *
                    seq(b"returns ") *
                    none_of(b" ,\n").repeat(1..).convert(String::from_utf8)
                ).opt()
            ) | (
                seq(b"ondestroy").map(
                    |s| std::str::from_utf8(s).expect(
                        "Failed to unwrap UTF"
                    ).to_string()
                ).map(|n| (((None, n), vec![]), None))
            )
        ) -
        maybe_whitespace()
    ).map(|(d, (((s, n), p), r))| {
        WurstFunction {
            doc: d,
            static_: s.is_some(),
            extensor: None,
            name: n,
            params: p,
            returns: r
        }
    })
}

fn class_declaration<'a>() -> Parser<
    'a,
    u8,
    (Option<String>, (((Option<String>, String), Option<String>), Vec<String>))
> {
    doc() + (
        // Only match public classes.
        (
            seq(b"public ") *
            seq(b"abstract ").opt() -
            seq(b"class ")
        ).map(
            |p| p.map(
                |slice| std::str::from_utf8(
                    slice
                ).expect("Couldn't unwrap UTF8 string.").into()
            )
        ) +
        name() +
        (
            sep() *
            seq(b"extends") *
            sep() *
            name() -
            maybe_sep()
        ).opt() +
        (
            sep() *
            seq(b"implements") *
            sep() *
            list(
                name(),
                (sep() | one_of(b",").repeat(1..1)).repeat(1..)
            )
        ).opt().map(|p| p.unwrap_or_else(|| vec![])) -
        sym(b'\n')
    )
}

fn class<'a>() -> Parser<'a, u8, WurstClass> {
    (
        class_declaration() +
        class_cruft().repeat(0..) *
        list(
            call(class_fn),
            call(class_cruft).repeat(1..)
        )
    ).map(|((d, (((a, n), e), i)), f)| WurstClass {
        doc: d,
        abstract_: a.is_some(),
        name: n,
        extends: e,
        implements: i,
        fns: f
    })
}

fn package_declaration<'a>() -> Parser<'a, u8, String> {
    seq(b"package ") *
    name() -
    linebreak()
}

fn package_contents<'a>() -> Parser<
    'a,
    u8,
    (Vec<WurstClass>, Vec<WurstFunction>, Vec<WurstEnum>, Vec<WurstInterface>)
> {
    (
        cruft().repeat(0..) *
        list(
            class().map(|c| FunctionOrClassTemporary::Class(c)) |
            free_function().map(|f| FunctionOrClassTemporary::Function(f)) |
            eenum().map(|e| FunctionOrClassTemporary::Enum(e)) |
            interface().map(|i| FunctionOrClassTemporary::Interface(i)),
            cruft().repeat(0..)
        ) -
        cruft().repeat(0..)
    ).map(
        |members| members.into_iter().fold(
            (vec![], vec![], vec![], vec![]),
            |acc, x| match x {
                FunctionOrClassTemporary::Class(class) => {
                    (
                        acc.0.into_iter().chain(
                            vec![class].into_iter()
                        ).collect(),
                        acc.1,
                        acc.2,
                        acc.3
                    )
                },
                FunctionOrClassTemporary::Function(func) => {
                    (
                        acc.0,
                        acc.1.into_iter().chain(
                            vec![func].into_iter()
                        ).collect(),
                        acc.2,
                        acc.3
                    )
                },
                FunctionOrClassTemporary::Enum(eenum) => {
                    (
                        acc.0,
                        acc.1,
                        acc.2.into_iter().chain(
                            vec![eenum].into_iter()
                        ).collect(),
                        acc.3
                    )
                },
                FunctionOrClassTemporary::Interface(interface) => {
                    (
                        acc.0,
                        acc.1,
                        acc.2,
                        acc.3.into_iter().chain(
                            vec![interface].into_iter()
                        ).collect()
                    )
                }
            }
        )
    )
}

fn package<'a>() -> Parser<'a, u8, WurstPackage> {
    doc() +
    package_declaration() +
    // Contents of a package can be on the same line, or all indented.
    // First, assume contents are indented.  If there are no parsed results,
    // try again without indentation.
    (
        indented() |
        empty().map(|()| vec![])
    ).repeat(0..).map(
        |lines| lines.into_iter().filter(
            |line| line.len() > 0
        ).fold(
            vec![],
            |accum, line| accum.into_iter().chain(
                line.into_iter().chain(vec![b'\n'].into_iter())
            ).collect()
        )
    ) >> |((doc, name), lines)| match lines.len() {
        0 => package_contents().map(
            move |sub_parse| ((doc.clone(), name.clone()), sub_parse)
        ),
        _ => {
            let sub_parse = package_contents().parse(
                &lines
            ).expect("subparse");

            take(0).map(
                move |_nil| ((doc.clone(), name.clone()), sub_parse.clone())
            )
        }
    }.map(|((doc, name), (classes, free_fns, enums, interfaces))| WurstPackage {
        doc,
        name,
        classes,
        free_fns,
        enums,
        interfaces,
    })
}

fn doc<'a>() -> Parser<'a, u8, Option<String>> {
    (
        maybe_emptylines() *
        seq(b"/**") *
        (
            !seq(b"*/") * take(1)
        ).repeat(0..).collect().map(
            |v| std::str::from_utf8(v).expect("UTF8!").trim_matches(
                |c| char::is_whitespace(c) || c == '*'
            ).into()
        ) -
        seq(b"*/") - maybe_endofline_space()
    ).opt()
}

fn function_params<'a>() -> Parser<'a, u8, Vec<WurstFnParam>> {
    maybe_whitespace() *
    list(
        // Parameters.
        none_of(b") ,").repeat(1..).convert(String::from_utf8) -
        sym(b' ').repeat(1..) +
        none_of(b") ,\r\n\t").repeat(1..).convert(String::from_utf8),
        sym(b',') *
        maybe_whitespace()
    ).map(|v| v.into_iter().map(|(t, n)| WurstFnParam {
        typ: t,
        name: n
    }).collect())
}

fn free_function<'a>() -> Parser<'a, u8, WurstFunction> {
    (
        doc() -
        maybe_whitespace() +
        (
            // Only match public free functions.
            seq(b"public function ") *
            // Optionally match extension fns.
            (
                name() - sym(b'.')
            ).opt() +
            name() -
            sym(b'(')
        ) +
        call(function_params) -
        sym(b')') +
        (
            // Return value.
            maybe_sep() *
            seq(b"returns ") *
            none_of(b" ,\n").repeat(1..).convert(String::from_utf8)
        ).opt() -
        maybe_whitespace()
    ).map(|(((d, (e, n)), p), r)| WurstFunction {
        doc: d,
        static_: false,
        extensor: e,
        name: n,
        params: p,
        returns: r
    })
}

fn eenum<'a>() -> Parser<'a, u8, WurstEnum> {
    (
        doc() -
        maybe_whitespace() +
        (
            seq(b"public enum ") *
            none_of(b"\r\n").repeat(1..)
        ).convert(String::from_utf8) -
        whitespace() +
        (
            maybe_sep() *
            none_of(b" \t\r\n/").repeat(1..) -
            one_of(b"\r\n").repeat(1..)
        ).convert(String::from_utf8).repeat(1..) -
        maybe_whitespace()
    ).map(|((d, n), v)| WurstEnum {
        doc: d,
        name: n,
        variants: v,
    })
}

fn interface_body<'a>() -> Parser<'a, u8, Vec<WurstFunction>> {
    interface_cruft().repeat(0..) *
    list(
        call(class_fn),
        call(interface_cruft).repeat(0..)
    )
}

fn interface<'a>() -> Parser<'a, u8, WurstInterface> {
    (
        doc() -
        maybe_whitespace() *
        seq(b"public interface") *
        sep() +
        name_generics() +
        (
            indented() |
            empty().map(|()| vec![])
        ).repeat(0..).map(
            |lines| lines.into_iter().filter(
                |line| line.len() > 0
            ).fold(
                vec![],
                |accum, line| accum.into_iter().chain(
                    line.into_iter().chain(vec![b'\n'].into_iter())
                ).collect()
            )
        ).map(move |lines| {
            interface_body().parse(
                &lines
            ).expect("subparse!")
        })
    ).map(|((d, n), f)| WurstInterface {
        doc: d,
        name: n,
        fns: f
    })
}

fn wurstdok<'a>() -> Parser<'a, u8, WurstDok> {
    package().map(|p| WurstDok::Package(p)) |
    eenum().map(|e| WurstDok::Enum(e)) |
    class().map(|c| WurstDok::Class(c)) |
    interface().map(|i| WurstDok::Interface(i)) |
    free_function().map(|f| WurstDok::FreeFunction(f))
}

fn wurstdoktor<'a>() -> Parser<'a, u8, Vec<WurstDok>> {
    (
        wurstdok() |
        !end() *
        take(1).map(|_| WurstDok::Nothing)
    ).repeat(0..).map(|v| {
        v.into_iter().filter(|w| w != &WurstDok::Nothing).collect()
    })
}

fn main() -> Result<(), Error> {
    let opt = Opt::parse();

    let stdin_buf: String = {
        let mut buf = String::new();

        std::io::stdin().lock().read_to_string(&mut buf).expect(
            "Failed to read stdin!"
        );

        buf
    };

    let result: Result<(), Error> = match (
        opt.yaml, opt.sqlite, opt.sqlitedb
    ) {
        (false, false, true) => {
            #[cfg(feature = "sqlitedb")]
            {
                let connection = rusqlite::Connection::open_in_memory()?;

                connection.execute(
                    "CREATE TABLE functions (id INTEGER PRIMARY KEY, doc TEXT, extensor TEXT, name TEXT, returns TEXT)",
                    rusqlite::NO_PARAMS
                )?;
                connection.execute(
                    "CREATE TABLE classes (doc TEXT, name TEXT, extends TEXT, implements BLOB, fns BLOB)",
                    rusqlite::NO_PARAMS
                )?;
                connection.execute(
                    "CREATE TABLE params (foreign_fn INTEGER, typ TEXT, name TEXT)",
                    rusqlite::NO_PARAMS
                )?;

                for v in wurstdoktor().parse(stdin_buf.as_bytes())?.into_iter() {
                    match v {
                        WurstDok::Class(class) => {
                            connection.execute_named(
                                "INSERT INTO classes (doc, name, extends, implements, fns) VALUES (:doc, :name, :extends, :implements, :fns)",
                                &serde_rusqlite::to_params_named(&class)?.to_slice()
                            )?;
                        },
                        WurstDok::FreeFunction(func) => {
                            connection.execute(
                                "INSERT INTO functions (doc, extensor, name, returns) VALUES (?, ?, ?, ?)",
                                &serde_rusqlite::to_params(
                                    &(func.doc, func.extensor, func.name, func.returns)
                                )?.to_slice()
                            )?;

                            let id = connection.last_insert_rowid();

                            for param in func.params {
                                connection.execute(
                                    "INSERT INTO params (foreign_fn, typ, name) VALUES (?, ?, ?)",
                                    &serde_rusqlite::to_params(
                                        &(id, param.typ, param.name)
                                    )?.to_slice()
                                )?;
                            }
                        },
                        _ => unreachable!(),
                    }
                }

                // Write the database to file using the backup API.
                rusqlite::Connection::backup(
                    &connection,
                    rusqlite::DatabaseName::Main,
                    "./sqlite.db",
                    None
                )?;

                Ok(())
            }
            #[cfg(not(feature = "sqlitedb"))]
            {
                Err(WurstdoktorError::SqliteUnavailable.into())
            }
        },
        (false, true, false) => {
            {
                let mut incr = 1;

                println!("BEGIN TRANSACTION;");
                println!(
                    "CREATE TABLE functions (id INTEGER PRIMARY KEY, doc TEXT, extensor TEXT, name TEXT, returns TEXT);"
                );
                println!(
                    "CREATE TABLE classes (doc TEXT, name TEXT, extends TEXT, implements BLOB, fns BLOB);"
                );
                println!(
                    "CREATE TABLE params (foreign_fn INTEGER, typ TEXT, name TEXT);"
                );

                for v in wurstdoktor().parse(stdin_buf.as_bytes())?.into_iter() {
                    match v {
                        WurstDok::Class(class) => {
                            println!(
                                "INSERT INTO classes (doc, name, extends, implements, fns) VALUES ({}, {}, {}, {:?}, {:?});",
                                class.doc.unwrap_or("NULL".into()),
                                class.name,
                                class.extends.unwrap_or("NULL".into()),
                                class.implements,
                                class.fns
                            );
                        },
                        WurstDok::FreeFunction(func) => {
                            println!(
                                "INSERT INTO functions (doc, extensor, name, returns) VALUES ({}, {}, {}, {});",
                                func.doc.unwrap_or("NULL".into()),
                                func.extensor.unwrap_or("NULL".into()),
                                func.name,
                                func.returns.unwrap_or("NULL".into())
                            );

                            incr += 1;

                            for param in func.params {
                                println!(
                                    "INSERT INTO params (foreign_fn, typ, name) VALUES ({}, {}, {});",
                                    incr, param.typ, param.name
                                );
                            }
                        },
                        _ => unreachable!(),
                    }
                }

                println!("COMMIT;");

                Ok(())
            }
        },
        (_, false, false) => {
            println!(
                "{}",
                serde_yaml::to_string(
                    &wurstdoktor().parse(stdin_buf.as_bytes())?
                )?
            );

            Ok(())
        },
        _ => {
            Err(WurstdoktorError::BadOutputArguments.into())
        }
    };

    match result {
        Err(err) => {
            println!("Error: {}", err);

            Err(err)
        },
        Ok(()) => Ok(()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fv_game_timer() -> Result<(), ()> {
        assert_eq!(
            wurstdoktor().parse(br#"
                package GameTimer
                import NoWurst
                import Basics
                import Timer
                timer gameTimer

                public real currentTime

                public function getElapsedGameTime() returns real
                    return gameTimer.getElapsed()

                init
                    gameTimer = CreateTimer()
                    gameTimer.start(100000, null)
                    CreateTimer().startPeriodic(ANIMATION_PERIOD) ->
                        currentTime += ANIMATION_PERIOD
            "#),
            Ok(vec![
                WurstDok::Package(
                    WurstPackage {
                        doc: None,
                        name: "GameTimer".into(),
                        classes: vec![],
                        enums: vec![],
                        interfaces: vec![],
                        free_fns: vec![
                            WurstFunction {
                                doc: None,
                                static_: false,
                                extensor: None,
                                name: "getElapsedGameTime".into(),
                                params: vec![],
                                returns: Some("real".into())
                            }
                        ]
                    }
                )
            ])
        );

        Ok(())
    }

    #[test]
    fn test_enum() -> Result<(), ()> {
        assert_eq!(
            wurstdoktor().parse(br#"
                /**
                    Hello world!
                */
                public enum Braap
                    _NONE
                    ONE
                    TWO

            "#),
            Ok(vec![
                WurstDok::Enum(
                    WurstEnum {
                        doc: Some("Hello world!".into()),
                        name: "Braap".into(),
                        variants: vec![
                            "_NONE".into(),
                            "ONE".into(),
                            "TWO".into(),
                        ]
                    }
                )
            ])
        );

        Ok(())
    }

    #[test]
    fn test_enum_pkg() -> Result<(), ()> {
        assert_eq!(
            wurstdoktor().parse(br#"
                package Test
                /**
                    Dude
                */
                public enum Q
                    Variant
            "#),
            Ok(vec![
                WurstDok::Package(
                    WurstPackage {
                        doc: None,
                        name: "Test".into(),
                        classes: vec![],
                        free_fns: vec![],
                        interfaces: vec![],
                        enums: vec![
                            WurstEnum {
                                doc: Some("Dude".into()),
                                name: "Q".into(),
                                variants: vec![
                                    "Variant".into(),
                                ]
                            }
                        ]
                    }
                ),
            ])
        );

        Ok(())
    }

    #[test]
    fn test_enum_pkg_with_extension_fn() -> Result<(), ()> {
        assert_eq!(
            wurstdoktor().parse(br#"
                package Test
                /**
                    Dude
                */
                public enum Q
                    Variant

                /**
                    Where's my car?
                */
                public function A.x() returns int
                    return 0

            "#),
            Ok(vec![
                WurstDok::Package(
                    WurstPackage {
                        doc: None,
                        name: "Test".into(),
                        classes: vec![],
                        interfaces: vec![],
                        free_fns: vec![
                            WurstFunction {
                                extensor: Some("A".into()),
                                returns: Some("int".into()),
                                doc: Some("Where's my car?".into()),
                                params: vec![],
                                name: "x".into(),
                                static_: false,
                            }
                        ],
                        enums: vec![
                            WurstEnum {
                                doc: Some("Dude".into()),
                                name: "Q".into(),
                                variants: vec![
                                    "Variant".into(),
                                ]
                            }
                        ]
                    }
                ),
            ])
        );

        Ok(())
    }

    #[test]
    fn test_pkg_two_fns() -> Result<(), ()> {
        assert_eq!(
            wurstdoktor().parse(br#"
/** A test package that bla bla bla. */
package test
public function braap(unit q)
    q.kill()

public function goth(unit g)
    g.remove()
            "#),
            Ok(vec![
                WurstDok::Package(
                    WurstPackage {
                        doc: Some("A test package that bla bla bla.".into()),
                        name: "test".into(),
                        classes: vec![],
                        enums: vec![],
                        interfaces: vec![],
                        free_fns: vec![
                            WurstFunction {
                                doc: None,
                                static_: false,
                                extensor: None,
                                name: "braap".into(),
                                params: vec![WurstFnParam {
                                    typ: "unit".into(),
                                    name: "q".into()
                                }],
                                returns: None
                            },
                            WurstFunction {
                                doc: None,
                                static_: false,
                                extensor: None,
                                name: "goth".into(),
                                params: vec![WurstFnParam {
                                    typ: "unit".into(),
                                    name: "g".into()
                                }],
                                returns: None
                            }
                        ],
                    }
                )
            ])
        );

        Ok(())
    }

    #[test]
    fn test_two_fns() -> Result<(), ()> {
        assert_eq!(
            wurstdoktor().parse(br#"
                /** Okay. */
                public function braap(unit q)
                    q.kill()

                public function goth(unit g)
                    g.remove()
            "#),
            Ok(vec![
                WurstDok::FreeFunction(
                    WurstFunction {
                        doc: Some("Okay.".into()),
                        static_: false,
                        extensor: None,
                        name: "braap".into(),
                        params: vec![WurstFnParam {
                            typ: "unit".into(),
                            name: "q".into()
                        }],
                        returns: None
                    }
                ),
                WurstDok::FreeFunction(
                    WurstFunction {
                        doc: None,
                        static_: false,
                        extensor: None,
                        name: "goth".into(),
                        params: vec![WurstFnParam {
                            typ: "unit".into(),
                            name: "g".into()
                        }],
                        returns: None
                    }
                )
            ])
        );

        Ok(())
    }

    #[test]
    fn test_free_fn() -> Result<(), ()> {
        assert_eq!(
            free_function().parse(br#"
                public function braap(unit q)
            "#),
            Ok(WurstFunction {
                    doc: None,
                    static_: false,
                    extensor: None,
                    name: "braap".into(),
                    params: vec![
                        WurstFnParam {
                            typ: "unit".into(),
                            name: "q".into()
                        }
                    ],
                    returns: None
                }
            )
        );
        Ok(())
    }

    #[test]
    fn test_pubfn() -> Result<(), ()> {
        assert_eq!(
            wurstdoktor().parse(br#"
                public function braap() returns real
            "#),
            Ok(vec![WurstDok::FreeFunction(
                WurstFunction {
                    doc: None,
                    static_: false,
                    extensor: None,
                    name: "braap".into(),
                    params: vec![],
                    returns: Some("real".into())
                }
            )])
        );

        Ok(())
    }

    #[test]
    fn test_hotdoc() -> Result<(), ()> {
        assert_eq!(
            doc().parse(br#"
/**
    Hello world!
*/
            "#),
            Ok(Some("Hello world!".into()))
        );

        Ok(())
    }

    #[test]
    fn test_fn_withdoc_and_extension() -> Result<(), ()> {
        assert_eq!(
            wurstdoktor().parse(br#"
                /**
                    Asdf
                */
                public function unit.braap()
            "#),
            Ok(
                vec![
                    WurstDok::FreeFunction(
                        WurstFunction {
                            doc: Some("Asdf".into()),
                            static_: false,
                            extensor: Some("unit".into()),
                            name: "braap".into(),
                            params: vec![],
                            returns: None
                        }
                    )
                ]
            )
        );

        Ok(())
    }

    #[test]
    fn test_param() -> Result<(), ()> {
        assert_eq!(
            function_params().parse(br#"
                unit man
            "#),
            Ok(
                vec![
                    WurstFnParam {
                        typ: "unit".into(),
                        name: "man".into()
                    }
                ]
            )
        );

        Ok(())
    }

    #[test]
    fn test_params() -> Result<(), ()> {
        assert_eq!(
            function_params().parse(br#"
                unit man,  vec2 billy
            "#),
            Ok(
                vec![
                    WurstFnParam {
                        typ: "unit".into(),
                        name: "man".into()
                    },
                    WurstFnParam {
                        typ: "vec2".into(),
                        name: "billy".into()
                    }
                ]
            )
        );

        Ok(())
    }

    #[test]
    fn test_fn_with_param() -> Result<(), ()> {
        assert_eq!(
            wurstdoktor().parse(br#"
                public function braap(unit man)
            "#),
            Ok(
                vec![
                    WurstDok::FreeFunction(
                        WurstFunction {
                            doc: None,
                            static_: false,
                            extensor: None,
                            name: "braap".into(),
                            params: vec![
                                WurstFnParam {
                                    typ: "unit".into(),
                                    name: "man".into()
                                }
                            ],
                            returns: None
                        }
                    )
                ]
            )
        );

        Ok(())
    }

    #[test]
    fn test_fn_with_param_and_ret() -> Result<(), ()> {
        assert_eq!(
            wurstdoktor().parse(br#"
                public function braap(unit man) returns bool
            "#),
            Ok(
                vec![
                    WurstDok::FreeFunction(
                        WurstFunction {
                            doc: None,
                            static_: false,
                            extensor: None,
                            name: "braap".into(),
                            params: vec![
                                WurstFnParam {
                                    typ: "unit".into(),
                                    name: "man".into()
                                }
                            ],
                            returns: Some("bool".into())
                        }
                    )
                ]
            )
        );

        Ok(())
    }

    #[test]
    fn test_pubfn_withdoc_and_extension_fns() -> Result<(), ()> {
        assert_eq!(
            wurstdoktor().parse(br#"
                /**
                    Hello world!
                */
                public function unit.braap(real x, real y, vec2 vel)
            "#),
            Ok(vec![WurstDok::FreeFunction(
                WurstFunction {
                    doc: Some("Hello world!".into()),
                    static_: false,
                    extensor: Some("unit".into()),
                    name: "braap".into(),
                    params: vec![
                        WurstFnParam {
                            typ: "real".into(),
                            name: "x".into()
                        },
                        WurstFnParam {
                            typ: "real".into(),
                            name: "y".into()
                        },
                        WurstFnParam {
                            typ: "vec2".into(),
                            name: "vel".into()
                        }
                    ],
                    returns: None
                }
            )])
        );

        Ok(())
    }

    #[test]
    fn test_empty_class() -> Result<(), ()> {
        assert_eq!(
            class().parse(br#"
/**
    Foobar!
*/
public class Foobar
    // Hello world.

            "#),
            Ok(WurstClass {
                doc: Some("Foobar!".into()),
                abstract_: false,
                name: "Foobar".into(),
                extends: None,
                implements: vec![],
                fns: vec![]
            })
        );

        Ok(())
    }

    #[test]
    fn test_just_class_fn() -> Result<(), ()> {
        assert_eq!(
            class_fn().parse(br#"
/**
* Own's this callable to prevent it from being freed automatically.
* You must manually `destroy` this object to free it
*/
function own() returns thistype
    owned = true
    return this
            "#),
            Ok(
                WurstFunction {
                    doc: Some("Own's this callable to prevent it from being freed automatically.\n* You must manually `destroy` this object to free it".into()),
                    static_: false,
                    extensor: None,
                    name: "own".into(),
                    params: vec![],
                    returns: Some("thistype".into()),
                }
            )
        );

        Ok(())
    }

    #[test]
    fn test_class_extends() -> Result<(), ()> {
        assert_eq!(
            wurstdoktor().parse(br#"
                public class A extends Number
                    int v
            "#),
            Ok(
                vec![
                    WurstDok::Class(
                        WurstClass {
                            doc: None,
                            abstract_: false,
                            name: "A".into(),
                            extends: Some("Number".into()),
                            implements: vec![],
                            fns: vec![],
                        }
                    ),
                ]
            )
        );

        Ok(())
    }

    #[test]
    fn test_two_classes() -> Result<(), ()> {
        assert_eq!(
            wurstdoktor().parse(br#"
                public class A
                    int v

                public class B
                    bool w
            "#),
            Ok(
                vec![
                    WurstDok::Class(
                        WurstClass {
                            doc: None,
                            abstract_: false,
                            name: "A".into(),
                            extends: None,
                            implements: vec![],
                            fns: vec![],
                        }
                    ),
                    WurstDok::Class(
                        WurstClass {
                            doc: None,
                            abstract_: false,
                            name: "B".into(),
                            extends: None,
                            implements: vec![],
                            fns: vec![],
                        }
                    ),
                ]
            )
        );

        Ok(())
    }

    #[test]
    fn test_class() -> Result<(), ()> {
        assert_eq!(
            wurstdoktor().parse(br#"
/**
* Ownable object
*
* Can be "owned" to prevent being freed with the `.own()` method
* Methods in the Lodash library will attempt to free callables that they are passed
*/
public abstract class Ownable
    static var count = 0
    var owned = false

    construct()
        count += 1

    ondestroy
        count -= 1

    /**
    * Own's this callable to prevent it from being freed automatically.
    * You must manually `destroy` this object to free it
    */
    function own() returns thistype
        owned = true
        return this

    /**
    * Destroys this callable if it has not been owned
    */
    function maybeFree() returns bool
        if not owned
            destroy this
            return true
        return false

/**
* Represents a range of numbers
*/
public class Range extends Ownable
    let incr = 1
    let start = 0
    let finish = INT_MAX
    var curr = start
            "#),
            Ok(vec![
                WurstDok::Class(
                    WurstClass {
                        doc: Some("Ownable object\n*\n* Can be \"owned\" to prevent being freed with the `.own()` method\n* Methods in the Lodash library will attempt to free callables that they are passed".into()),
                        abstract_: true,
                        name: "Ownable".into(),
                        extends: None,
                        implements: vec![],
                        fns: vec![
                            WurstFunction {
                                doc: None,
                                static_: false,
                                extensor: None,
                                name: "construct".into(),
                                params: vec![],
                                returns: None
                            },
                            WurstFunction {
                                doc: None,
                                static_: false,
                                extensor: None,
                                name: "ondestroy".into(),
                                params: vec![],
                                returns: None
                            },
                            WurstFunction {
                                doc: Some("Own's this callable to prevent it from being freed automatically.\n    * You must manually `destroy` this object to free it".into()),
                                static_: false,
                                extensor: None,
                                name: "own".into(),
                                params: vec![],
                                returns: Some("thistype".into()),
                            },
                            WurstFunction {
                                doc: Some("Destroys this callable if it has not been owned".into()),
                                static_: false,
                                extensor: None,
                                name: "maybeFree".into(),
                                params: vec![],
                                returns: Some("bool".into()),
                            },
                        ],
                    }
                ),
                WurstDok::Class(
                    WurstClass {
                        doc: Some("Represents a range of numbers".into()),
                        abstract_: false,
                        name: "Range".into(),
                        extends: Some("Ownable".into()),
                        implements: vec![],
                        fns: vec![],
                    }
                )
            ])
        );

        Ok(())
    }

    #[test]
    fn test_implements() -> Result<(), ()> {
        assert_eq!(
            wurstdoktor().parse(br#"
                public class Zed implements Ownable
                    real v
            "#),
            Ok(
                vec![
                    WurstDok::Class(
                        WurstClass {
                            doc: None,
                            abstract_: false,
                            name: "Zed".into(),
                            extends: None,
                            implements: vec!["Ownable".into()],
                            fns: vec![],
                        }
                    )
                ]
            )
        );

        Ok(())
    }

    #[test]
    fn test_name_generics() -> Result<(), ()> {
        assert_eq!(
            name_generics().parse(br#"Hello<World>"#),
            Ok("Hello<World>".into())
        );

        Ok(())
    }

    #[test]
    fn test_iface_type_params() -> Result<(), ()> {
        assert_eq!(
            wurstdoktor().parse(br#"
package Something
public interface CoolPredicate<K>
    function isCool(Classifier<K> c) returns bool

public interface BiPredicate<K, V>
    function bi(Pair<K, V> p)
            "#),
            Ok(
                vec![
                    WurstDok::Package(
                        WurstPackage {
                            doc: None,
                            name: "Something".into(),
                            enums: vec![],
                            free_fns: vec![],
                            classes: vec![],
                            interfaces: vec![
                                WurstInterface {
                                    doc: None,
                                    name: "CoolPredicate<K>".into(),
                                    fns: vec![
                                        WurstFunction {
                                            doc: None,
                                            extensor: None,
                                            name: "isCool".into(),
                                            static_: false,
                                            params: vec![
                                                WurstFnParam {
                                                    name: "c".into(),
                                                    typ: "Classifier<K>".into(),
                                                }
                                            ],
                                            returns: Some("bool".into()),
                                        }
                                    ],
                                },
                                WurstInterface {
                                    doc: None,
                                    name: "BiPredicate<K, V>".into(),
                                    fns: vec![
                                        WurstFunction {
                                            doc: None,
                                            extensor: None,
                                            name: "bi".into(),
                                            static_: false,
                                            params: vec![
                                                WurstFnParam {
                                                    name: "p".into(),
                                                    typ: "Pair<K, V>".into(),
                                                }
                                            ],
                                            returns: Some("bool".into()),
                                        }
                                    ],
                                },
                            ],
                        }
                    )
                ]
            )
        );

        Ok(())
    }

    #[test]
    fn test_interface() -> Result<(), ()> {
        assert_eq!(
            wurstdoktor().parse(br#"
package Q

/** Hello world */
public interface Boom
    function a()

    function b()

function irrelevant()
    let v = 5
            "#),
            Ok(
                vec![
                    WurstDok::Package(
                        WurstPackage {
                            doc: None,
                            name: "Q".into(),
                            free_fns: vec![],
                            classes: vec![],
                            enums: vec![],
                            interfaces: vec![
                                WurstInterface {
                                    doc: Some("Hello world".into()),
                                    name: "Boom".into(),
                                    fns: vec![
                                        WurstFunction {
                                            doc: None,
                                            static_: false,
                                            extensor: None,
                                            name: "a".into(),
                                            params: vec![],
                                            returns: None
                                        },
                                        WurstFunction {
                                            doc: None,
                                            static_: false,
                                            extensor: None,
                                            name: "b".into(),
                                            params: vec![],
                                            returns: None
                                        }
                                    ]
                                }
                            ],
                        }
                    )
                ]
            )
        );

        Ok(())
    }

    #[test]
    fn test_dialog_real() -> Result<(), ()> {
        assert_eq!(
            wurstdoktor().parse(br#"
package Dialog
import NoWurst
import Wurstunit

/**
	Dialogs are big dialog boxes at the center of the screen. The player can choose one button to click.
	WARNING: Dialogs cannot be displayed at map init! Dialogs pause the game in single player mode!
	In multiplayer mode dialogs do not pause the game, but prevent players, who see the dialog from playing the game.
**/

public function createDialog() returns dialog
	return DialogCreate()

public function dialog.addButton(string buttonText) returns button
	return this.addButton(buttonText, 0)

/** Hotkey: use ASCII numbers of the capital letter. */
public function dialog.addButton(string buttonText, int hotkey) returns button
	return DialogAddButton(this, buttonText, hotkey)

/** Adds a quit button to this dialog. If it is clicked, it ends the game for that player. */
public function dialog.addQuitButton(boolean doScoreScreen, string buttonText) returns button
	return this.addQuitButton(doScoreScreen, buttonText, 0)

/** Adds a quit button to this dialog. If it is clicked, it ends the game for that player.
Hotkey: use ASCII numbers of the capital letter. */
public function dialog.addQuitButton(boolean doScoreScreen, string buttonText, int hotkey) returns button
	return DialogAddQuitButton(this, doScoreScreen, buttonText, hotkey)

/** Removes all buttons from a dialog */
public function dialog.clear()
	DialogClear(this)

public function dialog.destr()
	DialogDestroy(this)

/** Toggles visibility of the dialog for a player. Dialogs are invisible by default
Dialogs cannot be shown at map initialization */
public function dialog.display(player whichPlayer, boolean flag)
	DialogDisplay(whichPlayer, this, flag)

public function dialog.setMessage(string messageText)
	DialogSetMessage(this, messageText)

@Test
function testDialog()
	let dia = createDialog()
	dia..addButton("text")
	..addQuitButton(true, "test")
	..display(Player(0), true)
	..clear()
	..destr()
            "#),
            Ok(
                vec![
                    WurstDok::Package(
                        WurstPackage {
                            doc: None,
                            name: "Dialog".into(),
                            classes: vec![],
                            enums: vec![],
                            interfaces: vec![],
                            free_fns: vec![
                                WurstFunction {
                                    doc: Some("Dialogs are big dialog boxes at the center of the screen. The player can choose one button to click.\n\tWARNING: Dialogs cannot be displayed at map init! Dialogs pause the game in single player mode!\n\tIn multiplayer mode dialogs do not pause the game, but prevent players, who see the dialog from playing the game.".into()),
                                    static_: false,
                                    extensor: None,
                                    name: "createDialog".into(),
                                    params: vec![],
                                    returns: Some("dialog".into()),
                                },
                                WurstFunction {
                                    doc: None,
                                    static_: false,
                                    extensor: Some("dialog".into()),
                                    name: "addButton".into(),
                                    params: vec![WurstFnParam {
                                        typ: "string".into(),
                                        name: "buttonText".into()
                                    }],
                                    returns: Some("button".into()),
                                },
                                WurstFunction {
                                    doc: Some("Hotkey: use ASCII numbers of the capital letter.".into()),
                                    static_: false,
                                    extensor: Some("dialog".into()),
                                    name: "addButton".into(),
                                    params: vec![WurstFnParam {
                                        typ: "string".into(),
                                        name: "buttonText".into()
                                    },WurstFnParam {
                                        typ: "int".into(),
                                        name: "hotkey".into()
                                    }],
                                    returns: Some("button".into()),
                                },
                                WurstFunction {
                                    doc: Some("Adds a quit button to this dialog. If it is clicked, it ends the game for that player.".into()),
                                    static_: false,
                                    extensor: Some("dialog".into()),
                                    name: "addQuitButton".into(),
                                    params: vec![WurstFnParam {
                                        typ: "boolean".into(),
                                        name: "doScoreScreen".into()
                                    },WurstFnParam {
                                        typ: "string".into(),
                                        name: "buttonText".into()
                                    }],
                                    returns: Some("button".into()),
                                },
                                WurstFunction {
                                    doc: Some("Adds a quit button to this dialog. If it is clicked, it ends the game for that player.\nHotkey: use ASCII numbers of the capital letter.".into()),
                                    static_: false,
                                    extensor: Some("dialog".into()),
                                    name: "addQuitButton".into(),
                                    params: vec![WurstFnParam {
                                        typ: "boolean".into(),
                                        name: "doScoreScreen".into()
                                    },WurstFnParam {
                                        typ: "string".into(),
                                        name: "buttonText".into()
                                    },WurstFnParam {
                                        typ: "int".into(),
                                        name: "hotkey".into()
                                    }],
                                    returns: Some("button".into()),
                                },
                                WurstFunction {
                                    doc: Some("Removes all buttons from a dialog".into()),
                                    static_: false,
                                    extensor: Some("dialog".into()),
                                    name: "clear".into(),
                                    params: vec![],
                                    returns: None,
                                },
                                WurstFunction {
                                    doc: None,
                                    static_: false,
                                    extensor: Some("dialog".into()),
                                    name: "destr".into(),
                                    params: vec![],
                                    returns: None,
                                },
                                WurstFunction {
                                    doc: Some("Toggles visibility of the dialog for a player. Dialogs are invisible by default\nDialogs cannot be shown at map initialization".into()),
                                    static_: false,
                                    extensor: Some("dialog".into()),
                                    name: "display".into(),
                                    params: vec![WurstFnParam {
                                        typ: "player".into(),
                                        name: "whichPlayer".into()
                                    },WurstFnParam {
                                        typ: "boolean".into(),
                                        name: "flag".into()
                                    }],
                                    returns: None,
                                },
                                WurstFunction {
                                    doc: None,
                                    static_: false,
                                    extensor: Some("dialog".into()),
                                    name: "setMessage".into(),
                                    params: vec![WurstFnParam {
                                        typ: "string".into(),
                                        name: "messageText".into()
                                    }],
                                    returns: None,
                                },
                            ],
                        }
                    ),
                ]
            )
        );

        Ok(())
    }
}
