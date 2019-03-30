extern crate pom;
extern crate serde_yaml;
extern crate structopt;

#[macro_use] extern crate failure;
#[macro_use] extern crate serde_derive;

use pom::parser::*;

use structopt::StructOpt;

use std::io::Read;

#[derive(Debug, Fail)]
#[fail(display = "Wurstdoktor error")]
#[allow(dead_code)]
enum WurstdoktorError {
    #[fail(display = "can't use sqlite without the feature enabled")]
    SqliteUnavailable,

    #[fail(display = "couldn't decide which format to write")]
    BadOutputArguments,
}

/// Wurstdoktor consumes wurst code via stdin, and produces structured data for
/// the public documentation found via stdout.
#[derive(StructOpt, Debug)]
#[structopt(name = "wurstdoktor")]
struct Opt {
    /// Write the parsed contents as YAML (default).
    #[structopt(long = "yaml", group = "outfmt")]
    yaml: bool,

    /// Write the parsed contents as SQL commands.
    #[structopt(long = "sqlite", group = "outfmt")]
    sqlite: bool,

    /// Write the parsed contents as a sqlite database (needs sqlitedb feature).
    #[structopt(long = "sqlitedb", group = "outfmt")]
    sqlitedb: bool,
}

#[derive(Serialize, Debug, PartialEq)]
struct WurstFnParam {
    typ: String,
    name: String
}

#[derive(Serialize, Debug, PartialEq)]
struct WurstClass {
    doc: Option<String>,
    name: String,
    extends: Option<String>,
    implements: Vec<String>,
    fns: Vec<WurstFunction>
}

#[derive(Serialize, Debug, PartialEq)]
struct WurstPackage {
    doc: Option<String>,
    name: String,
    classes: Vec<WurstClass>,
    free_fns: Vec<WurstFunction>,
}

#[derive(Serialize, Debug, PartialEq)]
struct WurstFunction {
    doc: Option<String>,
    extensor: Option<String>,
    name: String,
    params: Vec<WurstFnParam>,
    returns: Option<String>
}

#[derive(Serialize, Debug, PartialEq)]
enum WurstDok {
    Package(WurstPackage),
    Class(WurstClass),
    FreeFunction(WurstFunction),
    Nothing
}

enum FunctionOrClassTemporary {
    Function(WurstFunction),
    Class(WurstClass),
}

fn cruft<'a>() -> Parser<'a, u8, ()> {
    (
        !class_declaration() *
        !free_function() *
        take(1)
    ).discard()
}

static PRIVATE: &'static [u8] = b"private";

fn class_fn<'a>() -> Parser<'a, u8, WurstFunction> {
    (
        doc() + (
            // Only match non-private free functions.
            !seq(PRIVATE) *
            seq(b"function ") *
            // Optionally match extension fns.
            (
                none_of(b" (.").repeat(1..) - sym(b'.')
            ).convert(String::from_utf8).opt() +
            none_of(b" (.").repeat(1..).convert(String::from_utf8) -
            sym(b'(')
        ) + call(function_params) - sym(b')') + (
            // Return value.
            seq(b" returns ") *
            none_of(b" ,\n").repeat(1..).convert(String::from_utf8)
        ).opt()
    ).map(|(((d, (e, n)), p), r)| {
        WurstFunction {
            doc: d,
            extensor: e,
            name: n,
            params: p,
            returns: r
        }
    })
}

fn class_declaration<'a>() -> Parser<'a, u8, (Option<String>, String)> {
    doc() + (
        // Only match public classes.
        seq(b"public class ") *
        none_of(b" (,").repeat(1..).convert(String::from_utf8) -
        sym(b'\n')
    )
}

fn class<'a>() -> Parser<'a, u8, WurstClass> {
    (
        class_declaration() +
        cruft().repeat(0..) *
        list(
            call(class_fn),
            call(cruft).repeat(1..)
        ) - call(cruft).repeat(0..)
    ).map(|((d, n), f)| WurstClass {
        doc: d,
        name: n,
        extends: None,
        implements: vec![],
        fns: f
    })
}

fn package_declaration<'a>() -> Parser<'a, u8, String> {
    seq(b"package ") *
    none_of(b" (,\n\r").repeat(1..).convert(String::from_utf8) -
    sym(b'\n')
}

fn package<'a>() -> Parser<'a, u8, WurstPackage> {
    (
        doc() + (
            package_declaration() +
            cruft().repeat(0..) *
            list(
                class().map(|c| FunctionOrClassTemporary::Class(c)) |
                free_function().map(|f| FunctionOrClassTemporary::Function(f)),
                cruft().repeat(0..)
            ) - cruft().repeat(0..)
        )
    ).map(|(doc, (package_name, members))| {
        let (classes, fns) = members.into_iter().fold(
            (vec![], vec![]),
            |acc, x| match x {
                FunctionOrClassTemporary::Class(class) => {
                    (
                        acc.0.into_iter().chain(
                            vec![class].into_iter()
                        ).collect(),
                        acc.1
                    )
                },
                FunctionOrClassTemporary::Function(func) => {
                    (
                        acc.0,
                        acc.1.into_iter().chain(
                            vec![func].into_iter()
                        ).collect()
                    )
                },
            }
        );
        WurstPackage {
            doc: doc,
            name: package_name,
            classes: classes,
            free_fns: fns,
        }
    })
}

fn doc<'a>() -> Parser<'a, u8, Option<String>> {
    (
        one_of(b" \n\t\r").repeat(0..) *
        seq(b"/**") *
        one_of(b" \n\t\r").repeat(0..) *
        (
            one_of(b" \n\t\r").repeat(0..).convert(String::from_utf8) +
            none_of(b" \n\r\t*/").repeat(1..).convert(String::from_utf8)
        ).repeat(0..).map(|v| {
            v.into_iter().map(|(w, t)| format!("{}{}", w, t)).collect()
        }).map(|v: Vec<String>| v.join("")) -
        one_of(b" \n\t\r").repeat(0..) *
        seq(b"*/") -
        one_of(b" \n\t\r").repeat(0..)
    ).opt()
}

fn function_params<'a>() -> Parser<'a, u8, Vec<WurstFnParam>> {
    one_of(b" \t\r\n").repeat(0..) *
    list(
        // Parameters.
        none_of(b") ,").repeat(1..).convert(String::from_utf8) -
        sym(b' ').repeat(1..) +
        none_of(b") ,\r\n\t").repeat(1..).convert(String::from_utf8),
        sym(b',') *
        one_of(b" \r\t\n").repeat(0..)
    ).map(|v| v.into_iter().map(|(t, n)| WurstFnParam {
        typ: t,
        name: n
    }).collect())
}

fn free_function<'a>() -> Parser<'a, u8, WurstFunction> {
    (
        doc() -
        one_of(b"\r\n\t ").repeat(0..) +
        (
            // Only match public free functions.
            seq(b"public function ") *
            // Optionally match extension fns.
            (
                none_of(b" (.").repeat(1..) - sym(b'.')
            ).convert(String::from_utf8).opt() +
            none_of(b" (.").repeat(1..).convert(String::from_utf8) -
            sym(b'(')
        ) +
        call(function_params) -
        sym(b')') +
        (
            // Return value.
            one_of(b"\t ").repeat(0..) *
            seq(b"returns ") *
            none_of(b" ,\n").repeat(1..).convert(String::from_utf8)
        ).opt() -
        one_of(b" \r\t\n").repeat(0..)
    ).map(|(((d, (e, n)), p), r)| WurstFunction {
        doc: d,
        extensor: e,
        name: n,
        params: p,
        returns: r
    })
}

fn wurstdok<'a>() -> Parser<'a, u8, WurstDok> {
    package().map(|p| WurstDok::Package(p)) |
    class().map(|c| WurstDok::Class(c)) |
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

fn main() -> Result<(), failure::Error> {
    let opt = Opt::from_args();

    let stdin_buf: String = {
        let mut buf = String::new();

        std::io::stdin().lock().read_to_string(&mut buf).expect(
            "Failed to read stdin!"
        );

        buf
    };

    let result: Result<(), failure::Error> = match (
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
                        free_fns: vec![
                            WurstFunction {
                                doc: None,
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
    fn test_pkg_two_fns() -> Result<(), ()> {
        assert_eq!(
            wurstdoktor().parse(br#"
                /** A test package that fucks the what. */
                package test
                public function braap(unit q)
                    q.kill()

                public function goth(unit g)
                    g.remove()
            "#),
            Ok(vec![
                WurstDok::Package(
                    WurstPackage {
                        doc: Some("A test package that fucks the what.".into()),
                        name: "test".into(),
                        classes: vec![],
                        free_fns: vec![
                            WurstFunction {
                                doc: None,
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
    fn hotdoc() -> Result<(), ()> {
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
}