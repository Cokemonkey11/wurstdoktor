extern crate pom;
extern crate serde_yaml;

extern crate failure;
#[macro_use] extern crate serde_derive;
#[macro_use] extern crate structopt;

use pom::parser::*;

use structopt::StructOpt;

use std::io::Read;

/// Wurstdoktor consumes wurst code via stdin, and produces structured data for
/// the public documentation found via stdout.
#[derive(StructOpt, Debug)]
#[structopt(name = "wurstdoktor")]
struct Opt { }

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
struct WurstFunction {
    doc: Option<String>,
    extensor: Option<String>,
    name: String,
    params: Vec<WurstFnParam>,
    returns: Option<String>
}

#[derive(Serialize, Debug, PartialEq)]
enum WurstDok {
    Class(WurstClass),
    FreeFunction(WurstFunction),
    Nothing
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
        none_of(b" (,").repeat(1..).convert(String::from_utf8)
    )
}

fn class<'a>() -> Parser<'a, u8, WurstClass> {
    (
        class_declaration() +
        call(cruft).repeat(1..) *
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

fn free_function<'a>() -> Parser<'a, u8, WurstDok> {
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
    ).map(|(((d, (e, n)), p), r)| WurstDok::FreeFunction( WurstFunction {
        doc: d,
        extensor: e,
        name: n,
        params: p,
        returns: r
    }))
}

fn wurstdok<'a>() -> Parser<'a, u8, WurstDok> {
    class().map(|c| WurstDok::Class(c)) | free_function()
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
    let _ = Opt::from_args();

    let stdin_buf: String = {
        let mut buf = String::new();

        std::io::stdin().lock().read_to_string(&mut buf).expect(
            "Failed to read stdin!"
        );

        buf
    };

    println!(
        "{}",
        serde_yaml::to_string(&wurstdoktor().parse(stdin_buf.as_bytes())?)?
    );

    Ok(())
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
                WurstDok::FreeFunction(
                    WurstFunction {
                        doc: None,
                        extensor: None,
                        name: "getElapsedGameTime".into(),
                        params: vec![],
                        returns: Some("real".into())
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
                package test
                public function braap(unit q)
                    q.kill()

                public function goth(unit g)
                    g.remove()
            "#),
            Ok(vec![
                WurstDok::FreeFunction(
                    WurstFunction {
                        doc: None,
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
            Ok(WurstDok::FreeFunction(
                WurstFunction {
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
            ))
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