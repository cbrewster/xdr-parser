use nom::branch::alt;
use nom::bytes::complete::{tag, take_until, take_while};
use nom::character::complete::{alpha1, alphanumeric1, one_of};
use nom::combinator::{map, opt, recognize, value as nom_value};
use nom::error::{ContextError, ParseError};
use nom::multi::{many0, many0_count, many1, separated_list1};
use nom::sequence::{delimited, pair, preceded, separated_pair, tuple};
use nom::IResult;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Identifier<'a>(&'a str);

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Declaration<'a> {
    Regular {
        type_specifier: TypeSpecifier<'a>,
        identifier: Identifier<'a>,
    },
    FixedArray {
        type_specifier: TypeSpecifier<'a>,
        identifier: Identifier<'a>,
        value: Value<'a>,
    },
    VariableArray {
        type_specifier: TypeSpecifier<'a>,
        identifier: Identifier<'a>,
        value: Option<Value<'a>>,
    },
    OpaqueFixedArray {
        identifier: Identifier<'a>,
        value: Value<'a>,
    },
    OpaqueVariableArray {
        identifier: Identifier<'a>,
        value: Option<Value<'a>>,
    },
    String {
        identifier: Identifier<'a>,
        value: Option<Value<'a>>,
    },
    Pointer {
        type_specifier: TypeSpecifier<'a>,
        identifier: Identifier<'a>,
    },
    Void,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Value<'a> {
    Constant(Constant),
    Identifier(Identifier<'a>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Constant(i64);

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TypeSpecifier<'a> {
    Int { unsigned: bool },
    Hyper { unsigned: bool },
    Float,
    Double,
    Quadruple,
    Bool,
    EnumTypeSpec(EnumTypeSpec<'a>),
    StructTypeSpec(StructTypeSpec<'a>),
    UnionTypeSpec(Box<UnionTypeSpec<'a>>),
    Identifier(Identifier<'a>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct EnumTypeSpec<'a> {
    pub enum_body: EnumBody<'a>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct EnumBody<'a> {
    pub variants: Vec<(Identifier<'a>, Value<'a>)>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StructTypeSpec<'a> {
    pub struct_body: StructBody<'a>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StructBody<'a> {
    pub declarations: Vec<Declaration<'a>>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct UnionTypeSpec<'a> {
    pub union_body: UnionBody<'a>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct UnionBody<'a> {
    pub switch_declaration: Declaration<'a>,
    pub case_specs: Vec<CaseDef<'a>>,
    pub default_declaration: Declaration<'a>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct CaseDef<'a> {
    pub values: Value<'a>,
    pub declaration: Declaration<'a>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ConstantDef<'a> {
    pub identifier: Identifier<'a>,
    pub constant: Constant,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TypeDef<'a> {
    TypeDef {
        declaration: Declaration<'a>,
    },
    Enum {
        identifier: Identifier<'a>,
        enum_body: EnumBody<'a>,
    },
    Struct {
        identifier: Identifier<'a>,
        struct_body: StructBody<'a>,
    },
    Union {
        identifier: Identifier<'a>,
        union_body: UnionBody<'a>,
    },
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Definition<'a> {
    TypeDef(TypeDef<'a>),
    ConstantDef(ConstantDef<'a>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Specification<'a> {
    pub definitions: Vec<Definition<'a>>,
}

fn space<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, &'a str, E> {
    take_while(|c| " \t\r\n".contains(c))(i)
}

fn comment<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, &'a str, E> {
    recognize(tuple((tag("/*"), take_until("*/"), tag("*/"))))(i)
}

fn spc<'a, E: ParseError<&'a str> + ContextError<&'a str>>(i: &'a str) -> IResult<&'a str, (), E> {
    // TODO: Figure out a more elegant way to handle comments and whitespace.
    nom_value(
        (),
        tuple((space, many0(delimited(space, comment, space)), space)),
    )(i)
}

fn declaration<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Declaration, E> {
    preceded(
        spc,
        alt((
            map(tag("void"), |_| Declaration::Void),
            declaration_opaque_fixed_array,
            declaration_opaque_variable_array,
            declaration_string,
            declaration_fixed_array,
            declaration_variable_array,
            declaration_pointer,
            declaration_regular,
        )),
    )(i)
}

fn declaration_regular<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Declaration, E> {
    map(tuple((type_specifier, identifier)), |(t, i)| {
        Declaration::Regular {
            type_specifier: t,
            identifier: i,
        }
    })(i)
}

fn declaration_fixed_array<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Declaration, E> {
    map(
        tuple((
            type_specifier,
            identifier,
            delimited(preceded(spc, tag("[")), value, preceded(spc, tag("]"))),
        )),
        |(t, i, v)| Declaration::FixedArray {
            type_specifier: t,
            identifier: i,
            value: v,
        },
    )(i)
}

fn declaration_variable_array<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Declaration, E> {
    map(
        tuple((
            type_specifier,
            identifier,
            delimited(preceded(spc, tag("<")), opt(value), preceded(spc, tag(">"))),
        )),
        |(t, i, v)| Declaration::VariableArray {
            type_specifier: t,
            identifier: i,
            value: v,
        },
    )(i)
}

fn declaration_opaque_fixed_array<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Declaration, E> {
    map(
        tuple((
            preceded(tag("opaque"), identifier),
            delimited(preceded(spc, tag("[")), value, preceded(spc, tag("]"))),
        )),
        |(i, v)| Declaration::OpaqueFixedArray {
            identifier: i,
            value: v,
        },
    )(i)
}

fn declaration_opaque_variable_array<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Declaration, E> {
    map(
        tuple((
            preceded(tag("opaque"), identifier),
            delimited(preceded(spc, tag("<")), opt(value), preceded(spc, tag(">"))),
        )),
        |(i, v)| Declaration::OpaqueVariableArray {
            identifier: i,
            value: v,
        },
    )(i)
}

fn declaration_string<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Declaration, E> {
    map(
        tuple((
            preceded(tag("string"), identifier),
            delimited(preceded(spc, tag("<")), opt(value), preceded(spc, tag(">"))),
        )),
        |(i, v)| Declaration::String {
            identifier: i,
            value: v,
        },
    )(i)
}

fn declaration_pointer<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Declaration, E> {
    map(
        tuple((
            type_specifier,
            preceded(preceded(spc, tag("*")), identifier),
        )),
        |(t, i)| Declaration::Pointer {
            type_specifier: t,
            identifier: i,
        },
    )(i)
}

fn value<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Value, E> {
    preceded(
        spc,
        alt((
            map(constant, Value::Constant),
            map(identifier, Value::Identifier),
        )),
    )(i)
}

fn constant<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Constant, E> {
    preceded(
        spc,
        map(
            recognize(preceded(opt(tag("-")), many1(one_of("0123456789")))),
            |v: &str| Constant(v.parse().expect("failed to parse constant")),
        ),
    )(i)
}

fn identifier<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Identifier, E> {
    preceded(
        spc,
        map(
            recognize(pair(alpha1, many0_count(alt((alphanumeric1, tag("_")))))),
            |ident| Identifier(ident),
        ),
    )(i)
}

fn type_specifier<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, TypeSpecifier, E> {
    preceded(
        spc,
        alt((
            map(tuple(((tag("unsigned")), spc, tag("int"))), |_| {
                TypeSpecifier::Int { unsigned: true }
            }),
            map(tag("int"), |_| TypeSpecifier::Int { unsigned: false }),
            map(tuple(((tag("unsigned")), spc, tag("hyper"))), |_| {
                TypeSpecifier::Hyper { unsigned: true }
            }),
            map(tag("hyper"), |_| TypeSpecifier::Hyper { unsigned: false }),
            map(tag("float"), |_| TypeSpecifier::Float),
            map(tag("double"), |_| TypeSpecifier::Double),
            map(tag("quadruple"), |_| TypeSpecifier::Quadruple),
            map(tag("bool"), |_| TypeSpecifier::Bool),
            map(enum_type_spec, TypeSpecifier::EnumTypeSpec),
            // TODO: enum, struct, union type-specs
            map(identifier, TypeSpecifier::Identifier),
        )),
    )(i)
}

fn enum_type_spec<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, EnumTypeSpec, E> {
    map(
        preceded(spc, preceded(tag("enum"), enum_body)),
        |enum_body| EnumTypeSpec { enum_body },
    )(i)
}

fn enum_body<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, EnumBody, E> {
    map(
        delimited(
            preceded(spc, tag("{")),
            preceded(
                spc,
                separated_list1(
                    tag(","),
                    separated_pair(identifier, preceded(spc, tag("=")), value),
                ),
            ),
            preceded(spc, tag("}")),
        ),
        |variants| EnumBody { variants },
    )(i)
}

#[cfg(test)]
mod test {
    use nom::{error::ErrorKind, Err};

    use super::*;

    #[test]
    fn test_space() {
        assert_eq!(space::<(_, ErrorKind)>("   hi"), Ok(("hi", "   ")));
        assert_eq!(space::<(_, ErrorKind)>("\t hi"), Ok(("hi", "\t ")));
        assert_eq!(space::<(_, ErrorKind)>("\n\t hi"), Ok(("hi", "\n\t ")));
    }

    #[test]
    fn test_comment() {
        assert_eq!(
            comment::<(_, ErrorKind)>("/* some comment */"),
            Ok(("", "/* some comment */"))
        );
        assert_eq!(
            comment::<(_, ErrorKind)>("/* some comment */ somestuff"),
            Ok((" somestuff", "/* some comment */"))
        );
    }

    #[test]
    fn test_value() {
        assert_eq!(
            value::<(_, ErrorKind)>("123"),
            Ok(("", Value::Constant(Constant(123))))
        );
        assert_eq!(
            value::<(_, ErrorKind)>("ident"),
            Ok(("", Value::Identifier(Identifier("ident"))))
        );
    }

    #[test]
    fn test_identifier() {
        assert_eq!(
            identifier::<(_, ErrorKind)>("some_ident"),
            Ok(("", Identifier("some_ident")))
        );
        assert_eq!(
            identifier::<(_, ErrorKind)>("some_ident more"),
            Ok((" more", Identifier("some_ident")))
        );
        assert_eq!(
            identifier::<(_, ErrorKind)>("_invalid"),
            Err(Err::Error(("_invalid", ErrorKind::Alpha)))
        );
        assert_eq!(
            identifier::<(_, ErrorKind)>("123no_number"),
            Err(Err::Error(("123no_number", ErrorKind::Alpha)))
        );
    }

    #[test]
    fn test_constant() {
        assert_eq!(constant::<(_, ErrorKind)>("123"), Ok(("", Constant(123))));
        assert_eq!(constant::<(_, ErrorKind)>("-123"), Ok(("", Constant(-123))));
    }

    #[test]
    fn test_type_specified() {
        assert_eq!(
            type_specifier::<(_, ErrorKind)>("unsigned int"),
            Ok(("", TypeSpecifier::Int { unsigned: true }))
        );
        assert_eq!(
            type_specifier::<(_, ErrorKind)>("int"),
            Ok(("", TypeSpecifier::Int { unsigned: false }))
        );
        assert_eq!(
            type_specifier::<(_, ErrorKind)>("unsigned    hyper"),
            Ok(("", TypeSpecifier::Hyper { unsigned: true }))
        );
        assert_eq!(
            type_specifier::<(_, ErrorKind)>("hyper"),
            Ok(("", TypeSpecifier::Hyper { unsigned: false }))
        );
        assert_eq!(
            type_specifier::<(_, ErrorKind)>("float"),
            Ok(("", TypeSpecifier::Float))
        );
        assert_eq!(
            type_specifier::<(_, ErrorKind)>("double"),
            Ok(("", TypeSpecifier::Double))
        );
        assert_eq!(
            type_specifier::<(_, ErrorKind)>("quadruple"),
            Ok(("", TypeSpecifier::Quadruple))
        );
        assert_eq!(
            type_specifier::<(_, ErrorKind)>("bool"),
            Ok(("", TypeSpecifier::Bool))
        );
        assert_eq!(
            type_specifier::<(_, ErrorKind)>("enum { A = 1, B = 2 }"),
            Ok((
                "",
                TypeSpecifier::EnumTypeSpec(EnumTypeSpec {
                    enum_body: EnumBody {
                        variants: vec![
                            (Identifier("A"), Value::Constant(Constant(1))),
                            (Identifier("B"), Value::Constant(Constant(2)))
                        ]
                    }
                })
            ))
        );
        assert_eq!(
            type_specifier::<(_, ErrorKind)>("some_other_identifier"),
            Ok((
                "",
                TypeSpecifier::Identifier(Identifier("some_other_identifier"))
            ))
        );
    }

    #[test]
    fn test_declaration() {
        assert_eq!(
            declaration::<(_, ErrorKind)>("float my_float"),
            Ok((
                "",
                Declaration::Regular {
                    type_specifier: TypeSpecifier::Float,
                    identifier: Identifier("my_float"),
                }
            ))
        );

        assert_eq!(
            declaration::<(_, ErrorKind)>("float my_floats [ 100 ]"),
            Ok((
                "",
                Declaration::FixedArray {
                    type_specifier: TypeSpecifier::Float,
                    identifier: Identifier("my_floats"),
                    value: Value::Constant(Constant(100)),
                }
            ))
        );

        assert_eq!(
            declaration::<(_, ErrorKind)>("float my_floats < 100 >"),
            Ok((
                "",
                Declaration::VariableArray {
                    type_specifier: TypeSpecifier::Float,
                    identifier: Identifier("my_floats"),
                    value: Some(Value::Constant(Constant(100))),
                }
            ))
        );

        assert_eq!(
            declaration::<(_, ErrorKind)>("float my_floats <  >"),
            Ok((
                "",
                Declaration::VariableArray {
                    type_specifier: TypeSpecifier::Float,
                    identifier: Identifier("my_floats"),
                    value: None,
                }
            ))
        );

        assert_eq!(
            declaration::<(_, ErrorKind)>("opaque mysterious_data [ 100 ]"),
            Ok((
                "",
                Declaration::OpaqueFixedArray {
                    identifier: Identifier("mysterious_data"),
                    value: Value::Constant(Constant(100)),
                }
            ))
        );

        assert_eq!(
            declaration::<(_, ErrorKind)>("opaque mysterious_data < 100 >"),
            Ok((
                "",
                Declaration::OpaqueVariableArray {
                    identifier: Identifier("mysterious_data"),
                    value: Some(Value::Constant(Constant(100))),
                }
            ))
        );

        assert_eq!(
            declaration::<(_, ErrorKind)>("opaque mysterious_data <  >"),
            Ok((
                "",
                Declaration::OpaqueVariableArray {
                    identifier: Identifier("mysterious_data"),
                    value: None,
                }
            ))
        );

        assert_eq!(
            declaration::<(_, ErrorKind)>("string my_string < 100 >"),
            Ok((
                "",
                Declaration::String {
                    identifier: Identifier("my_string"),
                    value: Some(Value::Constant(Constant(100))),
                }
            ))
        );

        assert_eq!(
            declaration::<(_, ErrorKind)>("string my_string <>"),
            Ok((
                "",
                Declaration::String {
                    identifier: Identifier("my_string"),
                    value: None,
                }
            ))
        );

        assert_eq!(
            declaration::<(_, ErrorKind)>("hyper * star"),
            Ok((
                "",
                Declaration::Pointer {
                    type_specifier: TypeSpecifier::Hyper { unsigned: false },
                    identifier: Identifier("star"),
                }
            ))
        );

        assert_eq!(
            declaration::<(_, ErrorKind)>("void"),
            Ok(("", Declaration::Void))
        );
    }
}
