use nom::branch::alt;
use nom::bytes::complete::{tag, take_until, take_while};
use nom::character::complete::{alpha1, alphanumeric1, one_of};
use nom::combinator::{map, opt, recognize, value as nom_value};
use nom::error::{context, ContextError, ErrorKind, ParseError};
use nom::multi::{many0, many0_count, many1, separated_list1};
use nom::sequence::{delimited, pair, preceded, separated_pair, terminated, tuple};
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
    Constant(Constant<'a>),
    Identifier(Identifier<'a>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Constant<'a>(&'a str);

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TypeSpecifier<'a> {
    Int { unsigned: bool },
    Hyper { unsigned: bool },
    Float,
    Double,
    Quadruple,
    Bool,
    Void,
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
    pub default_declaration: Option<Declaration<'a>>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct CaseDef<'a> {
    pub values: Vec<Value<'a>>,
    pub declaration: Declaration<'a>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ConstantDef<'a> {
    pub identifier: Identifier<'a>,
    pub constant: Constant<'a>,
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
    ProgramDef(ProgramDef<'a>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Specification<'a> {
    pub definitions: Vec<Definition<'a>>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ProgramDef<'a> {
    pub identifier: Identifier<'a>,
    pub version_defs: Vec<VersionDef<'a>>,
    pub constant: Constant<'a>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct VersionDef<'a> {
    pub identifier: Identifier<'a>,
    pub procedure_defs: Vec<ProcedureDef<'a>>,
    pub constant: Constant<'a>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ProcedureDef<'a> {
    pub type_specifier: TypeSpecifier<'a>,
    pub identifier: Identifier<'a>,
    pub type_specifiers: Vec<TypeSpecifier<'a>>,
    pub constant: Constant<'a>,
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
    context(
        "declaration",
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
        ),
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
    context(
        "value",
        preceded(
            spc,
            alt((
                map(constant, Value::Constant),
                map(identifier, Value::Identifier),
            )),
        ),
    )(i)
}

fn constant<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Constant, E> {
    context(
        "constant",
        preceded(
            spc,
            alt((
                map(
                    recognize(tuple((
                        opt(tag("-")),
                        alt((tag("0x"), tag("0X"))),
                        many1(one_of("0123456789abcdefABCDEF")),
                    ))),
                    Constant,
                ),
                map(
                    recognize(tuple((
                        opt(tag("-")),
                        alt((tag("0o"), tag("0O"))),
                        many1(one_of("01234567")),
                    ))),
                    Constant,
                ),
                map(
                    recognize(preceded(opt(tag("-")), many1(one_of("0123456789")))),
                    Constant,
                ),
            )),
        ),
    )(i)
}

fn identifier<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Identifier, E> {
    let res = context(
        "identifier",
        preceded(
            spc,
            map(
                recognize(pair(alpha1, many0_count(alt((alphanumeric1, tag("_")))))),
                |ident| Identifier(ident),
            ),
        ),
    )(i);

    // TODO: Figure out a more nom-esque way to do this.
    if let Ok((_, ident)) = &res {
        if [
            "bool",
            "case",
            "const",
            "default",
            "double",
            "quadruple",
            "enum",
            "int",
            "float",
            "hyper",
            "opaque",
            "string",
            "struct",
            "switch",
            "typedef",
            "union",
            "unsigned",
            "void",
            "program",
            "version",
        ]
        .contains(&ident.0)
        {
            return Err(nom::Err::Error(E::from_error_kind(i, ErrorKind::Not)));
        }
    }

    res
}

fn type_specifier<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, TypeSpecifier, E> {
    context(
        "type_specifier",
        preceded(
            spc,
            alt((
                map(identifier, TypeSpecifier::Identifier),
                map(tuple(((tag("unsigned")), spc, tag("int"))), |_| {
                    TypeSpecifier::Int { unsigned: true }
                }),
                map(tag("int"), |_| TypeSpecifier::Int { unsigned: false }),
                map(tuple(((tag("unsigned")), spc, tag("hyper"))), |_| {
                    TypeSpecifier::Hyper { unsigned: true }
                }),
                map(tag("hyper"), |_| TypeSpecifier::Hyper { unsigned: false }),
                // TODO: This doesn't seem spec-compliant but, NFSv4 spec uses this...
                map(tag("unsigned"), |_| TypeSpecifier::Int { unsigned: true }),
                map(tag("float"), |_| TypeSpecifier::Float),
                map(tag("double"), |_| TypeSpecifier::Double),
                map(tag("quadruple"), |_| TypeSpecifier::Quadruple),
                map(tag("bool"), |_| TypeSpecifier::Bool),
                map(tag("void"), |_| TypeSpecifier::Void),
                map(enum_type_spec, TypeSpecifier::EnumTypeSpec),
                map(struct_type_spec, TypeSpecifier::StructTypeSpec),
                map(union_type_spec, |u| {
                    TypeSpecifier::UnionTypeSpec(Box::new(u))
                }),
            )),
        ),
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
    context(
        "enum_body",
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
        ),
    )(i)
}

fn struct_type_spec<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, StructTypeSpec, E> {
    map(
        preceded(spc, preceded(tag("struct"), struct_body)),
        |struct_body| StructTypeSpec { struct_body },
    )(i)
}

fn struct_body<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, StructBody, E> {
    context(
        "struct_body",
        map(
            delimited(
                preceded(spc, tag("{")),
                many1(terminated(declaration, preceded(spc, tag(";")))),
                preceded(spc, tag("}")),
            ),
            |declarations| StructBody { declarations },
        ),
    )(i)
}

fn union_type_spec<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, UnionTypeSpec, E> {
    map(
        preceded(spc, preceded(tag("union"), union_body)),
        |union_body| UnionTypeSpec { union_body },
    )(i)
}

fn union_body<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, UnionBody, E> {
    context(
        "union_body",
        map(
            tuple((
                preceded(
                    preceded(spc, tag("switch")),
                    delimited(
                        preceded(spc, tag("(")),
                        declaration,
                        preceded(spc, tag(")")),
                    ),
                ),
                delimited(
                    preceded(spc, tag("{")),
                    tuple((
                        many1(case_def),
                        opt(delimited(
                            tuple((spc, tag("default"), spc, tag(":"))),
                            declaration,
                            preceded(spc, tag(";")),
                        )),
                    )),
                    preceded(spc, tag("}")),
                ),
            )),
            |(switch_declaration, (case_specs, default_declaration))| UnionBody {
                switch_declaration,
                case_specs,
                default_declaration,
            },
        ),
    )(i)
}

fn case_def<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, CaseDef, E> {
    map(
        tuple((
            many1(delimited(
                tuple((spc, tag("case"))),
                value,
                preceded(spc, tag(":")),
            )),
            terminated(declaration, preceded(spc, tag(";"))),
        )),
        |(values, declaration)| CaseDef {
            values,
            declaration,
        },
    )(i)
}

fn constant_def<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, ConstantDef, E> {
    context(
        "constant_def",
        map(
            tuple((
                preceded(tuple((spc, tag("const"))), identifier),
                delimited(preceded(spc, tag("=")), constant, preceded(spc, tag(";"))),
            )),
            |(identifier, constant)| ConstantDef {
                identifier,
                constant,
            },
        ),
    )(i)
}

fn type_def<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, TypeDef, E> {
    context(
        "type_def",
        preceded(
            spc,
            alt((
                map(
                    delimited(tag("typedef"), declaration, preceded(spc, tag(";"))),
                    |declaration| TypeDef::TypeDef { declaration },
                ),
                map(
                    delimited(
                        tag("enum"),
                        tuple((identifier, enum_body)),
                        preceded(spc, tag(";")),
                    ),
                    |(identifier, enum_body)| TypeDef::Enum {
                        identifier,
                        enum_body,
                    },
                ),
                map(
                    delimited(
                        tag("struct"),
                        tuple((identifier, struct_body)),
                        preceded(spc, tag(";")),
                    ),
                    |(identifier, struct_body)| TypeDef::Struct {
                        identifier,
                        struct_body,
                    },
                ),
                map(
                    delimited(
                        tag("union"),
                        tuple((identifier, union_body)),
                        preceded(spc, tag(";")),
                    ),
                    |(identifier, union_body)| TypeDef::Union {
                        identifier,
                        union_body,
                    },
                ),
            )),
        ),
    )(i)
}

fn program_def<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, ProgramDef, E> {
    context(
        "program_def",
        map(
            tuple((
                preceded(preceded(spc, tag("program")), identifier),
                delimited(
                    preceded(spc, tag("{")),
                    many1(version_def),
                    preceded(spc, tag("}")),
                ),
                delimited(preceded(spc, tag("=")), constant, preceded(spc, tag(";"))),
            )),
            |(identifier, version_defs, constant)| ProgramDef {
                identifier,
                version_defs,
                constant,
            },
        ),
    )(i)
}

fn version_def<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, VersionDef, E> {
    context(
        "version_def",
        map(
            tuple((
                preceded(preceded(spc, tag("version")), identifier),
                delimited(
                    preceded(spc, tag("{")),
                    many1(procedure_def),
                    preceded(spc, tag("}")),
                ),
                delimited(preceded(spc, tag("=")), constant, preceded(spc, tag(";"))),
            )),
            |(identifier, procedure_defs, constant)| VersionDef {
                identifier,
                procedure_defs,
                constant,
            },
        ),
    )(i)
}

fn procedure_def<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, ProcedureDef, E> {
    context(
        "procedure_def",
        map(
            tuple((
                type_specifier,
                identifier,
                delimited(
                    preceded(spc, tag("(")),
                    separated_list1(tag(","), type_specifier),
                    preceded(spc, tag(")")),
                ),
                delimited(preceded(spc, tag("=")), constant, preceded(spc, tag(";"))),
            )),
            |(type_specifier, identifier, type_specifiers, constant)| ProcedureDef {
                identifier,
                type_specifier,
                type_specifiers,
                constant,
            },
        ),
    )(i)
}

fn definition<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Definition, E> {
    context(
        "definition",
        alt((
            map(type_def, Definition::TypeDef),
            map(constant_def, Definition::ConstantDef),
            map(program_def, Definition::ProgramDef),
        )),
    )(i)
}

pub(crate) fn specification<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Specification, E> {
    map(many0(delimited(spc, definition, spc)), |definitions| {
        Specification { definitions }
    })(i)
}

#[cfg(test)]
mod test {
    use nom::{
        error::{ErrorKind, VerboseError},
        Err,
    };

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
            Ok(("", Value::Constant(Constant("123"))))
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
            identifier::<(_, ErrorKind)>("uint64_t"),
            Ok(("", Identifier("uint64_t")))
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
        assert_eq!(constant::<(_, ErrorKind)>("123"), Ok(("", Constant("123"))));
        assert_eq!(
            constant::<(_, ErrorKind)>("-123"),
            Ok(("", Constant("-123")))
        );
        assert_eq!(constant::<(_, ErrorKind)>("0x1"), Ok(("", Constant("0x1"))));
    }

    #[test]
    fn test_type_specifier() {
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
                            (Identifier("A"), Value::Constant(Constant("1"))),
                            (Identifier("B"), Value::Constant(Constant("2")))
                        ]
                    }
                })
            ))
        );
        assert_eq!(
            type_specifier::<(_, ErrorKind)>("struct { int a; hyper b; }"),
            Ok((
                "",
                TypeSpecifier::StructTypeSpec(StructTypeSpec {
                    struct_body: StructBody {
                        declarations: vec![
                            Declaration::Regular {
                                type_specifier: TypeSpecifier::Int { unsigned: false },
                                identifier: Identifier("a"),
                            },
                            Declaration::Regular {
                                type_specifier: TypeSpecifier::Hyper { unsigned: false },
                                identifier: Identifier("b"),
                            },
                        ]
                    }
                })
            ))
        );
        assert_eq!(
                type_specifier::<(_, ErrorKind)>("union switch ( int my_msg ) { case 1 : int thing; case 2 : hyper other_thing; default:string fallback<>;}"),
                Ok((
                    "",
                    TypeSpecifier::UnionTypeSpec(Box::new(UnionTypeSpec {
                        union_body: UnionBody {
                            switch_declaration: Declaration::Regular {
                                type_specifier: TypeSpecifier::Int { unsigned: false },
                                identifier: Identifier("my_msg"),
                            },
                            case_specs: vec![
                                CaseDef {
                                    values: vec![Value::Constant(Constant("1"))],
                                    declaration: Declaration::Regular {
                                        type_specifier: TypeSpecifier::Int { unsigned: false },
                                        identifier: Identifier("thing"),
                                    },
                                },
                                CaseDef {
                                    values: vec![Value::Constant(Constant("2"))],
                                    declaration: Declaration::Regular {
                                        type_specifier: TypeSpecifier::Hyper { unsigned: false },
                                        identifier: Identifier("other_thing"),
                                    },
                                }
                            ],
                            default_declaration: Some(Declaration::String {
                                identifier: Identifier("fallback"),
                                value: None,
                            }),
                        },
                    })),
                )),
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
                    value: Value::Constant(Constant("100")),
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
                    value: Some(Value::Constant(Constant("100"))),
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
                    value: Value::Constant(Constant("100")),
                }
            ))
        );

        assert_eq!(
            declaration::<(_, ErrorKind)>("opaque mysterious_data < 100 >"),
            Ok((
                "",
                Declaration::OpaqueVariableArray {
                    identifier: Identifier("mysterious_data"),
                    value: Some(Value::Constant(Constant("100"))),
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
                    value: Some(Value::Constant(Constant("100"))),
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

    #[test]
    fn test_constant_def() {
        assert_eq!(
            constant_def::<(_, ErrorKind)>("const  my_thing = 10000;"),
            Ok((
                "",
                ConstantDef {
                    identifier: Identifier("my_thing"),
                    constant: Constant("10000"),
                }
            ))
        );
    }

    #[test]
    fn test_type_def() {
        assert_eq!(
            type_def::<(_, ErrorKind)>("enum cool_enum { A = 1, B = 2 };"),
            Ok((
                "",
                TypeDef::Enum {
                    identifier: Identifier("cool_enum"),
                    enum_body: EnumBody {
                        variants: vec![
                            (Identifier("A"), Value::Constant(Constant("1"))),
                            (Identifier("B"), Value::Constant(Constant("2")))
                        ]
                    }
                }
            ))
        );
        assert_eq!(
            type_def::<VerboseError<&str>>("struct cool_struct { int64_t a; uint32_t b; };"),
            Ok((
                "",
                TypeDef::Struct {
                    identifier: Identifier("cool_struct"),
                    struct_body: StructBody {
                        declarations: vec![
                            Declaration::Regular {
                                type_specifier: TypeSpecifier::Identifier(Identifier("int64_t")),
                                identifier: Identifier("a"),
                            },
                            Declaration::Regular {
                                type_specifier: TypeSpecifier::Identifier(Identifier("uint32_t")),
                                identifier: Identifier("b"),
                            },
                        ]
                    }
                }
            ))
        );
        assert_eq!(
            type_def::<(_, ErrorKind)>("union cool_union switch ( int my_msg ) { case 1 : int thing; case 2 : hyper other_thing; default:string fallback<>;} ;"),
            Ok((
                "",
                TypeDef::Union {
                    identifier: Identifier("cool_union"),
                    union_body: UnionBody {
                        switch_declaration: Declaration::Regular {
                            type_specifier: TypeSpecifier::Int { unsigned: false },
                            identifier: Identifier("my_msg"),
                        },
                        case_specs: vec![
                            CaseDef {
                                values: vec![Value::Constant(Constant("1"))],
                                declaration: Declaration::Regular {
                                    type_specifier: TypeSpecifier::Int { unsigned: false },
                                    identifier: Identifier("thing"),
                                },
                            },
                            CaseDef {
                                values: vec![Value::Constant(Constant("2"))],
                                declaration: Declaration::Regular {
                                    type_specifier: TypeSpecifier::Hyper { unsigned: false },
                                    identifier: Identifier("other_thing"),
                                },
                            }
                        ],
                        default_declaration: Some(Declaration::String {
                            identifier: Identifier("fallback"),
                            value: None,
                        }),
                    },
                },
            )),
        );
    }

    #[test]
    fn test_version_def() {
        let procedure_def = ProcedureDef {
            type_specifier: TypeSpecifier::Identifier(Identifier("uint64_t")),
            identifier: Identifier("poke_bear"),
            type_specifiers: vec![
                TypeSpecifier::Identifier(Identifier("myargs")),
                TypeSpecifier::Identifier(Identifier("otherargs")),
            ],
            constant: Constant("1"),
        };
        assert_eq!(
            version_def::<VerboseError<&str>>(
                "version RPC1 { uint64_t poke_bear (myargs, otherargs) = 1 ; } = 1;"
            ),
            Ok((
                "",
                VersionDef {
                    identifier: Identifier("RPC1"),
                    procedure_defs: vec![procedure_def],
                    constant: Constant("1"),
                }
            ))
        );
    }

    #[test]
    fn test_program_def() {
        let procedure_def = ProcedureDef {
            type_specifier: TypeSpecifier::Identifier(Identifier("uint64_t")),
            identifier: Identifier("poke_bear"),
            type_specifiers: vec![
                TypeSpecifier::Identifier(Identifier("myargs")),
                TypeSpecifier::Identifier(Identifier("otherargs")),
            ],
            constant: Constant("1"),
        };
        let version_def = VersionDef {
            identifier: Identifier("RPC1"),
            procedure_defs: vec![procedure_def],
            constant: Constant("1"),
        };
        assert_eq!(
            program_def::<VerboseError<&str>>(
                "program NET_BEAR { version RPC1 { uint64_t poke_bear (myargs, otherargs) = 1 ; } = 1; } = 1;"
            ),
            Ok((
                "",
                ProgramDef {
                    identifier: Identifier("NET_BEAR"),
                    version_defs: vec![version_def],
                    constant: Constant("1"),
                }
            ))
        );
    }

    #[test]
    fn test_nfs_v4() {
        let nfsv4_x = include_str!("../assets/nfsv4.x");
        let (left, _parsed) = specification::<(_, ErrorKind)>(nfsv4_x).expect("parses nfsv4 spec");
        debug_assert_eq!(left, "");
    }

    #[test]
    fn test_file() {
        let file = include_str!("../assets/file.x");
        let (left, _parsed) = specification::<(_, ErrorKind)>(file).expect("parses file spec");
        debug_assert_eq!(left, "");
    }
}

