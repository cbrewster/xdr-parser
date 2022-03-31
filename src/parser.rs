use nom::branch::alt;
use nom::bytes::complete::{tag, take};
use nom::character::complete::{alpha1, alphanumeric1};
use nom::combinator::{map, recognize};
use nom::multi::many0_count;
use nom::sequence::pair;
use nom::IResult;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Identifier<'a>(&'a str);

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Declaration<'a> {
    Regular {
        type_specifier: Box<TypeSpecifier<'a>>,
        identifier: Identifier<'a>,
    },
    FixedArray {
        type_specifier: Box<TypeSpecifier<'a>>,
        identifier: Identifier<'a>,
        value: Value<'a>,
        opaque: bool,
    },
    VariableArray {
        type_specifier: Box<TypeSpecifier<'a>>,
        identifier: Identifier<'a>,
        value: Value<'a>,
        opaque: bool,
    },
    String {
        identifier: Identifier<'a>,
        value: Value<'a>,
    },
    Pointer {
        identifier: Identifier<'a>,
        value: Value<'a>,
    },
    Void,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Value<'a> {
    Constant(Constant<'a>),
    Identifier(Identifier<'a>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Constant<'a> {
    Decimal(&'a str),
    Hexidecimal(&'a str),
    Octal(&'a str),
}

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
    UnionTypeSpec(UnionTypeSpec<'a>),
    Identifier(Identifier<'a>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct EnumTypeSpec<'a> {
    pub enum_body: EnumBody<'a>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct EnumBody<'a> {
    pub variants: Vec<(Identifier<'a>, Declaration<'a>)>,
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
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Specification<'a> {
    pub definitions: Vec<Definition<'a>>,
}

fn identifier(input: &str) -> IResult<&str, Identifier> {
    map(
        recognize(pair(alpha1, many0_count(alt((alphanumeric1, tag("_")))))),
        |ident| Identifier(ident),
    )(input)
}

#[cfg(test)]
mod test {
    use nom::{error::ErrorKind, Err};

    use super::*;

    #[test]
    fn test_identifier() {
        assert_eq!(identifier("some_ident"), Ok(("", Identifier("some_ident"))));
        assert_eq!(
            identifier("some_ident more"),
            Ok((" more", Identifier("some_ident")))
        );
        assert_eq!(
            identifier("_invalid"),
            Err(Err::Error(nom::error::Error {
                input: "_invalid",
                code: ErrorKind::Alpha
            }))
        );
        assert_eq!(
            identifier("123no_number"),
            Err(Err::Error(nom::error::Error {
                input: "123no_number",
                code: ErrorKind::Alpha
            }))
        );
    }
}
