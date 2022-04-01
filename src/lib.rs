pub mod parser;

// Parse an XDR specification. The parser supports both RFC 1831 and RFC 1832.
pub fn parse_specification(
    input: &str,
) -> nom::IResult<&str, parser::Specification, nom::error::Error<String>> {
    parser::specification::<(&str, nom::error::ErrorKind)>(input)
        .map_err(|err| err.map(|(input, code)| nom::error::Error::new(input.to_string(), code)))
}
