pub mod parser;

// Parse an XDR specification. The parser supports both RFC 1831 and RFC 1832.
pub fn parse_specification(input: &str) -> nom::IResult<&str, parser::Specification> {
    parser::specification(input)
}
