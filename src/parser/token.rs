#[derive(Debug)]
pub enum TokenTypes {
    End,

    /* Symbols */
    /** Operation **/
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    As,
    Inc,
    Dec,
    MulAs,
    DivAs,
    ModAs,

    /** Logical  **/
    Not,
    Or,
    And,

    /** Bitwise **/
    BinaryAnd,
    BinaryOr,
    BinaryXor,
    BinaryShiftL,
    BinaryShiftR,

    /** Comparation **/
    Equal,
    Inequal,
    Greater,
    Less,
    GoE,
    LoE,

    /** Parentheses **/
    ParenL,
    ParenR,
    BracketL,
    BracketR,
    BraceL,
    BraceR,

    /** Delimiter **/
    Dot,
    Comma,
    Colon,
    Semicolon,

    /* Keywords */
    Function,
    Flow,
    Map,
    If,
    Else,
    For,
    While,
    Loop,
    Break,

    /* Basic Data */
    Number,
    Char,
    String,

    /* Featured Operatiors */
    Arrow,

    /* Others */
    Identifier,
    Undefined,
}
