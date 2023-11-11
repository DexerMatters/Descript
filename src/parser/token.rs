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

    Neg,
    Pos,

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

    AndAs,
    OrAs,
    XorAs,
    ShiftLAs,
    ShiftRAs,

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

use TokenTypes::*;
pub fn match_binop_precedence(t: &TokenTypes) -> Option<i16> {
    match *t {
        ParenL | ParenR | BracketL | BracketR | Dot => 13,
        Not | Pos | Neg => 12,
        Mul | Div | Mod => 11,
        Add | Sub => 10,
        BinaryShiftL | BinaryShiftR => 9,
        Less | LoE | Greater | GoE => 8,
        Equal | Inequal => 7,
        BinaryAnd => 6,
        BinaryXor => 5,
        BinaryOr => 4,
        And => 3,
        Or => 2,
        As | Inc | Dec | MulAs | DivAs | ModAs | AndAs | XorAs | OrAs => 1,
        _ => None,
    }
}
