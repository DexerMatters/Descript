#[derive(Debug, Clone)]
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
        ParenL | ParenR | BracketL | BracketR | Dot => Some(13),
        Not | Pos | Neg => Some(12),
        Mul | Div | Mod => Some(11),
        Add | Sub => Some(10),
        BinaryShiftL | BinaryShiftR => Some(9),
        Less | LoE | Greater | GoE => Some(8),
        Equal | Inequal => Some(7),
        BinaryAnd => Some(6),
        BinaryXor => Some(5),
        BinaryOr => Some(4),
        And => Some(3),
        Or => Some(2),
        As | Inc | Dec | MulAs | DivAs | ModAs | AndAs | XorAs | OrAs => Some(1),
        _ => None,
    }
}
