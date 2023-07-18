
use std::convert::TryFrom;
use std::fmt::{Debug, Display, Formatter, Write};
use std::ops::{Index, IndexMut, Not, RangeInclusive};

use crate::board::board::{Board, BoardIndex, FILE_SIZE};
use crate::piece::Color::*;
use crate::piece::PieceType::*;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Square(pub u8);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MoveAction {
    Normal,
    Capture(PieceType),
    EnPassant,
    Promotion(PieceType, Option<PieceType>),
    Castle(Square)
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Move {
    pub from: Square,
    pub to: Square,
    pub move_type: MoveAction,
    pub previous_ep_square: Option<Square>
}

impl Square {
    pub(crate) const fn new(file: u8, rank: u8) -> Square {
        Square(rank * 8 + file)
    }

    pub fn flip(&self) -> Square {
        Square(self.0 ^ 56)
    }

    pub fn file(&self) -> u8 {
        self.0 % 8
    }

    pub fn rank(&self) -> u8 {
        self.0 / 8
    }

    pub fn direction_between(&self, to: &Square) -> (isize, isize) {
        let fd = (to.file() as isize - self.file() as isize).signum();
        let rd = (to.rank() as isize - self.rank() as isize).signum();
        (fd, rd)
    }
}

impl<T, const N: usize> Index<Square> for [T; N] {
    type Output = T;

    fn index(&self, index: Square) -> &Self::Output {
        &self[index.0 as usize]
    }
}

impl<T, const N: usize> IndexMut<Square> for [T; N] {

    fn index_mut(&mut self, index: Square) -> &mut Self::Output {
        &mut self[index.0 as usize]
    }
}

impl std::ops::Add<Square> for Square {
    type Output = Square;

    fn add(self, rhs: Square) -> Self::Output {
        Square(self.0 + rhs.0)
    }
}

impl std::ops::Sub<Square> for Square {
    type Output = Square;

    fn sub(self, rhs: Square) -> Self::Output {
        Square(self.0 - rhs.0)
    }
}

impl Move {

    // pub fn to_algebraic(&self, board: &Board) -> Option<String> {
    //     if let Move { from: _, to: _, move_type: MoveAction::Castle(sqr) } = self {
    //         return Some(if *sqr == CASTLING_SQUARES_WHITE.0 || *sqr == CASTLING_SQUARES_BLACK.0 {
    //             "0-0"
    //         } else {
    //             "0-0-0"
    //         }.to_string());
    //     }
    //     let dest_sqr = self.to.to_string();
    //     let piece = board.get(self.from)?;
    //     let piece_str = match piece {
    //         Piece { color: _, piece_type: PieceType::PAWN } => "".to_string(),
    //         p => {
    //             let c: char = Piece::into(p);
    //             c.to_ascii_uppercase().to_string()
    //         }
    //     };
    //
    //     let capture_string = match self {
    //         Move { from: _, to: _, move_type: MoveAction::Capture(_, _) } => "x",
    //         Move { from: _, to: _, move_type: MoveAction::Promotion(_, Some(_)) } => "x",
    //         _ => ""
    //     }.to_string();
    //     let mut ambi = board.all_available_moves()
    //         .filter(|m| m.to == self.to)
    //         .filter(|m| m.from != self.from)
    //         .filter(|m| board.get_piece(&m.from).unwrap().piece_type == piece.piece_type)
    //         .fold((true, true, true), |acc, m| {
    //             println!("{:?}", m);
    //             if m.from.file() == self.from.0 {
    //                 (false, acc.1, false)
    //             } else if m.from.rank() == self.from.1 {
    //                 (acc.0, false, false)
    //             } else {
    //                 (acc.0, acc.1, false)
    //             }
    //         });
    //
    //     if &capture_string == "x" {
    //         if piece.piece_type == PieceType::PAWN {
    //             ambi = (false, true, false);
    //         }
    //     }
    //
    //     let disambiguation = match ambi {
    //         (true, true, true) => "".to_string(),
    //         (true, true, false) => self.from.0.to_string(),
    //         (true, false, _) => self.from.1.to_string(),
    //         (false, true, _) => self.from.0.to_string(),
    //         (false, false, _) => self.from.to_string()
    //     };
    //
    //     let promo_string = if let Move { from: _, to: _, move_type: MoveAction::Promotion(p, Some(_)) } = self {
    //         let c: char = Piece::into(*p);
    //         c.to_string().to_ascii_uppercase()
    //     } else {
    //         "".to_string()
    //     };
    //
    //     Some([piece_str, disambiguation, capture_string, dest_sqr, promo_string].concat())
    // }

    pub fn to_uci(&self) -> String {
        match self.move_type {
            MoveAction::Promotion(piece_type, _)  => [self.from.to_string(), self.to.to_string(), Into::<char>::into(piece_type).to_string()].concat(),
            _ => [self.from.to_string(), self.to.to_string()].concat()
        }
    }

    pub fn from_uci(uci: &str, board: &Board) -> Result<Move, ()> {
        let from = Square::try_from(&uci[..2])?;
        let to = Square::try_from(&uci[2..4])?;
        let mut move_vect: Vec<Move> = vec![];
        board.create_moves(from, to, &mut move_vect);
        let mut move_iter = move_vect.into_iter();
        match uci.len() {
            4 => {
                move_iter.next().ok_or(())
            }
            5 => {
                let uci_promo_char = uci.chars().nth(4).unwrap();
                move_iter.filter_map(|x| {
                    if let MoveAction::Promotion(piece_type, _) = x.move_type {
                        if uci_promo_char == Into::<char>::into(piece_type) {
                            Some(x)
                        }
                        else {
                            None
                        }
                    }
                    else {
                        panic!("Illegal move")
                    }
                }).next().ok_or(())
            }
            _ => panic!("Illegal UCI string")
        }
    }
}

impl Display for Move {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.to_uci())
    }
}

impl Debug for Move {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.to_uci())
    }
}

impl Display for Square {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        let file = (('a' as u8) + self.file() as u8) as char;
        let rank = self.rank() + 1;
        write!(f, "{}{}", file, rank)
    }
}

impl Into<String> for Square {
    fn into(self) -> String {
        let file = (('a' as u8) + self.file() as u8) as char;
        let rank = self.rank() + 1;
        format!("{}{}", file, rank)
    }
}

impl TryFrom<&str> for Square {

    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let mut chars = value.chars();
        let file = match chars.next() {
            Some(r) => r,
            None => return Err(())
        };
        let rank = match chars.next() {
            Some(f) => f,
            None => return Err(())
        };
        let index = rank.to_digit(10);
        let rank_id = match index {
            Some(f) => f - 1,
            None => return Err(())
        } as u8;
        let file_id = ((file as BoardIndex) - ('a' as BoardIndex)) as u8;
        if file_id >= FILE_SIZE {
            return Err(());
        }
        if rank_id >= FILE_SIZE {
            return Err(());
        }
        Ok(Square::new(file_id, rank_id))
    }
}
impl TryFrom<char> for Piece {
    type Error = ();

    fn try_from(value: char) -> Result<Self, Self::Error> {
        let piece_type = match value.to_lowercase().to_string().as_str() {
            "p" => PAWN,
            "b" => BISHOP,
            "n" => KNIGHT,
            "r" => ROOK,
            "q" => QUEEN,
            "k" => KING,
            _ => return Err(())
        };
        if value.is_ascii_uppercase() {
            Ok(Piece{ piece_type, color: WHITE })
        } else {
            Ok(Piece{ piece_type, color: BLACK })
        }
    }
}

impl Into<char> for Piece {

    fn into(self) -> char {
        let Piece{piece_type, color} = self;
        match color {
            WHITE => Into::<char>::into(piece_type).to_ascii_uppercase(),
            BLACK => PieceType::into(piece_type)
        }
    }
}

impl Into<char> for PieceType {
    fn into(self) -> char {
        match self {
            PAWN => 'p',
            BISHOP => 'b',
            KNIGHT => 'n',
            ROOK => 'r',
            QUEEN => 'q',
            KING => 'k'
        }
    }
}


#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum PieceType {
    PAWN = 5,
    KNIGHT = 4,
    BISHOP = 3,
    ROOK = 2,
    QUEEN = 1,
    KING = 0
}

impl PieceType {
    pub const PIECES: RangeInclusive<usize> = (KING as usize)..=(PAWN as usize);
}

impl<T, const N: usize> Index<PieceType> for [T; N] {
    type Output = T;

    fn index(&self, index: PieceType) -> &Self::Output {
        &self[index as usize]
    }
}

impl<T, const N: usize> IndexMut<PieceType> for [T; N] {

    fn index_mut(&mut self, index: PieceType) -> &mut Self::Output {
        &mut self[index as usize]
    }
}

impl<T, const N: usize> Index<Color> for [T; N] {
    type Output = T;

    fn index(&self, index: Color) -> &Self::Output {
        &self[index as usize]
    }
}

impl<T, const N: usize> IndexMut<Color> for [T; N] {

    fn index_mut(&mut self, index: Color) -> &mut Self::Output {
        &mut self[index as usize]
    }
}

impl PieceType {
    pub fn is_slider(&self) -> bool {
        match self {
            KNIGHT | KING | PAWN => false,
            _ => true
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Color {
    WHITE = 0,
    BLACK = 1
}

impl Color {
    pub fn next(&self) -> Color {
        match self {
            WHITE => BLACK,
            BLACK => WHITE,
        }
    }
}

impl Not for Color {
    type Output = Color;

    fn not(self) -> Self::Output {
        match self {
            WHITE => BLACK,
            BLACK => WHITE
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Piece {
    pub piece_type: PieceType,
    pub color: Color
}

pub type PieceOpt = Option<Piece>;