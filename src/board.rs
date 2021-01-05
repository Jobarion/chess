mod pieces;
pub mod evaluator;

pub mod board {
    use std::str::{Split, FromStr};
    use std::str::SplitWhitespace;
    use std::fmt::{Display, Formatter, Error, Write};
    use positional::{Square, File, Rank};
    use std::convert::{TryFrom, Into};
    use std::iter;
    use crate::board::board::positional::{Move, MoveAction};
    use crate::board::pieces::{PawnIterator, PieceIterator, QueenIterator, RookIterator, BishopIterator, KingIterator, KnightIterator};
    use crate::board::board::PieceType::QUEEN;
    use std::path::Iter;
    use crate::board::board::Color::{BLACK, WHITE};
    use itertools::Itertools;

    type BoardIndex = u8;
    pub const RANK_SIZE: BoardIndex = 8;
    pub const FILE_SIZE: BoardIndex = 8;
    const CASTLING_SQUARES_WHITE: (Square, Square) = (Square(File(0), Rank(0)), Square(File(0), Rank(RANK_SIZE - 1)));
    const CASTLING_SQUARES_BLACK: (Square, Square) = (Square(File(FILE_SIZE-1), Rank(0)), Square(File(FILE_SIZE-1), Rank(RANK_SIZE - 1)));

    pub mod positional {
        use std::convert::TryFrom;
        use std::fmt::{Display, Formatter, Write};
        use crate::board::board::{BoardIndex, RANK_SIZE, FILE_SIZE, Piece, Board, PieceType, CASTLING_SQUARES_WHITE, CASTLING_SQUARES_BLACK, Color};

        #[derive(Copy, Clone, Debug, PartialEq)]
        pub struct Rank(pub BoardIndex);
        #[derive(Copy, Clone, Debug, PartialEq)]
        pub struct File(pub BoardIndex);
        #[derive(Copy, Clone, Debug, PartialEq)]
        pub struct Square(pub File, pub Rank);

        #[derive(Debug, Copy, Clone)]
        pub enum MoveAction {
            Normal,
            Capture(Piece, Square),
            Promotion(Piece, Option<Piece>),
            Castle(Square)
        }

        #[derive(Debug, Copy, Clone)]
        pub struct Move {
            pub from: Square,
            pub to: Square,
            pub move_type: MoveAction
        }

        impl Square {
            pub(crate) fn new(file: BoardIndex, rank: BoardIndex) -> Square {
                Square(File(file), Rank(rank))
            }
        }

        impl std::ops::Add<(i8, i8)> for Square {
            type Output = Square;

            fn add(self, rhs: (i8, i8)) -> Self::Output {
                let Square(f1, r1) = self;
                let (f2, r2) = rhs;
                Square(f1+f2, r1+r2)
            }
        }

        impl std::ops::Sub<(i8, i8)> for Square {
            type Output = Square;

            fn sub(self, rhs: (i8, i8)) -> Self::Output {
                let Square(f1, r1) = self;
                let (f2, r2) = rhs;
                Square(f1-f2, r1-r2)
            }
        }

        impl std::ops::Add for Square {
            type Output = Square;

            fn add(self, rhs: Self) -> Self::Output {
                let Square(f1, r1) = self;
                let Square(f2, r2) = rhs;
                Square(f1+f2, r1+r2)
            }
        }

        impl std::ops::Sub for Square {
            type Output = Square;

            fn sub(self, rhs: Self) -> Self::Output {
                let Square(f1, r1) = self;
                let Square(f2, r2) = rhs;
                Square(f1-f2, r1-r2)
            }
        }

        impl std::ops::Mul<u8> for Square {
            type Output = Square;

            fn mul(self, rhs: u8) -> Self::Output {
                let Square(file, rank) = self;
                Square(file*rhs, rank*rhs)
            }
        }

        impl std::ops::Add for File {
            type Output = File;

            fn add(self, rhs: Self) -> Self::Output {
                Self(self.0 + rhs.0)
            }
        }

        impl std::ops::Add<i8> for File {
            type Output = File;

            fn add(self, rhs: i8) -> Self::Output {
                Self((self.0 as i8 + rhs) as u8)
            }
        }

        impl std::ops::Sub for File {
            type Output = File;

            fn sub(self, rhs: Self) -> Self::Output {
                Self(self.0 - rhs.0)
            }
        }

        impl std::ops::Sub<i8> for File {
            type Output = File;

            fn sub(self, rhs: i8) -> Self::Output {
                Self(((self.0 as i8) - rhs) as u8)
            }
        }

        impl std::ops::Mul<u8> for File {
            type Output = File;

            fn mul(self, rhs: u8) -> Self::Output {
                Self(self.0 * rhs)
            }
        }

        impl std::ops::Add for Rank {
            type Output = Rank;

            fn add(self, rhs: Self) -> Self::Output {
                Self(self.0 + rhs.0)
            }
        }

        impl std::ops::Add<i8> for Rank {
            type Output = Rank;

            fn add(self, rhs: i8) -> Self::Output {
                Self(((self.0 as i8) + rhs) as u8)
            }
        }

        impl std::ops::Sub for Rank {
            type Output = Rank;

            fn sub(self, rhs: Self) -> Self::Output {
                Self(self.0 - rhs.0)
            }
        }

        impl std::ops::Sub<i8> for Rank {
            type Output = Rank;

            fn sub(self, rhs: i8) -> Self::Output {
                Self(((self.0 as i8) - rhs) as u8)
            }
        }

        impl std::ops::Mul<u8> for Rank {
            type Output = Rank;

            fn mul(self, rhs: u8) -> Self::Output {
                Self(self.0 * rhs)
            }
        }

        impl Move {

            pub fn to_algebraic(&self, board: &Board) -> Option<String> {
                if let Move{from: _, to: _, move_type: MoveAction::Castle(sqr)} = self {
                    return Some(if *sqr == CASTLING_SQUARES_WHITE.0 || *sqr == CASTLING_SQUARES_BLACK.0 {
                        "0-0"
                    }
                    else {
                        "0-0-0"
                    }.to_string());
                }
                let dest_sqr = self.to.to_string();
                let piece = board.get_piece(&self.from)?;
                let piece_str = match piece {
                    Piece{color: _, piece_type: PieceType::PAWN} => "".to_string(),
                    p => {
                        let c: char = Piece::into(p);
                        c.to_ascii_uppercase().to_string()
                    }
                };

                let capture_string = match self {
                    Move{from: _, to: _, move_type: MoveAction::Capture(_, _)} => "x",
                    Move{from: _, to: _, move_type: MoveAction::Promotion(_, Some(_))} => "x",
                    _ => ""
                }.to_string();
                let mut ambi = board.all_available_moves()
                    .filter(|m| m.to == self.to)
                    .filter(|m| m.from != self.from)
                    .filter(|m| board.get_piece(&m.from).unwrap().piece_type == piece.piece_type)
                    .fold((true, true, true), |acc, m| {
                        println!("{:?}", m);
                        let Square(f, r) = m.from;
                        if f == self.from.0 {
                            (false, acc.1, false)
                        }
                        else if r == self.from.1 {
                            (acc.0, false, false)
                        }
                        else {
                            (acc.0, acc.1, false)
                        }
                    });

                if &capture_string == "x" {
                    if piece.piece_type == PieceType::PAWN {
                        ambi = (false, true, false);
                    }
                }

                let disambiguation = match ambi {
                    (true, true, true) => "".to_string(),
                    (true, true, false) => self.from.0.to_string(),
                    (true, false, _) => self.from.1.to_string(),
                    (false, true, _) => self.from.0.to_string(),
                    (false, false, _) => self.from.to_string()
                };

                let promo_string = if let Move{from: _, to: _, move_type: MoveAction::Promotion(p, Some(_))} = self {
                    let c: char = Piece::into(*p);
                    c.to_string().to_ascii_uppercase()
                } else {
                    "".to_string()
                };

                Some([piece_str, disambiguation, capture_string, dest_sqr, promo_string].concat())
            }

            fn bounds_check(r: u8, f: u8) -> bool {
                f < 0 || r < 0 || f >= 8 || r >= 8
            }

            fn bounds_check_sqr(sqr: &Square) -> bool {
                let &Square(File(f), Rank(r)) = sqr;
                Move::bounds_check(r, f)
            }

            pub fn new(board: &Board, from: &Square, to: &Square) -> Move {
                let to_piece = board.get_piece(to);
                let from_piece = board.get_piece(from).expect("from_piece empty in Move instantiation");
                match to_piece {
                    None => {
                        //EnPassant
                        if let Some(ep_square) = board.en_passant_square {
                            if ep_square == *to && from_piece.piece_type == PieceType::PAWN {
                                let Square(file, _) = ep_square;
                                let &Square(_, rank) = from;
                                let captured_square = Square(file, rank);
                                let captured = board.get_piece(&captured_square).expect("invalid en passant square");
                                return Move{from: *from, to: *to, move_type: MoveAction::Capture(captured, captured_square) }
                            }
                        }
                        //Castle
                        if from_piece.piece_type == PieceType::KING {
                            let castle_options = Move::current_castle_options(board);
                            match castle_options {
                                (Some(castle_sqr), _) => {
                                    if *to == castle_sqr {
                                        return Move{from: *from, to: *to, move_type: MoveAction::Castle(*to)};
                                    }
                                }
                                (_, Some(castle_sqr)) => {
                                    if *to == castle_sqr {
                                        return Move{from: *from, to: *to, move_type: MoveAction::Castle(*to)};
                                    }
                                }
                                _ => {}
                            };
                            return Move{from: *from, to: *to, move_type: MoveAction::Normal }
                        }
                        Move{from: *from, to: *to, move_type: MoveAction::Normal }
                    },
                    Some(captured) => Move{from: *from, to: *to, move_type: MoveAction::Capture(captured, *to) }
                }
            }

            pub fn new_promotion(board: &Board, from: &Square, to: &Square, promoted: &PieceType) -> Move {
                let piece = board.get_piece(to);
                let promoted = Piece{piece_type: *promoted, color: board.get_piece(from).expect("from_piece empty in Move instantiation").color};
                Move{from: *from, to: *to, move_type: MoveAction::Promotion(promoted, piece)}
            }

            fn current_castle_options(board: &Board) -> (Option<Square>, Option<Square>) {
                let (options, (s1, s2)) = match board.active_player {
                    Color::WHITE => (board.castling_options_white, CASTLING_SQUARES_WHITE),
                    Color::BLACK => (board.castling_options_black, CASTLING_SQUARES_BLACK)
                };
                match options {
                    (true, true) => (Some(s1), Some(s2)),
                    (true, false) => (Some(s1), None),
                    (false, true) => (None, Some(s2)),
                    (false, false) => (None, None),
                }
            }
        }

        impl Display for Move {
            fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
                write!(f, "{} -> {}", self.from, self.to)
            }
        }

        impl Display for File {
            fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
                f.write_char((('a' as u8) + self.0) as char)
            }
        }

        impl Display for Rank {
            fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
                let &Rank(i) = self;
                write!(f, "{}", i + 1)
            }
        }

        impl Display for Square {
            fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
                let &Square(file, rank) = self;
                write!(f, "{}{}", file, rank)
            }
        }

        impl TryFrom<&char> for File {

            type Error = ();

            fn try_from(value: &char) -> Result<Self, Self::Error> {
                let index = (*value as BoardIndex) - ('a' as BoardIndex);
                match index {
                    0...RANK_SIZE => Ok(File(index)),
                    _ => Err(())
                }
            }
        }

        impl TryFrom<&char>  for Rank {

            type Error = ();

            fn try_from(value: &char) -> Result<Self, Self::Error> {
                let index = value.to_digit(10);
                let index = match index {
                    Some(f) => f - 1,
                    None => return Err(())
                } as u8;
                match index {
                    0...FILE_SIZE => Ok(Rank(index)),
                    _ => Err(())
                }
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
                let rank = Rank::try_from(&rank)?;
                let file = File::try_from(&file)?;
                Ok(Square(file, rank))
            }
        }
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    pub enum Color {
        WHITE,
        BLACK
    }

    impl Color {
        pub fn next(&self) -> Color {
            match self {
                WHITE => BLACK,
                BLACK => WHITE,
            }
        }
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    pub enum PieceType {
        PAWN,
        ROOK,
        KNIGHT,
        BISHOP,
        QUEEN,
        KING
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    pub struct Piece {
        pub piece_type: PieceType,
        pub color: Color
    }

    pub type PieceOpt = Option<Piece>;

//    impl PartialEq for Piece {
//        fn eq(&self, other: &Self) -> bool {
//            let &Piece(t1, c1) = self;
//            let &Piece(t2, c2) = other;
//            t1 == t2 && c1 == c2;
//        }
//
//        fn ne(&self, other: &Self) -> bool {
//            unimplemented!()
//        }
//    }

    impl Display for Piece {
        fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
            let c: char = Piece::into(*self);
            write!(f, "{}", c)
        }
    }

    impl Display for Board {
        fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
            write!(f, "Active player: {:?}\n", self.active_player);
            for rank in 0..RANK_SIZE {
                for file in 0..FILE_SIZE {
                    let sqr = Square(File(file), Rank(RANK_SIZE - rank - 1));
                    match self.get_piece(&sqr) {
                        Some(piece) => write!(f, "{}", piece),
                        None => write!(f, ".")
                    };
                }
                write!(f, "\n");
            }
            write!(f, "{} {}", self.halfmove_clock, self.fullmove)
        }
    }

    impl TryFrom<char> for Piece {
        type Error = ();

        fn try_from(value: char) -> Result<Self, Self::Error> {
            let piece_type = match value.to_lowercase().to_string().as_str() {
                "p" => PieceType::PAWN,
                "b" => PieceType::BISHOP,
                "n" => PieceType::KNIGHT,
                "r" => PieceType::ROOK,
                "q" => PieceType::QUEEN,
                "k" => PieceType::KING,
                _ => return Err(())
            };
            if value.is_ascii_uppercase() {
                Ok(Piece{ piece_type, color: Color::WHITE })
            } else {
                Ok(Piece{ piece_type, color: Color::BLACK })
            }
        }
    }

    impl Into<char> for Piece {

        fn into(self) -> char {
            let Piece{piece_type, color} = self;
            let symbol = match piece_type {
                PieceType::PAWN => 'P',
                PieceType::BISHOP => 'B',
                PieceType::KNIGHT => 'N',
                PieceType::ROOK => 'R',
                PieceType::QUEEN => 'Q',
                PieceType::KING => 'K'
            };
            match color {
                Color::WHITE => symbol,
                Color::BLACK => symbol.to_ascii_lowercase()
            }
        }
    }

    #[derive(Copy, Clone, Debug)]
    pub struct Board {
        pub board: [[PieceOpt; RANK_SIZE as usize]; FILE_SIZE as usize],
        pub active_player: Color,
        pub castling_options_white: (bool, bool),
        pub castling_options_black: (bool, bool),
        pub en_passant_square: Option<Square>,
        pub halfmove_clock: u32,
        pub fullmove: u32,
        pub kings: (Square, Square),
    }

    pub struct CheckCheckingIterator(Iterator<Item=Move>);

    //rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1

    impl Board {

        pub fn get_piece(&self, sqr: &Square) -> PieceOpt {
            let &Square(File(fid), Rank(rid)) = sqr;
            self.board[rid as usize][fid as usize]
        }

        fn set_piece(&mut self, sqr: &Square, piece: PieceOpt) -> PieceOpt {
            let &Square(File(fid), Rank(rid)) = sqr;
            let previous = self.get_piece(sqr);
            self.board[rid as usize][fid as usize] = piece;
            previous
        }

        fn move_piece(&mut self, from: &Square, to: &Square) -> PieceOpt {
            let &Square(File(from_fid), Rank(rid)) = from;
            let &Square(File(to_fid), Rank(rid)) = to;
            let previous = self.set_piece(to, self.get_piece(from));
            self.set_piece(from, None);
            previous
        }

        pub fn do_move(&self, piece_move: &Move) -> Board {
            let mut new = self.clone();
            new.en_passant_square = None;
            new.halfmove_clock += 1;

            //Remove castling ability
            if let Some(Piece{color, piece_type: PieceType::KING}) = new.get_piece(&piece_move.from) {
                match color {
                    Color::WHITE => new.castling_options_white = (false, false),
                    Color::BLACK => new.castling_options_black = (false, false)
                }
            }

            if let Some(Piece{color, piece_type: PieceType::ROOK}) = new.get_piece(&piece_move.from) {
                match color {
                    Color::WHITE => {
                        if piece_move.from == CASTLING_SQUARES_WHITE.0 {
                            new.castling_options_white = (false, new.castling_options_white.1)
                        }
                        else if piece_move.from == CASTLING_SQUARES_WHITE.1 {
                            new.castling_options_white = (new.castling_options_white.0, false)
                        }
                    },
                    Color::BLACK => {
                        if piece_move.from == CASTLING_SQUARES_BLACK.0 {
                            new.castling_options_black = (false, new.castling_options_black.1)
                        }
                        else if piece_move.from == CASTLING_SQUARES_BLACK.1 {
                            new.castling_options_black = (new.castling_options_black.0, false)
                        }
                    }
                }
            }

            //Execute move
            match piece_move {
                Move{from, to, move_type: MoveAction::Normal } => {
                    new.move_piece(from, to);
                    if let Some(Piece{color: _, piece_type: PieceType::PAWN}) = new.get_piece(to) {
                        new.halfmove_clock = 0;
                        let &Square(file, Rank(r1)) = from;
                        let &Square(_, Rank(r2)) = to;
                        let diff = (r1 as i8 - r2 as i8);
                        if diff.abs() == 2 {
                            let dr = (r2 as i8) - (r1 as i8);
                            new.en_passant_square = Some(Square(file, Rank((r1 as i8 - diff / 2) as u8)))
                        }
                    }
                    if let Some(Piece{color, piece_type: PieceType::KING}) = new.get_piece(to) {
                        new.kings = match color {
                            Color::WHITE => (*to, new.kings.1),
                            Color::BLACK => (new.kings.0, *to),
                        }
                    }
                },
                Move{from, to, move_type: MoveAction::Capture(_, square)} => {
                    new.halfmove_clock = 0;
                    new.set_piece(square, None);
                    new.move_piece(from, to);
                    if let Some(Piece{color, piece_type: PieceType::KING}) = new.get_piece(to) {
                        new.kings = match color {
                            Color::WHITE => (*to, new.kings.1),
                            Color::BLACK => (new.kings.0, *to),
                        }
                    }
                },
                Move{from, to, move_type: MoveAction::Promotion(piece, some)} => {
                    new.halfmove_clock = 0;
                    new.set_piece(from, None);
                    new.set_piece(to, Some(*piece));
                },
                Move{from, to, move_type: MoveAction::Castle(castle_from)} => {
                    new.move_piece(from, to);
                    let &Square(File(f1), Rank(r1)) = from;
                    let &Square(File(f2), Rank(r2)) = to;
                    let df = (f2 as i8) - (f1 as i8);
                    let dr = (r2 as i8) - (r1 as i8);
                    let castle_to = Square(File((f2 as i8 + df.signum()) as BoardIndex), Rank((r2 as i8 + dr.signum()) as BoardIndex));
                    new.move_piece(castle_from, &castle_to);
                },
                _ => unreachable!()
            };
            if new.active_player == Color::BLACK {
                new.fullmove += 1;
            }
            new.active_player = new.active_player.next();
            new
        }

        fn all_available_moves_unchecked(&self, piece_color: Color) -> impl Iterator<Item=Move> + '_{
            let mut iters: Vec<PieceIterator> = vec!();
            for file in 0..FILE_SIZE {
                for rank in 0..RANK_SIZE {
                    let sqr = Square(File(file), Rank(rank));
                    let piece = self.get_piece(&sqr);
                    if let Some(Piece{piece_type, color}) = piece {
                        if color == piece_color {
                            let moves = self.available_moves(sqr);
                            iters.push(moves);
                        }
                    }
                }
            }
            iters.into_iter().flat_map(|x| x)
        }

        pub fn all_available_moves<'a>(&'a self) -> impl Iterator<Item=Move> + 'a {
            self.all_available_moves_unchecked(self.active_player)
                .filter( move |m| {
                    let temp = self.do_move(m);
                    let check = temp.is_in_check(&self.active_player);
                    !check
                })
        }

        pub fn is_in_check(&self, color: &Color) -> bool {
            let king_square = match color {
                Color::WHITE => self.kings.0,
                Color::BLACK => self.kings.1
            };

            self.all_available_moves_unchecked(color.next())
                .any(|m| m.to == king_square)
        }

        fn available_moves(&self, sqr: Square) -> PieceIterator {
            let piece = self.get_piece(&sqr);
            let piece = match piece {
                None => return PieceIterator::None,
                Some(t) => t
            };

            match piece.piece_type {
                PieceType::KING => KingIterator::new(sqr, self, piece.color),
                PieceType::BISHOP => BishopIterator::new(sqr, self, piece.color),
                PieceType::KNIGHT => KnightIterator::new(sqr, self, piece.color),
                PieceType::ROOK => RookIterator::new(sqr, self, piece.color),
                PieceType::QUEEN => QueenIterator::new(sqr, self, piece.color),
                PieceType::PAWN => PawnIterator::new(sqr, self, piece.color),
            }
        }


/*
        pub fn to_fen(&self) -> String {
            let mut fen = String::with_capacity((FILE_SIZE * RANK_SIZE + FILE_SIZE + 5 + 1 + 4 + 2 + 10) as usize);
            for file in 0..(FILE_SIZE as usize) {
                let mut skip_counter = 0;
                let mut line_string = String::with_capacity(8);
                for rank in 0..(RANK_SIZE as usize) {
                    let piece = self.board[file][rank];
                    if let Piece::None = piece {
                        skip_counter += 1;
                    }
                    else {
                        if skip_counter > 0 {
                            line_string.push((('0' as u8) + skip_counter) as char);
                            skip_counter = 0;
                        }
                        line_string.push(piece.into());
                    }
                }
                if skip_counter > 0 {
                    line_string.push((('0' as u8) + skip_counter) as char);
                }
                if file > 0 {
                    fen.push('/');
                }
                fen.push_str(line_string.as_str());
            }
            fen.push(if self.white_active {'w'} else {'b'});
        }
*/
        pub fn from_fen(fen: String) -> Option<Self> {
            let mut fen_parts: SplitWhitespace = fen.split_whitespace();
            let mut positions: Split<char> = fen_parts.next()?.split('/');
            let pieces = Board::parse_fen_board(positions)?;
            let active_player = Board::parse_active_player(fen_parts.next()?)?;
            let castling_options = Board::parse_castling_options(fen_parts.next()?)?;
            let en_passant = Board::parse_en_passant(fen_parts.next()?)?;
            let halfmove_clock = match fen_parts.next()?.parse::<u32>() {
                Ok(t) => t,
                _ => 0
            };
            let fullmove = match fen_parts.next()?.parse::<u32>() {
                Ok(t) => t,
                _ => 0
            };
            let mut kings: (Square, Square) = (Square::new(FILE_SIZE, 0), Square::new(FILE_SIZE, 0));
            for file in 0..FILE_SIZE {
                for rank in 0..RANK_SIZE {
                    match pieces[rank as usize][file as usize] {
                        Some(Piece{piece_type: PieceType::KING, color: Color::WHITE}) => {
                            kings = (Square(File(file), Rank(rank)), kings.1);
                        },
                        Some(Piece{piece_type: PieceType::KING, color: Color::BLACK}) => {
                            kings = (kings.0, Square(File(file), Rank(rank)));
                        },
                        _ => {}
                    }
                }
            }
            if (kings.0).0 == File(FILE_SIZE)|| (kings.1).0 == File(FILE_SIZE) {
                return None;
            }
            Some(Board {
                kings,
                board: pieces,
                active_player: active_player,
                castling_options_white: castling_options[0],
                castling_options_black: castling_options[1],
                en_passant_square: en_passant,
                halfmove_clock: halfmove_clock,
                fullmove: fullmove,
            })
        }

        fn parse_en_passant(en_passant: &str) -> Option<Option<Square>> {
            if en_passant == "-" {
                return Some(None);
            }
            match Square::try_from(en_passant) {
                Ok(s) => Some(Some(s)),
                Err(_) => None
            }
        }

        fn parse_castling_options(castle_string: &str) -> Option<[(bool, bool); 2]> {
            let mut castling_white = (false, false);
            let mut castling_black = (false, false);
            if castle_string == "-" {
                return Some([castling_white, castling_black]);
            }
            for c in castle_string.chars() {
                match c {
                    'K' => castling_white = (true, castling_white.1),
                    'Q' => castling_white = (castling_white.0, true),
                    'k' => castling_black = (true, castling_black.1),
                    'q' => castling_black = (castling_black.0, true),
                    _ => return None,
                }
            }
            Some([castling_white, castling_black])
        }

        fn parse_active_player(active_player: &str) -> Option<Color> {
            match active_player {
                "w" => Some(Color::WHITE),
                "b" => Some(Color::BLACK),
                _ => None
            }
        }

        fn parse_fen_board(mut positions: Split<char>) -> Option<[[PieceOpt; 8]; 8]> {
            let mut pieces: [[PieceOpt; 8]; 8] = [[None; 8]; 8];
            for line_index in 0..8 {
                let mut line = positions.next()?.chars();
                let mut column_index: usize = 0;
                while column_index < 8 {
                    let piece = line.next()?;
                    match piece.to_digit(10) {
                        Some(n @ 1..=8) => {
                            column_index += n as usize;
                            continue;
                        }
                        Some(_) => return None,
                        _ => {}
                    };
                    let piece = match Piece::try_from(piece) {
                        Ok(t) => t,
                        _ => return None
                    };
                    pieces[7 - line_index][column_index] = Some(piece);
                    column_index += 1;
                }
                if column_index > 8 {
                    return None;
                }
            }
            Some(pieces)
        }
    }
}

#[cfg(test)]
mod check_test {
    use crate::board::board::{Board, PieceType};
    use crate::board::board::Color;
    use crate::board::pieces::PawnIterator;
    use crate::board::board::positional::{Square, Move};
    use crate::board::board::positional::MoveAction::Promotion;

    #[test]
    fn a() {
        let board = Board::from_fen("4k3/8/8/1p4pp/8/4q3/1K5q/8 w - - 18 141".to_string()).unwrap();
        assert!(board.is_in_check(&Color::WHITE));
        assert!(!board.is_in_check(&Color::BLACK));
    }

}