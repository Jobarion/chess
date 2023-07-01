pub mod board {
    use std::cmp::{max, min};
    use std::convert::{Into, TryFrom};
    use std::fmt::{Display, Error, Formatter, Write};
    use std::ops::Not;
    use std::pin::pin;
    use std::str::Split;
    use std::str::SplitWhitespace;

    use itertools::Itertools;

    use positional::Square;

    use crate::bitboard::{BitBoard, KING_MOVES, KNIGHT_MOVES, PAWN_CAPTURE_MOVES_BLACK, PAWN_CAPTURE_MOVES_WHITE, PAWN_MOVES_BLACK, PAWN_MOVES_WHITE};
    use crate::board::board::Color::{BLACK, WHITE};
    use crate::board::board::PieceType::{BISHOP, KING, QUEEN, ROOK};
    use crate::board::board::positional::{Move, MoveAction};

    type BoardIndex = usize;
    pub const RANK_SIZE: BoardIndex = 8;
    pub const FILE_SIZE: BoardIndex = 8;
    const CASTLING_SQUARES_WHITE: (Square, Square) = (Square::new(0, 0), Square::new(0, RANK_SIZE - 1));
    const CASTLING_SQUARES_BLACK: (Square, Square) = (Square::new(FILE_SIZE - 1, 0), Square::new(FILE_SIZE - 1, RANK_SIZE - 1));

    pub mod positional {
        use std::convert::TryFrom;
        use std::fmt::{Display, Formatter, Write};

        use crate::board::board::{Board, BoardIndex, CASTLING_SQUARES_BLACK, CASTLING_SQUARES_WHITE, Color, FILE_SIZE, Piece, PieceType, RANK_SIZE};

        #[derive(Copy, Clone, Debug, PartialEq)]
        pub struct Square(pub BoardIndex);

        #[derive(Debug, Copy, Clone)]
        pub enum MoveAction {
            Normal,
            Capture(PieceType, Square),
            Promotion(PieceType, Option<PieceType>),
            Castle(Square)
        }

        #[derive(Debug, Copy, Clone)]
        pub struct Move {
            pub from: Square,
            pub to: Square,
            pub move_type: MoveAction
        }

        impl Square {
            pub(crate) const fn new(file: BoardIndex, rank: BoardIndex) -> Square {
                Square(rank * 8 + file)
            }

            pub fn file(&self) -> BoardIndex {
                self.0 % 8
            }

            pub fn rank(&self) -> BoardIndex {
                self.0 / 8
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

            pub fn to_uci(&self) -> Option<String> {
                Some([self.from.to_string(), self.to.to_string()].concat())
            }

        }

        impl Display for Move {
            fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
                write!(f, "{} -> {}", self.from, self.to)
            }
        }

        impl Display for Square {
            fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
                let file = (('a' as u8) + self.file() as u8) as char;
                let rank = self.rank() + 1;
                write!(f, "{}{}", file, rank)
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
                } as BoardIndex;
                let file_id = (file as BoardIndex) - ('a' as BoardIndex);
                if file_id >= FILE_SIZE {
                    return Err(());
                }
                if rank_id >= FILE_SIZE {
                    return Err(());
                }
                Ok(Square::new(file_id, rank_id))
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
    pub enum PieceType {
        PAWN,
        ROOK,
        KNIGHT,
        BISHOP,
        QUEEN,
        KING
    }

    impl PieceType {
        fn is_slider(&self) -> bool {
            match self {
                PieceType::KNIGHT => false,
                PieceType::KING => false,
                PieceType::PAWN => false,
                _ => true
            }
        }
    }

    #[derive(Debug)]
    pub struct Perft {
        nodes: u64,
        captures: u64,
        promotions: u64,
        checks: u64,
        double_checks: u64,
    }

    impl Perft {
        fn new() -> Perft {
            Perft {
                nodes: 0,
                captures: 0,
                promotions: 0,
                checks: 0,
                double_checks: 0,
            }
        }
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    pub struct Piece {
        pub piece_type: PieceType,
        pub color: Color
    }

    pub type PieceOpt = Option<Piece>;

    impl Display for Piece {
        fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
            let c: char = Piece::into(*self);
            write!(f, "{}", c)
        }
    }

    impl Display for Board {
        fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
            let _ = write!(f, "Active player: {:?}\n", self.active_player);
            for rank in (0..RANK_SIZE).rev() {
                for file in 0..FILE_SIZE {
                    let sqr = Square::new(file, rank);
                    let _ = match self.board[sqr.0] {
                        Some(piece) => write!(f, "{}", piece),
                        None => write!(f, ".")
                    };
                }
                let _ = write!(f, "\n");
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
                Ok(Piece{ piece_type, color: WHITE })
            } else {
                Ok(Piece{ piece_type, color: BLACK })
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
        pub board: [PieceOpt; (RANK_SIZE * FILE_SIZE) as usize],
        pub active_player: Color,
        pub castling_options_white: (bool, bool),
        pub castling_options_black: (bool, bool),
        pub en_passant_square: Option<Square>,
        pub halfmove_clock: u32,
        pub fullmove: u32,

        pub white: BitBoard,
        pub black: BitBoard,
        pub pawns: BitBoard,
        pub rooks: BitBoard,
        pub knights: BitBoard,
        pub bishops: BitBoard,
        pub queens: BitBoard,
        pub kings: BitBoard
    }
    //rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1

    impl Board {

        fn new(board: [PieceOpt; (RANK_SIZE * FILE_SIZE) as usize], active_player: Color, castling_options_white: (bool, bool), castling_options_black: (bool, bool), en_passant_square: Option<Square>, halfmove_clock: u32, fullmove: u32) -> Board {
            let mut board = Board {
                board,
                active_player,
                castling_options_white,
                castling_options_black,
                en_passant_square,
                halfmove_clock,
                fullmove,
                white: BitBoard(0),
                black: BitBoard(0),
                pawns: BitBoard(0),
                rooks: BitBoard(0),
                knights: BitBoard(0),
                bishops: BitBoard(0),
                queens: BitBoard(0),
                kings: BitBoard(0)
            };
            for n in 0..64 {
                let piece = board.board[n as usize];
                if let Some(p) = piece {
                    board.set(p, Square(n));
                }
            }
            board
        }


        pub(crate) fn get(&self, sqr: Square) -> PieceOpt {
            self.board[sqr.0]
        }

        pub fn set(&mut self, piece: Piece, sqr: Square) -> PieceOpt {
            let mut previous = self.unset(sqr);
            self.board[sqr.0] = Some(piece);
            match piece {
                Piece { piece_type: PieceType::PAWN, color: _ } => self.pawns = self.pawns | sqr,
                Piece { piece_type: PieceType::ROOK, color: _ } => self.rooks = self.rooks | sqr,
                Piece { piece_type: PieceType::KNIGHT, color: _ } => self.knights = self.knights | sqr,
                Piece { piece_type: PieceType::BISHOP, color: _ } => self.bishops = self.bishops | sqr,
                Piece { piece_type: PieceType::QUEEN, color: _ } => self.queens = self.queens | sqr,
                Piece { piece_type: PieceType::KING, color: _ } => self.kings = self.kings | sqr,
            }
            match piece {
                Piece{ piece_type: _, color: Color::WHITE } => self.white = self.white | sqr,
                Piece{ piece_type: _, color: Color::BLACK } => self.black = self.black | sqr,
            }
            previous
        }

        pub fn unset(&mut self, sqr: Square) -> PieceOpt {
            let piece = self.board[sqr.0];
            let anti_mask = BitBoard(!(1 << sqr.0));
            self.board[sqr.0] = None;
            match piece {
                Some(Piece { piece_type: PieceType::PAWN, color: _ }) => self.pawns = self.pawns & anti_mask,
                Some(Piece { piece_type: PieceType::ROOK, color: _ }) => self.rooks = self.rooks & anti_mask,
                Some(Piece { piece_type: PieceType::KNIGHT, color: _ }) => self.knights = self.knights & anti_mask,
                Some(Piece { piece_type: PieceType::BISHOP, color: _ }) => self.bishops = self.bishops & anti_mask,
                Some(Piece { piece_type: PieceType::QUEEN, color: _ }) => self.queens = self.queens & anti_mask,
                Some(Piece { piece_type: PieceType::KING, color: _ }) => self.kings = self.kings & anti_mask,
                _ => {}
            }
            match piece {
                Some(Piece{ piece_type: _, color: WHITE }) => self.white = self.white & anti_mask,
                Some(Piece{ piece_type: _, color: BLACK }) => self.black = self.black & anti_mask,
                _ => {}
            }
            piece
        }

        pub fn apply_move(&mut self, piece_move: &Move) {
            let moved_piece = self.unset(piece_move.from).unwrap();
            self.unset(piece_move.to);
            debug_assert_eq!(moved_piece.color, self.active_player);

            match piece_move {
                Move{from, to, move_type: MoveAction::Promotion(promotion_type, _)} => {
                    self.set(Piece{piece_type: *promotion_type, color: self.active_player}, *to);
                }
                Move{from, to, move_type: _} => {
                    self.set(moved_piece, *to);
                }
            }
            self.active_player = !self.active_player;
        }

        pub fn undo_move(&mut self, piece_move: &Move) {
            self.active_player = !self.active_player;
            let moved_piece = self.unset(piece_move.to).unwrap();

            debug_assert_eq!(moved_piece.color, self.active_player);

            match piece_move {
                Move{from, to, move_type: MoveAction::Promotion(_, None)} => {
                    self.set(Piece{piece_type: PieceType::PAWN, color: self.active_player}, *from);
                }
                Move{from, to, move_type: MoveAction::Promotion(_, Some(captured_type))} => {
                    self.set(Piece{piece_type: PieceType::PAWN, color: self.active_player}, *from);
                    self.set(Piece{piece_type: *captured_type, color: !self.active_player}, *to);
                }
                Move{from, to, move_type: MoveAction::Capture(captured_type, capture_square)} => {
                    self.set(moved_piece, *from);
                    self.set(Piece{piece_type: *captured_type, color: !self.active_player}, *capture_square);
                }
                Move{from, to, move_type: _} => {
                    self.set(moved_piece, *from);
                }
            }
        }

        fn mask_for_color(&self, color: Color) -> BitBoard {
            match color {
                WHITE => self.white,
                BLACK => self.black,
            }
        }

        fn active_color_mask(&self) -> BitBoard {
            self.mask_for_color(self.active_player)
        }

        fn opponent_color_mask(&self) -> BitBoard {
            self.mask_for_color(!self.active_player)
        }

        pub fn perft(&mut self, depth: u8) -> Perft {
            let mut perft = Perft::new();
            self._perft(&mut perft, depth);
            perft
        }

        fn _perft(&mut self, mut perft: &mut Perft, depth: u8) {
            for lmove in self.legal_moves() {
                //Don't descend into king captures
                if let MoveAction::Capture(KING, _) = lmove.move_type {
                    continue;
                }
                self.apply_move(&lmove);
                if depth == 1 {
                    match lmove.move_type {
                        MoveAction::Capture(_, _) => perft.captures = perft.captures + 1,
                        MoveAction::Promotion(_, _) => perft.promotions = perft.promotions + 1,
                        _ => {}
                    }
                    perft.nodes = perft.nodes + 1;
                }
                else {
                    //TODO remove

                    self._perft(&mut perft, depth - 1);
                }
                self.undo_move(&lmove)
            }
        }

        pub(crate) fn legal_moves(&mut self) -> Vec<Move> {
            let player_masks: (BitBoard, BitBoard) = match self.active_player {
                WHITE => (self.white, self.black),
                BLACK => (self.black, self.white)
            };

            let king = self.kings & player_masks.0;
            let king_square = Square(king.0.trailing_zeros() as BoardIndex);

            let (king_danger_squares, attackers) = self.generate_king_danger_squares();

            // println!("Danger");
            // self.print_highlighted(king_danger_squares);
            //King is in check
            // println!("Attacker count is {}", attackers.0.count_ones());
            if attackers.0.count_ones() > 0  {
                return self.generate_king_danger_moves(king_square, king_danger_squares, attackers);
            }
            let mut moves: Vec<Move> = vec!();
            // let pin_mask = self.generate_pin_mask(king_square, self.active_player);
            // println!("Pin mask");
            // self.print_highlighted(pin_mask);
            //TODO: Should we use iterators here? Copy pasting the same board iterator loop seems dumb
            for square_id in 0..(FILE_SIZE * RANK_SIZE) {
                // println!("Checking {}", Square(square_id));
                if (self.active_color_mask() & Square(square_id)).0 == 0 {
                    continue;
                }
                let sqr = Square(square_id);
                let mut move_mask = self.generate_move_squares_by_piece(sqr, self.active_player, false);
                if let Some(Piece{piece_type: KING, color: _}) = self.board[sqr.0] {
                    move_mask = move_mask & !king_danger_squares;
                }
                // println!("Movable piece at {} with {} moves", sqr, move_mask.0.count_ones());
                moves.append(&mut self.moves_from_target_bitboard(sqr, move_mask));
            }
            moves
        }

        fn generate_king_danger_moves(&self, king_square: Square, king_danger_squares: BitBoard, attackers: BitBoard) -> Vec<Move> {
            let mut moves: Vec<Move> = vec!();

            let evasive_king_moves = KING_MOVES[king_square.0] & !king_danger_squares & !self.active_color_mask();
            // println!("Evasive");
            // self.print_highlighted(evasive_king_moves);
            moves.append(&mut self.moves_from_target_bitboard(king_square, evasive_king_moves));

            //Double check, the king can only run away
            if attackers.0.count_ones() > 1 {
                return moves;
            }

            let attacker_square = Square(attackers.0.trailing_zeros() as BoardIndex);

            // let knight_capture_moves = KNIGHT_MOVES[attackers.0 as usize] & self.knights & self.active_color_mask();
            // //To check if we can capture the attacker with a pawn, we try to capture from the attackers square with an opposite colored pawn
            // let pawn_capture_moves = if self.active_player == WHITE {
            //     PAWN_CAPTURE_MOVES_BLACK[attackers.0 as usize] & self.pawns & self.white
            // } else {
            //     PAWN_CAPTURE_MOVES_WHITE[attackers.0 as usize] & self.pawns & self.black
            // };

            // moves.append(&mut self.moves_from_source_bitboard(attacker_square, knight_capture_moves));
            // moves.append(&mut self.moves_from_source_bitboard(attacker_square, pawn_capture_moves));

            // println!("Attackers");
            // self.print_highlighted(attackers);

            let block_mask = if self.board[attacker_square.0].unwrap().piece_type.is_slider() {
                Board::ray_between_squares_exclusive(king_square, attacker_square)
            } else {
                BitBoard(0xFFFFFFFFFFFFFFFF)
            };

            // println!("Block mask");
            // self.print_highlighted(block_mask);

            let check_prevention_mask = block_mask | attackers;

            //TODO: Can we get faster by doing self.active_player_mask() and popping the bits out?
            //TODO: If above works, we can skip knights and pawns because for those we can calculate the capture directly. Maybe it helps. Doesn't necessarily work for blocks though
            for square_id in 0..(FILE_SIZE * RANK_SIZE) {
                if (self.active_color_mask() & Square(square_id)).0 == 0 {
                    continue;
                }
                let sqr = Square(square_id);
                //Generate moves and only accept moves that end on attacker squares
                let mut move_mask = self.generate_move_squares_by_piece(sqr, self.active_player, false) & check_prevention_mask;
                if move_mask.0 == 0 {
                    continue;
                }
                //King can't move on danger squares to block/capture
                if let Some(Piece{piece_type: KING, color: _}) = self.board[sqr.0] {
                    move_mask = move_mask & !king_danger_squares;
                }
                moves.append(&mut self.moves_from_target_bitboard(sqr, move_mask));
            }

            moves
        }

        fn ray_between_squares_exclusive(from: Square, to: Square) -> BitBoard {
            let file_diff: i8 = to.file() as i8 - from.file() as i8;
            let rank_diff: i8 = to.rank() as i8 - from.rank() as i8;
            let df = file_diff.signum();
            let dr = rank_diff.signum();
            let mut file = from.file() as i8 + df ;
            let mut rank = from.rank() as i8 + dr;
            let mut ray_mask = BitBoard(0);
            while file as BoardIndex != to.file() {
                ray_mask = ray_mask.set(Square::new(file as BoardIndex, rank as BoardIndex));
                file = file + df;
                rank = rank + dr;
            }
            ray_mask
        }

        fn generate_king_danger_squares(&mut self) -> (BitBoard, BitBoard) {
            let mut attacked_squares = BitBoard(0);
            let mut attackers = BitBoard(0);
            let king = self.kings & self.active_color_mask();
            //Remove active king for danger square check
            if self.active_player == WHITE {
                self.white = self.white ^ king;
            }
            else {
                self.black = self.black ^ king;
            }
            self.kings = self.kings ^ king;
            // if king.0 == 0 {
            //     println!("Final board {}", self);
            //     panic!();
            // }
            self.board[king.0.trailing_zeros() as usize] = None;

            for square_id in 0..(FILE_SIZE * RANK_SIZE) {
                if (self.opponent_color_mask() & Square(square_id)).0 == 0 {
                    continue;
                }
                let sqr = Square(square_id);
                let attacked_bp = self.generate_move_squares_by_piece(sqr, !self.active_player, true);
                attacked_squares = attacked_squares | attacked_bp;
                if (king & attacked_bp).0 != 0 {
                    attackers = attackers | sqr;
                }
            }
            //Place king back
            self.kings = self.kings ^ king;
            if self.active_player == WHITE {
                self.white = self.white ^ king;
            }
            else {
                self.black = self.black ^ king;
            }
            self.board[king.0.trailing_zeros() as usize] = Some(Piece{piece_type: PieceType::KING, color: self.active_player});
            (attacked_squares, attackers)
        }

        fn generate_move_squares_by_piece(&self, sqr: Square, color_to_move: Color, danger_only: bool) -> BitBoard {
            match self.board[sqr.0] {
                None => BitBoard(0),
                Some(Piece{piece_type: _, color}) if color != color_to_move => BitBoard(0),
                Some(Piece{piece_type, color }) => match piece_type {
                    PieceType::KING => KING_MOVES[sqr.0] & !self.mask_for_color(color_to_move),
                    PieceType::KNIGHT => KNIGHT_MOVES[sqr.0] & !self.mask_for_color(color_to_move),
                    PieceType::PAWN => if color_to_move == WHITE {
                        if danger_only {
                            PAWN_CAPTURE_MOVES_WHITE[sqr.0]
                        }
                        else {
                            let mut pawn_moves = PAWN_CAPTURE_MOVES_WHITE[sqr.0] & self.mask_for_color(!color_to_move);
                            let push_sqr = Square::new(sqr.file(), sqr.rank() + 1);
                            if self.board[push_sqr.0].is_none() {
                                pawn_moves = pawn_moves | push_sqr;
                                let push_sqr = Square::new(sqr.file(), sqr.rank() + 2);
                                if self.board[push_sqr.0].is_none() && sqr.rank() == 1 {
                                    pawn_moves = pawn_moves | push_sqr;
                                }
                            }
                            pawn_moves
                        }
                    } else {
                        if danger_only {
                            PAWN_CAPTURE_MOVES_BLACK[sqr.0]
                        }
                        else {
                            let mut pawn_moves = PAWN_CAPTURE_MOVES_BLACK[sqr.0] & self.mask_for_color(!color_to_move);
                            let push_sqr = Square::new(sqr.file(), sqr.rank() - 1);
                            if self.board[push_sqr.0].is_none() {
                                pawn_moves = pawn_moves | push_sqr;
                                let push_sqr = Square::new(sqr.file(), sqr.rank() - 2);
                                if self.board[push_sqr.0].is_none() && sqr.rank() == 6 {
                                    pawn_moves = pawn_moves | push_sqr;
                                }
                            }
                            pawn_moves
                        }
                    },
                    PieceType::ROOK => self.generate_rook_moves(sqr, color),
                    PieceType::BISHOP => self.generate_bishop_moves(sqr, color),
                    PieceType::QUEEN => self.queen_attack_squares(sqr, color),
                }
            }
        }

        fn generate_pin_mask(&self, sqr: Square, pinned_color: Color) -> BitBoard {
            self.generate_pin_mask_rq(sqr, pinned_color) | self.generate_pin_mask_bq(sqr, pinned_color)
        }

        fn generate_pin_mask_rq(&self, sqr: Square, pinned_color: Color) -> BitBoard {
            let mut pin_squares = BitBoard(0);
            let mut first_hit = true;
            let mut pin_squares_ray = BitBoard(0);
            for n in (sqr.file() + 1)..8 {
                let sq = Square::new(n, sqr.rank());
                if let Some(Piece{piece_type, color}) = self.board[sq.0] {
                    if color != pinned_color {
                        if !first_hit && (piece_type == QUEEN || piece_type == ROOK) {
                            pin_squares = pin_squares | pin_squares_ray;
                        }
                        break;
                    }
                    else if first_hit {
                        first_hit = false;
                        pin_squares_ray = pin_squares_ray | sq;
                    }
                    else {
                        break;
                    }
                }
            }
            first_hit = true;
            pin_squares_ray = BitBoard(0);
            for n in (0..sqr.file()).rev() {
                let sq = Square::new(n, sqr.rank());
                if let Some(Piece{piece_type, color}) = self.board[sq.0] {
                    if color != pinned_color {
                        if !first_hit && (piece_type == QUEEN || piece_type == ROOK) {
                            pin_squares = pin_squares | pin_squares_ray;
                        }
                        break;
                    }
                    else if first_hit {
                        first_hit = false;
                        pin_squares_ray = pin_squares_ray | sq;
                    }
                    else {
                        break;
                    }
                }
            }
            first_hit = true;
            pin_squares_ray = BitBoard(0);
            for n in (sqr.rank() + 1)..8 {
                let sq = Square::new(sqr.file(), n);
                if let Some(Piece{piece_type, color}) = self.board[sq.0] {
                    if color != pinned_color {
                        if !first_hit && (piece_type == QUEEN || piece_type == ROOK) {
                            pin_squares = pin_squares | pin_squares_ray;
                        }
                        break;
                    }
                    else if first_hit {
                        first_hit = false;
                        pin_squares_ray = pin_squares_ray | sq;
                    }
                    else {
                        break;
                    }
                }
            }
            first_hit = true;
            pin_squares_ray = BitBoard(0);
            for n in (0..sqr.rank()).rev() {
                let sq = Square::new(sqr.file(), n);
                if let Some(Piece{piece_type, color}) = self.board[sq.0] {
                    if color != pinned_color {
                        if !first_hit && (piece_type == QUEEN || piece_type == ROOK) {
                            pin_squares = pin_squares | pin_squares_ray;
                        }
                        break;
                    }
                    else if first_hit {
                        first_hit = false;
                        pin_squares_ray = pin_squares_ray | sq;
                    }
                    else {
                        break;
                    }
                }
            }
            pin_squares
        }

        fn generate_pin_mask_bq(&self, sqr: Square, pinned_color: Color) -> BitBoard {
            let mut pin_squares = BitBoard(0);
            let mut first_hit = true;
            let mut pin_squares_ray = BitBoard(0);
            let rank = sqr.rank();
            let file = sqr.file();
            let min = min(rank, file);
            let max = max(rank, file);
            for n in 1..8 {
                if max + n >= 8 {
                    break;
                }
                let sq = Square::new(sqr.file() + n, sqr.rank() + n);
                if let Some(Piece{piece_type, color}) = self.board[sq.0] {
                    if color != pinned_color {
                        if !first_hit && (piece_type == QUEEN || piece_type == BISHOP) {
                            pin_squares = pin_squares | pin_squares_ray;
                        }
                        break;
                    }
                    else if first_hit {
                        first_hit = false;
                        pin_squares_ray = pin_squares_ray | sq;
                    }
                    else {
                        break;
                    }
                }
            }
            first_hit = true;
            pin_squares_ray = BitBoard(0);
            for n in 1..8 {
                if min < n {
                    break;
                }
                let sq = Square::new(sqr.file() - n, sqr.rank() - n);
                if let Some(Piece{piece_type, color}) = self.board[sq.0] {
                    if color != pinned_color {
                        if !first_hit && (piece_type == QUEEN || piece_type == BISHOP) {
                            pin_squares = pin_squares | pin_squares_ray;
                        }
                        break;
                    }
                    else if first_hit {
                        first_hit = false;
                        pin_squares_ray = pin_squares_ray | sq;
                    }
                    else {
                        break;
                    }
                }
            }
            first_hit = true;
            pin_squares_ray = BitBoard(0);
            for n in 1..8 {
                if rank < n || file + n >= 8 {
                    break;
                }
                let sq = Square::new(sqr.file() + n, sqr.rank() - n);
                if let Some(Piece{piece_type, color}) = self.board[sq.0] {
                    if color != pinned_color {
                        if !first_hit && (piece_type == QUEEN || piece_type == BISHOP) {
                            pin_squares = pin_squares | pin_squares_ray;
                        }
                        break;
                    }
                    else if first_hit {
                        first_hit = false;
                        pin_squares_ray = pin_squares_ray | sq;
                    }
                    else {
                        break;
                    }
                }
            }
            first_hit = true;
            pin_squares_ray = BitBoard(0);
            for n in 1..8 {
                if file < n || rank + n >= 8 {
                    break;
                }
                let sq = Square::new(sqr.file() - n, sqr.rank() + n);
                if let Some(Piece{piece_type, color}) = self.board[sq.0] {
                    if color != pinned_color {
                        if !first_hit && (piece_type == QUEEN || piece_type == BISHOP) {
                            pin_squares = pin_squares | pin_squares_ray;
                        }
                        break;
                    }
                    else if first_hit {
                        first_hit = false;
                        pin_squares_ray = pin_squares_ray | sq;
                    }
                    else {
                        break;
                    }
                }
            }
            pin_squares
        }

        fn generate_rook_moves(&self, sqr: Square, rook_color: Color) -> BitBoard {
            let mut move_squares = BitBoard(0);
            for n in (sqr.file() + 1)..8 {
                let sq = Square::new(n, sqr.rank());
                if let Some(Piece{piece_type, color}) = self.board[sq.0] {
                    if color != rook_color {
                        move_squares = move_squares | sq;
                    }
                    break;
                }
                move_squares = move_squares | sq;
            }
            for n in (0..sqr.file()).rev() {
                let sq = Square::new(n, sqr.rank());
                if let Some(Piece{piece_type, color}) = self.board[sq.0] {
                    if color != rook_color {
                        move_squares = move_squares | sq;
                    }
                    break;
                }
                move_squares = move_squares | sq;
            }
            for n in (sqr.rank() + 1)..8 {
                let sq = Square::new(sqr.file(), n);
                if let Some(Piece{piece_type, color}) = self.board[sq.0] {
                    if color != rook_color {
                        move_squares = move_squares | sq;
                    }
                    break;
                }
                move_squares = move_squares | sq;
            }
            for n in (0..sqr.rank()).rev() {
                let sq = Square::new(sqr.file(), n);
                if let Some(Piece{piece_type, color}) = self.board[sq.0] {
                    if color != rook_color {
                        move_squares = move_squares | sq;
                    }
                    break;
                }
                move_squares = move_squares | sq;
            }
            move_squares
        }

        fn generate_bishop_moves(&self, sqr: Square, bishop_color: Color) -> BitBoard {
            let mut move_squares = BitBoard(0);
            let rank = sqr.rank();
            let file = sqr.file();
            let min = min(rank, file);
            let max = max(rank, file);
            for n in 1..8 {
                if max + n >= 8 {
                    break;
                }
                let sq = Square::new(sqr.file() + n, sqr.rank() + n);
                if let Some(Piece{piece_type, color}) = self.board[sq.0] {
                    if color != bishop_color {
                        move_squares = move_squares | sq;
                    }
                    break;
                }
                move_squares = move_squares | sq;
            }
            for n in 1..8 {
                if min < n {
                    break;
                }
                let sq = Square::new(sqr.file() - n, sqr.rank() - n);
                if let Some(Piece{piece_type, color}) = self.board[sq.0] {
                    if color != bishop_color {
                        move_squares = move_squares | sq;
                    }
                    break;
                }
                move_squares = move_squares | sq;
            }
            for n in 1..8 {
                if rank < n || file + n >= 8 {
                    break;
                }
                let sq = Square::new(sqr.file() + n, sqr.rank() - n);
                if let Some(Piece{piece_type, color}) = self.board[sq.0] {
                    if color != bishop_color {
                        move_squares = move_squares | sq;
                    }
                    break;
                }
                move_squares = move_squares | sq;
            }
            for n in 1..8 {
                if file < n || rank + n >= 8 {
                    break;
                }
                let sq = Square::new(sqr.file() - n, sqr.rank() + n);
                if let Some(Piece{piece_type, color}) = self.board[sq.0] {
                    if color != bishop_color {
                        move_squares = move_squares | sq;
                    }
                    break;
                }
                move_squares = move_squares | sq;
            }
            move_squares
        }

        fn queen_attack_squares(&self, sqr: Square, color: Color) -> BitBoard {
            self.generate_bishop_moves(sqr, color) | self.generate_rook_moves(sqr, color)
        }

        fn moves_from_target_bitboard(&self, from: Square, mut possible_targets: BitBoard) -> Vec<Move> {
            let mut moves: Vec<Move> = vec!();
            while possible_targets.0 != 0 {
                let move_id = possible_targets.0.trailing_zeros();
                let to = Square(move_id as BoardIndex);
                possible_targets = possible_targets.toggle(to);
                self.create_moves(from, to, &mut moves);
            }
            moves
        }

        fn moves_from_source_bitboard(&self, to: Square, mut possible_sources: BitBoard) -> Vec<Move> {
            let mut moves: Vec<Move> = vec!();
            while possible_sources.0 != 0 {
                let move_id = possible_sources.0.trailing_zeros();
                let from = Square(move_id as BoardIndex);
                possible_sources = possible_sources.toggle(from);
                self.create_moves(from, to, &mut moves);
            }
            moves
        }

        fn create_moves(&self, from: Square, to: Square, moves: &mut Vec<Move>) {
            let from_piece = self.board[from.0].expect("From piece must exist");
            let piece_move = match self.board[to.0] {
                Some(Piece{piece_type, color}) => match from_piece.piece_type {
                    PieceType::PAWN => match from_piece.color {
                        WHITE if to.rank() == RANK_SIZE - 1 => Move { from, to, move_type: MoveAction::Promotion(QUEEN, Some(piece_type)) },
                        BLACK if to.rank() == 0 => Move { from, to, move_type: MoveAction::Promotion(QUEEN, Some(piece_type)) },
                        _ => Move { from, to, move_type: MoveAction::Capture(piece_type, to) }
                    }
                    _ => Move { from, to, move_type: MoveAction::Capture(piece_type, to) }
                },
                None => match from_piece.piece_type {
                    PieceType::PAWN => match self.en_passant_square {
                        Some(ep_sqr) if ep_sqr == to => Move { from, to, move_type: MoveAction::Capture(from_piece.piece_type, ep_sqr)},
                        _ => match from_piece.color {
                            WHITE if to.rank() == RANK_SIZE - 1 => Move { from, to, move_type: MoveAction::Promotion(QUEEN, None) },
                            BLACK if to.rank() == 0 => Move { from, to, move_type: MoveAction::Promotion(QUEEN, None) },
                            _ => Move { from, to, move_type: MoveAction::Normal }
                        }
                    },
                    PieceType::KING => match self.current_castle_options() {
                        (Some(castle_sqr), _) if to == castle_sqr => Move{from, to, move_type: MoveAction::Castle(to)},
                        (_, Some(castle_sqr)) if to == castle_sqr => Move{from, to, move_type: MoveAction::Castle(to)},
                        _ => Move{from, to, move_type: MoveAction::Normal}
                    }
                    _ => Move{from, to, move_type: MoveAction::Normal}
                },
            };
            moves.push(piece_move);
        }

        fn current_castle_options(&self) -> (Option<Square>, Option<Square>) {
            let (options, (s1, s2)) = match self.active_player {
                WHITE => (self.castling_options_white, CASTLING_SQUARES_WHITE),
                BLACK => (self.castling_options_black, CASTLING_SQUARES_BLACK)
            };
            match options {
                (true, true) => (Some(s1), Some(s2)),
                (true, false) => (Some(s1), None),
                (false, true) => (None, Some(s2)),
                (false, false) => (None, None),
            }
        }

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
                    match pieces[Square::new(file, rank).0] {
                        Some(Piece{piece_type: PieceType::KING, color: Color::WHITE}) => {
                            kings = (Square::new(file, rank), kings.1);
                        },
                        Some(Piece{piece_type: PieceType::KING, color: Color::BLACK}) => {
                            kings = (kings.0, Square::new(file, rank));
                        },
                        _ => {}
                    }
                }
            }
            if (kings.0).file() == FILE_SIZE || (kings.1).file() == FILE_SIZE {
                return None;
            }
            Some(Board::new(
                pieces,
                active_player,
                castling_options[0],
                castling_options[1],
                en_passant,
                halfmove_clock,
                fullmove
            ))
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
                "w" => Some(WHITE),
                "b" => Some(BLACK),
                _ => None
            }
        }

        fn parse_fen_board(mut positions: Split<char>) -> Option<[PieceOpt; 64]> {
            let mut pieces: [PieceOpt; 64] = [None; 64];
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
                    let square = Square::new(column_index, 7 - line_index);
                    pieces[square.0] = Some(piece);
                    column_index += 1;
                }
                if column_index > 8 {
                    return None;
                }
            }
            Some(pieces)
        }

        pub fn print_highlighted(&self, attack_squares: BitBoard) {
            print!("Active player: {:?}\n", self.active_player);
            for rank in (0..RANK_SIZE).rev() {
                for file in 0..FILE_SIZE {
                    let sqr = Square::new(file, rank);
                    if attack_squares.is_set(sqr) {
                        print!("x");
                        continue;
                    }
                    match self.board[sqr.0] {
                        Some(piece) => print!("{}", piece),
                        None => print!(".")
                    };
                }
                print!("\n");
            }
            println!("{} {}", self.halfmove_clock, self.fullmove)
        }
    }
}

#[cfg(test)]
mod check_test {
    use crate::board::board::{Board, PieceType};
    use crate::board::board::Color;
    use crate::board::board::positional::{Move, Square};
    use crate::board::board::positional::MoveAction::Promotion;
    use crate::board::pieces::PawnIterator;

    #[test]
    fn a() {
        let board = Board::from_fen("4k3/8/8/1p4pp/8/4q3/1K5q/8 w - - 18 141".to_string()).unwrap();
        assert!(board.is_in_check(&Color::WHITE));
        assert!(!board.is_in_check(&Color::BLACK));
    }

}