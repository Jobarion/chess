pub mod board {
    use std::convert::{Into, TryFrom};
    use std::fmt::{Display, Error, Formatter};
    use std::str::Split;
    use std::str::SplitWhitespace;

    use crate::bitboard::*;
    use crate::board::board::CastleState::{Allowed, Forbidden};
    use crate::evaluator::{PIECE_VALUE, PST};
    use crate::hashing::{PerftData, TranspositionTable, Zobrist, ZobristHash};
    use crate::movegen::MoveType;
    use crate::piece::*;
    use crate::piece::PieceType::*;
    use crate::piece::Color::*;

    pub(crate) type BoardIndex = usize;
    pub const RANK_SIZE: u8 = 8;
    pub const FILE_SIZE: u8 = 8;

    #[derive(Debug, Copy, Clone, PartialEq, Eq)]
    pub enum CastleState {
        Allowed,
        Forbidden(u32),
    }

    #[derive(Debug)]
    pub struct Perft {
        pub nodes: u64,
        pub captures: u64,
        pub en_passants: u64,
        pub castles: u64,
        pub promotions: u64,
        pub checks: u64,
        pub double_checks: u64,
    }

    impl Perft {
        fn new() -> Perft {
            Perft {
                nodes: 0,
                captures: 0,
                en_passants: 0,
                castles: 0,
                promotions: 0,
                checks: 0,
                double_checks: 0,
            }
        }
    }

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
                    let _ = match self.board[sqr.0 as usize] {
                        Some(piece) => write!(f, "{}", piece),
                        None => write!(f, ".")
                    };
                }
                let _ = write!(f, "\n");
            }
            write!(f, "{} {}", self.halfmove_clock, self.fullmove)
        }
    }

    #[derive(Copy, Clone, Debug)]
    pub enum GamePhase {
        Midgame = 0,
        Endgame = 1,
    }

    impl<T, const N: usize> std::ops::Index<GamePhase> for [T; N] {
        type Output = T;

        fn index(&self, index: GamePhase) -> &Self::Output {
            &self[index as usize]
        }
    }

    impl<T, const N: usize> std::ops::IndexMut<GamePhase> for [T; N] {

        fn index_mut(&mut self, index: GamePhase) -> &mut Self::Output {
            &mut self[index as usize]
        }
    }

    #[derive(Copy, Clone, Debug)]
    pub struct EvalInfo {
        pub material: [u32; 2],
        pub psqt: [i16; 2]
    }

    #[derive(Copy, Clone, Debug)]
    pub struct Board {
        pub board: [PieceOpt; (RANK_SIZE * FILE_SIZE) as usize],
        pub active_player: Color,
        pub castling_options_white: (CastleState, CastleState),
        pub castling_options_black: (CastleState, CastleState),
        pub en_passant_square: Option<Square>,
        pub halfmove_clock: u32,
        pub fullmove: u32,

        pub piece_bbs: [[BitBoard; 6]; 2],
        pub color_bbs: [BitBoard; 2], //WHITE BLACK
        pub eval_info: [EvalInfo; 2], //MIDGAME ENDGAME
        pub zobrist_key: ZobristHash,
        pub zobrist_hash_data: Zobrist,
    }
    //rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1

    impl Board {

        fn new(piece_table: [PieceOpt; (RANK_SIZE * FILE_SIZE) as usize], active_player: Color, castling_options_white: (CastleState, CastleState), castling_options_black: (CastleState, CastleState), en_passant_square: Option<Square>, halfmove_clock: u32, fullmove: u32) -> Board {
            let mut board = Board {
                board: [None; 64],
                active_player,
                castling_options_white,
                castling_options_black,
                en_passant_square,
                halfmove_clock,
                fullmove,
                zobrist_key: 0,
                piece_bbs: [[BitBoard(0); 6]; 2],
                color_bbs: [BitBoard(0); 2],
                eval_info: [EvalInfo {
                    material: [0; 2],
                    psqt: [0; 2]
                }; 2],
                zobrist_hash_data: Zobrist::new(),
            };

            for n in 0..64 {
                let piece = piece_table[n as usize];
                if let Some(p) = piece {
                    board.set(p, Square(n));
                }
            }
            board.zobrist_key = board.rehash();
            board
        }

        pub fn rehash(&self) -> ZobristHash {
            let mut hash: ZobristHash = 0;
            for n in 0..64 {
                let sqr = Square(n);
                if let Some(Piece{color, piece_type}) = self.board[sqr] {
                    hash ^= self.zobrist_hash_data.piece(color, piece_type, sqr);
                }
            }
            hash ^= self.zobrist_hash_data.castling(self.castling_options_white, self.castling_options_black);
            hash ^= self.zobrist_hash_data.side(self.active_player);
            hash ^= self.zobrist_hash_data.ep(self.en_passant_square);
            hash
        }


        pub(crate) fn get(&self, sqr: Square) -> PieceOpt {
            self.board[sqr]
        }

        pub fn set(&mut self, piece: Piece, sqr: Square) -> PieceOpt {
            let previous = self.unset(sqr);
            self.board[sqr] = Some(piece);
            self.zobrist_key ^= self.zobrist_hash_data.piece(piece.color, piece.piece_type, sqr);
            self.piece_bbs[piece.color][piece.piece_type] |= sqr;
            self.color_bbs[piece.color] |= sqr;

            self.eval_info[GamePhase::Midgame].material[piece.color] += PIECE_VALUE[GamePhase::Midgame][piece.piece_type];
            self.eval_info[GamePhase::Endgame].material[piece.color] += PIECE_VALUE[GamePhase::Endgame][piece.piece_type];
            let correct_square = match piece.color {
                White => sqr,
                Black => sqr.flip()
            };
            self.eval_info[GamePhase::Midgame].psqt[piece.color] += PST[GamePhase::Midgame][piece.piece_type][correct_square];
            self.eval_info[GamePhase::Endgame].psqt[piece.color] += PST[GamePhase::Endgame][piece.piece_type][correct_square];

            previous
        }

        pub fn unset(&mut self, sqr: Square) -> PieceOpt {
            if let Some(piece) = self.board[sqr] {
                self.zobrist_key ^= self.zobrist_hash_data.piece(piece.color, piece.piece_type, sqr);
                let anti_mask = BitBoard(!(1 << sqr.0));
                self.board[sqr] = None;
                self.piece_bbs[piece.color][piece.piece_type] &= anti_mask;
                self.color_bbs[piece.color] &= anti_mask;
                self.eval_info[GamePhase::Midgame].material[piece.color] -= PIECE_VALUE[GamePhase::Midgame][piece.piece_type];
                self.eval_info[GamePhase::Endgame].material[piece.color] -= PIECE_VALUE[GamePhase::Endgame][piece.piece_type];
                let correct_square = match piece.color {
                    White => sqr,
                    Black => sqr.flip()
                };
                self.eval_info[GamePhase::Midgame].psqt[piece.color] -= PST[GamePhase::Midgame][piece.piece_type][correct_square];
                self.eval_info[GamePhase::Endgame].psqt[piece.color] -= PST[GamePhase::Endgame][piece.piece_type][correct_square];
                Some(piece)
            } else {
                None
            }
        }

        pub fn change_active(&mut self) {
            self.zobrist_key ^= self.zobrist_hash_data.side(self.active_player);
            self.active_player = !self.active_player;
            self.zobrist_key ^= self.zobrist_hash_data.side(self.active_player);
        }

        fn set_ep_square(&mut self, ep: Option<Square>) {
            self.zobrist_key ^= self.zobrist_hash_data.ep(self.en_passant_square);
            self.en_passant_square = ep;
            self.zobrist_key ^= self.zobrist_hash_data.ep(self.en_passant_square);
        }

        fn set_castle_options(&mut self, white: (CastleState, CastleState), black: (CastleState, CastleState)) {
            self.zobrist_key ^= self.zobrist_hash_data.castling(self.castling_options_white, self.castling_options_black);
            self.castling_options_white = white;
            self.castling_options_black = black;
            self.zobrist_key ^= self.zobrist_hash_data.castling(self.castling_options_white, self.castling_options_black);
        }

        pub fn occupied(&self) -> BitBoard {
            self.color_bbs[White] | self.color_bbs[Black]
        }

        pub fn apply_move(&mut self, piece_move: &Move) {
            let moved_piece = self.unset(piece_move.from).unwrap();
            self.unset(piece_move.to);
            let old_ep_square = self.en_passant_square;
            let mut new_ep_square = None;
            match piece_move {
                Move{to, move_type: MoveAction::Promotion(promotion_type, _), ..} => {
                    self.set(Piece{piece_type: *promotion_type, color: self.active_player}, *to);
                    self.halfmove_clock = 0;
                },
                Move{to, move_type: MoveAction::Capture(_), ..} => {
                    self.set(moved_piece, *to);
                    self.halfmove_clock = 0;
                },
                Move{from, to, move_type: MoveAction::Normal, ..} if moved_piece.piece_type == Pawn => {
                    let pawn_starting_rank = match self.active_player {
                        White => 1,
                        Black => 6
                    };
                    if from.rank() == pawn_starting_rank && (to.rank() as i32 - from.rank() as i32).abs() == 2 {
                        new_ep_square = Some(Square::new(from.file(), (from.rank() + to.rank()) / 2));
                    }
                    self.set(moved_piece, *to);
                    self.halfmove_clock = 0;
                }
                Move{to, move_type: MoveAction::Normal, ..} => {
                    self.set(moved_piece, *to);
                    // self.halfmove_clock += 1;
                }
                Move{from, to, move_type: MoveAction::EnPassant, ..} => {
                    self.set(moved_piece, *to);
                    let old_ep_square = old_ep_square.expect("EnPassant move requires en passant square");
                    self.unset(Square::new(old_ep_square.file(), from.rank()));
                    self.halfmove_clock = 0;
                }
                Move { to, move_type: MoveAction::Castle(csquare), ..} => {
                    self.set(moved_piece, *to);
                    let rook = self.unset(Square::new(if csquare.file() == 3 { 0 } else { 7 }, csquare.rank())).unwrap();
                    self.set(rook, *csquare);
                }
            }
            self.set_ep_square(new_ep_square);
            //Capturing the rook doesn't change castle options
            let mut copt_white = self.castling_options_white.clone();
            let mut copt_black = self.castling_options_black.clone();
            if self.castling_options_white.0 == Allowed {
                copt_white.0 = match moved_piece {
                    Piece {piece_type: King, color: White } => Forbidden(self.fullmove * 2),
                    Piece {piece_type: Rook, color: White } if piece_move.from == Square::new(0, 0) => Forbidden(self.fullmove * 2),
                    _ if piece_move.to == Square::new(0, 0) => Forbidden(self.fullmove * 2 + 1), //Capture
                    _ => Allowed
                }
            }
            if self.castling_options_white.1 == Allowed {
                copt_white.1 = match moved_piece {
                    Piece {piece_type: King, color: White } => Forbidden(self.fullmove * 2),
                    Piece {piece_type: Rook, color: White } if piece_move.from == Square::new(7, 0) => Forbidden(self.fullmove * 2),
                    _ if piece_move.to == Square::new(7, 0) => Forbidden(self.fullmove * 2 + 1), //Capture
                    _ => Allowed
                }
            }
            if self.castling_options_black.0 == Allowed {
                copt_black.0 = match moved_piece {
                    Piece {piece_type: King, color: Black } => Forbidden(self.fullmove * 2 + 1),
                    Piece {piece_type: Rook, color: Black } if piece_move.from == Square::new(0, 7) => Forbidden(self.fullmove * 2 + 1),
                    _ if piece_move.to == Square::new(0, 7) => Forbidden(self.fullmove * 2), //Capture
                    _ => Allowed
                }
            }
            if self.castling_options_black.1 == Allowed {
                copt_black.1 = match moved_piece {
                    Piece {piece_type: King, color: Black } => Forbidden(self.fullmove * 2 + 1),
                    Piece {piece_type: Rook, color: Black } if piece_move.from == Square::new(7, 7) => Forbidden(self.fullmove * 2 + 1),
                    _ if piece_move.to == Square::new(7, 7) => Forbidden(self.fullmove * 2), //Capture
                    _ => Allowed
                }
            }
            self.set_castle_options(copt_white, copt_black);
            if self.active_player == Black {
                self.fullmove += 1;
            }
            self.change_active();
        }

        pub fn undo_move(&mut self, piece_move: &Move) {
            self.change_active();
            if self.active_player == Black {
                self.fullmove -= 1;
            }
            let half_move_timer = if self.active_player == White {
                self.fullmove * 2
            } else {
                self.fullmove * 2 + 1
            };
            let mut copt_white = (Allowed, Allowed);
            let mut copt_black = (Allowed, Allowed);
            copt_white.0 = match self.castling_options_white.0 {
                Forbidden(m) if m == half_move_timer => Allowed,
                _ => self.castling_options_white.0
            };
            copt_white.1 = match self.castling_options_white.1 {
                Forbidden(m) if m == half_move_timer => Allowed,
                _ => self.castling_options_white.1
            };
            copt_black.0 = match self.castling_options_black.0 {
                Forbidden(m) if m == half_move_timer => Allowed,
                _ => self.castling_options_black.0
            };
            copt_black.1 = match self.castling_options_black.1 {
                Forbidden(m) if m == half_move_timer => Allowed,
                _ => self.castling_options_black.1
            };
            self.set_castle_options(copt_white, copt_black);
            let moved_piece = self.unset(piece_move.to).unwrap();

            debug_assert_eq!(moved_piece.color, self.active_player);
            self.set_ep_square(piece_move.previous_ep_square);

            match piece_move {
                Move{from, to: _, move_type: MoveAction::Promotion(_, None), previous_ep_square: _ } => {
                    self.set(Piece{piece_type: PieceType::Pawn, color: self.active_player}, *from);
                }
                Move{from, to, move_type: MoveAction::Promotion(_, Some(captured_type)), previous_ep_square: _} => {
                    self.set(Piece{piece_type: PieceType::Pawn, color: self.active_player}, *from);
                    self.set(Piece{piece_type: *captured_type, color: !self.active_player}, *to);
                }
                Move{from, to, move_type: MoveAction::Capture(captured_type), previous_ep_square: _} => {
                    self.set(moved_piece, *from);
                    self.set(Piece{piece_type: *captured_type, color: !self.active_player}, *to);
                }
                Move{from, to: _, move_type: MoveAction::EnPassant, previous_ep_square} => {
                    self.set(moved_piece, *from);
                    let previous_ep_square = previous_ep_square.expect("EnPassant undo requires previous en passant square");
                    self.set(Piece{piece_type: PieceType::Pawn, color: !self.active_player}, Square::new(previous_ep_square.file(), from.rank()));
                }
                Move { from, to: _, move_type: MoveAction::Normal, previous_ep_square: _ } => {
                    self.set(moved_piece, *from);
                }
                Move { from, to: _, move_type: MoveAction::Castle(csquare), previous_ep_square: _ } => {
                    self.set(moved_piece, *from);
                    let rook = self.unset(*csquare).unwrap();
                    self.set(rook, Square::new(if csquare.file() == 3 { 0 } else { 7 }, csquare.rank()));
                }
            }
        }

        pub fn perft(&mut self, depth: u8, hash_size: usize) -> u64 {
            let mut tt_table = TranspositionTable::new(hash_size);
            if depth > 0 {
                self._perft(depth, &mut tt_table)
            } else {
                1
            }
        }

        fn _perft(&mut self, depth: u8, mut tt_table: &mut TranspositionTable<PerftData, 2>) -> u64 {
            if let Some(hash_result) = tt_table.retrieve(self.zobrist_key).and_then(|r|r.get_nodes(depth)) {
                return hash_result;
            }
            let mut perft = 0;
            for lmove in self.legal_moves(MoveType::All).legal_moves {
                //Don't descend into king captures
                if let MoveAction::Capture(King) = lmove.move_type {
                    continue;
                }
                self.apply_move(&lmove);
                if depth == 1 {
                    perft += 1;
                }
                else {
                    perft += self._perft(depth - 1, &mut tt_table);
                }
                self.undo_move(&lmove);
            }
            tt_table.store(PerftData {
                nodes: perft,
                depth
            }, self.zobrist_key);
            perft
        }

        pub fn to_fen(&self) -> String {
            let mut fen = "".to_string();
            for rank in (0..8).rev() {
                let mut empty_counter = 0;
                for file in 0..8 {
                    if let Some(piece) = self.board[Square::new(file, rank)] {
                        if empty_counter > 0 {
                            fen.push_str(empty_counter.to_string().as_str());
                            empty_counter = 0;
                        }
                        fen.push(piece.into());
                    } else {
                        empty_counter += 1;
                    }
                }
                if empty_counter > 0 {
                    fen.push_str(empty_counter.to_string().as_str())
                }
                if rank != 0 {
                    fen.push('/');
                }
            }
            fen.push(' ');
            fen.push_str(match self.active_player {
                White => "w ",
                Black => "b "
            });
            let mut castle_opts = "".to_string();
            castle_opts.push_str(match self.castling_options_white {
                (CastleState::Allowed, CastleState::Allowed) => "KQ",
                (CastleState::Allowed, CastleState::Forbidden(_)) => "Q",
                (CastleState::Forbidden(_), CastleState::Allowed) => "K",
                (CastleState::Forbidden(_), CastleState::Forbidden(_)) => ""
            });
            castle_opts.push_str(match self.castling_options_black {
                (CastleState::Allowed, CastleState::Allowed) => "kq",
                (CastleState::Allowed, CastleState::Forbidden(_)) => "q",
                (CastleState::Forbidden(_), CastleState::Allowed) => "k",
                (CastleState::Forbidden(_), CastleState::Forbidden(_)) => ""
            });
            if castle_opts.is_empty() {
                castle_opts.push_str("-");
            }
            fen.push_str(castle_opts.as_str());
            fen.push_str(" ");
            fen.push_str(match self.en_passant_square {
                Some(ep_square) => Into::<String>::into(ep_square),
                None => "-".to_string()
            }.as_str());
            fen.push_str(" ");
            fen.push_str(self.halfmove_clock.to_string().as_str());
            fen.push_str(" ");
            fen.push_str(self.fullmove.to_string().as_str());

            fen
        }

        pub fn from_fen(mut fen: &str) -> Option<Self> {
            if fen == "startpos" {
                fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
            }
            let mut fen_parts: SplitWhitespace = fen.split_whitespace();
            let positions: Split<char> = fen_parts.next()?.split('/');
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
                    match pieces[Square::new(file, rank)] {
                        Some(Piece{piece_type: PieceType::King, color: Color::White }) => {
                            kings = (Square::new(file, rank), kings.1);
                        },
                        Some(Piece{piece_type: PieceType::King, color: Color::Black }) => {
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

        fn parse_castling_options(castle_string: &str) -> Option<[(CastleState, CastleState); 2]> {
            let mut castling_white = (Forbidden(0), Forbidden(0));
            let mut castling_black = (Forbidden(0), Forbidden(0));
            if castle_string == "-" {
                return Some([castling_white, castling_black]);
            }
            for c in castle_string.chars() {
                match c {
                    'Q' => castling_white = (Allowed, castling_white.1),
                    'K' => castling_white = (castling_white.0, Allowed),
                    'q' => castling_black = (Allowed, castling_black.1),
                    'k' => castling_black = (castling_black.0, Allowed),
                    _ => return None,
                }
            }
            Some([castling_white, castling_black])
        }

        fn parse_active_player(active_player: &str) -> Option<Color> {
            match active_player {
                "w" => Some(White),
                "b" => Some(Black),
                _ => None
            }
        }

        fn parse_fen_board(mut positions: Split<char>) -> Option<[PieceOpt; 64]> {
            let mut pieces: [PieceOpt; 64] = [None; 64];
            for line_index in 0..8 {
                let mut line = positions.next()?.chars();
                let mut column_index: u8 = 0;
                while column_index < 8 {
                    let piece = line.next()?;
                    match piece.to_digit(10) {
                        Some(n @ 1..=8) => {
                            column_index += n as u8;
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
                    pieces[square] = Some(piece);
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
                    match self.board[sqr] {
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
