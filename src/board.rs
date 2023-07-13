pub mod board {
    use std::any::type_name;
    use std::arch::x86_64::{__m128i, __m256i, _mm256_and_si256, _mm256_or_si256, _mm256_set_epi64x, _mm256_sllv_epi64, _mm256_srlv_epi64, _mm256_storeu_si256, _mm_and_si128, _mm_or_si128, _mm_set_epi64x, _mm_sllv_epi64, _mm_srlv_epi64, _mm_store_si128};
    use std::cmp::{max, min};
    use std::convert::{Into, TryFrom};
    use std::ffi::c_int;
    use std::fmt::{Display, Error, Formatter, Write};
    use std::ops::Not;
    use std::pin::pin;
    use std::str::Split;
    use std::str::SplitWhitespace;

    use itertools::Itertools;

    use crate::bitboard::{BISHOP_MOVES, BitBoard, DIAGONALS_NE_SW, DIAGONALS_NW_SE, FILES, KING_MOVES, KNIGHT_MOVES, PAWN_CAPTURE_MOVES_BLACK, PAWN_CAPTURE_MOVES_WHITE, PAWN_MOVES_BLACK, PAWN_MOVES_WHITE, RANKS, ROOK_MOVES};
    use crate::board::board::CastleState::{Allowed, Forbidden};
    use crate::piece::*;
    use crate::piece::PieceType::*;
    use crate::piece::Color::*;

    pub(crate) type BoardIndex = usize;
    pub const RANK_SIZE: BoardIndex = 8;
    pub const FILE_SIZE: BoardIndex = 8;
    pub(crate) const CASTLING_SQUARES_WHITE: (Square, Square) = (Square::new(2, 0), Square::new(6, 0));
    pub(crate) const CASTLING_SQUARES_BLACK: (Square, Square) = (Square::new(2, 7), Square::new(6, 7));

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

    pub struct LegalMoveData {
        pub legal_moves: Vec<Move>,
        pub king_danger_mask: BitBoard,
        pub pin_mask: BitBoard,
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

        fn new(board: [PieceOpt; (RANK_SIZE * FILE_SIZE) as usize], active_player: Color, castling_options_white: (CastleState, CastleState), castling_options_black: (CastleState, CastleState), en_passant_square: Option<Square>, halfmove_clock: u32, fullmove: u32) -> Board {
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
            self.board[sqr]
        }

        pub fn set(&mut self, piece: Piece, sqr: Square) -> PieceOpt {
            let mut previous = self.unset(sqr);
            self.board[sqr] = Some(piece);
            match piece {
                Piece { piece_type: PieceType::PAWN, color: _ } => self.pawns |= sqr,
                Piece { piece_type: PieceType::ROOK, color: _ } => self.rooks |= sqr,
                Piece { piece_type: PieceType::KNIGHT, color: _ } => self.knights |= sqr,
                Piece { piece_type: PieceType::BISHOP, color: _ } => self.bishops |= sqr,
                Piece { piece_type: PieceType::QUEEN, color: _ } => self.queens |= sqr,
                Piece { piece_type: PieceType::KING, color: _ } => self.kings |= sqr,
            }
            match piece {
                Piece{ piece_type: _, color: Color::WHITE } => self.white |= sqr,
                Piece{ piece_type: _, color: Color::BLACK } => self.black |= sqr,
            }
            previous
        }

        pub fn unset(&mut self, sqr: Square) -> PieceOpt {
            let piece = self.board[sqr];
            let anti_mask = BitBoard(!(1 << sqr.0));
            self.board[sqr] = None;
            match piece {
                Some(Piece { piece_type: PAWN, color: _ }) => self.pawns &= anti_mask,
                Some(Piece { piece_type: ROOK, color: _ }) => self.rooks &= anti_mask,
                Some(Piece { piece_type: KNIGHT, color: _ }) => self.knights &= anti_mask,
                Some(Piece { piece_type: BISHOP, color: _ }) => self.bishops &= anti_mask,
                Some(Piece { piece_type: QUEEN, color: _ }) => self.queens &= anti_mask,
                Some(Piece { piece_type: KING, color: _ }) => self.kings &= anti_mask,
                _ => {}
            }
            match piece {
                Some(Piece{ piece_type: _, color: WHITE }) => self.white &= anti_mask,
                Some(Piece{ piece_type: _, color: BLACK }) => self.black &= anti_mask,
                _ => {}
            }
            piece
        }

        pub fn apply_move(&mut self, piece_move: &Move) {
            let moved_piece = self.unset(piece_move.from).unwrap();
            self.unset(piece_move.to);
            let old_ep_square = self.en_passant_square;
            self.en_passant_square = None;
            match piece_move {
                Move{from, to, move_type: MoveAction::Promotion(promotion_type, _), previous_ep_square} => {
                    self.set(Piece{piece_type: *promotion_type, color: self.active_player}, *to);
                    self.halfmove_clock = 0;
                },
                Move{from, to, move_type: MoveAction::Capture(_), previous_ep_square} => {
                    self.set(moved_piece, *to);
                    self.halfmove_clock = 0;
                },
                Move{from, to, move_type: MoveAction::Normal, previous_ep_square} if moved_piece.piece_type == PAWN => {
                    let pawn_starting_rank = match self.active_player {
                        WHITE => 1,
                        BLACK => 6
                    };
                    if from.rank() == pawn_starting_rank && (to.rank() as i32 - from.rank() as i32).abs() == 2 {
                        self.en_passant_square = Some(Square::new(from.file(), (from.rank() + to.rank()) / 2));
                    }
                    self.set(moved_piece, *to);
                    self.halfmove_clock = 0;
                }
                Move{from, to, move_type: MoveAction::Normal, previous_ep_square} => {
                    self.set(moved_piece, *to);
                    // self.halfmove_clock += 1;
                }
                Move{from, to, move_type: MoveAction::EnPassant, previous_ep_square} => {
                    self.set(moved_piece, *to);
                    let old_ep_square = old_ep_square.expect("EnPassant move requires en passant square");
                    self.unset(Square::new(old_ep_square.file(), from.rank()));
                    self.halfmove_clock = 0;
                }
                Move { from, to, move_type: MoveAction::Castle(csquare), previous_ep_square: _ } => {
                    self.set(moved_piece, *to);
                    let rook = self.unset(Square::new(if csquare.file() == 3 { 0 } else { 7 }, csquare.rank())).unwrap();
                    self.set(rook, *csquare);
                }
            }
            //Capturing the rook doesn't change castle options
            if self.castling_options_white.0 == Allowed {
                self.castling_options_white.0 = match moved_piece {
                    Piece {piece_type: KING, color: WHITE} => Forbidden(self.fullmove * 2),
                    Piece {piece_type: ROOK, color: WHITE} if piece_move.from == Square::new(0, 0) => Forbidden(self.fullmove * 2),
                    _ if piece_move.to == Square::new(0, 0) => Forbidden(self.fullmove * 2 + 1), //Capture
                    _ => Allowed
                }
            }
            if self.castling_options_white.1 == Allowed {
                self.castling_options_white.1 = match moved_piece {
                    Piece {piece_type: KING, color: WHITE} => Forbidden(self.fullmove * 2),
                    Piece {piece_type: ROOK, color: WHITE} if piece_move.from == Square::new(7, 0) => Forbidden(self.fullmove * 2),
                    _ if piece_move.to == Square::new(7, 0) => Forbidden(self.fullmove * 2 + 1), //Capture
                    _ => Allowed
                }
            }
            if self.castling_options_black.0 == Allowed {
                self.castling_options_black.0 = match moved_piece {
                    Piece {piece_type: KING, color: BLACK} => Forbidden(self.fullmove * 2 + 1),
                    Piece {piece_type: ROOK, color: BLACK} if piece_move.from == Square::new(0, 7) => Forbidden(self.fullmove * 2 + 1),
                    _ if piece_move.to == Square::new(0, 7) => Forbidden(self.fullmove * 2), //Capture
                    _ => Allowed
                }
            }
            if self.castling_options_black.1 == Allowed {
                self.castling_options_black.1 = match moved_piece {
                    Piece {piece_type: KING, color: BLACK} => Forbidden(self.fullmove * 2 + 1),
                    Piece {piece_type: ROOK, color: BLACK} if piece_move.from == Square::new(7, 7) => Forbidden(self.fullmove * 2 + 1),
                    _ if piece_move.to == Square::new(7, 7) => Forbidden(self.fullmove * 2), //Capture
                    _ => Allowed
                }
            }
            if self.active_player == BLACK {
                self.fullmove += 1;
            }
            self.active_player = !self.active_player;
        }

        pub fn undo_move(&mut self, piece_move: &Move) {
            self.active_player = !self.active_player;
            if self.active_player == BLACK {
                self.fullmove -= 1;
            }
            let half_move_timer = if self.active_player == WHITE {
                self.fullmove * 2
            } else {
                self.fullmove * 2 + 1
            };
            self.castling_options_white.0 = match self.castling_options_white.0 {
                Forbidden(m) if m == half_move_timer => Allowed,
                _ => self.castling_options_white.0
            };
            self.castling_options_white.1 = match self.castling_options_white.1 {
                Forbidden(m) if m == half_move_timer => Allowed,
                _ => self.castling_options_white.1
            };
            self.castling_options_black.0 = match self.castling_options_black.0 {
                Forbidden(m) if m == half_move_timer => Allowed,
                _ => self.castling_options_black.0
            };
            self.castling_options_black.1 = match self.castling_options_black.1 {
                Forbidden(m) if m == half_move_timer => Allowed,
                _ => self.castling_options_black.1
            };
            let moved_piece = self.unset(piece_move.to).unwrap();

            debug_assert_eq!(moved_piece.color, self.active_player);
            self.en_passant_square = piece_move.previous_ep_square;

            match piece_move {
                Move{from, to, move_type: MoveAction::Promotion(_, None), previous_ep_square } => {
                    self.set(Piece{piece_type: PieceType::PAWN, color: self.active_player}, *from);
                }
                Move{from, to, move_type: MoveAction::Promotion(_, Some(captured_type)), previous_ep_square} => {
                    self.set(Piece{piece_type: PieceType::PAWN, color: self.active_player}, *from);
                    self.set(Piece{piece_type: *captured_type, color: !self.active_player}, *to);
                }
                Move{from, to, move_type: MoveAction::Capture(captured_type), previous_ep_square} => {
                    self.set(moved_piece, *from);
                    self.set(Piece{piece_type: *captured_type, color: !self.active_player}, *to);
                }
                Move{from, to, move_type: MoveAction::EnPassant, previous_ep_square} => {
                    self.set(moved_piece, *from);
                    let previous_ep_square = previous_ep_square.expect("EnPassant undo requires previous en passant square");
                    self.set(Piece{piece_type: PieceType::PAWN, color: !self.active_player}, Square::new(previous_ep_square.file(), from.rank()));
                }
                Move { from, to, move_type: MoveAction::Normal, previous_ep_square: _ } => {
                    self.set(moved_piece, *from);
                }
                Move { from, to, move_type: MoveAction::Castle(csquare), previous_ep_square: _ } => {
                    self.set(moved_piece, *from);
                    let rook = self.unset(*csquare).unwrap();
                    self.set(rook, Square::new(if csquare.file() == 3 { 0 } else { 7 }, csquare.rank()));
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
            if depth > 0 {
                self._perft(&mut perft, depth);
            }
            else {
                perft.nodes = 1
            }
            perft
        }

        fn _perft(&mut self, mut perft: &mut Perft, depth: u8) {
            for lmove in self.legal_moves().legal_moves {
                //Don't descend into king captures
                if let MoveAction::Capture(KING) = lmove.move_type {
                    continue;
                }
                // let old_castle_ops = (self.castling_options_white, self.castling_options_black).clone();
                self.apply_move(&lmove);
                if depth == 1 {
                    match lmove.move_type {
                        MoveAction::Capture(_) => perft.captures += 1,
                        MoveAction::EnPassant => {
                            perft.captures += 1;
                            perft.en_passants += 1;
                        },
                        MoveAction::Promotion(_, None) => perft.promotions += 1,
                        MoveAction::Promotion(_, Some(_)) => {
                            perft.promotions += 1;
                            perft.captures += 1;
                        },
                        MoveAction::Castle(_) => {
                            perft.castles += 1;
                        }
                        _ => {}
                    }
                    perft.nodes += 1;
                }
                else {
                    self._perft(&mut perft, depth - 1);
                }
                self.undo_move(&lmove);

                // if old_castle_ops != (self.castling_options_white, self.castling_options_black) {
                //     println!("Diff in move {}, depth {}", lmove.to_uci(), depth);
                //     break;
                // }
            }
        }

        pub(crate) fn legal_moves(&self) -> LegalMoveData {
            let player_masks: (BitBoard, BitBoard) = match self.active_player {
                WHITE => (self.white, self.black),
                BLACK => (self.black, self.white)
            };

            let king = self.kings & player_masks.0;
            let king_square = Square(king.0.trailing_zeros() as BoardIndex);

            let king_danger_squares = self.generate_king_danger_squares();

            // println!("Danger squares");
            // self.print_highlighted(king_danger_squares);

            if king_danger_squares & king_square != 0 {
                return self.generate_king_danger_moves(king_square, king_danger_squares);
            }
            let pin_mask = self.generate_pin_mask(king_square, self.active_player);

            let mut moves: Vec<Move> = vec!();

            //Checking once if the pin mask is set is probably slightly faster, but it would duplicate the entire loop block below
            for sqr in self.active_color_mask() & self.rooks {
                let mut move_mask = self.generate_rook_moves_ks(sqr, self.active_player);
                move_mask &= Board::calculate_partial_pin_mask(sqr, king_square, pin_mask);
                moves.append(&mut self.moves_from_target_bitboard(sqr, move_mask));
            }
            for sqr in self.active_color_mask() & self.bishops {
                let mut move_mask = self.generate_bishop_moves_ks(sqr, self.active_player);
                move_mask &= Board::calculate_partial_pin_mask(sqr, king_square, pin_mask);
                moves.append(&mut self.moves_from_target_bitboard(sqr, move_mask));
            }
            for sqr in self.active_color_mask() & self.queens {
                let mut move_mask = self.generate_queen_moves_ks(sqr, self.active_player);
                move_mask &= Board::calculate_partial_pin_mask(sqr, king_square, pin_mask);
                moves.append(&mut self.moves_from_target_bitboard(sqr, move_mask));
            }
            for sqr in self.active_color_mask() & self.knights {
                if pin_mask.is_set(sqr) {
                    continue;
                }
                let move_mask = KNIGHT_MOVES[sqr] & !self.active_color_mask();
                moves.append(&mut self.moves_from_target_bitboard(sqr, move_mask));
            }
            let ep_mask = self.en_passant_square
                .map(| sqr| BitBoard::from(sqr))
                .unwrap_or(BitBoard(0));
            if self.active_player == WHITE {
                for sqr in self.white & self.pawns {
                    let mut move_mask = self.generate_pawn_moves_squares_white(sqr, ep_mask);
                    move_mask &= Board::calculate_partial_pin_mask(sqr, king_square, pin_mask);
                    moves.append(&mut self.moves_from_target_bitboard(sqr, move_mask));
                }
            }
            else {
                for sqr in self.black & self.pawns {
                    let mut move_mask = self.generate_pawn_moves_squares_black(sqr, ep_mask);
                    move_mask &= Board::calculate_partial_pin_mask(sqr, king_square, pin_mask);
                    moves.append(&mut self.moves_from_target_bitboard(sqr, move_mask));
                }
            }
            let move_mask = self.generate_king_moves(king_square, king_danger_squares) & !king_danger_squares;
            moves.append(&mut self.moves_from_target_bitboard(king_square, move_mask));

            LegalMoveData { legal_moves: moves, king_danger_mask: king_danger_squares, pin_mask }
        }

        fn generate_king_danger_moves(&self, king_square: Square, king_danger_squares: BitBoard) -> LegalMoveData {
            let mut moves: Vec<Move> = vec!();

            let evasive_king_moves = KING_MOVES[king_square] & !king_danger_squares & !self.active_color_mask();
            moves.append(&mut self.moves_from_target_bitboard(king_square, evasive_king_moves));

            //Double check, the king can only run away
            let attackers = self.generate_attacker_squares(king_square);
            if attackers.0.count_ones() > 1 {
                return LegalMoveData { legal_moves: moves, king_danger_mask: king_danger_squares, pin_mask: BitBoard(0) };
            }

            let attacker_square = Square(attackers.0.trailing_zeros() as BoardIndex);

            // println!("Attackers");
            // self.print_highlighted(attackers);

            let pin_mask = self.generate_pin_mask(king_square, self.active_player);

            let block_mask = if self.board[attacker_square].unwrap().piece_type.is_slider() {
                Board::ray_between_squares_exclusive(king_square, attacker_square)
            } else {
                BitBoard(0)
            };

            // println!("Block mask");
            // self.print_highlighted(block_mask);

            let check_prevention_mask = block_mask | attackers;

            // println!("Legal check prevention mask");
            // self.print_highlighted(check_prevention_mask);

            let mut ep_check_capture_mask = BitBoard(0);
            if let Some(ep_square) = self.en_passant_square {
                let moved_pawn_square = Square::new(ep_square.file(), if ep_square.rank() == 2 { 3 } else { 4 });
                if attackers & moved_pawn_square != 0 {
                    ep_check_capture_mask |= ep_square;
                }
            }

            for sqr in self.active_color_mask() & self.rooks {
                let mut move_mask = self.generate_rook_moves_ks(sqr, self.active_player) & check_prevention_mask;
                move_mask &= Board::calculate_partial_pin_mask(sqr, king_square, pin_mask);
                moves.append(&mut self.moves_from_target_bitboard(sqr, move_mask));
            }
            for sqr in self.active_color_mask() & self.bishops {
                let mut move_mask = self.generate_bishop_moves_ks(sqr, self.active_player) & check_prevention_mask;
                move_mask &= Board::calculate_partial_pin_mask(sqr, king_square, pin_mask);
                moves.append(&mut self.moves_from_target_bitboard(sqr, move_mask));
            }
            for sqr in self.active_color_mask() & self.queens {
                let mut move_mask = self.generate_queen_moves_ks(sqr, self.active_player) & check_prevention_mask;
                move_mask &= Board::calculate_partial_pin_mask(sqr, king_square, pin_mask);
                moves.append(&mut self.moves_from_target_bitboard(sqr, move_mask));
            }
             for sqr in self.active_color_mask() & self.knights {
                 if pin_mask.is_set(sqr) {
                     continue;
                 }
                 let move_mask = KNIGHT_MOVES[sqr] & !self.active_color_mask() & check_prevention_mask;
                 moves.append(&mut self.moves_from_target_bitboard(sqr, move_mask));
             }
            let ep_mask = self.en_passant_square
                .map(| sqr| BitBoard::from(sqr))
                .unwrap_or(BitBoard(0));
            let pawn_check_prevention_mask= check_prevention_mask | ep_check_capture_mask;
            if self.active_player == WHITE {
                for sqr in self.white & self.pawns {
                    let mut move_mask = self.generate_pawn_moves_squares_white(sqr, ep_mask) & pawn_check_prevention_mask;
                    move_mask &= Board::calculate_partial_pin_mask(sqr, king_square, pin_mask);
                    moves.append(&mut self.moves_from_target_bitboard(sqr, move_mask));
                }
            }
            else {
                for sqr in self.black & self.pawns {
                    let mut move_mask = self.generate_pawn_moves_squares_black(sqr, ep_mask) & pawn_check_prevention_mask;
                    move_mask &= Board::calculate_partial_pin_mask(sqr, king_square, pin_mask);
                    moves.append(&mut self.moves_from_target_bitboard(sqr, move_mask));
                }
            }

            LegalMoveData { legal_moves: moves, king_danger_mask: king_danger_squares, pin_mask }
        }

        fn calculate_partial_pin_mask(piece_square: Square, king_square: Square, pin_mask: BitBoard) -> BitBoard {
            if pin_mask.is_set(piece_square) {
                match king_square.direction_between(&piece_square) {
                    (1, 0) | (-1, 0) => RANKS[piece_square.rank()],
                    (0, 1) | (0, -1) => FILES[piece_square.file()],
                    (1, 1) | (-1, -1) => DIAGONALS_NW_SE[piece_square],
                    (1, -1) | (-1, 1) => DIAGONALS_NE_SW[piece_square],
                    _ => panic!()
                }
            } else {
                BitBoard(0xFFFFFFFFFFFFFFFF)
            }
        }

        fn ray_between_squares_exclusive(from: Square, to: Square) -> BitBoard {
            let file_diff: i8 = to.file() as i8 - from.file() as i8;
            let rank_diff: i8 = to.rank() as i8 - from.rank() as i8;
            let df = file_diff.signum();
            let dr = rank_diff.signum();
            let mut file = from.file() as i8 + df ;
            let mut rank = from.rank() as i8 + dr;
            let mut ray_mask = BitBoard(0);
            while file as BoardIndex != to.file() || rank as BoardIndex != to.rank() {
                ray_mask = ray_mask.set(Square::new(file as BoardIndex, rank as BoardIndex));
                file = file + df;
                rank = rank + dr;
            }
            ray_mask
        }

        fn ray_between_squares_inclusive(from: Square, to: Square) -> BitBoard {
            let file_diff: i8 = to.file() as i8 - from.file() as i8;
            let rank_diff: i8 = to.rank() as i8 - from.rank() as i8;
            let df = file_diff.signum();
            let dr = rank_diff.signum();
            let mut file = from.file() as i8;
            let mut rank = from.rank() as i8;
            let mut ray_mask = BitBoard(0);
            loop {
                ray_mask = ray_mask.set(Square::new(file as BoardIndex, rank as BoardIndex));
                if file as BoardIndex == to.file() && rank as BoardIndex == to.rank() {
                    break;
                }
                file = file + df;
                rank = rank + dr;
            }
            ray_mask
        }

        //TODO simdify?
        fn generate_pawn_moves_squares_white(&self, sqr: Square, ep_mask: BitBoard) -> BitBoard {
            let free = !(self.white | self.black);
            let mut push_mask = BitBoard::from(sqr) << 8 & free;
            push_mask |= push_mask << 8 & free & RANKS[3];
            push_mask | (PAWN_CAPTURE_MOVES_WHITE[sqr] & (self.black | ep_mask))
        }

        fn generate_pawn_moves_squares_black(&self, sqr: Square, ep_mask: BitBoard) -> BitBoard {
            let free = !(self.white | self.black);
            let mut push_mask = BitBoard::from(sqr) >> 8 & free;
            push_mask |= push_mask >> 8 & free & RANKS[4];
            push_mask | (PAWN_CAPTURE_MOVES_BLACK[sqr] & (self.white | ep_mask))
        }

        fn generate_king_moves(&self, sqr: Square, king_danger_squares: BitBoard) -> BitBoard {
            let mut king_moves = KING_MOVES[sqr] & !self.mask_for_color(self.active_player);
            let no_castle_squares = king_danger_squares | ((self.white | self.black) ^ self.kings);
            if let (Some(c_square), _) = self.current_castle_options() {
                let rook_passing_square = Square::new((c_square.file() as isize + sqr.direction_between(&c_square).0) as usize, c_square.rank());
                if Board::ray_between_squares_inclusive(sqr, c_square) & no_castle_squares == 0 && self.board[rook_passing_square] == None {
                    king_moves |= c_square;
                }
            }
            if let (_, Some(c_square)) = self.current_castle_options() {
                if Board::ray_between_squares_inclusive(sqr, c_square) & no_castle_squares == 0 {
                    king_moves |= c_square;
                }
            }
            king_moves
        }

        // fn generate_pin_mask_avx(&self, sqr: Square, pinned_color: Color) -> BitBoard {
        //     println!("{}", self);
        //     let king_mask = BitBoard::from(sqr);
        //     let mut free = !(self.white | self.black);
        //     let first_hit_pieces = self.ks_queen_moves(free, king_mask) & self.mask_for_color(pinned_color);
        //     self.print_highlighted(first_hit_pieces);
        //     free |= first_hit_pieces;
        //     self.print_highlighted(free);
        //     let second_hit_pieces = self.ks_queen_moves(free, king_mask);
        //     self.print_highlighted(second_hit_pieces);
        //     let potential_attackers = self.ks_queen_moves(free, king_mask) & self.mask_for_color(!pinned_color);
        //     self.print_highlighted(potential_attackers);
        //     let bq_attackers = (potential_attackers & BISHOP_MOVES[sqr] & (self.bishops | self.queens));
        //     let rq_attackers = (potential_attackers & ROOK_MOVES[sqr] & (self.rooks | self.queens));
        //     rq_attackers | bq_attackers
        // }

        fn generate_pin_mask(&self, sqr: Square, pinned_color: Color) -> BitBoard {
            self.generate_pin_mask_rq(sqr, pinned_color) | self.generate_pin_mask_bq(sqr, pinned_color)
        }

        fn generate_pin_mask_rq(&self, sqr: Square, pinned_color: Color) -> BitBoard {
            let mut pin_squares = BitBoard(0);
            let mut first_hit = true;
            let mut pin_squares_ray = BitBoard(0);
            for n in (sqr.file() + 1)..8 {
                let sq = Square::new(n, sqr.rank());
                if let Some(Piece{piece_type, color}) = self.board[sq] {
                    if color != pinned_color {
                        if !first_hit && (piece_type == QUEEN || piece_type == ROOK) {
                            pin_squares |= pin_squares_ray;
                        }
                        break;
                    }
                    else if first_hit {
                        first_hit = false;
                        pin_squares_ray |= sq;
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
                if let Some(Piece{piece_type, color}) = self.board[sq] {
                    if color != pinned_color {
                        if !first_hit && (piece_type == QUEEN || piece_type == ROOK) {
                            pin_squares |= pin_squares_ray;
                        }
                        break;
                    }
                    else if first_hit {
                        first_hit = false;
                        pin_squares_ray |= sq;
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
                if let Some(Piece{piece_type, color}) = self.board[sq] {
                    if color != pinned_color {
                        if !first_hit && (piece_type == QUEEN || piece_type == ROOK) {
                            pin_squares |= pin_squares_ray;
                        }
                        break;
                    }
                    else if first_hit {
                        first_hit = false;
                        pin_squares_ray |= sq;
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
                if let Some(Piece{piece_type, color}) = self.board[sq] {
                    if color != pinned_color {
                        if !first_hit && (piece_type == QUEEN || piece_type == ROOK) {
                            pin_squares |= pin_squares_ray;
                        }
                        break;
                    }
                    else if first_hit {
                        first_hit = false;
                        pin_squares_ray |= sq;
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
                if let Some(Piece{piece_type, color}) = self.board[sq] {
                    if color != pinned_color {
                        if !first_hit && (piece_type == QUEEN || piece_type == BISHOP) {
                            pin_squares |= pin_squares_ray;
                        }
                        break;
                    }
                    else if first_hit {
                        first_hit = false;
                        pin_squares_ray |= sq;
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
                if let Some(Piece{piece_type, color}) = self.board[sq] {
                    if color != pinned_color {
                        if !first_hit && (piece_type == QUEEN || piece_type == BISHOP) {
                            pin_squares |= pin_squares_ray;
                        }
                        break;
                    }
                    else if first_hit {
                        first_hit = false;
                        pin_squares_ray |= sq;
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
                if let Some(Piece{piece_type, color}) = self.board[sq] {
                    if color != pinned_color {
                        if !first_hit && (piece_type == QUEEN || piece_type == BISHOP) {
                            pin_squares |= pin_squares_ray;
                        }
                        break;
                    }
                    else if first_hit {
                        first_hit = false;
                        pin_squares_ray |= sq;
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
                if let Some(Piece{piece_type, color}) = self.board[sq] {
                    if color != pinned_color {
                        if !first_hit && (piece_type == QUEEN || piece_type == BISHOP) {
                            pin_squares |= pin_squares_ray;
                        }
                        break;
                    }
                    else if first_hit {
                        first_hit = false;
                        pin_squares_ray |= sq;
                    }
                    else {
                        break;
                    }
                }
            }
            pin_squares
        }

        fn generate_attacker_squares(&self, king_sqr: Square) -> BitBoard {
            let mut mask = if self.active_player == WHITE {
                PAWN_CAPTURE_MOVES_WHITE[king_sqr] & self.pawns
            } else {
                PAWN_CAPTURE_MOVES_BLACK[king_sqr] & self.pawns
            };
            mask |= KNIGHT_MOVES[king_sqr] & self.knights;
            let king_mask = BitBoard(1 << king_sqr.0);
            mask |= self.ks_rook_moves(!(self.white | self.black), king_mask) & (self.rooks | self.queens);
            mask |= self.ks_bishop_moves(!(self.white | self.black), king_mask) & (self.bishops | self.queens);
            mask & self.opponent_color_mask()
        }

        fn generate_king_danger_squares(&self) -> BitBoard {
            let opponent_color_mask = self.opponent_color_mask();
            let pawn_attack_mask = match self.active_player {
                WHITE => PAWN_CAPTURE_MOVES_BLACK,
                BLACK => PAWN_CAPTURE_MOVES_WHITE
            };
            let mut danger_mask = KING_MOVES[(self.kings & opponent_color_mask).0.trailing_zeros() as usize];
            for pawn_sqr in self.pawns & opponent_color_mask {
                danger_mask |= pawn_attack_mask[pawn_sqr];
            }
            for knight_sqr in self.knights & opponent_color_mask {
                danger_mask |= KNIGHT_MOVES[knight_sqr];
            }
            //Remove attacked king for sliding attacks
            let occlusion_mask = (self.white | self.black) ^ (self.kings & self.active_color_mask());
            danger_mask |= self.ks_rook_moves(!occlusion_mask, (self.rooks | self.queens) & opponent_color_mask);
            danger_mask | self.ks_bishop_moves(!occlusion_mask, (self.bishops | self.queens) & opponent_color_mask)
        }

        pub fn ks_rook_moves(&self, propagate_mask: BitBoard, rook_sliders: BitBoard) -> BitBoard {
            unsafe {
                Board::kogge_stone_north_east(rook_sliders, propagate_mask) | Board::kogge_stone_south_west(rook_sliders, propagate_mask)
            }
        }

        pub fn ks_bishop_moves(&self, propagate_mask: BitBoard, bishop_sliders: BitBoard) -> BitBoard {
            unsafe {
                Board::kogge_stone_north_ew(bishop_sliders, propagate_mask) | Board::kogge_stone_south_ew(bishop_sliders, propagate_mask)
            }
        }

        pub fn ks_queen_moves(&self, propagate_mask: BitBoard, queen_sliders: BitBoard) -> BitBoard {
            unsafe {
                Board::kogge_stone_rshift_avx2(queen_sliders, propagate_mask) | Board::kogge_stone_lshift_avx2(queen_sliders, propagate_mask)
            }
        }

        #[target_feature(enable = "avx2")]
        unsafe fn kogge_stone_rshift_avx2(gen: BitBoard, pro: BitBoard) -> BitBoard {
            let s1 = _mm256_set_epi64x(8, 1, 7, 9);
            let s2 = _mm256_set_epi64x(16, 2, 14, 18);
            let s3 = _mm256_set_epi64x(32, 4, 28, 36);
            let file_mask = _mm256_set_epi64x(-1 , !FILES[7].0 as i64, !FILES[0].0 as i64, !FILES[7].0 as i64);

            let mut gen = _mm256_set_epi64x(gen.0 as i64, gen.0 as i64, gen.0 as i64, gen.0 as i64);
            let mut pro = _mm256_set_epi64x(pro.0 as i64, pro.0 as i64, pro.0 as i64, pro.0 as i64);
            pro = _mm256_and_si256(pro, file_mask);

            gen = _mm256_or_si256(gen, _mm256_and_si256(pro, _mm256_srlv_epi64(gen, s1)));
            pro = _mm256_and_si256(pro, _mm256_srlv_epi64(pro, s1));
            gen = _mm256_or_si256(gen, _mm256_and_si256(pro, _mm256_srlv_epi64(gen, s2)));
            pro = _mm256_and_si256(pro, _mm256_srlv_epi64(pro, s2));
            gen = _mm256_or_si256(gen, _mm256_and_si256(pro, _mm256_srlv_epi64(gen, s3)));

            gen = _mm256_and_si256(_mm256_srlv_epi64(gen, s1), file_mask);

            let mut back_key = [0u64; 4];
            _mm256_storeu_si256(back_key.as_mut_ptr() as *mut __m256i, gen);
            BitBoard(back_key[0] | back_key[1] | back_key[2] | back_key[3])
        }

        #[target_feature(enable = "avx2")]
        unsafe fn kogge_stone_lshift_avx2(gen: BitBoard, pro: BitBoard) -> BitBoard {
            let s1 = _mm256_set_epi64x(8, 1, 7, 9);
            let s2 = _mm256_set_epi64x(16, 2, 14, 18);
            let s3 = _mm256_set_epi64x(32, 4, 28, 36);
            let file_mask = _mm256_set_epi64x(-1, !FILES[0].0 as i64, !FILES[7].0 as i64, !FILES[0].0 as i64);

            let mut gen = _mm256_set_epi64x(gen.0 as i64, gen.0 as i64, gen.0 as i64, gen.0 as i64);
            let mut pro = _mm256_set_epi64x(pro.0 as i64, pro.0 as i64, pro.0 as i64, pro.0 as i64);
            pro = _mm256_and_si256(pro, file_mask);

            gen = _mm256_or_si256(gen, _mm256_and_si256(pro, _mm256_sllv_epi64(gen, s1)));
            pro = _mm256_and_si256(pro, _mm256_sllv_epi64(pro, s1));
            gen = _mm256_or_si256(gen, _mm256_and_si256(pro, _mm256_sllv_epi64(gen, s2)));
            pro = _mm256_and_si256(pro, _mm256_sllv_epi64(pro, s2));
            gen = _mm256_or_si256(gen, _mm256_and_si256(pro, _mm256_sllv_epi64(gen, s3)));

            gen = _mm256_and_si256(_mm256_sllv_epi64(gen, s1), file_mask);

            let mut back_key = [0u64; 4];
            _mm256_storeu_si256(back_key.as_mut_ptr() as *mut __m256i, gen);
            BitBoard(back_key[0] | back_key[1] | back_key[2] | back_key[3])
        }


        #[target_feature(enable = "avx2")]
        unsafe fn kogge_stone_south_west(gen: BitBoard, pro: BitBoard) -> BitBoard {
            let s1 = _mm_set_epi64x(8, 1);
            let s2 = _mm_set_epi64x(16, 2);
            let s3 = _mm_set_epi64x(32, 4);
            let file_mask = _mm_set_epi64x(-1 , !FILES[7].0 as i64);

            let mut gen = _mm_set_epi64x(gen.0 as i64, gen.0 as i64);
            let mut pro = _mm_set_epi64x(pro.0 as i64, pro.0 as i64);
            pro = _mm_and_si128(pro, file_mask);

            gen = _mm_or_si128(gen, _mm_and_si128(pro, _mm_srlv_epi64(gen, s1)));
            pro = _mm_and_si128(pro, _mm_srlv_epi64(pro, s1));
            gen = _mm_or_si128(gen, _mm_and_si128(pro, _mm_srlv_epi64(gen, s2)));
            pro = _mm_and_si128(pro, _mm_srlv_epi64(pro, s2));
            gen = _mm_or_si128(gen, _mm_and_si128(pro, _mm_srlv_epi64(gen, s3)));

            gen = _mm_and_si128(_mm_srlv_epi64(gen, s1), file_mask);

            let mut back_key = [0u64; 2];
            _mm_store_si128(back_key.as_mut_ptr() as *mut __m128i, gen);
            BitBoard(back_key[0] | back_key[1])
        }

        #[target_feature(enable = "avx2")]
        unsafe fn kogge_stone_north_east(gen: BitBoard, pro: BitBoard) -> BitBoard {
            let s1 = _mm_set_epi64x(8, 1);
            let s2 = _mm_set_epi64x(16, 2);
            let s3 = _mm_set_epi64x(32, 4);
            let file_mask = _mm_set_epi64x(-1 , !FILES[0].0 as i64);

            let mut gen = _mm_set_epi64x(gen.0 as i64, gen.0 as i64);
            let mut pro = _mm_set_epi64x(pro.0 as i64, pro.0 as i64);
            pro = _mm_and_si128(pro, file_mask);

            gen = _mm_or_si128(gen, _mm_and_si128(pro, _mm_sllv_epi64(gen, s1)));
            pro = _mm_and_si128(pro, _mm_sllv_epi64(pro, s1));
            gen = _mm_or_si128(gen, _mm_and_si128(pro, _mm_sllv_epi64(gen, s2)));
            pro = _mm_and_si128(pro, _mm_sllv_epi64(pro, s2));
            gen = _mm_or_si128(gen, _mm_and_si128(pro, _mm_sllv_epi64(gen, s3)));

            gen = _mm_and_si128(_mm_sllv_epi64(gen, s1), file_mask);

            let mut back_key = [0u64; 2];
            _mm_store_si128(back_key.as_mut_ptr() as *mut __m128i, gen);
            BitBoard(back_key[0] | back_key[1])
        }

        #[target_feature(enable = "avx2")]
        unsafe fn kogge_stone_south_ew(gen: BitBoard, pro: BitBoard) -> BitBoard {
            let s1 = _mm_set_epi64x(7, 9);
            let s2 = _mm_set_epi64x(14,  18);
            let s3 = _mm_set_epi64x(28, 36);
            let file_mask = _mm_set_epi64x(!FILES[0].0 as i64 , !FILES[7].0 as i64);

            let mut gen = _mm_set_epi64x(gen.0 as i64, gen.0 as i64);
            let mut pro = _mm_set_epi64x(pro.0 as i64, pro.0 as i64);
            pro = _mm_and_si128(pro, file_mask);

            gen = _mm_or_si128(gen, _mm_and_si128(pro, _mm_srlv_epi64(gen, s1)));
            pro = _mm_and_si128(pro, _mm_srlv_epi64(pro, s1));
            gen = _mm_or_si128(gen, _mm_and_si128(pro, _mm_srlv_epi64(gen, s2)));
            pro = _mm_and_si128(pro, _mm_srlv_epi64(pro, s2));
            gen = _mm_or_si128(gen, _mm_and_si128(pro, _mm_srlv_epi64(gen, s3)));

            gen = _mm_and_si128(_mm_srlv_epi64(gen, s1), file_mask);

            let mut back_key = [0u64; 2];
            _mm_store_si128(back_key.as_mut_ptr() as *mut __m128i, gen);
            BitBoard(back_key[0] | back_key[1])
        }

        #[target_feature(enable = "avx2")]
        unsafe fn kogge_stone_north_ew(gen: BitBoard, pro: BitBoard) -> BitBoard {
            let s1 = _mm_set_epi64x(7, 9);
            let s2 = _mm_set_epi64x(14,  18);
            let s3 = _mm_set_epi64x(28, 36);
            let file_mask = _mm_set_epi64x(!FILES[7].0 as i64 , !FILES[0].0 as i64);

            let mut gen = _mm_set_epi64x(gen.0 as i64, gen.0 as i64);
            let mut pro = _mm_set_epi64x(pro.0 as i64, pro.0 as i64);
            pro = _mm_and_si128(pro, file_mask);

            gen = _mm_or_si128(gen, _mm_and_si128(pro, _mm_sllv_epi64(gen, s1)));
            pro = _mm_and_si128(pro, _mm_sllv_epi64(pro, s1));
            gen = _mm_or_si128(gen, _mm_and_si128(pro, _mm_sllv_epi64(gen, s2)));
            pro = _mm_and_si128(pro, _mm_sllv_epi64(pro, s2));
            gen = _mm_or_si128(gen, _mm_and_si128(pro, _mm_sllv_epi64(gen, s3)));

            gen = _mm_and_si128(_mm_sllv_epi64(gen, s1), file_mask);

            let mut back_key = [0u64; 2];
            _mm_store_si128(back_key.as_mut_ptr() as *mut __m128i, gen);
            BitBoard(back_key[0] | back_key[1])
        }

        fn generate_rook_moves_ks(&self, sqr: Square, rook_color: Color) -> BitBoard {
            self.ks_rook_moves(!(self.white | self.black), BitBoard::from(sqr)) & !self.mask_for_color(rook_color)
        }

        fn generate_bishop_moves_ks(&self, sqr: Square, rook_color: Color) -> BitBoard {
            self.ks_bishop_moves(!(self.white | self.black), BitBoard::from(sqr)) & !self.mask_for_color(rook_color)
        }

        fn generate_queen_moves_ks(&self, sqr: Square, queen_color: Color) -> BitBoard {
            self.ks_queen_moves(!(self.white | self.black), BitBoard::from(sqr)) & !self.mask_for_color(queen_color)
        }

        fn moves_from_target_bitboard(&self, from: Square, mut possible_targets: BitBoard) -> Vec<Move> {
            let mut moves: Vec<Move> = vec!();
            while possible_targets != 0 {
                let move_id = possible_targets.0.trailing_zeros();
                let to = Square(move_id as BoardIndex);
                possible_targets = possible_targets.toggle(to);
                self.create_moves(from, to, &mut moves);
            }
            moves
        }

        fn moves_from_source_bitboard(&self, to: Square, mut possible_sources: BitBoard) -> Vec<Move> {
            let mut moves: Vec<Move> = vec!();
            while possible_sources != 0 {
                let move_id = possible_sources.0.trailing_zeros();
                let from = Square(move_id as BoardIndex);
                possible_sources = possible_sources.toggle(from);
                self.create_moves(from, to, &mut moves);
            }
            moves
        }

        pub fn create_moves(&self, from: Square, to: Square, moves: &mut Vec<Move>) {
            let from_piece = self.board[from].expect("From piece must exist");
            match self.board[to] {
                Some(Piece{piece_type, color}) => match from_piece.piece_type {
                    PieceType::PAWN if (from_piece.color == WHITE && to.rank() == 7) || (from_piece.color == BLACK && to.rank() == 0) => {
                        moves.push(Move { from, to, move_type: MoveAction::Promotion(QUEEN, Some(piece_type)), previous_ep_square: self.en_passant_square });
                        moves.push(Move { from, to, move_type: MoveAction::Promotion(KNIGHT, Some(piece_type)), previous_ep_square: self.en_passant_square });
                        moves.push(Move { from, to, move_type: MoveAction::Promotion(ROOK, Some(piece_type)), previous_ep_square: self.en_passant_square });
                        moves.push(Move { from, to, move_type: MoveAction::Promotion(BISHOP, Some(piece_type)), previous_ep_square: self.en_passant_square });
                    }
                    _ => moves.push(Move { from, to, move_type: MoveAction::Capture(piece_type), previous_ep_square: self.en_passant_square })
                },
                None => match from_piece.piece_type {
                    PieceType::PAWN => match self.en_passant_square {
                        Some(ep_sqr) if ep_sqr == to => {
                            let king_square = Square((self.active_color_mask() & self.kings).0.trailing_zeros() as BoardIndex);
                            if king_square.rank() == from.rank() {
                                let attacker_dir = king_square.direction_between(&from);
                                let mut file = king_square.file() as isize + attacker_dir.0;
                                while file < 8 && file >= 0 {
                                    if file == to.file() as isize || file == from.file() as isize {
                                        file += attacker_dir.0;
                                        continue;
                                    }
                                    match self.board[Square::new(file as usize, king_square.rank())] {
                                        None => file += attacker_dir.0,
                                        Some(Piece{color, piece_type: ROOK}) | Some(Piece{color, piece_type: QUEEN}) if color == !self.active_player => return,
                                        _ => break
                                    }
                                }
                            }
                            moves.push(Move { from, to, move_type: MoveAction::EnPassant, previous_ep_square: self.en_passant_square })
                        },
                        _ if (from_piece.color == WHITE && to.rank() == 7) || (from_piece.color == BLACK && to.rank() == 0) => {
                            moves.push(Move { from, to, move_type: MoveAction::Promotion(QUEEN, None), previous_ep_square: self.en_passant_square });
                            moves.push(Move { from, to, move_type: MoveAction::Promotion(KNIGHT, None), previous_ep_square: self.en_passant_square });
                            moves.push(Move { from, to, move_type: MoveAction::Promotion(ROOK, None), previous_ep_square: self.en_passant_square });
                            moves.push(Move { from, to, move_type: MoveAction::Promotion(BISHOP, None), previous_ep_square: self.en_passant_square });
                        }
                        _ => moves.push(Move { from, to, move_type: MoveAction::Normal, previous_ep_square: self.en_passant_square}),
                    },
                    PieceType::KING => match self.current_castle_options() {
                        (Some(castle_sqr), _) if to == castle_sqr && castle_sqr.rank() == 0 => moves.push(Move{from, to, move_type: MoveAction::Castle(Square::new(3, 0)), previous_ep_square: self.en_passant_square}),
                        (Some(castle_sqr), _) if to == castle_sqr && castle_sqr.rank() == 7 => moves.push(Move{from, to, move_type: MoveAction::Castle(Square::new(3, 7)), previous_ep_square: self.en_passant_square}),
                        (_, Some(castle_sqr)) if to == castle_sqr && castle_sqr.rank() == 0 => moves.push(Move{from, to, move_type: MoveAction::Castle(Square::new(5, 0)), previous_ep_square: self.en_passant_square}),
                        (_, Some(castle_sqr)) if to == castle_sqr && castle_sqr.rank() == 7 => moves.push(Move{from, to, move_type: MoveAction::Castle(Square::new(5, 7)), previous_ep_square: self.en_passant_square}),
                        _ => moves.push(Move{from, to, move_type: MoveAction::Normal, previous_ep_square: self.en_passant_square})
                    }
                    _ => moves.push(Move{from, to, move_type: MoveAction::Normal, previous_ep_square: self.en_passant_square})
                },
            };
        }

        fn current_castle_options(&self) -> (Option<Square>, Option<Square>) {
            let (options, (s1, s2)) = match self.active_player {
                WHITE => (&self.castling_options_white, CASTLING_SQUARES_WHITE),
                BLACK => (&self.castling_options_black, CASTLING_SQUARES_BLACK)
            };
            match options {
                (CastleState::Allowed, CastleState::Allowed) => (Some(s1), Some(s2)),
                (CastleState::Allowed, CastleState::Forbidden(_)) => (Some(s1), None),
                (CastleState::Forbidden(_), CastleState::Allowed) => (None, Some(s2)),
                (CastleState::Forbidden(_), CastleState::Forbidden(_)) => (None, None),
            }
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
                WHITE => "w ",
                BLACK => "b "
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
                    match pieces[Square::new(file, rank)] {
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
