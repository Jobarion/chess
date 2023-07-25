use std::arch::x86_64::*;
use std::cmp::{max, min};
use crate::{BitBoard, Board, Color, Move, Square};
use crate::bitboard::*;
use crate::board::board::{BoardIndex, CastleState};
use crate::Color::*;
use crate::piece::{MoveAction, Piece, PieceType};
use crate::piece::PieceType::*;

pub(crate) const CASTLING_SQUARES_WHITE: (Square, Square) = (Square::new(2, 0), Square::new(6, 0));
pub(crate) const CASTLING_SQUARES_BLACK: (Square, Square) = (Square::new(2, 7), Square::new(6, 7));
const PROMOTION_RANK: [u8; 2] = [7, 0];

pub struct LegalMoveData {
    pub legal_moves: Vec<Move>,
    pub king_danger_mask: BitBoard,
    pub pin_mask: BitBoard,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum MoveType {
    All,
    Capture,
    CaptureCheckEvade,
    Quiet
}

impl Board {

    pub(crate) fn legal_moves(&self, move_type: MoveType) -> LegalMoveData {

        let king = self.piece_bbs[self.active_player][PieceType::King];
        let king_square = Square(king.0.trailing_zeros() as u8);

        let king_danger_squares = self.generate_king_danger_squares();

        // println!("Danger squares");
        // self.print_highlighted(king_danger_squares);

        if king_danger_squares & king_square != 0 {
            return self.generate_king_danger_moves(king_square, king_danger_squares, move_type);
        }
        let allowed_mask = match move_type {
            MoveType::All => BitBoard::ALL,
            MoveType::Capture => self.color_bbs[!self.active_player],
            MoveType::CaptureCheckEvade => self.color_bbs[!self.active_player],
            MoveType::Quiet => !self.color_bbs[!self.active_player]
        };
        let pin_mask = self.generate_pin_mask(king_square, self.active_player);

        let mut moves: Vec<Move> = vec!();

        //Checking once if the pin mask is set is probably slightly faster, but it would duplicate the entire loop block below
        for sqr in self.piece_bbs[self.active_player][PieceType::Rook] {
            let mut move_mask = self.generate_rook_moves_ks(sqr, self.active_player) & allowed_mask;
            move_mask &= Board::calculate_partial_pin_mask(sqr, king_square, pin_mask);
            self.create_other_moves(sqr, move_mask, &mut moves);
        }
        for sqr in self.piece_bbs[self.active_player][PieceType::Bishop] {
            let mut move_mask = self.generate_bishop_moves_ks(sqr, self.active_player) & allowed_mask;
            move_mask &= Board::calculate_partial_pin_mask(sqr, king_square, pin_mask);
            self.create_other_moves(sqr, move_mask, &mut moves);
        }
        for sqr in self.piece_bbs[self.active_player][PieceType::Queen] {
            let mut move_mask = self.generate_queen_moves_ks(sqr, self.active_player) & allowed_mask;
            move_mask &= Board::calculate_partial_pin_mask(sqr, king_square, pin_mask);
            self.create_other_moves(sqr, move_mask, &mut moves);
        }
        for sqr in self.piece_bbs[self.active_player][PieceType::Knight] {
            if pin_mask.is_set(sqr) {
                continue;
            }
            let move_mask = KNIGHT_MOVES[sqr] & !self.color_bbs[self.active_player] & allowed_mask;
            self.create_other_moves(sqr, move_mask, &mut moves);
        }
        let ep_mask = self.en_passant_square
            .map(| sqr| BitBoard::from(sqr))
            .unwrap_or(BitBoard(0));
        if self.active_player == White {
            for sqr in self.piece_bbs[White][Pawn] {
                let mut move_mask = self.generate_pawn_moves_squares_white(sqr, ep_mask) & allowed_mask;
                move_mask &= Board::calculate_partial_pin_mask(sqr, king_square, pin_mask);
                self.create_pawn_moves(sqr, move_mask, &mut moves);
            }
        }
        else {
            for sqr in self.piece_bbs[Black][Pawn] {
                let mut move_mask = self.generate_pawn_moves_squares_black(sqr, ep_mask) & allowed_mask;
                move_mask &= Board::calculate_partial_pin_mask(sqr, king_square, pin_mask);
                self.create_pawn_moves(sqr, move_mask, &mut moves);
                // moves.append(&mut self.moves_from_target_bitboard(sqr, move_mask));
            }
        }
        //Normal king moves
        let move_mask = self.generate_king_moves(king_square, king_danger_squares) & allowed_mask;
        self.create_other_moves(king_square, move_mask, &mut moves);

        //King castle moves
        let move_mask = self.generate_king_castle_moves(king_square, king_danger_squares) & allowed_mask;
        self.create_king_castle_moves(king_square, move_mask, &mut moves);


        LegalMoveData { legal_moves: moves, king_danger_mask: king_danger_squares, pin_mask }
    }

    fn generate_king_danger_moves(&self, king_square: Square, king_danger_squares: BitBoard, move_type: MoveType) -> LegalMoveData {
        let allowed_mask = match move_type {
            MoveType::All | MoveType::CaptureCheckEvade => BitBoard::ALL,
            MoveType::Capture => self.color_bbs[!self.active_player],
            MoveType::Quiet => !self.color_bbs[!self.active_player]
        };
        let mut moves: Vec<Move> = vec!();

        let evasive_king_moves = KING_MOVES[king_square] & !king_danger_squares & !self.color_bbs[self.active_player] & allowed_mask;
        self.create_other_moves(king_square, evasive_king_moves, &mut moves);

        //Double check, the king can only run away
        let attackers = self.generate_attacker_squares(king_square);
        if attackers.0.count_ones() > 1 {
            return LegalMoveData { legal_moves: moves, king_danger_mask: king_danger_squares, pin_mask: BitBoard(0) };
        }

        let attacker_square = Square(attackers.0.trailing_zeros() as u8);

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
        let pawn_allowed_mask = match move_type {
            MoveType::Capture => allowed_mask | ep_check_capture_mask,
            _ => allowed_mask
        };

        for sqr in self.piece_bbs[self.active_player][Rook] {
            let mut move_mask = self.generate_rook_moves_ks(sqr, self.active_player) & check_prevention_mask & allowed_mask;
            move_mask &= Board::calculate_partial_pin_mask(sqr, king_square, pin_mask);
            self.create_other_moves(sqr, move_mask, &mut moves);
        }
        for sqr in self.piece_bbs[self.active_player][Bishop] {
            let mut move_mask = self.generate_bishop_moves_ks(sqr, self.active_player) & check_prevention_mask & allowed_mask;
            move_mask &= Board::calculate_partial_pin_mask(sqr, king_square, pin_mask);
            self.create_other_moves(sqr, move_mask, &mut moves);
        }
        for sqr in self.piece_bbs[self.active_player][Queen] {
            let mut move_mask = self.generate_queen_moves_ks(sqr, self.active_player) & check_prevention_mask & allowed_mask;
            move_mask &= Board::calculate_partial_pin_mask(sqr, king_square, pin_mask);
            self.create_other_moves(sqr, move_mask, &mut moves);
        }
        for sqr in self.piece_bbs[self.active_player][Knight] {
            if pin_mask.is_set(sqr) {
                continue;
            }
            let move_mask = KNIGHT_MOVES[sqr] & !self.color_bbs[self.active_player] & check_prevention_mask & allowed_mask;
            self.create_other_moves(sqr, move_mask, &mut moves);
        }
        let ep_mask = self.en_passant_square
            .map(| sqr| BitBoard::from(sqr))
            .unwrap_or(BitBoard(0));
        let pawn_check_prevention_mask= check_prevention_mask | ep_check_capture_mask;
        if self.active_player == White {
            for sqr in self.piece_bbs[White][Pawn] {
                let mut move_mask = self.generate_pawn_moves_squares_white(sqr, ep_mask) & pawn_check_prevention_mask & pawn_allowed_mask;
                move_mask &= Board::calculate_partial_pin_mask(sqr, king_square, pin_mask);
                self.create_pawn_moves(sqr, move_mask, &mut moves);
            }
        }
        else {
            for sqr in self.piece_bbs[Black][Pawn] {
                let mut move_mask = self.generate_pawn_moves_squares_black(sqr, ep_mask) & pawn_check_prevention_mask & pawn_allowed_mask;
                move_mask &= Board::calculate_partial_pin_mask(sqr, king_square, pin_mask);
                self.create_pawn_moves(sqr, move_mask, &mut moves);
            }
        }

        LegalMoveData { legal_moves: moves, king_danger_mask: king_danger_squares, pin_mask }
    }

    //Pieces can always move on the line between itself and the king, even if they are pinned.
    fn calculate_partial_pin_mask(piece_square: Square, king_square: Square, pin_mask: BitBoard) -> BitBoard {
        if pin_mask.is_set(piece_square) {
            match king_square.direction_between(&piece_square) {
                (1, 0) | (-1, 0) => RANKS[piece_square.rank() as usize],
                (0, 1) | (0, -1) => FILES[piece_square.file() as usize],
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
        while file as u8 != to.file() || rank as u8 != to.rank() {
            ray_mask = ray_mask.set(Square::new(file as u8, rank as u8));
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
            ray_mask = ray_mask.set(Square::new(file as u8, rank as u8));
            if file as u8 == to.file() && rank as u8 == to.rank() {
                break;
            }
            file = file + df;
            rank = rank + dr;
        }
        ray_mask
    }

    fn generate_pawn_moves_squares_white(&self, sqr: Square, ep_mask: BitBoard) -> BitBoard {
        let free = !self.occupied();
        let mut push_mask = BitBoard::from(sqr) << 8 & free;
        push_mask |= push_mask << 8 & free & RANKS[3];
        push_mask | (PAWN_CAPTURE_MOVES_WHITE[sqr] & (self.color_bbs[Black] | ep_mask))
    }

    fn generate_pawn_moves_squares_black(&self, sqr: Square, ep_mask: BitBoard) -> BitBoard {
        let free = !self.occupied();
        let mut push_mask = BitBoard::from(sqr) >> 8 & free;
        push_mask |= push_mask >> 8 & free & RANKS[4];
        push_mask | (PAWN_CAPTURE_MOVES_BLACK[sqr] & (self.color_bbs[White] | ep_mask))
    }

    fn generate_king_castle_moves(&self, sqr: Square, king_danger_squares: BitBoard) -> BitBoard {
        let mut king_moves = BitBoard::NONE;
        let no_castle_squares = king_danger_squares | (self.occupied() ^ self.piece_bbs[White][King] ^ self.piece_bbs[Black][King]);
        if let (Some(c_square), _) = self.current_castle_options() {
            let rook_passing_square = Square::new((c_square.file() as isize + sqr.direction_between(&c_square).0) as u8, c_square.rank());
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

    fn generate_king_moves(&self, sqr: Square, king_danger_squares: BitBoard) -> BitBoard {
        KING_MOVES[sqr] & !self.color_bbs[self.active_player] & !king_danger_squares
    }

    fn generate_pin_mask(&self, sqr: Square, pinned_color: Color) -> BitBoard {
        self.generate_pin_mask_rq_ks(sqr, pinned_color) | self.generate_pin_mask_bq_ks(sqr, pinned_color)
    }

    fn generate_pin_mask_rq_ks(&self, sqr: Square, pinned_color: Color) -> BitBoard {
        let rook_lines = self.ks_rook_moves(!self.occupied(), BitBoard::from(sqr));
        let pinned_pieces = rook_lines & self.color_bbs[pinned_color];
        let rook_lines = self.ks_rook_moves(!self.occupied() ^ pinned_pieces, BitBoard::from(sqr));
        let pinning_pieces = rook_lines & (self.piece_bbs[!pinned_color][Rook] | self.piece_bbs[!pinned_color][Queen]);
        let pinned_pieces =  self.ks_rook_moves(!BitBoard::from(sqr), pinning_pieces) & pinned_pieces;
        return pinned_pieces
    }

    fn generate_pin_mask_bq_ks(&self, sqr: Square, pinned_color: Color) -> BitBoard {
        let bishop_lines = self.ks_bishop_moves(!self.occupied(), BitBoard::from(sqr));
        let pinned_pieces = bishop_lines & self.color_bbs[pinned_color];
        let bishop_lines = self.ks_bishop_moves(!self.occupied() ^ pinned_pieces, BitBoard::from(sqr));
        let pinning_pieces = bishop_lines & (self.piece_bbs[!pinned_color][Bishop] | self.piece_bbs[!pinned_color][Queen]);
        let pinned_pieces =  self.ks_bishop_moves(!BitBoard::from(sqr), pinning_pieces) & pinned_pieces;
        return pinned_pieces
    }

    fn generate_attacker_squares(&self, king_sqr: Square) -> BitBoard {
        let mut mask = if self.active_player == White {
            PAWN_CAPTURE_MOVES_WHITE[king_sqr] & self.piece_bbs[Black][Pawn]
        } else {
            PAWN_CAPTURE_MOVES_BLACK[king_sqr] & self.piece_bbs[White][Pawn]
        };
        mask |= KNIGHT_MOVES[king_sqr] & self.piece_bbs[!self.active_player][Knight];
        let king_mask = BitBoard(1 << king_sqr.0);
        let opp_boards = &self.piece_bbs[!self.active_player];
        mask |= self.ks_rook_moves(!self.occupied(), king_mask) & (opp_boards[Rook] | opp_boards[Queen]);
        mask |= self.ks_bishop_moves(!self.occupied(), king_mask) & (opp_boards[Bishop] | opp_boards[Queen]);
        mask
    }

    fn generate_king_danger_squares(&self) -> BitBoard {
        let _opp_mask = self.color_bbs[!self.active_player];
        let opp_c = !self.active_player;
        let pawn_attack_mask = match self.active_player {
            White => PAWN_CAPTURE_MOVES_BLACK,
            Black => PAWN_CAPTURE_MOVES_WHITE
        };
        let mut danger_mask = KING_MOVES[self.piece_bbs[opp_c][King].0.trailing_zeros() as usize];
        for pawn_sqr in self.piece_bbs[opp_c][Pawn] {
            danger_mask |= pawn_attack_mask[pawn_sqr];
        }
        for knight_sqr in self.piece_bbs[opp_c][Knight] {
            danger_mask |= KNIGHT_MOVES[knight_sqr];
        }
        //Remove attacked king for sliding attacks
        let occlusion_mask = self.occupied() ^ self.piece_bbs[self.active_player][King];
        danger_mask |= self.ks_rook_moves(!occlusion_mask, self.piece_bbs[opp_c][Rook] | self.piece_bbs[opp_c][Queen]);
        danger_mask | self.ks_bishop_moves(!occlusion_mask, self.piece_bbs[opp_c][Bishop] | self.piece_bbs[opp_c][Queen])
    }

    pub fn create_king_castle_moves(&self, from: Square, mut possible_targets: BitBoard, moves: &mut Vec<Move>) {
        for to in possible_targets {
            let rook_target = Square(((to.0 as isize - from.0 as isize).signum() + from.0 as isize) as u8);
            let move_type = MoveAction::Castle(rook_target);
            moves.push(Move { from, to, move_type, previous_ep_square: self.en_passant_square });
        }
    }

    pub fn create_pawn_moves(&self, from: Square, mut possible_targets: BitBoard, moves: &mut Vec<Move>) {
        let from_piece = self.board[from].expect("From piece must exist");
        for to in possible_targets {
            //Promotion
            if PROMOTION_RANK[from_piece.color] == to.rank() {
                let captured_opt = self.board[to].map(|p|p.piece_type);
                moves.push(Move { from, to, move_type: MoveAction::Promotion(Queen, captured_opt), previous_ep_square: self.en_passant_square });
                moves.push(Move { from, to, move_type: MoveAction::Promotion(Rook, captured_opt), previous_ep_square: self.en_passant_square });
                moves.push(Move { from, to, move_type: MoveAction::Promotion(Knight, captured_opt), previous_ep_square: self.en_passant_square });
                moves.push(Move { from, to, move_type: MoveAction::Promotion(Bishop, captured_opt), previous_ep_square: self.en_passant_square });
            }
            else {
                match self.board[to] {
                    Some(Piece{piece_type, ..}) => moves.push(Move { from, to, move_type: MoveAction::Capture(piece_type), previous_ep_square: self.en_passant_square }),
                    None => {
                        match self.en_passant_square {
                            Some(ep_sqr) if ep_sqr == to => {
                                let ep_pawn_square = Square::new(ep_sqr.file(), from.rank());
                                let free_without_ep_pawns = !self.occupied() ^ from ^ ep_pawn_square;
                                let ep_xray_pieces = self.piece_bbs[!self.active_player][Queen] | self.piece_bbs[!self.active_player][Rook];
                                let ep_xray_test = self.ks_rook_moves(free_without_ep_pawns, ep_xray_pieces);
                                let king_square = Square(self.piece_bbs[self.active_player][King].0.trailing_zeros() as u8);
                                if ep_xray_test & king_square == 0 {
                                    moves.push(Move { from, to, move_type: MoveAction::EnPassant, previous_ep_square: self.en_passant_square })
                                }
                            },
                            _ => moves.push(Move { from, to, move_type: MoveAction::Normal, previous_ep_square: self.en_passant_square })
                        }
                    }
                }
            }
        }
    }

    pub fn create_other_moves(&self, from: Square, mut possible_targets: BitBoard, moves: &mut Vec<Move>) {
        for to in possible_targets {
            let action = match self.board[to] {
                Some(Piece{piece_type, ..}) => MoveAction::Capture(piece_type),
                None => MoveAction::Normal
            };
            moves.push(Move{from, to, move_type: action, previous_ep_square: self.en_passant_square});
        }
    }

    fn current_castle_options(&self) -> (Option<Square>, Option<Square>) {
        let (options, (s1, s2)) = match self.active_player {
            White => (&self.castling_options_white, CASTLING_SQUARES_WHITE),
            Black => (&self.castling_options_black, CASTLING_SQUARES_BLACK)
        };
        match options {
            (CastleState::Allowed, CastleState::Allowed) => (Some(s1), Some(s2)),
            (CastleState::Allowed, CastleState::Forbidden(_)) => (Some(s1), None),
            (CastleState::Forbidden(_), CastleState::Allowed) => (None, Some(s2)),
            (CastleState::Forbidden(_), CastleState::Forbidden(_)) => (None, None),
        }
    }
}