use std::arch::x86_64::{__m128i, __m256i, _mm256_and_si256, _mm256_or_si256, _mm256_set_epi64x, _mm256_sllv_epi64, _mm256_srlv_epi64, _mm256_storeu_si256, _mm_and_si128, _mm_or_si128, _mm_set_epi64x, _mm_sllv_epi64, _mm_srlv_epi64, _mm_store_si128};
use std::cmp::{max, min};
use crate::{BitBoard, Board, Color, Move, Square};
use crate::bitboard::*;
use crate::board::board::CastleState;
use crate::Color::*;
use crate::piece::{MoveAction, Piece, PieceType};
use crate::piece::PieceType::*;

pub(crate) const CASTLING_SQUARES_WHITE: (Square, Square) = (Square::new(2, 0), Square::new(6, 0));
pub(crate) const CASTLING_SQUARES_BLACK: (Square, Square) = (Square::new(2, 7), Square::new(6, 7));

pub struct LegalMoveData {
    pub legal_moves: Vec<Move>,
    pub king_danger_mask: BitBoard,
    pub pin_mask: BitBoard,
}

impl Board {

    pub(crate) fn legal_moves(&self) -> LegalMoveData {
        let king = self.piece_bbs[self.active_player][PieceType::KING];
        let king_square = Square(king.0.trailing_zeros() as u8);

        let king_danger_squares = self.generate_king_danger_squares();

        // println!("Danger squares");
        // self.print_highlighted(king_danger_squares);

        if king_danger_squares & king_square != 0 {
            return self.generate_king_danger_moves(king_square, king_danger_squares);
        }
        let pin_mask = self.generate_pin_mask(king_square, self.active_player);

        let mut moves: Vec<Move> = vec!();

        //Checking once if the pin mask is set is probably slightly faster, but it would duplicate the entire loop block below
        for sqr in self.piece_bbs[self.active_player][PieceType::ROOK] {
            let mut move_mask = self.generate_rook_moves_ks(sqr, self.active_player);
            move_mask &= Board::calculate_partial_pin_mask(sqr, king_square, pin_mask);
            moves.append(&mut self.moves_from_target_bitboard(sqr, move_mask));
        }
        for sqr in self.piece_bbs[self.active_player][PieceType::BISHOP] {
            let mut move_mask = self.generate_bishop_moves_ks(sqr, self.active_player);
            move_mask &= Board::calculate_partial_pin_mask(sqr, king_square, pin_mask);
            moves.append(&mut self.moves_from_target_bitboard(sqr, move_mask));
        }
        for sqr in self.piece_bbs[self.active_player][PieceType::QUEEN] {
            let mut move_mask = self.generate_queen_moves_ks(sqr, self.active_player);
            move_mask &= Board::calculate_partial_pin_mask(sqr, king_square, pin_mask);
            moves.append(&mut self.moves_from_target_bitboard(sqr, move_mask));
        }
        for sqr in self.piece_bbs[self.active_player][PieceType::KNIGHT] {
            if pin_mask.is_set(sqr) {
                continue;
            }
            let move_mask = KNIGHT_MOVES[sqr] & !self.color_bbs[self.active_player];
            moves.append(&mut self.moves_from_target_bitboard(sqr, move_mask));
        }
        let ep_mask = self.en_passant_square
            .map(| sqr| BitBoard::from(sqr))
            .unwrap_or(BitBoard(0));
        if self.active_player == WHITE {
            for sqr in self.piece_bbs[WHITE][PAWN] {
                let mut move_mask = self.generate_pawn_moves_squares_white(sqr, ep_mask);
                move_mask &= Board::calculate_partial_pin_mask(sqr, king_square, pin_mask);
                moves.append(&mut self.moves_from_target_bitboard(sqr, move_mask));
            }
        }
        else {
            for sqr in self.piece_bbs[BLACK][PAWN] {
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

        let evasive_king_moves = KING_MOVES[king_square] & !king_danger_squares & !self.color_bbs[self.active_player];
        moves.append(&mut self.moves_from_target_bitboard(king_square, evasive_king_moves));

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

        for sqr in self.piece_bbs[self.active_player][ROOK] {
            let mut move_mask = self.generate_rook_moves_ks(sqr, self.active_player) & check_prevention_mask;
            move_mask &= Board::calculate_partial_pin_mask(sqr, king_square, pin_mask);
            moves.append(&mut self.moves_from_target_bitboard(sqr, move_mask));
        }
        for sqr in self.piece_bbs[self.active_player][BISHOP] {
            let mut move_mask = self.generate_bishop_moves_ks(sqr, self.active_player) & check_prevention_mask;
            move_mask &= Board::calculate_partial_pin_mask(sqr, king_square, pin_mask);
            moves.append(&mut self.moves_from_target_bitboard(sqr, move_mask));
        }
        for sqr in self.piece_bbs[self.active_player][QUEEN] {
            let mut move_mask = self.generate_queen_moves_ks(sqr, self.active_player) & check_prevention_mask;
            move_mask &= Board::calculate_partial_pin_mask(sqr, king_square, pin_mask);
            moves.append(&mut self.moves_from_target_bitboard(sqr, move_mask));
        }
        for sqr in self.piece_bbs[self.active_player][KNIGHT] {
            if pin_mask.is_set(sqr) {
                continue;
            }
            let move_mask = KNIGHT_MOVES[sqr] & !self.color_bbs[self.active_player] & check_prevention_mask;
            moves.append(&mut self.moves_from_target_bitboard(sqr, move_mask));
        }
        let ep_mask = self.en_passant_square
            .map(| sqr| BitBoard::from(sqr))
            .unwrap_or(BitBoard(0));
        let pawn_check_prevention_mask= check_prevention_mask | ep_check_capture_mask;
        if self.active_player == WHITE {
            for sqr in self.piece_bbs[WHITE][PAWN] {
                let mut move_mask = self.generate_pawn_moves_squares_white(sqr, ep_mask) & pawn_check_prevention_mask;
                move_mask &= Board::calculate_partial_pin_mask(sqr, king_square, pin_mask);
                moves.append(&mut self.moves_from_target_bitboard(sqr, move_mask));
            }
        }
        else {
            for sqr in self.piece_bbs[BLACK][PAWN] {
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

    //TODO simdify?
    fn generate_pawn_moves_squares_white(&self, sqr: Square, ep_mask: BitBoard) -> BitBoard {
        let free = !self.occupied();
        let mut push_mask = BitBoard::from(sqr) << 8 & free;
        push_mask |= push_mask << 8 & free & RANKS[3];
        push_mask | (PAWN_CAPTURE_MOVES_WHITE[sqr] & (self.color_bbs[BLACK] | ep_mask))
    }

    fn generate_pawn_moves_squares_black(&self, sqr: Square, ep_mask: BitBoard) -> BitBoard {
        let free = !self.occupied();
        let mut push_mask = BitBoard::from(sqr) >> 8 & free;
        push_mask |= push_mask >> 8 & free & RANKS[4];
        push_mask | (PAWN_CAPTURE_MOVES_BLACK[sqr] & (self.color_bbs[WHITE] | ep_mask))
    }

    fn generate_king_moves(&self, sqr: Square, king_danger_squares: BitBoard) -> BitBoard {
        let mut king_moves = KING_MOVES[sqr] & !self.color_bbs[self.active_player];
        let no_castle_squares = king_danger_squares | (self.occupied() ^ self.piece_bbs[WHITE][KING] ^ self.piece_bbs[BLACK][KING]);
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
            PAWN_CAPTURE_MOVES_WHITE[king_sqr] & self.piece_bbs[BLACK][PAWN]
        } else {
            PAWN_CAPTURE_MOVES_BLACK[king_sqr] & self.piece_bbs[WHITE][PAWN]
        };
        mask |= KNIGHT_MOVES[king_sqr] & self.piece_bbs[!self.active_player][KNIGHT];
        let king_mask = BitBoard(1 << king_sqr.0);
        let opp_boards = &self.piece_bbs[!self.active_player];
        mask |= self.ks_rook_moves(!self.occupied(), king_mask) & (opp_boards[ROOK] | opp_boards[QUEEN]);
        mask |= self.ks_bishop_moves(!self.occupied(), king_mask) & (opp_boards[BISHOP] | opp_boards[QUEEN]);
        mask
    }

    fn generate_king_danger_squares(&self) -> BitBoard {
        let _opp_mask = self.color_bbs[!self.active_player];
        let opp_c = !self.active_player;
        let pawn_attack_mask = match self.active_player {
            WHITE => PAWN_CAPTURE_MOVES_BLACK,
            BLACK => PAWN_CAPTURE_MOVES_WHITE
        };
        let mut danger_mask = KING_MOVES[self.piece_bbs[opp_c][KING].0.trailing_zeros() as usize];
        for pawn_sqr in self.piece_bbs[opp_c][PAWN] {
            danger_mask |= pawn_attack_mask[pawn_sqr];
        }
        for knight_sqr in self.piece_bbs[opp_c][KNIGHT] {
            danger_mask |= KNIGHT_MOVES[knight_sqr];
        }
        //Remove attacked king for sliding attacks
        let occlusion_mask = self.occupied() ^ self.piece_bbs[self.active_player][KING];
        danger_mask |= self.ks_rook_moves(!occlusion_mask, self.piece_bbs[opp_c][ROOK] | self.piece_bbs[opp_c][QUEEN]);
        danger_mask | self.ks_bishop_moves(!occlusion_mask, self.piece_bbs[opp_c][BISHOP] | self.piece_bbs[opp_c][QUEEN])
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
        self.ks_rook_moves(!self.occupied(), BitBoard::from(sqr)) & !self.color_bbs[rook_color]
    }

    fn generate_bishop_moves_ks(&self, sqr: Square, bishop_color: Color) -> BitBoard {
        self.ks_bishop_moves(!self.occupied(), BitBoard::from(sqr)) & !self.color_bbs[bishop_color]
    }

    fn generate_queen_moves_ks(&self, sqr: Square, queen_color: Color) -> BitBoard {
        self.ks_queen_moves(!self.occupied(), BitBoard::from(sqr)) & !self.color_bbs[queen_color]
    }

    fn moves_from_target_bitboard(&self, from: Square, mut possible_targets: BitBoard) -> Vec<Move> {
        let mut moves: Vec<Move> = vec!();
        while possible_targets != 0 {
            let move_id = possible_targets.0.trailing_zeros();
            let to = Square(move_id as u8);
            possible_targets = possible_targets.toggle(to);
            self.create_moves(from, to, &mut moves);
        }
        moves
    }

    // fn moves_from_source_bitboard(&self, to: Square, mut possible_sources: BitBoard) -> Vec<Move> {
    //     let mut moves: Vec<Move> = vec!();
    //     while possible_sources != 0 {
    //         let move_id = possible_sources.0.trailing_zeros();
    //         let from = Square(move_id as u8);
    //         possible_sources = possible_sources.toggle(from);
    //         self.create_moves(from, to, &mut moves);
    //     }
    //     moves
    // }

    pub fn create_moves(&self, from: Square, to: Square, moves: &mut Vec<Move>) {
        let from_piece = self.board[from].expect("From piece must exist");
        match self.board[to] {
            Some(Piece{piece_type, color: _}) => match from_piece.piece_type {
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
                        let king_square = Square(self.piece_bbs[self.active_player][KING].0.trailing_zeros() as u8);
                        if king_square.rank() == from.rank() {
                            let attacker_dir = king_square.direction_between(&from);
                            let mut file = king_square.file() as isize + attacker_dir.0;
                            while file < 8 && file >= 0 {
                                if file == to.file() as isize || file == from.file() as isize {
                                    file += attacker_dir.0;
                                    continue;
                                }
                                match self.board[Square::new(file as u8, king_square.rank())] {
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
}