use std::cmp::{max, min, Ordering};
use std::fmt::{Debug, Display, Formatter};

use std::ops::{Neg};

use core::option::Option;
use std::collections::HashMap;

use std::time::{SystemTime, UNIX_EPOCH};
use itertools::{Itertools};

use crate::board::board::{Board, GamePhase};
use crate::Color::*;
use crate::evaluator::Evaluation::{Estimate, Winning, Losing, Stalemate};
use crate::hashing::{AlphaBetaData, NodeType, TranspositionTable, ZobristHash};
use crate::movegen::{LegalMoveData, MoveType};
use crate::piece::{Move, MoveAction, PieceType};

use crate::piece::PieceType::*;

pub const MIN_EVAL: Evaluation = Losing(0);
pub const MAX_EVAL: Evaluation = Winning(0);

const PST_MG_PAWN: [i16; 64] = [0, 0, 0, 0, 0, 0, 0, 0, -35, -1, -20, -23, -15, 24, 38, -22, -26, -4, -4, -10, 3, 3, 33, -12, -27, -2, -5, 12, 17, 6, 10, -25, -14, 13, 6, 21, 23, 12, 17, -23, -6, 7, 26, 31, 65, 56, 25, -20, 98, 134, 61, 95, 68, 126, 34, -11, 0, 0, 0, 0, 0, 0, 0, 0];
const PST_MG_KNIGHT: [i16; 64] = [-105, -21, -58, -33, -17, -28, -19, -23, -29, -53, -12, -3, -1, 18, -14, -19, -23, -9, 12, 10, 19, 17, 25, -16, -13, 4, 16, 13, 28, 19, 21, -8, -9, 17, 19, 53, 37, 69, 18, 22, -47, 60, 37, 65, 84, 129, 73, 44, -73, -41, 72, 36, 23, 62, 7, -17, -167, -89, -34, -49, 61, -97, -15, -107];
const PST_MG_BISHOP: [i16; 64] = [-33, -3, -14, -21, -13, -12, -39, -21, 4, 15, 16, 0, 7, 21, 33, 1, 0, 15, 15, 15, 14, 27, 18, 10, -6, 13, 13, 26, 34, 12, 10, 4, -4, 5, 19, 50, 37, 37, 7, -2, -16, 37, 43, 40, 35, 50, 37, -2, -26, 16, -18, -13, 30, 59, 18, -47, -29, 4, -82, -37, -25, -42, 7, -8];
const PST_MG_ROOK: [i16; 64] = [-19, -13, 1, 17, 16, 7, -37, -26, -44, -16, -20, -9, -1, 11, -6, -71, -45, -25, -16, -17, 3, 0, -5, -33, -36, -26, -12, -1, 9, -7, 6, -23, -24, -11, 7, 26, 24, 35, -8, -20, -5, 19, 26, 36, 17, 45, 61, 16, 27, 32, 58, 62, 80, 67, 26, 44, 32, 42, 32, 51, 63, 9, 31, 43];
const PST_MG_QUEEN: [i16; 64] = [-1, -18, -9, 10, -15, -25, -31, -50, -35, -8, 11, 2, 8, 15, -3, 1, -14, 2, -11, -2, -5, 2, 14, 5, -9, -26, -9, -10, -2, -4, 3, -3, -27, -27, -16, -16, -1, 17, -2, 1, -13, -17, 7, 8, 29, 56, 47, 57, -24, -39, -5, 1, -16, 57, 28, 54, -28, 0, 29, 12, 59, 44, 43, 45];
const PST_MG_KING: [i16; 64] = [-15, 36, 12, -54, 8, -28, 24, 14, 1, 7, -8, -64, -43, -16, 9, 8, -14, -14, -22, -46, -44, -30, -15, -27, -49, -1, -27, -39, -46, -44, -33, -51, -17, -20, -12, -27, -30, -25, -14, -36, -9, 24, 2, -16, -20, 6, 22, -22, 29, -1, -20, -7, -8, -4, -38, -29, -65, 23, 16, -15, -56, -34, 2, 13];

const PST_EG_PAWN: [i16; 64] = [0, 0, 0, 0, 0, 0, 0, 0, 13, 8, 8, 10, 13, 0, 2, -7, 4, 7, -6, 1, 0, -5, -1, -8, 13, 9, -3, -7, -7, -8, 3, -1, 32, 24, 13, 5, -2, 4, 17, 17, 94, 100, 85, 67, 56, 53, 82, 84, 178, 173, 158, 134, 147, 132, 165, 187, 0, 0, 0, 0, 0, 0, 0, 0];
const PST_EG_KNIGHT: [i16; 64] = [-29, -51, -23, -15, -22, -18, -50, -64, -42, -20, -10, -5, -2, -20, -23, -44, -23, -3, -1, 15, 10, -3, -20, -22, -18, -6, 16, 25, 16, 17, 4, -18, -17, 3, 22, 22, 22, 11, 8, -18, -24, -20, 10, 9, -1, -9, -19, -41, -25, -8, -25, -2, -9, -25, -24, -52, -58, -38, -13, -28, -31, -27, -63, -99];
const PST_EG_BISHOP: [i16; 64] = [-23, -9, -23, -5, -9, -16, -5, -17, -14, -18, -7, -1, 4, -9, -15, -27, -12, -3, 8, 10, 13, 3, -7, -15, -6, 3, 13, 19, 7, 10, -3, -9, -3, 9, 12, 9, 14, 10, 3, 2, 2, -8, 0, -1, -2, 6, 0, 4, -8, -4, 7, -12, -3, -13, -4, -14, -14, -21, -11, -8, -7, -9, -17, -24];
const PST_EG_ROOK: [i16; 64] = [-9, 2, 3, -1, -5, -13, 4, -20, -6, -6, 0, 2, -9, -9, -11, -3, -4, 0, -5, -1, -7, -12, -8, -16, 3, 5, 8, 4, -5, -6, -8, -11, 4, 3, 13, 1, 2, 1, -1, 2, 7, 7, 7, 5, 4, -3, -5, -3, 11, 13, 13, 11, -3, 3, 8, 3, 13, 10, 18, 15, 12, 12, 8, 5];
const PST_EG_QUEEN: [i16; 64] = [-33, -28, -22, -43, -5, -32, -20, -41, -22, -23, -30, -16, -16, -23, -36, -32, -16, -27, 15, 6, 9, 17, 10, 5, -18, 28, 19, 47, 31, 34, 39, 23, 3, 22, 24, 45, 57, 40, 57, 36, -20, 6, 9, 49, 47, 35, 19, 9, -17, 20, 32, 41, 58, 25, 30, 0, -9, 22, 22, 27, 27, 19, 10, 20];
const PST_EG_KING: [i16; 64] = [-53, -34, -21, -11, -28, -14, -24, -43, -27, -11, 4, 13, 14, 4, -5, -17, -19, -3, 11, 21, 23, 16, 7, -9, -18, -4, 21, 24, 27, 23, 9, -11, -8, 22, 24, 27, 26, 33, 26, 3, 10, 17, 23, 15, 20, 45, 44, 13, -12, 17, 14, 17, 17, 38, 23, 11, -74, -35, -18, -18, -11, 15, 4, -17];

pub const PST: [[[i16; 64]; 6]; 2] = [
    [PST_MG_KING, PST_MG_QUEEN, PST_MG_ROOK, PST_MG_BISHOP, PST_MG_KNIGHT, PST_MG_PAWN],
    [PST_EG_KING, PST_EG_QUEEN, PST_EG_ROOK, PST_EG_BISHOP, PST_EG_KNIGHT, PST_EG_PAWN]
];

const PIECE_VALUE_MG: [u32; 6] = [0, 1025, 477, 365, 337, 82];
const PIECE_VALUE_EG: [u32; 6] = [0, 936, 512, 297, 281, 94];

const PIECE_VALUE_MG_EG_TRANSITION_END: u32 = ((PIECE_VALUE_MG[1] + PIECE_VALUE_MG[2] + PIECE_VALUE_MG[3] + PIECE_VALUE_MG[4]) / 4 * 3 + PIECE_VALUE_MG[5] * 5) * 2;
const PIECE_VALUE_MG_EG_TRANSITION_START: u32 = ((PIECE_VALUE_MG[1] + PIECE_VALUE_MG[2] + PIECE_VALUE_MG[3] + PIECE_VALUE_MG[4]) / 4 * 6 + PIECE_VALUE_EG[5] * 5) * 2;

pub const PIECE_VALUE: [[u32; 6]; 2] = [PIECE_VALUE_MG, PIECE_VALUE_EG];


#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Evaluation {
    Winning(u8),
    Losing(u8),
    Estimate(f32),
    Stalemate
}

pub trait MoveFinder {
    fn find_move(&self, board: &mut Board, meta: &mut MinMaxMetadata) -> MoveSuggestion;
}

impl Eq for Evaluation {}

impl PartialOrd<Self> for Evaluation {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Evaluation {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Winning(a), Winning(b)) => b.cmp(a),
            (Winning(_), _) => Ordering::Greater,
            (_, Winning(_)) => Ordering::Less,
            (Losing(a), Losing(b)) => a.cmp(b),
            (Losing(_), _) => Ordering::Less,
            (_, Losing(_)) => Ordering::Greater,
            (Estimate(fl), Estimate(fr)) => fl.total_cmp(&fr),
            (Estimate(fl), _) => fl.total_cmp(&0.0),
            (_, Estimate(fr)) => 0.0_f32.total_cmp(fr),
            _ => Ordering::Equal
        }
    }
}

impl Neg for Evaluation {
    type Output = Evaluation;

    fn neg(self) -> Self::Output {
        match self {
            Estimate(f) => Estimate(-f),
            Winning(moves) => Losing(moves),
            Losing(moves) => Winning(moves),
            Stalemate => Stalemate,
        }
    }
}

impl Display for Evaluation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Estimate(e) => write!(f, "{}", e),
            Winning(0) => write!(f, "Won"),
            Winning(moves) => write!(f, "M{}", moves),
            Losing(0) => write!(f, "Lost"),
            Losing(moves) => write!(f, "M-{}", moves),
            Stalemate => write!(f, "{}", 0),
        }
    }
}

pub fn eval_position_direct(board: &Board) -> Evaluation {
    let material_white_mg = board.eval_info[GamePhase::Midgame].material[White];
    let material_black_mg = board.eval_info[GamePhase::Midgame].material[Black];

    let material_sum = material_white_mg + material_black_mg;
    let eg_ratio = if material_sum >= PIECE_VALUE_MG_EG_TRANSITION_START {
        0_f32
    } else if material_sum <= PIECE_VALUE_MG_EG_TRANSITION_END {
        1_f32
    } else {
        let diff = (PIECE_VALUE_MG_EG_TRANSITION_START - PIECE_VALUE_MG_EG_TRANSITION_END) as f32;
        let progress = (PIECE_VALUE_MG_EG_TRANSITION_START - material_sum) as f32;
        progress / diff
    };
    let mg_ratio = 1_f32 - eg_ratio;

    let material_eval_mg: i32 = material_white_mg as i32 - material_black_mg as i32;
    let material_eval_eg: i32 = board.eval_info[GamePhase::Endgame].material[White] as i32 - board.eval_info[GamePhase::Endgame].material[Black] as i32;

    let pst_eval_mg: i16 = board.eval_info[GamePhase::Midgame].psqt[White] - board.eval_info[GamePhase::Midgame].psqt[Black];
    let pst_eval_eg: i16 = board.eval_info[GamePhase::Endgame].psqt[White] - board.eval_info[GamePhase::Endgame].psqt[Black];

    let material_eval = material_eval_mg as f32 * mg_ratio + material_eval_eg as f32 * eg_ratio;
    let pst_eval = pst_eval_mg as f32 * mg_ratio + pst_eval_eg as f32 * eg_ratio;

    let mut eval = material_eval + pst_eval;
    if board.active_player == Black {
        eval = -eval;
    }
    Estimate(eval)

}

#[derive(Debug, Copy, Clone)]
pub struct MoveSuggestion(pub Evaluation, pub Option<Move>);

const MAX_DEPTH: usize = 128;
const KILLER_MOVE_SLOTS: usize = 2;
type KillerMoves = [[Option<Move>; KILLER_MOVE_SLOTS]; MAX_DEPTH];
type ScoredMove = (Move, u32);

pub struct MinMaxMetadata<'a> {
    pub killer_moves: KillerMoves,
    pub tt_table: &'a mut TranspositionTable<AlphaBetaData>,
    pub ply: u8,
    pub node_count: u64,
    pub path: Vec<Move>,
    pub max_time: u128,
    pub should_terminate: bool,
}

impl MinMaxMetadata<'_> {
    pub fn new(max_time: u128, tt_table: &mut TranspositionTable<AlphaBetaData>) -> MinMaxMetadata<'_> {
        MinMaxMetadata {
            killer_moves: [[None; KILLER_MOVE_SLOTS]; MAX_DEPTH],
            ply: 0,
            node_count: 0,
            path: vec![],
            max_time,
            tt_table,
            should_terminate: false,
        }
    }

    pub fn check_termination(&mut self) {
        if !self.should_terminate && self.node_count % 5000 == 0 {
            let time = SystemTime::now().duration_since(UNIX_EPOCH).expect("Time went backwards");
            if self.max_time != 0 && self.max_time < time.as_millis() {
                self.should_terminate = true;
            }
        }
    }
}

pub struct AlphaBetaSearch;

impl AlphaBetaSearch {

    pub fn find_move(board: &mut Board, depth: u8, mut meta: &mut MinMaxMetadata) -> MoveSuggestion {
        let alpha = MIN_EVAL;
        let beta = MAX_EVAL;
        AlphaBetaSearch::eval_negamax(depth, board, alpha, beta, &mut meta)
    }

    fn eval_negamax(depth: u8, board: &mut Board, mut alpha: Evaluation, mut beta: Evaluation, mut meta: &mut MinMaxMetadata) -> MoveSuggestion {
        meta.check_termination();
        if meta.should_terminate {
            return MoveSuggestion(MIN_EVAL, None)
        }

        meta.node_count += 1;
        if board.is_stalemate() {
            return MoveSuggestion(Stalemate, None);
        }

        let alpha_original = alpha;

        if let Some(tt_entry) = meta.tt_table.retrieve(board.zobrist_key) {
            let tt_eval = match tt_entry.eval {
                Losing(x) => Losing(x + meta.ply - tt_entry.ply),
                Winning(x) => Losing(x + meta.ply - tt_entry.ply),
                x=> x
            };
            if tt_entry.depth >= depth {
                match tt_entry.node_type {
                    NodeType::Exact => return MoveSuggestion(tt_eval, Some(tt_entry.best_move)),
                    NodeType::LowerBound => {
                        alpha = max(alpha, tt_eval);
                    },
                    NodeType::UpperBound => {
                        beta = min(beta, tt_eval);
                    },
                    _ => ()
                }
                if alpha >= beta {
                    return MoveSuggestion(tt_eval, Some(tt_entry.best_move));
                }
            }
        }

        if depth == 0 {
            return AlphaBetaSearch::eval_quiescence(board, alpha, beta, &mut meta);
        }

        let mut max_eval = MIN_EVAL;
        let mut max_suggestion = None;
        let LegalMoveData { legal_moves, king_danger_mask, .. } = board.legal_moves(MoveType::All);
        let mut legal_moves_scored = AlphaBetaSearch::score_moves(legal_moves, &board, meta);
        legal_moves_scored.sort_by(|m1, m2| m2.1.cmp(&m1.1));

        for (m, _) in legal_moves_scored {
            meta.ply += 1;
            board.apply_move(&m);
            let eval = -AlphaBetaSearch::eval_negamax(depth - 1, board, -beta, -alpha, &mut meta).0;
            board.undo_move(&m);
            meta.ply -= 1;

            if eval > max_eval {
                max_suggestion = Some(MoveSuggestion(eval, Some(m)));
                max_eval = eval;
            }

            alpha = max(alpha, max_eval);

            if alpha >= beta {
                if m.move_type == MoveAction::Normal {
                    AlphaBetaSearch::store_killer_move(m.clone(), &mut meta);
                }
                break;
            }
        }

        if let Some(MoveSuggestion(eval, Some(best_move))) = max_suggestion {
            let flag = if eval <= alpha_original {
                NodeType::UpperBound
            } else if eval >= beta {
                NodeType::LowerBound
            } else {
                NodeType::Exact
            };

            meta.tt_table.store(AlphaBetaData::create(depth, flag, eval, best_move, meta.ply), board.zobrist_key);
        }

        max_suggestion.unwrap_or_else(|| {
            if board.piece_bbs[board.active_player][King] & king_danger_mask != 0 {
                MoveSuggestion(Losing(meta.ply), None)
            } else {
                MoveSuggestion(Stalemate, None)
            }
        })
    }

    fn eval_quiescence(board: &mut Board, mut alpha: Evaluation, mut beta: Evaluation, mut meta: &mut MinMaxMetadata) -> MoveSuggestion {
        meta.check_termination();
        if meta.should_terminate {
            return MoveSuggestion(MIN_EVAL, None)
        }

        meta.node_count += 1;
        if board.halfmove_clock >= 50 {
            return MoveSuggestion(Stalemate, None);
        }

        let stand_pat = eval_position_direct(&board);

        if stand_pat >= beta {
            return MoveSuggestion(beta, None);
        }
        if alpha < stand_pat {
            alpha = stand_pat;
        }

        let mut max_suggestion = None;
        let LegalMoveData { legal_moves, king_danger_mask, .. } = board.legal_moves(MoveType::CaptureCheckEvade);
        let mut legal_moves_scored = AlphaBetaSearch::score_moves(legal_moves, &board, meta);
        legal_moves_scored.sort_by(|m1, m2| m2.1.cmp(&m1.1));

        for (m, _) in legal_moves_scored {
            meta.ply += 1;
            board.apply_move(&m);
            let eval = -AlphaBetaSearch::eval_quiescence(board, -beta, -alpha, &mut meta).0;
            board.undo_move(&m);
            meta.ply -= 1;

            if eval >= beta {
                return MoveSuggestion(beta, None);
            }
            if alpha > eval {
                alpha = eval;
                max_suggestion = Some(MoveSuggestion(eval, Some(m)));
            }
        }

        max_suggestion.unwrap_or_else(|| if board.piece_bbs[board.active_player][King] & king_danger_mask != 0 {
            MoveSuggestion(Losing(meta.ply), None)
        } else {
            MoveSuggestion(alpha, None)
        })
    }

    fn store_killer_move(killer_move: Move, meta: &mut MinMaxMetadata) {
        let killer_moves_level = &mut meta.killer_moves[meta.ply as usize];
        if let Some(old_kmove) = &killer_moves_level[0] {
            if *old_kmove != killer_move {
                for n in 0..(KILLER_MOVE_SLOTS - 1) {
                    killer_moves_level[n + 1] = killer_moves_level[n];
                }
                killer_moves_level[0] = Some(killer_move);
            }
        } else {
            killer_moves_level[0] = Some(killer_move);
        }
    }


    const KILLER_OFFSET: u32 = 6; //MVV-LVA is 6 bits wide
    //const _next_OFFSET: u32 = 7; //killer move is 1 bit wide

    fn score_moves(move_vec: Vec<Move>, board: &Board, meta: &MinMaxMetadata) -> Vec<ScoredMove> {
        move_vec.into_iter()
            .map(|m| {
                //max is 55 (assuming no king captures)
                let mvv_lva_score = match m {
                    Move{move_type: MoveAction::EnPassant, ..} => AlphaBetaSearch::mvv_lva_score(PieceType::Pawn, PieceType::Pawn),
                    Move{move_type: MoveAction::Capture(captured), from, ..} => AlphaBetaSearch::mvv_lva_score(captured, board.board[from].unwrap().piece_type),
                    Move{move_type: MoveAction::Promotion(_, Some(captured)), ..} => AlphaBetaSearch::mvv_lva_score(captured, PieceType::Pawn),
                    _ => 0
                };
                let killer_score = match m {
                    Move{move_type: MoveAction::Normal, ..} | Move{move_type: MoveAction::Promotion(_, None), ..} => {
                        let killer_table = meta.killer_moves[meta.ply as usize];
                        let mut score = 0_u32;
                        for n in 0..KILLER_MOVE_SLOTS {
                            if let Some(killer_move) = &killer_table[n] {
                                if killer_move == &m {
                                    score = 1;
                                    break;
                                }
                            }
                            else {
                                break;
                            }
                        }
                        score
                    },
                    _ => 0
                };
                // let killer_score = 0;
                // let mvv_lva_score = 0;
                let score = mvv_lva_score | killer_score << AlphaBetaSearch::KILLER_OFFSET;
                (m, score)
            })
            .collect_vec()
    }

    fn mvv_lva_score(victim: PieceType, attacker: PieceType) -> u32 {
        let v_score = AlphaBetaSearch::mvv_lva_piece_score(victim) * 10;
        let a_score = 6 - AlphaBetaSearch::mvv_lva_piece_score(attacker);
        v_score + a_score
    }

    fn mvv_lva_piece_score(piece: PieceType) -> u32 {
        match piece {
            PieceType::Pawn => 1,
            PieceType::Knight => 2,
            PieceType::Bishop => 3,
            PieceType::Rook => 4,
            PieceType::Queen => 5,
            PieceType::King => 6,
        }
    }
}