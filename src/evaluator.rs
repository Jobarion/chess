use std::cmp::Ordering;
use std::fmt::{Debug, Display, Formatter};
use std::future::poll_fn;
use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Neg, Not, Shl, Shr, Sub, SubAssign};
use std::process::Output;
use core::option::Option;
use std::sync::{Arc, Mutex};
use itertools::Itertools;
use crate::bitboard::BitBoard;
use crate::board::board::{Board, LegalMoveData};
use crate::Color::*;
use crate::evaluator::Evaluation::{Estimate, Mate, Stalemate};
use crate::piece::{Color, Move, MoveAction, Piece, PieceType, Square};
use crate::piece::MoveAction::Normal;
use crate::piece::PieceType::*;

const MIN_EVAL: Evaluation = Mate(Color::BLACK, 0);
const MAX_EVAL: Evaluation = Mate(Color::WHITE, 0);


#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Evaluation {
    Mate(Color, u8),
    Estimate(f32),
    Stalemate
}
pub trait Evaluator {
    fn evaluate_board(&self, board: &mut Board) -> Evaluation;
}

pub trait MoveFinder {
    fn find_move(&self, board: &mut Board) -> MoveSuggestion;
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
            (Mate(Color::WHITE, _), Mate(Color::BLACK, _)) => Ordering::Greater,
            (Mate(Color::BLACK, _), Mate(Color::WHITE, _)) => Ordering::Less,
            (Mate(Color::WHITE, ml), Mate(Color::WHITE, mr)) => mr.cmp(&ml),
            (Mate(Color::BLACK, ml), Mate(Color::BLACK, mr)) => ml.cmp(&mr),
            (Mate(Color::WHITE, _), _) => Ordering::Greater,
            (Mate(Color::BLACK, _), _) => Ordering::Less,
            (_, Mate(Color::WHITE, _)) => Ordering::Less,
            (_, Mate(Color::BLACK, _)) => Ordering::Greater,
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
            Mate(color, moves) => Mate(color.next(), moves),
            Stalemate => Stalemate,
        }
    }
}

impl Display for Evaluation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Estimate(e) => write!(f, "{}", e),
            Mate(color, 0) => write!(f, "Winner: {:?}", color),
            Mate(color, moves) => write!(f, "M{} for {:?}", moves, color),
            Stalemate => write!(f, "{}", 0),
        }
    }
}

pub fn eval_position_direct(board: &Board) -> Evaluation {
    let material_black = (board.piece_bbs[BLACK][PAWN].0.count_ones() * 100 +
        board.piece_bbs[BLACK][KNIGHT].0.count_ones() * 320 +
        board.piece_bbs[BLACK][BISHOP].0.count_ones() * 330 +
        board.piece_bbs[BLACK][ROOK].0.count_ones() * 500 +
        board.piece_bbs[BLACK][QUEEN].0.count_ones() * 900) as i32;

    let material_white = (board.piece_bbs[WHITE][PAWN].0.count_ones() * 100 +
        board.piece_bbs[WHITE][KNIGHT].0.count_ones() * 320 +
        board.piece_bbs[WHITE][BISHOP].0.count_ones() * 330 +
        board.piece_bbs[WHITE][ROOK].0.count_ones() * 500 +
        board.piece_bbs[WHITE][QUEEN].0.count_ones() * 900) as i32;

    return Estimate((material_white - material_black) as f32)
}


#[derive(Debug, Copy, Clone)]
pub struct MoveSuggestion(pub Evaluation, pub Option<Move>);

pub struct Stats {
    pub evaluated_positions: u128,
    pub best_move: Option<MoveSuggestion>
}

pub struct MinMaxEvaluator {
    max_depth: usize,
    pub(crate) stats: Arc<Mutex<Stats>>,
}

const MAX_DEPTH: usize = 128;
const KILLER_MOVE_SLOTS: usize = 2;
type KillerMoves = [[Option<Move>; KILLER_MOVE_SLOTS]; MAX_DEPTH];
type ScoredMove = (Move, u32);

struct MinMaxMetadata {
    pub killer_moves: KillerMoves,
    pub ply: usize,
    pub node_count: u64,
    pub path: Vec<Move>,
}

impl MinMaxMetadata {
    pub fn new() -> MinMaxMetadata {
        MinMaxMetadata {
            killer_moves: [[None; KILLER_MOVE_SLOTS]; MAX_DEPTH],
            ply: 0,
            node_count: 0,
            path: vec![],
        }
    }
}

impl MinMaxEvaluator {

    pub(crate) fn new(max_depth: usize) -> MinMaxEvaluator {
        MinMaxEvaluator {max_depth, stats: Arc::new(Mutex::new(Stats { evaluated_positions: 0, best_move: None })) }
    }

    pub fn reset_stats(&self) {
        let stats = Arc::clone(&self.stats);
        let mut stats = stats.lock().unwrap();
        (*stats).evaluated_positions = 0;
        (*stats).best_move = None;
    }

    fn eval_minmax(&self, max_depth: usize, mut board: &mut Board, mut alpha: Evaluation, mut beta: Evaluation, max: bool, mut meta: &mut MinMaxMetadata) -> MoveSuggestion {
        meta.node_count += 1;
        if board.halfmove_clock >= 50 {
            return MoveSuggestion(Stalemate, None);
        }
        let mut minmax_eval = if max {
            MIN_EVAL
        } else {
            MAX_EVAL
        };
        let mut minmax_suggestion = None;
        let LegalMoveData { legal_moves, king_danger_mask, pin_mask } = board.legal_moves();
        let mut legal_moves_scored = MinMaxEvaluator::score_moves(legal_moves, &board, meta);
        legal_moves_scored.sort_by(|m1, m2| m2.1.cmp(&m1.1));

        for (m, _) in legal_moves_scored {
            meta.path.push(m.clone());
            let MoveSuggestion(eval, move_opt) = if max_depth == meta.ply {
                board.apply_move(&m);
                let eval = eval_position_direct(board);
                board.undo_move(&m);
                match eval {
                    Estimate(val) => MoveSuggestion(Estimate(val), Some(m)),
                    Stalemate => MoveSuggestion(Stalemate, None),
                    Mate(c, 0) => MoveSuggestion(Mate(c, 0), None),
                    Mate(c, val) => MoveSuggestion(Mate(c, val), Some(m))
                }
            } else {
                meta.ply += 1;
                board.apply_move(&m);
                let eval = match self.eval_minmax(max_depth, board, alpha, beta, !max, &mut meta) {
                    MoveSuggestion(Estimate(eval), _) => MoveSuggestion(Estimate(eval), Some(m)),
                    MoveSuggestion(Mate(color, moves), _) => MoveSuggestion(Mate(color, moves + 1), Some(m)),
                    MoveSuggestion(Stalemate, _) => MoveSuggestion(Stalemate, Some(m)),
                };
                board.undo_move(&m);
                meta.ply -= 1;
                eval
            };
            meta.path.pop();
            if !max {
                if eval < minmax_eval {
                    minmax_eval = eval;
                    minmax_suggestion = Some(MoveSuggestion(eval, move_opt));
                }
                beta = beta.min(eval);
                if minmax_eval <= alpha {
                    if let Some(m) = &move_opt {
                        if m.move_type == MoveAction::Normal {
                            self.store_killer_move(m.clone(), &mut meta);
                        }
                    }
                    break;
                }
            } else {
                if eval > minmax_eval {
                    minmax_eval = eval;
                    minmax_suggestion = Some(MoveSuggestion(eval, move_opt));
                }
                alpha = alpha.max(eval);
                if minmax_eval >= beta {
                    if let Some(m) = &move_opt {
                        if m.move_type == MoveAction::Normal {
                            self.store_killer_move(m.clone(), &mut meta);
                        }
                    }
                    break;
                }
            }
        }

        minmax_suggestion.unwrap_or_else(|| {
            if board.piece_bbs[board.active_player][KING] & king_danger_mask != 0 {
                MoveSuggestion(Mate(board.active_player.next(), 0), None)
            } else {
                MoveSuggestion(Stalemate, None)
            }
        })
    }

    fn store_killer_move(&self, killer_move: Move, meta: &mut MinMaxMetadata) {
        let killer_moves_level = &mut meta.killer_moves[meta.ply];
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
                    Move{move_type: MoveAction::EnPassant, ..} => MinMaxEvaluator::mvv_lva_score(PieceType::PAWN, PieceType::PAWN),
                    Move{move_type: MoveAction::Capture(captured), from, ..} => MinMaxEvaluator::mvv_lva_score(captured, board.board[from].unwrap().piece_type),
                    Move{move_type: MoveAction::Promotion(_, Some(captured)), ..} => MinMaxEvaluator::mvv_lva_score(captured, PieceType::PAWN),
                    _ => 0
                };
                let killer_score = match m {
                    Move{move_type: MoveAction::Normal, ..} | Move{move_type: MoveAction::Promotion(_, None), ..} => {
                        let killer_table = meta.killer_moves[meta.ply];
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
                let score = mvv_lva_score | killer_score << MinMaxEvaluator::KILLER_OFFSET;
                (m, score)
            })
            .collect_vec()
    }

    fn mvv_lva_score(victim: PieceType, attacker: PieceType) -> u32 {
        let v_score = MinMaxEvaluator::mvv_lva_piece_score(victim) * 10;
        let a_score = 6 - MinMaxEvaluator::mvv_lva_piece_score(attacker);
        v_score + a_score
    }

    fn mvv_lva_piece_score(piece: PieceType) -> u32 {
        match piece {
            PieceType::PAWN => 1,
            PieceType::KNIGHT => 2,
            PieceType::BISHOP => 3,
            PieceType::ROOK => 4,
            PieceType::QUEEN => 5,
            PieceType::KING => 6,
        }
    }
}

impl MoveFinder for MinMaxEvaluator {
    fn find_move(&self, board: &mut Board) -> MoveSuggestion {

        let mut meta = MinMaxMetadata::new();

        let move_suggestion = match board.active_player {
            Color::WHITE => self.eval_minmax(self.max_depth, board, Mate(Color::BLACK, 0), Mate(Color::WHITE, 0), true, &mut meta),
            Color::BLACK => self.eval_minmax(self.max_depth, board, Mate(Color::BLACK, 0), Mate(Color::WHITE, 0), false, &mut meta),
        };

        println!("Nodes searched: {}", meta.node_count);
        move_suggestion
    }
}

impl Evaluator for MinMaxEvaluator {
    fn evaluate_board(&self, board: &mut Board) -> Evaluation {
        self.find_move(board).0
    }
}