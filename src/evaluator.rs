use std::cmp::Ordering;
use std::fmt::{Debug, Display, Formatter};
use std::future::poll_fn;
use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Neg, Not, Shl, Shr, Sub, SubAssign};
use std::process::Output;
use std::sync::{Arc, Mutex};
use itertools::Itertools;
use crate::bitboard::BitBoard;
use crate::board::board::{Board, LegalMoveData};
use crate::evaluator::Evaluation::{Estimate, Mate, Stalemate};
use crate::piece::{Color, Move, Square};
use crate::piece::MoveAction::Normal;

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
    let material_black = ((board.black & board.pawns).0.count_ones() +
        (board.black & board.knights).0.count_ones() * 3 +
        (board.black & board.bishops).0.count_ones() * 3 +
        (board.black & board.rooks).0.count_ones() * 5 +
        (board.black & board.queens).0.count_ones() * 9) as i32;

    let material_white = ((board.white & board.pawns).0.count_ones() +
        (board.white & board.knights).0.count_ones() * 3 +
        (board.white & board.bishops).0.count_ones() * 3 +
        (board.white & board.rooks).0.count_ones() * 5 +
        (board.white & board.queens).0.count_ones() * 9) as i32;

    return Estimate((material_white - material_black) as f32)
}


#[derive(Debug, Copy, Clone)]
pub struct MoveSuggestion(pub Evaluation, pub Option<Move>);

pub struct Stats {
    pub evaluated_positions: u128,
    pub best_move: Option<MoveSuggestion>
}

pub struct MiniMaxEvaluator {
    max_depth: u8,
    pub(crate) stats: Arc<Mutex<Stats>>,
}

impl MiniMaxEvaluator {

    pub(crate) fn new(max_depth: u8) -> MiniMaxEvaluator {
        MiniMaxEvaluator{max_depth, stats: Arc::new(Mutex::new(Stats { evaluated_positions: 0, best_move: None })) }
    }

    pub fn reset_stats(&self) {
        let stats = Arc::clone(&self.stats);
        let mut stats = stats.lock().unwrap();
        (*stats).evaluated_positions = 0;
        (*stats).best_move = None;
    }

    fn eval_min(&self, depth: u8, mut board: &mut Board, alpha: Evaluation, mut beta: Evaluation) -> MoveSuggestion {
        if board.halfmove_clock >= 50 {
            return MoveSuggestion(Stalemate, None);
        }
        let mut min_eval= MAX_EVAL;
        let mut min_suggestion = None;
        let LegalMoveData{legal_moves, king_danger_mask, pin_mask} = board.legal_moves();

        for m in legal_moves {
            let MoveSuggestion(eval, move_opt) = if depth <= 1 {
                board.apply_move(&m);
                let eval = eval_position_direct(board);
                board.undo_move(&m);
                match eval {
                    Estimate(val) => MoveSuggestion(Estimate(val), Some(m)),
                    Stalemate => MoveSuggestion(Stalemate, None),
                    Mate(c, 0) => MoveSuggestion(Mate(c, 0), None),
                    Mate(c, val) => MoveSuggestion(Mate(c, val), Some(m))
                }
            }
            else {
                board.apply_move(&m);
                let eval = match self.eval_max(depth - 1, board, alpha, beta) {
                    MoveSuggestion(Estimate(eval), _) => MoveSuggestion(Estimate(eval), Some(m)),
                    MoveSuggestion(Mate(color, moves), _) => MoveSuggestion(Mate(color, moves + 1), Some(m)),
                    MoveSuggestion(Stalemate, _) => MoveSuggestion(Stalemate, Some(m)),
                };
                board.undo_move(&m);
                eval
            };
            if eval < min_eval {
                min_eval = eval;
                min_suggestion = Some(MoveSuggestion(eval, move_opt));
                if depth == self.max_depth {
                    let stats = Arc::clone(&self.stats);
                    let mut stats = stats.lock().unwrap();
                    (*stats).best_move = min_suggestion.clone();
                }
            }
            beta = beta.min(eval);
            if min_eval <= alpha {
                break;
            }
        }

        min_suggestion.unwrap_or_else(|| {
            if board.kings & king_danger_mask != 0 {
                MoveSuggestion(Mate(board.active_player.next(), 0), None)
            }
            else {
                MoveSuggestion(Stalemate, None)
            }
        })
    }

    fn eval_max(&self, depth: u8, mut board: &mut Board, mut alpha: Evaluation, beta: Evaluation) -> MoveSuggestion {
        if board.halfmove_clock >= 50 {
            return MoveSuggestion(Stalemate, None);
        }
        let mut max_eval = MIN_EVAL;
        let mut max_suggestion = None;
        let LegalMoveData{legal_moves, king_danger_mask, pin_mask} = board.legal_moves();

        for m in legal_moves {
            let MoveSuggestion(eval, move_opt) = if depth <= 1 {
                board.apply_move(&m);
                let eval = eval_position_direct(board);
                board.undo_move(&m);
                match eval {
                    Estimate(val) => MoveSuggestion(Estimate(val), Some(m)),
                    Stalemate => MoveSuggestion(Stalemate, None),
                    Mate(c, 0) => MoveSuggestion(Mate(c, 0), None),
                    Mate(c, val) => MoveSuggestion(Mate(c, val), Some(m))
                }
            }
            else {
                board.apply_move(&m);
                let eval = match self.eval_min(depth - 1, board, alpha, beta) {
                    MoveSuggestion(Estimate(eval), _) => MoveSuggestion(Estimate(eval), Some(m)),
                    MoveSuggestion(Mate(color, moves), _) => MoveSuggestion(Mate(color, moves + 1), Some(m)),
                    MoveSuggestion(Stalemate, _) => MoveSuggestion(Stalemate, Some(m)),
                };
                board.undo_move(&m);
                eval
            };
            if eval > max_eval  {
                max_eval  = eval;
                max_suggestion = Some(MoveSuggestion(eval, move_opt));
                if depth == self.max_depth {
                    let stats = Arc::clone(&self.stats);
                    let mut stats = stats.lock().unwrap();
                    (*stats).best_move = max_suggestion.clone();
                }
            }
            alpha = alpha.max(eval);
            if max_eval >= beta {
                break;
            }
        }

        max_suggestion.unwrap_or_else(|| {
            if board.kings & king_danger_mask != 0 {
                MoveSuggestion(Mate(board.active_player.next(), 0), None)
            }
            else {
                MoveSuggestion(Stalemate, None)
            }
        })
    }
}

impl MoveFinder for MiniMaxEvaluator {
    fn find_move(&self, board: &mut Board) -> MoveSuggestion {
        match board.active_player {
            Color::WHITE => self.eval_max(self.max_depth, board, Mate(Color::BLACK, 0), Mate(Color::WHITE, 0)),
            Color::BLACK => self.eval_min(self.max_depth, board, Mate(Color::BLACK, 0), Mate(Color::WHITE, 0)),
        }
    }
}

impl Evaluator for MiniMaxEvaluator {
    fn evaluate_board(&self, board: &mut Board) -> Evaluation {
        self.find_move(board).0
    }
}