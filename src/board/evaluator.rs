use crate::board::board::{PieceType, Board, Piece, Color, FILE_SIZE, RANK_SIZE};
use crate::board::board::positional::{Square, File, Rank, Move};
use crate::board::evaluator::EvalResult::{Estimate, Winner, Stalemate};
use std::cmp::Ordering;
use std::fmt::{Display, Formatter, Error};
use self::rand::Rng;
use crate::CALL_COUNT;
use crate::board::board::Color::BLACK;

extern crate rand;

const MIN_EVAL: EvalResult = EvalResult::Winner(Color::BLACK, 0);
const MAX_EVAL: EvalResult = EvalResult::Winner(Color::WHITE, 0);

pub trait Evaluator {
    fn evaluate_board(&self, board: &Board) -> EvalResult;
}

pub trait MoveFinder {
    fn find_move(&self, board: &Board) -> MoveSuggestion;
}

pub struct MiniMaxEvaluator {
    max_depth: u8,
    evaluator: MaterialEvaluator,
}

#[derive(Debug, Copy, Clone)]
pub enum EvalResult {
    Estimate(f32),
    Winner(Color, u32),
    Stalemate,
}

impl Eq for EvalResult {

}

impl PartialEq for EvalResult {

    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Estimate(a), Estimate(b)) => a == b,
            (Stalemate, Stalemate) => true,
            (Winner(c1, m1), Winner(c2, m2)) => c1 == c2 && m1 == m2,
            _ => false
        }
    }
}

impl PartialOrd for EvalResult {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for EvalResult {

    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Estimate(e1), Estimate(e2)) => e1.partial_cmp(&e2).unwrap(),
            (Estimate(e1), Stalemate) => e1.partial_cmp(&0.0).unwrap(),
            (Stalemate, Estimate(e1)) => 0.0.partial_cmp(e1).unwrap(),
            (Winner(c1, _), Winner(c2, _)) if *c1 == Color::WHITE && *c2 == Color::BLACK => Ordering::Greater,
            (Winner(c1, _), Winner(c2, _)) if *c1 == Color::BLACK && *c2 == Color::WHITE => Ordering::Less,
            (Winner(Color::WHITE, m1), Winner( Color::WHITE, m2)) => m2.cmp(&m1),
            (Winner(Color::BLACK, m1), Winner( Color::BLACK, m2)) => m1.cmp(&m2),
            (_, Winner(Color::WHITE, _)) => Ordering::Less,
            (_, Winner(Color::BLACK, _)) => Ordering::Greater,
            (Winner(Color::WHITE, _), _) => Ordering::Greater,
            (Winner(Color::BLACK, _), _) => Ordering::Less,
            _ => Ordering::Equal
        }
    }
}

#[derive(Debug)]
pub struct MoveSuggestion(pub EvalResult, pub Option<Move>);

impl MiniMaxEvaluator {

    pub(crate) fn new(max_depth: u8) -> MiniMaxEvaluator {
        MiniMaxEvaluator{max_depth, evaluator: MaterialEvaluator}
    }

    fn eval_min(&self, depth: u8, board: &Board, alpha: EvalResult, mut beta: EvalResult) -> MoveSuggestion {
        if board.halfmove_clock >= 50 {
            return MoveSuggestion(Stalemate, None);
        }
        let mut min_eval= MAX_EVAL;
        let mut min_suggestion = None;
        for m in board.all_available_moves() {
            let MoveSuggestion(eval, move_opt) = if depth <= 1 {
                let eval = self.evaluator.evaluate_board(&board.do_move(&m));
                match eval {
                    Estimate(val) => MoveSuggestion(Estimate(val), Some(m)),
                    Stalemate => MoveSuggestion(Stalemate, None),
                    Winner(c, 0) => MoveSuggestion(Winner(c, 0), None),
                    Winner(c, val) => MoveSuggestion(Winner(c, val), Some(m))
                }
            }
            else {
                match self.eval_max(depth - 1, &board.do_move(&m), alpha, beta) {
                    MoveSuggestion(Estimate(eval), _) => MoveSuggestion(Estimate(eval), Some(m)),
                    MoveSuggestion(Winner(color, moves), _) => MoveSuggestion(Winner(color, moves + 1), Some(m)),
                    MoveSuggestion(Stalemate, _) => MoveSuggestion(Stalemate, Some(m)),
                }
            };
            if eval < min_eval {
                min_eval = eval;
                min_suggestion = Some(MoveSuggestion(eval, move_opt));
            }
            beta = beta.min(eval);
            if alpha >= beta {
                break;
            }
        }

        min_suggestion.unwrap_or_else(|| {
            if board.is_in_check(&board.active_player) {
                MoveSuggestion(Winner(board.active_player.next(), 0), None)
            }
            else {
                MoveSuggestion(Stalemate, None)
            }
        })
    }

    fn eval_max(&self, depth: u8, board: &Board, mut alpha: EvalResult, beta: EvalResult) -> MoveSuggestion {
        if board.halfmove_clock >= 50 {
            return MoveSuggestion(Stalemate, None);
        }
        let mut max_eval = MIN_EVAL;
        let mut max_suggestion = None;
        for m in board.all_available_moves() {
            let MoveSuggestion(eval, move_opt) = if depth <= 1 {
                let eval = self.evaluator.evaluate_board(&board.do_move(&m));
                match eval {
                    Estimate(val) => MoveSuggestion(Estimate(val), Some(m)),
                    Stalemate => MoveSuggestion(Stalemate, None),
                    Winner(c, 0) => MoveSuggestion(Winner(c, 0), None),
                    Winner(c, val) => MoveSuggestion(Winner(c, val), Some(m))
                }
            }
            else {
                match self.eval_min(depth - 1, &board.do_move(&m), alpha, beta) {
                    MoveSuggestion(Estimate(eval), _) => MoveSuggestion(Estimate(eval), Some(m)),
                    MoveSuggestion(Winner(color, moves), _) => MoveSuggestion(Winner(color, moves + 1), Some(m)),
                    MoveSuggestion(Stalemate, _) => MoveSuggestion(Stalemate, Some(m)),
                }
            };
            if eval > max_eval  {
                max_eval  = eval;
                max_suggestion = Some(MoveSuggestion(eval, move_opt));
            }
            alpha = alpha.max(eval);
            if alpha >= beta {
                break;
            }
        }

        max_suggestion.unwrap_or_else(|| {
            if board.is_in_check(&board.active_player) {
                MoveSuggestion(Winner(board.active_player.next(), 0), None)
            }
            else {
                MoveSuggestion(Stalemate, None)
            }
        })
    }
}

impl MoveFinder for MiniMaxEvaluator {
    fn find_move(&self, board: &Board) -> MoveSuggestion {
        match board.active_player {
            Color::WHITE => self.eval_max(self.max_depth, board, EvalResult::Winner(Color::BLACK, 0), EvalResult::Winner(Color::WHITE, 0)),
            Color::BLACK => self.eval_min(self.max_depth, board, EvalResult::Winner(Color::BLACK, 0), EvalResult::Winner(Color::WHITE, 0)),
        }
    }
}

impl Evaluator for MiniMaxEvaluator {
    fn evaluate_board(&self, board: &Board) -> EvalResult {
        self.find_move(board).0
    }
}


pub struct MaterialEvaluator;

impl Evaluator for MaterialEvaluator {

    fn evaluate_board(&self, board: &Board) -> EvalResult {
        CALL_COUNT.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        if board.halfmove_clock >= 50 {
            return Stalemate
        }
        let mut evaluation = 0.0;
        let mut bishop_count = 0;
        for file in 0..FILE_SIZE {
            for rank in 0..RANK_SIZE {
                let sqr = Square(File(file), Rank(rank));
                let piece = board.get_piece(&sqr);
                if let Some(Piece{piece_type, color}) = piece {
                    let value = match piece_type {
                        PieceType::PAWN => 1.0,
                        PieceType::ROOK => 5.0,
                        PieceType::KING => 3.5,
                        PieceType::BISHOP => 3.0,
                        PieceType::KNIGHT => 3.0,
                        PieceType::QUEEN => 9.0,
                    };
                    let factor = match color {
                        Color::WHITE => 1,
                        Color::BLACK => -1,
                    };
                    if piece_type == PieceType::BISHOP {
                        bishop_count += factor;
                    };
                    evaluation += value * factor as f32;
                }
            }
        }
        if bishop_count == 2 {
            evaluation += 1.0;
        }
        else if bishop_count == -2 {
            evaluation -= 1.0;
        }
        let mut rng = rand::thread_rng();
        let jitter = (rng.gen::<f32>() - 0.5) / 10000.0;
        EvalResult::Estimate(evaluation + jitter)
    }
}

impl Display for EvalResult {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Estimate(e) => write!(f, "{}", e),
            Winner(color, 0) => write!(f, "Winner: {:?}", color),
            Winner(color, moves) => write!(f, "M{} for {:?}", moves, color),
            Stalemate => write!(f, "{}", 0),
        }
    }
}