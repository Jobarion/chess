use crate::board::board::{PieceType, Board, Piece, Color, FILE_SIZE, RANK_SIZE};
use crate::board::board::positional::{Square, File, Rank};

pub trait Evaluator {
    fn evaluate_board(board: &Board) -> f32;
}

pub struct MaterialEvaluator;

impl Evaluator for MaterialEvaluator {

    fn evaluate_board(board: &Board) -> f32 {
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
                        PieceType::KING => 10000.0,
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
        evaluation
    }
}