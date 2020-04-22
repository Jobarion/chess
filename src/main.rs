use std::option::Option::{Some, None};
use crate::board::board::{Board, Piece, Color};
use crate::board::board::positional::{Move, Square};
use crate::board::evaluator::MaterialEvaluator;
use crate::board::evaluator::Evaluator;
use std::convert::TryFrom;

mod board;


//use crate::board::positional::{Square, File, Rank};

fn main() {
    main_1();
}

fn main_2() {
    let mut board = Board::from_fen("rqr5/1P6/8/k1K5/8/8/8/8 w - - 0 1".to_string()).unwrap();
    let moves = board.all_available_moves();
    let mut board_clone = board.clone();
    println!("==Available==");
//    board.all_available_moves()
//        .for_each(|m| {
//            println!("{:?}", m);
//            let change = board_clone.do_move(&m);
//            let val_1 = MaterialEvaluator::evaluate_board(&board_clone);
//            println!("{}\nEval: {}\n", board_clone, val_1);
//            board_clone.undo_move(&m, &change);
//        });
    let m = moves.max_by(|m1, m2| {
        let changeset = board_clone.do_move(m1);
        let val_1 = MaterialEvaluator::evaluate_board(&board_clone);
        board_clone.undo_move(m1, &changeset);
        let changeset = board_clone.do_move(m2);
        let val_2 = MaterialEvaluator::evaluate_board(&board_clone);
        board_clone.undo_move(m2, &changeset);
        val_1.partial_cmp(&val_2).expect("Evaluation must be finite")
    }).unwrap();
    println!("{:?}", m);
}

fn main_1() {
    let mut board = Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1".to_string()).unwrap();
    let eval = MaterialEvaluator;
    for i in 0..150{
        let moves = board.all_available_moves();
        let mut board_clone = board.clone();
//        println!("==Available@{}==", i);
//        board.all_available_moves()
//            .for_each(|m| {
//                let change = board_clone.do_move(&m);
//                let val_1 = MaterialEvaluator::evaluate_board(&board_clone);
//                println!("Eval: {} {:?}", val_1, m);
//                board_clone.undo_move(&m, &change);
//            });
        let m = moves.max_by(|m1, m2| {
                let changeset = board_clone.do_move(m1);
                let val_1 = MaterialEvaluator::evaluate_board(&board_clone);
                board_clone.undo_move(m1, &changeset);
                let changeset = board_clone.do_move(m2);
                let val_2 = MaterialEvaluator::evaluate_board(&board_clone);
                board_clone.undo_move(m2, &changeset);
                let factor = match board.active_player {
                    Color::WHITE => 1.0,
                    Color::BLACK => -1.0,
                };
                (factor*val_1).partial_cmp(&(factor*val_2)).expect("Evaluation must be finite")
            }).unwrap();
//        println!("==Taken==");
        if board.active_player == Color::WHITE {
            print!("{}. ", board.fullmove)
        }
        print!("{} ", m.to_algebraic(&board).unwrap());

        board.do_move(&m);
//        println!("{}\n", board);
    }
//    let m2 = Move::new(&board, &s3, &s4);
//    let cs2 = board.do_move(&m2);
//    println!("{}", board);
//    board.undo_move(&m2, &cs2);
//    println!("{}", board);
//    board.undo_move(&m1, &cs1);
//    println!("{}", board);
}


fn square(val: &str) -> Square {
    Square::try_from(val).expect("s1")
}