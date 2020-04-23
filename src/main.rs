use std::option::Option::{Some, None};
use crate::board::board::{Board, Piece, Color};
use crate::board::board::positional::{Move, Square};
use crate::board::evaluator::{MaterialEvaluator, MiniMaxEvaluator, MoveFinder, MoveSuggestion, EvalResult};
use crate::board::evaluator::Evaluator;
use std::convert::TryFrom;
use crate::board::evaluator::EvalResult::Stalemate;
use std::time::{SystemTime, UNIX_EPOCH};
use std::sync::atomic::{AtomicUsize, Ordering};

static CALL_COUNT: AtomicUsize = AtomicUsize::new(0);


mod board;


fn main() {
    let mut board = Board::from_fen("8/1B3p2/5p2/8/P1NP1P2/3kP3/3b2R1/2R3K1 w - - 1 41".to_string()).unwrap();

//    let mut board = Board::from_fen("4k3/8/8/1p4pp/8/4q3/7q/K7 b - - 19 141".to_string()).unwrap();
//    let b2 = board.do_move(&Move::new(&board, &square("e3"), &square("e1")));
    let evaluator = MiniMaxEvaluator::new(4);
//    board.all_available_moves().for_each(|m| {
//        let b = board.do_move(&m);
//        println!("{} {}", m.to_algebraic(&board).unwrap(), evaluator.evaluate_board(&b));
//    });
    let mut pgn = "".to_string();
//    println!("{}", board);
    for i in 0..1 {
        let start = SystemTime::now();
        let MoveSuggestion(eval, m_opt) = evaluator.find_move(&board);
        if let None = m_opt {
            println!("Game over. {:?}", eval);
            break;
        }
//        let eval2 = evaluator.evaluate_board(&b2);
//        println!("{} {} {:?}", eval, eval2, eval.cmp(&eval2));
        let m = m_opt.unwrap();
        let since_the_epoch = SystemTime::now().duration_since(start)
            .expect("Time went backwards");
        println!("Analysed positions: {} in {:?} msec", CALL_COUNT.load(Ordering::SeqCst), since_the_epoch.as_millis());
        CALL_COUNT.store(0, Ordering::SeqCst);
        println!("Move: {} ({})", m.to_algebraic(&board).unwrap(), eval);
        if board.active_player == Color::WHITE {
            pgn = [pgn, board.fullmove.to_string(), ". ".to_string()].concat();
        }
        pgn = [pgn, m.to_algebraic(&board).unwrap(), " ".to_string()].concat();
        println!("PGN: {}", pgn);
        println!();
        board = board.do_move(&m);
        println!("{}", board);
    }
}


//fn main_1() {
//    let mut board = Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1".to_string()).unwrap();
//    let eval = MaterialEvaluator;
//    for i in 0..150{
//        let m = board.all_available_moves().max_by(|m1, m2| {
//                let state_1 = board.do_move(m1);
//                let val_1 = MaterialEvaluator::evaluate_board(&state_1);
//                let state_2 = board.do_move(m2);
//                let val_2 = MaterialEvaluator::evaluate_board(&state_2);
//                let factor = match board.active_player {
//                    Color::WHITE => 1.0,
//                    Color::BLACK => -1.0,
//                };
//                (factor*val_1).partial_cmp(&(factor*val_2)).expect("Evaluation must be finite")
//            }).unwrap();
////        println!("==Taken==");
//        if board.active_player == Color::WHITE {
//            print!("{}. ", board.fullmove)
//        }
//        print!("{} ", m.to_algebraic(&board).unwrap());
//
//        board = board.do_move(&m);
////        println!("{}\n", board);
//    }
////    let m2 = Move::new(&board, &s3, &s4);
////    let cs2 = board.do_move(&m2);
////    println!("{}", board);
////    board.undo_move(&m2, &cs2);
////    println!("{}", board);
////    board.undo_move(&m1, &cs1);
////    println!("{}", board);
//}


fn square(val: &str) -> Square {
    Square::try_from(val).expect("s1")
}