use std::option::Option::{Some, None};
use crate::board::board::{Board, Piece, Color};
use crate::board::board::positional::{Move, Square};
use crate::board::evaluator::{MiniMaxEvaluator, MoveFinder, MoveSuggestion, EvalResult};
use crate::board::evaluator::Evaluator;
use std::convert::TryFrom;
use crate::board::evaluator::EvalResult::Stalemate;
use std::time::{SystemTime, UNIX_EPOCH};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::{thread, env};
use std::time::Duration;
use console::Term;
use std::io::Write;
use std::sync::{Arc, Mutex};
use clap::{App, Arg};

mod board;

const TERMINAL: bool = true;
const DEFAULT_DEPTH: u8 = 6;

fn main() {
    let matches = App::new("Chess")
            .version("1.0")
            .author("Jonas B. <jonas+chess@joba.me>")
            .arg(Arg::with_name("fen")
                .long("fen")
                .help("FEN string")
                .default_value("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"))
            .arg(Arg::with_name("depth")
                .long("depth")
                .help("Evaluation depth")
                .default_value("6"))
        .get_matches();
    matches.value_of("fen");
    evaluator(matches.value_of("depth").unwrap().parse::<u8>().unwrap(), matches.value_of("fen").unwrap())
}

fn evaluator(depth: u8, fen: &str) {
    println!("Evaluating {}\nDepth: {}\n", fen, depth);

    let mut board = Board::from_fen(fen.to_string()).unwrap();
    println!("{}\n\n\n\n", board);

    let evaluator = MiniMaxEvaluator::new(depth);
    let mut pgn = "".to_string();
    for i in 0..1 {
        spawn_watch_thread(evaluator.stats.clone(), board.clone());
        let start = SystemTime::now();
        let MoveSuggestion(eval, m_opt) = evaluator.find_move(&board);
        evaluator.reset_stats();
        if let None = m_opt {
            println!("Game over. {:?}", eval);
            break;
        }

        let m = m_opt.unwrap();
        let since_the_epoch = SystemTime::now().duration_since(start)
            .expect("Time went backwards");
        println!("Move: {} ({})", m.to_algebraic(&board).unwrap(), eval);
        if board.active_player == Color::WHITE {
            pgn = [pgn, board.fullmove.to_string(), ". ".to_string()].concat();
        }
        pgn = [pgn, m.to_algebraic(&board).unwrap(), " ".to_string()].concat();
//        println!("PGN: {}", pgn);
        println!();
        board = board.do_move(&m);
        println!("{}", board);
    }
}

fn spawn_watch_thread(stats: Arc<Mutex<board::evaluator::Stats>>, board: Board) {
    thread::spawn(move || {
        let mut term = Term::stdout();
        let mut last = 0;
        loop {
            thread::sleep(Duration::from_millis(500));
            let mut stats = stats.lock().unwrap();
            let current = (*stats).evaluated_positions;
            let best_move =(*stats).best_move.clone();
            let best_move = match best_move {
                None => "Best move: ?".to_string(),
                Some(MoveSuggestion(eval, None)) => eval.to_string(),
                Some(MoveSuggestion(eval, Some(mv))) => format_args!("Best move: {} ({})", mv.to_algebraic(&board).unwrap(), eval).to_string(),
            };

            if current < last {
                break;
            }
            let diff = (current - last) * 2;
            last = current;
            if TERMINAL {
                term.clear_last_lines(2);
                term.write_fmt(format_args!("Evaluated {} boards/sec ({} total)\n", diff, current));
                term.write_line(best_move.as_str());
            }
            else {
                println!("Evaluated {} boards/sec ({} total)", diff, current);
                println!("{}", best_move);
            }
        }
    });

}

fn square(val: &str) -> Square {
    Square::try_from(val).expect("s1")
}