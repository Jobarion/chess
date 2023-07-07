use crate::board::board::{Board};
use std::convert::TryFrom;
use std::{io, thread};
use std::io::{Error, Write};
use std::process::{Command, Stdio};
use std::str::FromStr;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant, SystemTime};
use clap::{App, Arg};
use console::Term;
use itertools::Itertools;
use crate::bitboard::BitBoard;
use crate::evaluator::{MiniMaxEvaluator, MoveFinder, MoveSuggestion, Stats};
use crate::piece::{Color, Move, Square};

mod board;
mod bitboard;
mod piece;
mod evaluator;

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
            .arg(Arg::with_name("uci")
                .long("uci")
                .help("UCI output")
                .default_value("false"))
            .arg(Arg::with_name("moves")
                .long("moves")
                .help("Additional moves")
                .default_value(""))
        .get_matches();
    let fen = matches.value_of("fen");
    let mut board = Board::from_fen(fen.unwrap().to_string()).unwrap();

    run_engine();
    // board.legal_moves();

    // println!("{}", board);
    // board.print_highlighted(board.kogge_stone_avx2_queen());
    // board.print_highlighted(board.kogge_stone_avx2_bishop());
    // board.print_highlighted(board.kogge_stone_avx2_rook());
    // compare_perft(fen.unwrap().to_string(), 3).unwrap();

    // compare_perft("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1".to_string(), 5).unwrap();



    // let result = Board::generate_rook_all_moves(BitBoard(0) | Square::new(3, 4), BitBoard(0xFFFFFFFFFFFFFFFF));
    // println!("{}", result);

    // let a = Move::from_uci("h3g2".to_string(), &board).unwrap();
    // println!("{}", board.to_fen());
    // board.apply_move(&a);
    // println!("\t{}", board.to_fen());
    // let b = Move::from_uci("a1b1".to_string(), &board).unwrap();
    // board.apply_move(&b);
    // println!("\t\t{}", board.to_fen());
    // let c = Move::from_uci("g2h1q".to_string(), &board).unwrap();
    // board.apply_move(&c);
    // println!("\t\t\t{}", board.to_fen());
    // board.undo_move(&c);
    // println!("\t\t{}", board.to_fen());
    // board.undo_move(&b);
    // println!("\t{}", board.to_fen());
    // board.undo_move(&a);
    // println!("{}", board.to_fen());

    // for n in 1..10 {
    //     let start = Instant::now();
    //     println!("Perft {} {:?} in {}.{}s", n, board.perft(n), start.elapsed().as_secs(), start.elapsed().as_millis() % 1000);
    // }

    // for start_move in board.legal_moves() {
    //
    //     board.apply_move(&start_move);
    //     println!("Perft {}: {:?}", start_move.to_uci(), board.perft(4));
    //     // println!("Perft {}", start_move.to_uci());
    //     board.undo_move(&start_move);
    // }
    // let mut board = Board::from_fen("r5kr/qqqqqqq1/5Q2/5B2/5NQ1/5BQ1/P4NQ1/KB4Q1 w - - 0 2".to_string()).unwrap();
    // // //
    // for piece_move in board.legal_moves().legal_moves {
    //     println!("=== {} ===", piece_move.to_uci());
    //     board.apply_move(&piece_move);
    //     // let legal_next = board.legal_moves();
    //     // let next = legal_next.first().unwrap();
    //     // board.apply_move(&next);
    //     // board.undo_move(&next);
    //     // println!("{}", legal_next.len());
    //     // println!("{}", board.to_fen());
    //     board.undo_move(&piece_move);
    // }
}

fn run_engine() {
    let mut board = Board::from_fen("5rkr/qqqqqqq1/8/5B2/5NQ1/5BQ1/P4NQ1/KB4Q1 w - - 0 1".to_string()).unwrap();
    let evaluator = MiniMaxEvaluator::new(8);

    for i in 0..1 {
        spawn_watch_thread(evaluator.stats.clone(), board.clone());
        let start = SystemTime::now();
        let MoveSuggestion(eval, m_opt) = evaluator.find_move(&mut board);
        evaluator.reset_stats();
        if let None = m_opt {
            println!("Game over. {:?}", eval);
            break;
        }

        let m = m_opt.unwrap();
        let since_the_epoch = SystemTime::now().duration_since(start)
            .expect("Time went backwards");
        println!("Move: {} ({})", m.to_uci(), eval);
        println!();
        board.apply_move(&m);
        println!("{}", board);
    }
}

fn spawn_watch_thread(stats: Arc<Mutex<Stats>>, board: Board) {
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
                Some(MoveSuggestion(eval, Some(mv))) => format_args!("Best move: {} ({})", mv.to_uci(), eval).to_string(),
            };

            if current < last {
                break;
            }
            let diff = (current - last) * 2;
            last = current;
            if TERMINAL {
                term.clear_last_lines(2);
                // term.write_fmt(format_args!("Evaluated {} boards/sec ({} total)\n", diff, current));
                // term.write_line(best_move.as_str());
            }
            else {
                // println!("Evaluated {} boards/sec ({} total)", diff, current);
                // println!("{}", best_move);
            }
        }
    });

}

fn compare_perft(fen: String, depth: u8) -> io::Result<()>{
    println!("Comparing '{}' at depth {}", fen, depth);
    let mut child = Command::new("./stockfish")
        .stdout(Stdio::piped())
        .stdin(Stdio::piped())
        .spawn()?;

    let child_stdin = child.stdin.as_mut().unwrap();

    child_stdin.write_all(format!("position fen {}\n", fen).as_bytes())?;
    child_stdin.write_all(format!("go perft {}\n", depth).as_bytes())?;
    child_stdin.write_all(format!("quit\n").as_bytes())?;

    let mut board = Board::from_fen(fen).unwrap();

    let board_clone = board.clone();
    let our_valid_moves = board.legal_moves().legal_moves;
    let stockfish_valid_moves = String::from_utf8(child.wait_with_output()?.stdout).unwrap()
        .split("\n")
        .skip(1)
        .take_while(|x| x.len() > 1)
        .map(|x| {
            let mut parts = x.split(": ");
            let uci = parts.next().unwrap();
            let count = parts.next().unwrap().trim();
            let count = u64::from_str(count).unwrap();
            let uci_move = Move::from_uci(uci.to_string(), &board_clone);
            match uci_move {
                Err(_) => panic!("Invalid move (we can't generate the move struct) {}", uci),
                Ok(m) => (m, count)
            }
        }).collect_vec();
    if let Some((bad_move, scount, pcount)) = stockfish_valid_moves.iter()
        .map(|(pmove, count)| {
            // println!("\tTrying to apply {}", pmove.to_uci());
            let move_known = our_valid_moves.contains(&pmove);
            if move_known {
                board.apply_move(&pmove);
                let perft = board.perft(depth - 1);
                board.undo_move(&pmove);
                (pmove, count, perft.nodes)
            } else {
                (pmove, count, 0)
            }
        })
        .filter(|(_, scount, pcount)| *scount != pcount)
        .next() {
        if pcount > 0 {
            println!("Found difference. {}, stockfish {}, us {}", bad_move.to_uci(), scount, pcount);
            board.apply_move(&bad_move);
            println!("{}\n\n", board);
            let new_fen = board.to_fen();
            compare_perft(new_fen, depth - 1)
        } else {
            println!("Found stockfish move we don't know. {}, stockfish {}, us {}", bad_move.to_uci(), scount, pcount);
            Ok(())
        }
    } else {
        let stockfish_valid_moves = stockfish_valid_moves.into_iter().map(|(pmove, _)| pmove).collect_vec();
        for our_move in our_valid_moves {
            if !stockfish_valid_moves.contains(&our_move) {
                board.apply_move(&our_move);
                let bad_move_perft = board.perft(depth - 1);
                println!("Found move stockfish doesn't know. {}, stockfish 0, us {}", our_move.to_uci(), bad_move_perft.nodes);
            }
        }
        Ok(())
    }
}


fn square(val: &str) -> Square {
    Square::try_from(val).expect("s1")
}