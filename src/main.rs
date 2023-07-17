use crate::board::board::{Board};
use std::convert::TryFrom;
use std::{io, thread};
use std::io::{Error, Write};
use std::process::{Command, Stdio};
use std::str::FromStr;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};
use clap::arg;
use console::Term;
use itertools::Itertools;
use crate::bitboard::BitBoard;
use crate::evaluator::{eval_position_direct, MinMaxEvaluator, MinMaxMetadata, MoveFinder, MoveSuggestion};
use crate::iter_deep::eval_iter_deep;
use crate::piece::{Color, Move, Square};

mod board;
mod bitboard;
mod piece;
mod evaluator;
mod lichess;
mod iter_deep;
mod hashing;
mod movegen;

const TERMINAL: bool = true;
const DEFAULT_DEPTH: u8 = 6;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let matches = clap::Command::new("Chess")
        .version("1.0")
        .author("Jonas B. <jonas+chess@joba.me>")
        .arg(arg!(--fen <FEN> "FEN string").default_value("startpos"))
        .arg(arg!(--depth <DEPTH> "Evaluation depth").default_value("6"))
        .arg(arg!(--uci "UCI output").default_value("false"))
        .get_matches();

    let fen: &String = matches.get_one("fen").unwrap();
    let mut board = Board::from_fen(fen).unwrap();

    // lichess::start_event_loop().await;

    // run_engine();


    // compare_perft("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1".to_string(), 5).unwrap();



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

    for n in 1..10 {
        let start = Instant::now();
        println!("Perft {} {:?} in {}.{}s", n, board.perft(n), start.elapsed().as_secs(), start.elapsed().as_millis() % 1000);
    }

    // for start_move in board.legal_moves() {
    //
    //     board.apply_move(&start_move);
    //     println!("Perft {}: {:?}", start_move.to_uci(), board.perft(4));
    //     // println!("Perft {}", start_move.to_uci());
    //     board.undo_move(&start_move);
    // }
    // let mut board = Board::from_fen("5rkr/qqqq2Q1/8/5B2/5N2/5B2/P4N2/KB4Q1 b - - 0 3".to_string()).unwrap();
    // println!("{:?}", board.legal_moves());
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
    Ok(())
}

fn run_engine() {
    let mut board = Board::from_fen("6k1/3qrpp1/pp5p/1r5n/8/1P3PP1/PQ4BP/2R3K1 w - - 0 1").unwrap();

    // for n in 1..10 {
    //     let evaluator = MinMaxEvaluator::new(n);
    //     let start = Instant::now();
    //     let eval = evaluator.find_move(&mut board, &mut MinMaxMetadata::new(0));
    //     println!("eval depth {} {} in {}.{}s", n, eval.0, start.elapsed().as_secs(), start.elapsed().as_millis() % 1000);
    // }
    let now = SystemTime::now().duration_since(UNIX_EPOCH).expect("Time went backwards").as_millis();
    let result = eval_iter_deep(&mut board, now + 10*1000);
    println!("{:?}", result);
}

fn compare_perft(fen: &str, depth: u8) -> io::Result<()>{
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
            compare_perft(new_fen.as_str(), depth - 1)
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