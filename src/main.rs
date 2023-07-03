use crate::board::board::{Board};
use std::convert::TryFrom;
use std::io;
use std::io::{Error, Write};
use std::process::{Command, Stdio};
use std::str::FromStr;
use std::time::Instant;
use clap::{App, Arg};
use itertools::Itertools;
use crate::bitboard::BitBoard;
use crate::piece::{Move, Square};

mod board;
mod bitboard;
mod piece;

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
    // compare_perft(fen.unwrap().to_string(), 7).unwrap();

    // compare_perft("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1".to_string(), 7).unwrap();


    //
    // let mut board = Board::from_fen("r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10".to_string()).unwrap();

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
    // let mut board = Board::from_fen("8/8/3p4/1Pp4r/1K5k/5p2/4P1P1/1R6 w - c6 0 3".to_string()).unwrap();
    // //
    // let legal_moves = board.legal_moves();
    // for piece_move in legal_moves {
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

fn compare_perft(fen: String, depth: u8) -> io::Result<()>{
    println!("Comparing '{}' at depth {}", fen, depth);
    let mut child = Command::new(".\\stockfish.exe")
        .stdout(Stdio::piped())
        .stdin(Stdio::piped())
        .spawn()?;

    let child_stdin = child.stdin.as_mut().unwrap();

    child_stdin.write_all(format!("position fen {}\n", fen).as_bytes())?;
    child_stdin.write_all(format!("go perft {}\n", depth).as_bytes())?;
    child_stdin.write_all(format!("quit\n").as_bytes())?;

    let mut board = Board::from_fen(fen).unwrap();

    let board_clone = board.clone();
    let our_valid_moves = board.legal_moves();
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