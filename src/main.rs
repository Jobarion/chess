use crate::board::board::{Board};
use crate::board::board::positional::{Square};
use std::convert::TryFrom;
use std::time::Instant;
use clap::{App, Arg};

mod board;
mod bitboard;

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
    // let mut board = Board::from_fen("rnbkqbnr/pppp1ppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1".to_string()).unwrap();
    let mut board = Board::from_fen(fen.unwrap().to_string()).unwrap();
    /*
    CURRENTLY BROKEN: PINS :(
     */

    for n in 1..10 {
        let start = Instant::now();
        println!("Perft {} {:?} in {}.{}s", n, board.perft(n), start.elapsed().as_secs(), start.elapsed().as_millis() % 1000);
    }
    // let legal_moves = board.legal_moves();
    // println!("{}\n{}", board, legal_moves.len());
    // for piece_move in legal_moves {
    //     println!("=======");
    //     board.apply_move(&piece_move);
    //     // let legal_next = board.legal_moves();
    //     // let next = legal_next.first().unwrap();
    //     // board.apply_move(&next);
    //     // println!("{}", legal_next.len());
    //     println!("{}\n", board);
    //     // board.undo_move(&next);
    //     board.undo_move(&piece_move);
    //     //println!("{}\n", board);
    // }
}


fn square(val: &str) -> Square {
    Square::try_from(val).expect("s1")
}