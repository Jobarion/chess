use crate::board::board::{Board};
use std::convert::TryFrom;
use std::error::Error;
use std::io::Write;
use std::process::{Command, Stdio};
use std::str::FromStr;
use std::time::{SystemTime, UNIX_EPOCH};
use clap::{arg, command, value_parser};
use itertools::Itertools;
use crate::bitboard::BitBoard;
use crate::evaluator::{AlphaBetaSearch, MoveFinder, MoveSuggestion};
use crate::iter_deep::eval_iter_deep;
use crate::lichess::LichessBot;
use crate::movegen::MoveType;
use crate::piece::{Color, Move, Square};

mod board;
mod bitboard;
mod piece;
mod evaluator;
mod lichess;
mod iter_deep;
mod hashing;
mod movegen;

#[tokio::main]
async fn main() {
    let matches = command!("Chess")
        .version("1.0")
        .author("Jonas B. <jonas+chess@joba.me>")
        .subcommand_required(true)
        .subcommand(command!("eval")
            .arg(arg!(--fen <FEN> "FEN string").default_value("startpos"))
            .arg(arg!(--time <SECONDS> "Evaluation time seconds").value_parser(value_parser!(u32)).default_value("10"))
            .arg(arg!(--uci "UCI output").value_parser(value_parser!(bool)).default_value("false"))
            .arg(arg!(--hash <HASH_SIZE> "Hash table size in MB").value_parser(value_parser!(usize)).default_value("32"))
        )
        .subcommand(command!("lichess")
            .arg(arg!(--token <TOKEN> "Authorization token").required(true))
            .arg(arg!(--challengers <PLAYERS> "Whitelisted players"))
            .arg(arg!(--hash <HASH_SIZE> "Hash table size in MB").value_parser(value_parser!(usize)).default_value("32"))
        )
        .subcommand(command!("perft")
            .arg(arg!(--fen <FEN> "FEN string").default_value("startpos"))
            .arg(arg!(--depth <DEPTH> "Perft depth").value_parser(value_parser!(u8)).required(true))
            .arg(arg!(--hash <HASH_SIZE> "Hash table size in MB").value_parser(value_parser!(usize)).default_value("0"))
        )
        .get_matches();

    match matches.subcommand() {
        Some(("eval", sub_matches)) => {
            let fen = sub_matches.get_one::<String>("fen").unwrap();
            let time = sub_matches.get_one::<u32>("time").unwrap();
            let uci_only = sub_matches.get_flag("uci");
            let hash_size = sub_matches.get_one::<usize>("hash").unwrap();
            run_eval(fen.as_str(), *time, *hash_size, uci_only);
        },
        Some(("perft", sub_matches)) => {
            let fen = sub_matches.get_one::<String>("fen").unwrap();
            let depth = sub_matches.get_one::<u8>("depth").unwrap();
            let hash_size = sub_matches.get_one::<usize>("hash").unwrap();
            run_perft(fen.as_str(), *depth, *hash_size);
        },
        Some(("lichess", sub_matches)) => {
            let token = sub_matches.get_one::<String>("token").unwrap();
            let players: Vec<String> = sub_matches.get_many("challengers").map(|v|v.cloned().collect_vec()).unwrap_or(vec![]);
            let hash_size = sub_matches.get_one::<usize>("hash").unwrap();
            run_lichess(token.clone(), players, *hash_size).await.unwrap();
        },
        _ => unreachable!("Match is exhaustive")
    }
    // run_eval("k7/8/8/8/8/3K4/8/8 w - - 0 1", 10, 0, false);
    // run_eval("8/8/k7/b1R4p/8/K7/8/8 w - - 0 1", 10, 0, false);

    // let fen: &String = matches.get_one("fen").unwrap();
    // let fen = "rnbqkb1r/p3pppp/1p6/2ppP3/3N4/2P5/PPP1QPPP/R1B1KB1R w KQkq - 0 1";
    // iter_deep::eval_iter_deep(&mut board, SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_millis() + 20000);
    // println!("{}", board.zobrist_key);

    // let mut tt = hashing::TranspositionTable::<PerftData, 2>::new(90);
    // lichess::start_event_loop().await;

    // run_engine();


    // compare_perft("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 6).unwrap();



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

    // for n in 0..10 {
    //     let start = Instant::now();
    //     println!("Perft {} {:?} in {}.{}s", n, board.perft(n, true), start.elapsed().as_secs(), start.elapsed().as_millis() % 1000);
    // }

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
}

fn run_eval(fen: &str, time_seconds: u32, hash_size: usize, uci_only: bool) {
    let mut board = Board::from_fen(fen).unwrap();
    // let m = Move::from_uci("c5a5", &board).unwrap();
    // board.apply_move(&m);
    // let m = Move::from_uci("f1b5", &board).unwrap();
    // board.apply_move(&m);
    // println!("{}", evaluator::eval_position_direct(&board));

    let end_time_millis = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_millis() + time_seconds as u128 * 1000;
    let result = iter_deep::eval_iter_deep(&mut board, end_time_millis, hash_size, false, uci_only);
    if let Some(MoveSuggestion(eval, Some(best_move))) = result {
        if uci_only {
            println!("{}", best_move.to_uci());
        }
        else {
            println!("{} ({})", best_move.to_uci(), eval);
        }
    }
}

fn run_perft(fen: &str, depth: u8, hash_size: usize) {
    let mut board = Board::from_fen(fen).unwrap();
    println!("{}", board.perft(depth, hash_size));
}

async fn run_lichess(token: String, whitelist: Vec<String>, hash_size: usize) -> Result<(), Box<dyn Error + Send + Sync>> {
    let mut bot = LichessBot::new(token, whitelist, hash_size);
    bot.start_event_loop().await
}

fn compare_perft(fen: &str, depth: u8) -> std::io::Result<()>{
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

    let our_valid_moves = board.legal_moves(MoveType::All).legal_moves;
    let stockfish_valid_moves = String::from_utf8(child.wait_with_output()?.stdout).unwrap()
        .split("\n")
        .skip(1)
        .take_while(|x| x.len() > 1)
        .map(|x| {
            let mut parts = x.split(": ");
            let uci = parts.next().unwrap();
            let count = parts.next().unwrap().trim();
            let count = u64::from_str(count).unwrap();
            let uci_move = Move::from_uci(uci, &board);
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
                let perft = board.perft(depth - 1, 0);
                board.undo_move(&pmove);
                (pmove, count, perft)
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
                let bad_move_perft = board.perft(depth - 1, 0);
                println!("Found move stockfish doesn't know. {}, stockfish 0, us {}", our_move.to_uci(), bad_move_perft);
            }
        }
        Ok(())
    }
}


fn square(val: &str) -> Square {
    Square::try_from(val).expect("s1")
}