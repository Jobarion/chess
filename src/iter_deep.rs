use std::time::{Instant, SystemTime, UNIX_EPOCH};
use crate::{Board, AlphaBetaSearch, MoveSuggestion};
use crate::evaluator::MinMaxMetadata;
use crate::hashing::{AlphaBetaData, TranspositionTable};

const MAX_DEPTH: u8 = 100;
const START_DEPTH: u8 = 2;

pub fn eval_iter_deep(mut board: &mut Board, end_time: u128, hash_size: usize, conserve_time: bool, quiet: bool) -> Option<MoveSuggestion> {
    let mut best_move: Option<MoveSuggestion> = None;
    let mut previous_elapsed = 0_u128;
    let mut tt_table = TranspositionTable::<AlphaBetaData, 2>::new(hash_size);
    for depth in START_DEPTH..MAX_DEPTH {
        let start = Instant::now();
        let mut meta = MinMaxMetadata::new(end_time, &mut tt_table);
        let move_at_depth = AlphaBetaSearch::find_move(&mut board, depth, &mut meta);
        if meta.should_terminate {
            if !quiet {
                println!("Terminated depth {}", depth);
            }
            break;
        } else {
            best_move = Some(move_at_depth);
            if !quiet {
                println!("Completed depth {} with result {:?}, nodes: {}", depth, move_at_depth, meta.node_count);
            }
        }
        let elapsed_time = start.elapsed().as_millis();
        if previous_elapsed == 0 {
            previous_elapsed = elapsed_time;
        }
        else {
            let expected_duration_next = elapsed_time * (elapsed_time / previous_elapsed);
            let current_time = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_millis();
            if current_time > end_time {
                break;
            }
            let time_left = end_time - current_time;
            previous_elapsed = elapsed_time;
            if !quiet {
                println!("Time left: {}. Expected for next iteration: {}", time_left, expected_duration_next);
            }
            if conserve_time && (current_time > end_time || expected_duration_next > time_left) {
                if !quiet {
                    println!("Terminating early");
                }
                break;
            }
        }
    }
    best_move
}