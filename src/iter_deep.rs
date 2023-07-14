use std::time::{Instant, SystemTime, UNIX_EPOCH};
use crate::{Board, MinMaxEvaluator, MoveFinder, MoveSuggestion};
use crate::evaluator::MinMaxMetadata;

const MAX_DEPTH: usize = 100;
const START_DEPTH: usize = 2;

pub fn eval_iter_deep(mut board: &mut Board, end_time: u128) -> Option<MoveSuggestion> {
    let mut best_move: Option<MoveSuggestion> = None;
    let mut previous_elapsed = 0_u128;
    for depth in 2..MAX_DEPTH {
        let start = Instant::now();
        let evaluator = MinMaxEvaluator::new(depth);
        let mut meta = MinMaxMetadata::new(end_time);
        let move_at_depth = evaluator.find_move(&mut board, &mut meta);
        if meta.should_terminate {
            println!("Terminated depth {} with result {:?}", depth, move_at_depth);
            // if let Some(old_best) = &best_move {
            //     if move_at_depth.0 > old_best.0 {
            //         best_move = Some(move_at_depth);
            //     }
            // }
            break;
        } else {
            println!("Completed depth {} with result {:?}", depth, move_at_depth);
            best_move = Some(move_at_depth);
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
            println!("Time left: {}. Expected for next iteration: {}", time_left, expected_duration_next);
            if current_time > end_time || expected_duration_next / 2 > time_left {
                println!("Terminating early");
                break;
            }
        }
    }
    best_move
}