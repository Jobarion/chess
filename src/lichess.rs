use std::error::Error;
use std::io::ErrorKind;
use clap::builder::Str;
use tokio_util::io::StreamReader;
use futures_util::stream::TryStreamExt;
use itertools::Itertools;
use reqwest::header::AUTHORIZATION;
use reqwest::Response;
use tokio::io::AsyncBufReadExt;
use serde::{Serialize, Deserialize};
use serde::ser::StdError;
use tokio::spawn;
use crate::{Board, Color, MinMaxEvaluator, Move, MoveFinder, MoveSuggestion};
use crate::Color::{BLACK, WHITE};
use crate::lichess::BotEvent::{Challenge, GameStart};
use crate::lichess::GameEvent::GameFull;

const AUTHORIZATION_HEADER: &str = "Bearer lip_rs2nWprOVDbDtyyj4g2o";
const BOT_ID: &str = "jobabot"; //We should fetch this

fn convert_err(err: reqwest::Error) -> std::io::Error {
    std::io::Error::new(ErrorKind::Other, err)
}

#[derive(Deserialize, Debug)]
#[serde(tag = "type")]
#[serde(rename_all = "camelCase")]
enum BotEvent {
    Challenge{challenge: ChallengeData},
    GameStart{game: GameStartData},
    KeepAlive
}

#[derive(Deserialize, Debug)]
struct ChallengeData {
    id: String,
    url: String,
    challenger: Player
}

#[derive(Deserialize, Debug)]
struct GameStartData {
    id: String,
    fen: String
}

pub async fn start_event_loop() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let client = reqwest::Client::new();
    let response = client
        .get("https://lichess.org/api/stream/event")
        .header(AUTHORIZATION, AUTHORIZATION_HEADER)
        .send()
        .await?
        .bytes_stream();


    let reader = StreamReader::new(response.map_err(convert_err));
    let mut lines = reader.lines();


    while let Some(line) = lines.next_line().await? {
        let event: BotEvent = serde_json::from_str(line.as_str()).unwrap_or(BotEvent::KeepAlive);
        // println!("Event loop, event = {:?}", event);
        match event {
            Challenge{challenge: ChallengeData{id, url, challenger}} => {
                if challenger.id.as_str() == "puad" || challenger.id.as_str() == "jobarion" {
                    accept_challenge(id).await?;
                } else {
                    decline_challenge(id).await?;
                }
                ()
            },
            GameStart{game: GameStartData{id, fen}} => {
                println!("Game has started {}", id);
                std::thread::spawn(|| game_loop(id));
                // game_loop(id).await?;
            },
            BotEvent::KeepAlive => ()
        }
    }
    Ok(())
}

async fn accept_challenge(id: String) -> Result<Response, reqwest::Error> {
    let client = reqwest::Client::new();
    println!("Trying to accept challenge {}", id);
    client.post(format!("https://lichess.org/api/challenge/{}/accept", id))
        .header(AUTHORIZATION, AUTHORIZATION_HEADER)
        .send()
        .await
}

async fn decline_challenge(id: String) -> Result<Response, reqwest::Error> {
    let client = reqwest::Client::new();
    println!("Trying to decline challenge {}", id);
    client.post(format!("https://lichess.org/api/challenge/{}/decline", id))
        .header(AUTHORIZATION, AUTHORIZATION_HEADER)
        .send()
        .await
}

#[derive(Deserialize, Debug)]
#[serde(tag = "type")]
#[serde(rename_all = "camelCase")]
enum GameEvent {
    #[serde(rename_all = "camelCase")]
    GameFull{
        white: Player,
        black: Player,
        initial_fen: String,
        state: GameState
    },
    GameState{
        #[serde(flatten)]
        state: GameState
    },
    KeepAlive
}

#[derive(Deserialize, Debug)]
struct Player {
    id: String,
    name: String,
    rating: u32,
}

#[derive(Deserialize, Debug)]
struct GameState {
    moves: String,
    wtime: u32,
    btime: u32
}

#[tokio::main]
async fn game_loop(id: String) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let client = reqwest::Client::new();

    let response = client
        .get(format!("https://lichess.org/api/bot/game/stream/{}", id.clone()))
        .header(AUTHORIZATION, AUTHORIZATION_HEADER)
        .send()
        .await?
        .bytes_stream();


    let reader = StreamReader::new(response.map_err(convert_err));
    let mut lines = reader.lines();

    let mut color_opt: Option<Color> = None;

    let mut fen_opt: Option<String> = None;

    while let Some(line) = lines.next_line().await? {
        let event: GameEvent = serde_json::from_str(line.as_str()).unwrap_or(GameEvent::KeepAlive);
        // println!("Game loop, event = {:?}", event);

        let mut board_opt: Option<Board> = None;

        match event {
            GameEvent::GameState{ state } => {
                if let Some(fen) = &fen_opt {
                    let mut board = Board::from_fen(fen.as_str()).unwrap();
                    for uci in state.moves.split(" ").filter(|m| m.len() >= 4) {
                        let m = Move::from_uci(uci.to_string(), &board).unwrap();
                        board.apply_move(&m);
                    }
                    board_opt = Some(board);
                }
            }
            GameEvent::GameFull{ white, black, initial_fen, state } => {
                println!("Full game with fen {}. Current moves: {}. white: {:?}, black: {:?}", initial_fen, state.moves, white, black);
                fen_opt = Some(initial_fen.clone());
                if let Some(fen) = &fen_opt {
                    let mut board = Board::from_fen(fen.as_str()).unwrap();
                    for uci in state.moves.split(" ").filter(|m| m.len() >= 4) {
                        let m = Move::from_uci(uci.to_string(), &board).unwrap();
                        board.apply_move(&m);
                    }
                    board_opt = Some(board);
                }
                if white.id == BOT_ID.to_string() {
                    color_opt = Some(WHITE);
                }
                else if black.id == BOT_ID.to_string() {
                    color_opt = Some(BLACK);
                }
            },
            GameEvent::KeepAlive => (),
        }

        if let Some(mut board) = &mut board_opt {
            println!("Current position\n{}\n", board);
            if let Some(color) = color_opt {
                if board.active_player == color {
                    println!("It's my turn");
                    let evaluator = MinMaxEvaluator::new(7);
                    if let MoveSuggestion(eval, Some(pmove)) = evaluator.find_move(&mut board) {
                        println!("Best move '{}', eval: '{}'", pmove, eval);
                        make_move(id.clone(), pmove.to_uci()).await?;
                    }
                }
                else {
                    println!("It's not my turn yet");
                }
            }
            else {
                println!("I am only observing this game");
            }
        }
    }
    Ok(())
}

async fn make_move(id: String, uci: String) -> Result<Response, reqwest::Error> {
    let client = reqwest::Client::new();
    client.post(format!("https://lichess.org/api/bot/game/{}/move/{}", id, uci))
        .header(AUTHORIZATION, AUTHORIZATION_HEADER)
        .send()
        .await
}