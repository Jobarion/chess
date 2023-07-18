use std::cmp::{min};
use std::error::Error;
use std::io::ErrorKind;
use std::time::{SystemTime, UNIX_EPOCH};

use tokio_util::io::StreamReader;
use futures_util::stream::TryStreamExt;

use reqwest::header::AUTHORIZATION;
use reqwest::Response;
use tokio::io::AsyncBufReadExt;
use serde::{Deserialize};


use crate::{Board, Color, eval_iter_deep, Move, MoveSuggestion};
use crate::Color::{BLACK, WHITE};

use crate::lichess::BotEvent::{Challenge, GameStart};

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

#[derive(Deserialize, Debug)]
struct ProfileData {
    id: String
}

pub struct LichessBot {
    auth_token: String,
    whitelist: Vec<String>,
    bot_id: String,
    hash_size: usize,
}

impl LichessBot {
    pub fn new(auth_token: String, whitelist: Vec<String>, hash_size: usize) -> LichessBot {
        LichessBot {
            auth_token,
            whitelist,
            bot_id: "".to_string(),
            hash_size,
        }
    }

    pub async fn start_event_loop(&mut self) -> Result<(), Box<dyn Error + Send + Sync>> {
        println!("Connecting to Lichess");
        let client = reqwest::Client::new();
        let profile_data: ProfileData = serde_json::from_str(client
            .get("https://lichess.org/api/account")
            .header(AUTHORIZATION, format!("Bearer {}", self.auth_token))
            .send()
            .await?
            .text()
            .await?
            .as_str())?;
        println!("Authorized as {}", profile_data.id);
        self.bot_id = profile_data.id;

        let response = client
            .get("https://lichess.org/api/stream/event")
            .header(AUTHORIZATION, format!("Bearer {}", self.auth_token))
            .send()
            .await?
            .bytes_stream();
        println!("Connected.");

        let reader = StreamReader::new(response.map_err(convert_err));
        let mut lines = reader.lines();


        while let Some(line) = lines.next_line().await? {
            let event: BotEvent = serde_json::from_str(line.as_str()).unwrap_or(BotEvent::KeepAlive);
            // println!("Event loop, event = {:?}", event);
            match event {
                Challenge{challenge: ChallengeData{id, url: _, challenger}} => {
                    if self.whitelist.contains(&challenger.id) {
                        self.accept_challenge(id).await?;
                    } else {
                        self.decline_challenge(id).await?;
                    }
                    ()
                },
                GameStart{game: GameStartData{id, fen: _}} => {
                    println!("Game has started {}", id);
                    let other = LichessBot {
                        auth_token: self.auth_token.clone(),
                        whitelist: self.whitelist.clone(),
                        bot_id: self.bot_id.clone(),
                        hash_size: self.hash_size,
                    };
                    std::thread::spawn(|| other.game_loop(id));
                    // game_loop(id).await?;
                },
                BotEvent::KeepAlive => ()
            }
        }
        Ok(())
    }

    async fn accept_challenge(&self, id: String) -> Result<Response, reqwest::Error> {
        let client = reqwest::Client::new();
        println!("Trying to accept challenge {}", id);
        client.post(format!("https://lichess.org/api/challenge/{}/accept", id))
            .header(AUTHORIZATION, format!("Bearer {}", self.auth_token))
            .send()
            .await
    }

    async fn decline_challenge(&self, id: String) -> Result<Response, reqwest::Error> {
        let client = reqwest::Client::new();
        println!("Trying to decline challenge {}", id);
        client.post(format!("https://lichess.org/api/challenge/{}/decline", id))
            .header(AUTHORIZATION, format!("Bearer {}", self.auth_token))
            .send()
            .await
    }

    #[tokio::main]
    async fn game_loop(self, id: String) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        let client = reqwest::Client::new();

        let response = client
            .get(format!("https://lichess.org/api/bot/game/stream/{}", id.clone()))
            .header(AUTHORIZATION, format!("Bearer {}", self.auth_token))
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
            let mut us_time = 0_u64;
            let mut them_time = 0_u64;
            let mut us_inc = 0_u64;
            let mut them_inc = 0_u64;

            match event {
                GameEvent::GameState{ state } => {
                    if let Some(fen) = &fen_opt {
                        let mut board = Board::from_fen(fen.as_str()).unwrap();
                        for uci in state.moves.split(" ").filter(|m| m.len() >= 4) {
                            let m = Move::from_uci(uci, &board).unwrap();
                            board.apply_move(&m);
                        }
                        if let Some(color) = color_opt {
                            if color == WHITE {
                                us_time = state.wtime;
                                them_time = state.btime;
                                us_inc = state.winc;
                                them_inc = state.binc;
                            }
                            else {
                                them_time = state.wtime;
                                us_time = state.btime;
                                them_inc = state.winc;
                                us_inc = state.binc;
                            }
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
                            let m = Move::from_uci(uci, &board).unwrap();
                            board.apply_move(&m);
                        }
                        board_opt = Some(board);
                    }

                    if white.id == self.bot_id.to_string() {
                        color_opt = Some(WHITE);
                        us_time = state.wtime;
                        them_time = state.btime;
                        us_inc = state.winc;
                        them_inc = state.binc;
                    }
                    else if black.id == self.bot_id.to_string() {
                        color_opt = Some(BLACK);
                        them_time = state.wtime;
                        us_time = state.btime;
                        them_inc = state.winc;
                        us_inc = state.binc;
                    }
                },
                GameEvent::KeepAlive => (),
            }

            if let Some(mut board) = &mut board_opt {
                println!("Current position\n{}\n", board);
                if let Some(color) = color_opt {
                    if board.active_player == color {
                        println!("It's my turn");
                        println!("We have {}s, they have {}s", us_time / 1000, them_time / 1000);
                        let max_time = LichessBot::calc_move_time(us_time, them_time, us_inc, them_inc);
                        let time = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_millis();
                        let end_time = time + max_time as u128;
                        if let Some(MoveSuggestion(eval, Some(pmove))) = eval_iter_deep(&mut board, end_time, 32, true, false) {
                            println!("Best move '{}', eval: '{}'", pmove, eval);
                            self.make_move(id.clone(), pmove.to_uci()).await?;
                        } else {
                            println!("Couldn't find move");
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

    fn calc_move_time(us_time: u64, them_time: u64, us_inc: u64, _them_inc: u64) -> u64 {
        if us_time >= 1_000_000 {
            return 20 * 1000;
        }
        if them_time > us_time {
            //Expect the game to take 30 more moves (incl. incr).
            //If time is low and increment high, just take half of our time and live of increment.
            println!("{} {} {} {}", us_time, us_inc, us_time / 30 + us_inc, us_time / 2);
            min(us_time / 30 + us_inc, us_time / 2)
        } else {
            them_time / 30 + (us_time - them_time) / 5 //Use up our time advantage
        }
    }

    async fn make_move(&self, id: String, uci: String) -> Result<Response, reqwest::Error> {
        let client = reqwest::Client::new();
        client.post(format!("https://lichess.org/api/bot/game/{}/move/{}", id, uci))
            .header(AUTHORIZATION, format!("Bearer {}", self.auth_token))
            .send()
            .await
    }
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
    wtime: u64,
    btime: u64,
    winc: u64,
    binc: u64,
}