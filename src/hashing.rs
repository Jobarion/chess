use crate::evaluator::{Evaluation};
use rand_chacha::ChaChaRng;
use rand::{Rng, SeedableRng};

use crate::board::board::CastleState;
use crate::board::board::CastleState::Allowed;
use crate::piece::{Color, Move, MoveAction, PieceType, Square};

pub type ZobristHash = u64;

const COLOR_COUNT: usize = 2;
const PIECE_COUNT: usize = 6;
const SQUARE_COUNT: usize = 64;

#[derive(Copy, Clone, Debug)]
pub struct Zobrist {
    pieces: [[[ZobristHash; SQUARE_COUNT]; PIECE_COUNT]; COLOR_COUNT],
    castling: [ZobristHash; 16],
    ep_rank: [ZobristHash; 65],
    side: [ZobristHash; 2]
}

const RNG_SEED: [u8; 32] = [125; 32];

impl Zobrist {

    pub fn new() -> Zobrist {
        let mut rng = ChaChaRng::from_seed(RNG_SEED);
        Zobrist {
            pieces: [
                [
                    Zobrist::generate_zobrist_keys::<64>(&mut rng),
                    Zobrist::generate_zobrist_keys::<64>(&mut rng),
                    Zobrist::generate_zobrist_keys::<64>(&mut rng),
                    Zobrist::generate_zobrist_keys::<64>(&mut rng),
                    Zobrist::generate_zobrist_keys::<64>(&mut rng),
                    Zobrist::generate_zobrist_keys::<64>(&mut rng),
                ],
                [
                    Zobrist::generate_zobrist_keys::<64>(&mut rng),
                    Zobrist::generate_zobrist_keys::<64>(&mut rng),
                    Zobrist::generate_zobrist_keys::<64>(&mut rng),
                    Zobrist::generate_zobrist_keys::<64>(&mut rng),
                    Zobrist::generate_zobrist_keys::<64>(&mut rng),
                    Zobrist::generate_zobrist_keys::<64>(&mut rng),
                ]
            ],
            castling: Zobrist::generate_zobrist_keys::<16>(&mut rng),
            side: Zobrist::generate_zobrist_keys::<2>(&mut rng),
            ep_rank: Zobrist::generate_zobrist_keys::<65>(&mut rng),
        }
    }

    pub fn side(&self, color: Color) -> ZobristHash {
        self.side[color]
    }

    pub fn ep(&self, sqr: Option<Square>) -> ZobristHash {
        self.ep_rank[sqr.map_or(64, |s|s.0) as usize]
    }

    pub fn castling(&self, white: (CastleState, CastleState), black: (CastleState, CastleState)) -> ZobristHash {
        let mut id = 0;
        if white.0 == Allowed {
            id |= 1;
        }
        if white.1 == Allowed {
            id |= 2;
        }
        if black.0 == Allowed {
            id |= 4;
        }
        if black.1 == Allowed {
            id |= 8;
        }
        self.castling[id]
    }

    pub fn piece(&self, color: Color, piece_type: PieceType, sqr: Square) -> ZobristHash {
        self.pieces[color][piece_type][sqr]
    }

    fn generate_zobrist_keys<const COUNT: usize>(rng: &mut ChaChaRng) -> [ZobristHash; COUNT] {
        [0; COUNT]
            .map(|_| rng.gen::<ZobristHash>())
    }
}

/*
 *  TT Table implementation
 */

#[derive(Copy, Clone)]
pub struct PerftData {
    pub nodes: u64,
    pub depth: u8,
}

impl PerftData {
    pub fn get_nodes(&self, depth: u8) -> Option<u64> {
        if self.depth == depth {
            Some(self.nodes)
        } else {
            None
        }
    }
}

impl IHashData for PerftData {
    fn new() -> Self {
        PerftData {
            nodes: 0,
            depth: 0,
        }
    }

    fn depth(&self) -> u8 {
        self.depth
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum NodeType {
    UpperBound,
    LowerBound,
    Exact,
    None
}

#[derive(Copy, Clone)]
pub struct AlphaBetaData {
    pub depth: u8,
    pub ply: u8,
    pub node_type: NodeType,
    pub eval: Evaluation,
    pub best_move: Move,
}

impl IHashData for AlphaBetaData {
    fn new() -> Self {
        Self {
            depth: 0,
            eval: Evaluation::Stalemate,
            node_type: NodeType::None,
            best_move: Move {
                from: Square(0),
                to: Square(0),
                move_type: MoveAction::Normal,
                previous_ep_square: None,
            },
            ply: 0
        }
    }

    fn depth(&self) -> u8 {
        self.depth
    }
}

impl AlphaBetaData {

    pub fn create(depth: u8, node_type: NodeType, eval: Evaluation, best_move: Move, ply: u8) -> AlphaBetaData {
        AlphaBetaData {
            depth,
            node_type,
            eval,
            best_move,
            ply
        }
    }
}

pub trait IHashData {
    fn new() -> Self;
    fn depth(&self) -> u8;
}

#[derive(Copy, Clone)]
struct Entry<N> {
    data: N,
    verification: u32
}

impl<N: IHashData + Copy + Clone> Entry<N> {
    pub fn new() -> Self {
        Self {
            data: N::new(),
            verification: 0
        }
    }
}

const BUCKET_SIZE: usize = 2;
const NEW_ID: usize = 0;
const DEEP_ID: usize = 1;

#[derive(Copy, Clone)]
struct Bucket<N> {
    entries: [Entry<N>; BUCKET_SIZE],
}

impl<N: IHashData + Copy + Clone> Bucket<N> {

    pub fn new() -> Self {
        Self {
            entries: [Entry::new(); 2]
        }
    }

    pub fn store(&mut self, data: N, verification: u32) {
        let to_insert = Entry {
            data,
            verification
        };
        if self.entries[DEEP_ID].data.depth() <= data.depth() {
            self.entries[DEEP_ID] = to_insert;
        } else {
            self.entries[NEW_ID] = to_insert;
        }
    }

    pub fn retrieve(&self, verification: u32) -> Option<&N> {
        for n in 0..BUCKET_SIZE {
            if verification == self.entries[n].verification {
                return Some(&self.entries[n].data);
            }
        }
        None
    }
}

fn calc_tt_table_size<N>(mb_used: usize) -> usize {
    let entry_size = std::mem::size_of::<Entry<N>>();
    let bucket_size = BUCKET_SIZE * entry_size;
    let table_size = 1024 * 1024 * mb_used / bucket_size;
    table_size
}

pub struct TranspositionTable<N> {
    buckets: Vec<Bucket<N>>,
    mb_size: usize,
}

impl<N: IHashData + Copy + Clone> TranspositionTable<N> {
    pub fn new(mb_size: usize) -> Self {
        Self {
            buckets: vec![Bucket::<N>::new(); calc_tt_table_size::<N>(mb_size)],
            mb_size,
        }
    }

    pub fn store(&mut self, data: N, hash: ZobristHash) {
        if self.mb_size != 0 {
            let bucket_id = self.calculate_bucket_id(hash);
            let verification = Self::calculate_verification(hash);
            self.buckets[bucket_id].store(data, verification);
        }
    }

    pub fn retrieve(&self, hash: ZobristHash) -> Option<&N> {
        if self.mb_size != 0 {
            let bucket_id = self.calculate_bucket_id(hash);
            let verification = Self::calculate_verification(hash);
            self.buckets[bucket_id].retrieve(verification)
        } else {
            None
        }
    }

    fn calculate_bucket_id(&self, hash: ZobristHash) -> usize {
        (((hash & 0xFFFFFFFF00000000) >> 32) as usize) % self.buckets.len()
    }

    fn calculate_verification(hash: ZobristHash) -> u32 {
        (hash & 0x00000000FFFFFFFF) as u32
    }
}
