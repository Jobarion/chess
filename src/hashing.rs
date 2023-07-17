use const_random::const_random;

pub struct Zobrist;
impl Zobrist {
    pub const PIECES: [[[u64; 64]; 6]; 2] = [
        [
            Zobrist::generate_zobrist_keys::<64>(),
            Zobrist::generate_zobrist_keys::<64>(),
            Zobrist::generate_zobrist_keys::<64>(),
            Zobrist::generate_zobrist_keys::<64>(),
            Zobrist::generate_zobrist_keys::<64>(),
            Zobrist::generate_zobrist_keys::<64>(),
        ],
        [
            Zobrist::generate_zobrist_keys::<64>(),
            Zobrist::generate_zobrist_keys::<64>(),
            Zobrist::generate_zobrist_keys::<64>(),
            Zobrist::generate_zobrist_keys::<64>(),
            Zobrist::generate_zobrist_keys::<64>(),
            Zobrist::generate_zobrist_keys::<64>(),
        ]];
    pub const SIDE: u64 = const_random!(u64);
    pub const CASTLING: [u64; 16] = Zobrist::generate_zobrist_keys::<16>();
    pub const EP_RANK: [u64; 8] = Zobrist::generate_zobrist_keys::<8>();

    const fn generate_zobrist_keys<const COUNT: usize>() -> [u64; COUNT] {
        let mut arr = [0; COUNT];
        let mut n = 0;
        while n < COUNT {
            arr[n] = const_random!(u64);
            n += 1;
        }
        arr
    }
}