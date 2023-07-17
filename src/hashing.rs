use const_random::const_random;

pub struct Zobrist;
impl Zobrist {

    const COLOR_COUNT: usize = 2;
    const PIECE_COUNT: usize = 6;
    const SQUARE_COUNT: usize = 64;
    const HASH_TABLE_SIZE: usize = Zobrist::SQUARE_COUNT * Zobrist::PIECE_COUNT * Zobrist::COLOR_COUNT + 4 + 8 + 1; //

    const ZB_HASH_KEYS: [u64; Zobrist::HASH_TABLE_SIZE] = Zobrist::generate_zobrist_keys();

    pub const PIECES: [[[u64; Zobrist::SQUARE_COUNT]; Zobrist::PIECE_COUNT]; Zobrist::COLOR_COUNT] = [
        [
            Zobrist::extract_zobrist_keys::<64>(0 * 64),
            Zobrist::extract_zobrist_keys::<64>(1 * 64),
            Zobrist::extract_zobrist_keys::<64>(2 * 64),
            Zobrist::extract_zobrist_keys::<64>(3 * 64),
            Zobrist::extract_zobrist_keys::<64>(4 * 64),
            Zobrist::extract_zobrist_keys::<64>(5 * 64),
        ],
        [
            Zobrist::extract_zobrist_keys::<64>(6 * 64),
            Zobrist::extract_zobrist_keys::<64>(7 * 64),
            Zobrist::extract_zobrist_keys::<64>(8 * 64),
            Zobrist::extract_zobrist_keys::<64>(9 * 64),
            Zobrist::extract_zobrist_keys::<64>(10 * 64),
            Zobrist::extract_zobrist_keys::<64>(11 * 64),
        ]];
    pub const CASTLING: [u64; 4] = Zobrist::extract_zobrist_keys::<4>(12 * 64);
    pub const EP_RANK: [u64; 8] = Zobrist::extract_zobrist_keys::<8>(12 * 64 + 4);
    pub const SIDE: u64 = Zobrist::ZB_HASH_KEYS[12 * 64 + 12];

    const fn extract_zobrist_keys<const COUNT: usize>(offset: usize) -> [u64; COUNT] {
        let mut arr = [0; COUNT];
        let mut n = 0;
        while n < COUNT {
            arr[n] = Zobrist::ZB_HASH_KEYS[n + offset];
            n += 1;
        }
        arr
    }

    const fn generate_zobrist_keys<>() -> [u64; 781] {
        let mut arr = [0; 781];
        let mut n = 0;
        let mut tmp_array = const_random!([u8; 6248]);

        while n < 781 {
            arr[n] |= (tmp_array[n * 8 + 0] as u64) << 0;
            arr[n] |= (tmp_array[n * 8 + 1] as u64) << 8;
            arr[n] |= (tmp_array[n * 8 + 2] as u64) << 16;
            arr[n] |= (tmp_array[n * 8 + 3] as u64) << 24;
            arr[n] |= (tmp_array[n * 8 + 4] as u64) << 32;
            arr[n] |= (tmp_array[n * 8 + 5] as u64) << 40;
            arr[n] |= (tmp_array[n * 8 + 6] as u64) << 48;
            arr[n] |= (tmp_array[n * 8 + 7] as u64) << 56;
            n += 1;
        }
        arr
    }
}