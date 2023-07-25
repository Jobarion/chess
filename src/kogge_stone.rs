use std::arch::x86_64::*;
use crate::bitboard::{BitBoard, FILES};
use crate::board::board::Board;
use crate::piece::{Color, Square};

impl Board {

    pub(crate) fn generate_rook_moves_ks(&self, sqr: Square, rook_color: Color) -> BitBoard {
        self.ks_rook_moves(!self.occupied(), BitBoard::from(sqr)) & !self.color_bbs[rook_color]
    }

    pub(crate) fn generate_bishop_moves_ks(&self, sqr: Square, bishop_color: Color) -> BitBoard {
        self.ks_bishop_moves(!self.occupied(), BitBoard::from(sqr)) & !self.color_bbs[bishop_color]
    }

    pub(crate) fn generate_queen_moves_ks(&self, sqr: Square, queen_color: Color) -> BitBoard {
        self.ks_queen_moves(!self.occupied(), BitBoard::from(sqr)) & !self.color_bbs[queen_color]
    }

    pub(crate) fn ks_rook_moves(&self, propagate_mask: BitBoard, rook_sliders: BitBoard) -> BitBoard {
        unsafe {
            Board::kogge_stone_north_east(rook_sliders, propagate_mask) | Board::kogge_stone_south_west(rook_sliders, propagate_mask)
        }
    }

    pub(crate) fn ks_bishop_moves(&self, propagate_mask: BitBoard, bishop_sliders: BitBoard) -> BitBoard {
        unsafe {
            Board::kogge_stone_north_ew(bishop_sliders, propagate_mask) | Board::kogge_stone_south_ew(bishop_sliders, propagate_mask)
        }
    }

    pub(crate) fn ks_queen_moves(&self, propagate_mask: BitBoard, queen_sliders: BitBoard) -> BitBoard {
        unsafe {
            Board::kogge_stone_rshift_avx2(queen_sliders, propagate_mask) | Board::kogge_stone_lshift_avx2(queen_sliders, propagate_mask)
        }
    }

    #[target_feature(enable = "avx2")]
    unsafe fn kogge_stone_rshift_avx2(gen: BitBoard, pro: BitBoard) -> BitBoard {
        let s1 = _mm256_set_epi64x(8, 1, 7, 9);
        let s2 = _mm256_set_epi64x(16, 2, 14, 18);
        let s3 = _mm256_set_epi64x(32, 4, 28, 36);
        let file_mask = _mm256_set_epi64x(-1 , !FILES[7].0 as i64, !FILES[0].0 as i64, !FILES[7].0 as i64);

        let mut gen = _mm256_set1_epi64x(gen.0 as i64);
        let mut pro = _mm256_set1_epi64x(pro.0 as i64);
        pro = _mm256_and_si256(pro, file_mask);

        gen = _mm256_or_si256(gen, _mm256_and_si256(pro, _mm256_srlv_epi64(gen, s1)));
        pro = _mm256_and_si256(pro, _mm256_srlv_epi64(pro, s1));
        gen = _mm256_or_si256(gen, _mm256_and_si256(pro, _mm256_srlv_epi64(gen, s2)));
        pro = _mm256_and_si256(pro, _mm256_srlv_epi64(pro, s2));
        gen = _mm256_or_si256(gen, _mm256_and_si256(pro, _mm256_srlv_epi64(gen, s3)));

        gen = _mm256_and_si256(_mm256_srlv_epi64(gen, s1), file_mask);

        let mut back_key = [0u64; 4];
        _mm256_storeu_si256(back_key.as_mut_ptr() as *mut __m256i, gen);
        BitBoard(back_key[0] | back_key[1] | back_key[2] | back_key[3])
    }

    #[target_feature(enable = "avx2")]
    unsafe fn kogge_stone_lshift_avx2(gen: BitBoard, pro: BitBoard) -> BitBoard {
        let s1 = _mm256_set_epi64x(8, 1, 7, 9);
        let s2 = _mm256_set_epi64x(16, 2, 14, 18);
        let s3 = _mm256_set_epi64x(32, 4, 28, 36);
        let file_mask = _mm256_set_epi64x(-1, !FILES[0].0 as i64, !FILES[7].0 as i64, !FILES[0].0 as i64);

        let mut gen = _mm256_set1_epi64x(gen.0 as i64);
        let mut pro = _mm256_set1_epi64x(pro.0 as i64);
        pro = _mm256_and_si256(pro, file_mask);

        gen = _mm256_or_si256(gen, _mm256_and_si256(pro, _mm256_sllv_epi64(gen, s1)));
        pro = _mm256_and_si256(pro, _mm256_sllv_epi64(pro, s1));
        gen = _mm256_or_si256(gen, _mm256_and_si256(pro, _mm256_sllv_epi64(gen, s2)));
        pro = _mm256_and_si256(pro, _mm256_sllv_epi64(pro, s2));
        gen = _mm256_or_si256(gen, _mm256_and_si256(pro, _mm256_sllv_epi64(gen, s3)));

        gen = _mm256_and_si256(_mm256_sllv_epi64(gen, s1), file_mask);

        let mut back_key = [0u64; 4];
        _mm256_storeu_si256(back_key.as_mut_ptr() as *mut __m256i, gen);
        BitBoard(back_key[0] | back_key[1] | back_key[2] | back_key[3])
    }

    #[target_feature(enable = "avx2")]
    unsafe fn kogge_stone_south_west(gen: BitBoard, pro: BitBoard) -> BitBoard {
        let s1 = _mm_set_epi64x(8, 1);
        let s2 = _mm_set_epi64x(16, 2);
        let s3 = _mm_set_epi64x(32, 4);
        let file_mask = _mm_set_epi64x(-1 , !FILES[7].0 as i64);

        let mut gen = _mm_set1_epi64x(gen.0 as i64);
        let mut pro = _mm_set1_epi64x(pro.0 as i64);
        pro = _mm_and_si128(pro, file_mask);

        gen = _mm_or_si128(gen, _mm_and_si128(pro, _mm_srlv_epi64(gen, s1)));
        pro = _mm_and_si128(pro, _mm_srlv_epi64(pro, s1));
        gen = _mm_or_si128(gen, _mm_and_si128(pro, _mm_srlv_epi64(gen, s2)));
        pro = _mm_and_si128(pro, _mm_srlv_epi64(pro, s2));
        gen = _mm_or_si128(gen, _mm_and_si128(pro, _mm_srlv_epi64(gen, s3)));

        gen = _mm_and_si128(_mm_srlv_epi64(gen, s1), file_mask);

        let mut back_key = [0u64; 2];
        _mm_store_si128(back_key.as_mut_ptr() as *mut __m128i, gen);
        BitBoard(back_key[0] | back_key[1])
    }

    #[target_feature(enable = "avx2")]
    unsafe fn kogge_stone_north_east(gen: BitBoard, pro: BitBoard) -> BitBoard {
        let s1 = _mm_set_epi64x(8, 1);
        let s2 = _mm_set_epi64x(16, 2);
        let s3 = _mm_set_epi64x(32, 4);
        let file_mask = _mm_set_epi64x(-1 , !FILES[0].0 as i64);

        let mut gen = _mm_set1_epi64x(gen.0 as i64);
        let mut pro = _mm_set1_epi64x(pro.0 as i64);
        pro = _mm_and_si128(pro, file_mask);

        gen = _mm_or_si128(gen, _mm_and_si128(pro, _mm_sllv_epi64(gen, s1)));
        pro = _mm_and_si128(pro, _mm_sllv_epi64(pro, s1));
        gen = _mm_or_si128(gen, _mm_and_si128(pro, _mm_sllv_epi64(gen, s2)));
        pro = _mm_and_si128(pro, _mm_sllv_epi64(pro, s2));
        gen = _mm_or_si128(gen, _mm_and_si128(pro, _mm_sllv_epi64(gen, s3)));

        gen = _mm_and_si128(_mm_sllv_epi64(gen, s1), file_mask);

        let mut back_key = [0u64; 2];
        _mm_store_si128(back_key.as_mut_ptr() as *mut __m128i, gen);
        BitBoard(back_key[0] | back_key[1])
    }

    #[target_feature(enable = "avx2")]
    unsafe fn kogge_stone_south_ew(gen: BitBoard, pro: BitBoard) -> BitBoard {
        let s1 = _mm_set_epi64x(7, 9);
        let s2 = _mm_set_epi64x(14,  18);
        let s3 = _mm_set_epi64x(28, 36);
        let file_mask = _mm_set_epi64x(!FILES[0].0 as i64 , !FILES[7].0 as i64);

        let mut gen = _mm_set1_epi64x(gen.0 as i64);
        let mut pro = _mm_set1_epi64x(pro.0 as i64);
        pro = _mm_and_si128(pro, file_mask);

        gen = _mm_or_si128(gen, _mm_and_si128(pro, _mm_srlv_epi64(gen, s1)));
        pro = _mm_and_si128(pro, _mm_srlv_epi64(pro, s1));
        gen = _mm_or_si128(gen, _mm_and_si128(pro, _mm_srlv_epi64(gen, s2)));
        pro = _mm_and_si128(pro, _mm_srlv_epi64(pro, s2));
        gen = _mm_or_si128(gen, _mm_and_si128(pro, _mm_srlv_epi64(gen, s3)));

        gen = _mm_and_si128(_mm_srlv_epi64(gen, s1), file_mask);

        let mut back_key = [0u64; 2];
        _mm_store_si128(back_key.as_mut_ptr() as *mut __m128i, gen);
        BitBoard(back_key[0] | back_key[1])
    }

    #[target_feature(enable = "avx2")]
    unsafe fn kogge_stone_north_ew(gen: BitBoard, pro: BitBoard) -> BitBoard {
        let s1 = _mm_set_epi64x(7, 9);
        let s2 = _mm_set_epi64x(14,  18);
        let s3 = _mm_set_epi64x(28, 36);
        let file_mask = _mm_set_epi64x(!FILES[7].0 as i64 , !FILES[0].0 as i64);

        let mut gen = _mm_set1_epi64x(gen.0 as i64);
        let mut pro = _mm_set1_epi64x(pro.0 as i64);
        pro = _mm_and_si128(pro, file_mask);

        gen = _mm_or_si128(gen, _mm_and_si128(pro, _mm_sllv_epi64(gen, s1)));
        pro = _mm_and_si128(pro, _mm_sllv_epi64(pro, s1));
        gen = _mm_or_si128(gen, _mm_and_si128(pro, _mm_sllv_epi64(gen, s2)));
        pro = _mm_and_si128(pro, _mm_sllv_epi64(pro, s2));
        gen = _mm_or_si128(gen, _mm_and_si128(pro, _mm_sllv_epi64(gen, s3)));

        gen = _mm_and_si128(_mm_sllv_epi64(gen, s1), file_mask);

        let mut back_key = [0u64; 2];
        _mm_store_si128(back_key.as_mut_ptr() as *mut __m128i, gen);
        BitBoard(back_key[0] | back_key[1])
    }
}