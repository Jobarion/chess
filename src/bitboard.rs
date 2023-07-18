use std::fmt::{Debug, Display, Formatter};
use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Not, Shl, Shr, Sub, SubAssign};
use crate::piece::Square;

#[derive(Copy, Clone, Debug)]
pub struct BitBoard(pub u64);

pub const KNIGHT_MOVES: [BitBoard; 64] = [BitBoard(132096), BitBoard(329728), BitBoard(659712), BitBoard(1319424), BitBoard(2638848), BitBoard(5277696), BitBoard(10489856), BitBoard(4202496), BitBoard(33816580), BitBoard(84410376), BitBoard(168886289), BitBoard(337772578), BitBoard(675545156), BitBoard(1351090312), BitBoard(2685403152), BitBoard(1075839008), BitBoard(8657044482), BitBoard(21609056261), BitBoard(43234889994), BitBoard(86469779988), BitBoard(172939559976), BitBoard(345879119952), BitBoard(687463207072), BitBoard(275414786112), BitBoard(2216203387392), BitBoard(5531918402816), BitBoard(11068131838464), BitBoard(22136263676928), BitBoard(44272527353856), BitBoard(88545054707712), BitBoard(175990581010432), BitBoard(70506185244672), BitBoard(567348067172352), BitBoard(1416171111120896), BitBoard(2833441750646784), BitBoard(5666883501293568), BitBoard(11333767002587136), BitBoard(22667534005174272), BitBoard(45053588738670592), BitBoard(18049583422636032), BitBoard(145241105196122112), BitBoard(362539804446949376), BitBoard(725361088165576704), BitBoard(1450722176331153408), BitBoard(2901444352662306816), BitBoard(5802888705324613632), BitBoard(11533718717099671552), BitBoard(4620693356194824192), BitBoard(288234782788157440), BitBoard(576469569871282176), BitBoard(1224997833292120064), BitBoard(2449995666584240128), BitBoard(4899991333168480256), BitBoard(9799982666336960512), BitBoard(1152939783987658752), BitBoard(2305878468463689728), BitBoard(1128098930098176), BitBoard(2257297371824128), BitBoard(4796069720358912), BitBoard(9592139440717824), BitBoard(19184278881435648), BitBoard(38368557762871296), BitBoard(4679521487814656), BitBoard(9077567998918656)];
pub const PAWN_CAPTURE_MOVES_WHITE: [BitBoard; 64] = [BitBoard(512), BitBoard(1280), BitBoard(2560), BitBoard(5120), BitBoard(10240), BitBoard(20480), BitBoard(40960), BitBoard(16384), BitBoard(131072), BitBoard(327680), BitBoard(655360), BitBoard(1310720), BitBoard(2621440), BitBoard(5242880), BitBoard(10485760), BitBoard(4194304), BitBoard(33554432), BitBoard(83886080), BitBoard(167772160), BitBoard(335544320), BitBoard(671088640), BitBoard(1342177280), BitBoard(2684354560), BitBoard(1073741824), BitBoard(8589934592), BitBoard(21474836480), BitBoard(42949672960), BitBoard(85899345920), BitBoard(171798691840), BitBoard(343597383680), BitBoard(687194767360), BitBoard(274877906944), BitBoard(2199023255552), BitBoard(5497558138880), BitBoard(10995116277760), BitBoard(21990232555520), BitBoard(43980465111040), BitBoard(87960930222080), BitBoard(175921860444160), BitBoard(70368744177664), BitBoard(562949953421312), BitBoard(1407374883553280), BitBoard(2814749767106560), BitBoard(5629499534213120), BitBoard(11258999068426240), BitBoard(22517998136852480), BitBoard(45035996273704960), BitBoard(18014398509481984), BitBoard(144115188075855872), BitBoard(360287970189639680), BitBoard(720575940379279360), BitBoard(1441151880758558720), BitBoard(2882303761517117440), BitBoard(5764607523034234880), BitBoard(11529215046068469760), BitBoard(4611686018427387904), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0)];
pub const PAWN_CAPTURE_MOVES_BLACK: [BitBoard; 64] = [BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(2), BitBoard(5), BitBoard(10), BitBoard(20), BitBoard(40), BitBoard(80), BitBoard(160), BitBoard(64), BitBoard(512), BitBoard(1280), BitBoard(2560), BitBoard(5120), BitBoard(10240), BitBoard(20480), BitBoard(40960), BitBoard(16384), BitBoard(131072), BitBoard(327680), BitBoard(655360), BitBoard(1310720), BitBoard(2621440), BitBoard(5242880), BitBoard(10485760), BitBoard(4194304), BitBoard(33554432), BitBoard(83886080), BitBoard(167772160), BitBoard(335544320), BitBoard(671088640), BitBoard(1342177280), BitBoard(2684354560), BitBoard(1073741824), BitBoard(8589934592), BitBoard(21474836480), BitBoard(42949672960), BitBoard(85899345920), BitBoard(171798691840), BitBoard(343597383680), BitBoard(687194767360), BitBoard(274877906944), BitBoard(2199023255552), BitBoard(5497558138880), BitBoard(10995116277760), BitBoard(21990232555520), BitBoard(43980465111040), BitBoard(87960930222080), BitBoard(175921860444160), BitBoard(70368744177664), BitBoard(562949953421312), BitBoard(1407374883553280), BitBoard(2814749767106560), BitBoard(5629499534213120), BitBoard(11258999068426240), BitBoard(22517998136852480), BitBoard(45035996273704960), BitBoard(18014398509481984)];
pub const KING_MOVES: [BitBoard; 64] = [BitBoard(770), BitBoard(1797), BitBoard(3594), BitBoard(7188), BitBoard(14376), BitBoard(28752), BitBoard(57504), BitBoard(49216), BitBoard(197123), BitBoard(460039), BitBoard(920078), BitBoard(1840156), BitBoard(3680312), BitBoard(7360624), BitBoard(14721248), BitBoard(12599488), BitBoard(50463488), BitBoard(117769984), BitBoard(235539968), BitBoard(471079936), BitBoard(942159872), BitBoard(1884319744), BitBoard(3768639488), BitBoard(3225468928), BitBoard(12918652928), BitBoard(30149115904), BitBoard(60298231808), BitBoard(120596463616), BitBoard(241192927232), BitBoard(482385854464), BitBoard(964771708928), BitBoard(825720045568), BitBoard(3307175149568), BitBoard(7718173671424), BitBoard(15436347342848), BitBoard(30872694685696), BitBoard(61745389371392), BitBoard(123490778742784), BitBoard(246981557485568), BitBoard(211384331665408), BitBoard(846636838289408), BitBoard(1975852459884544), BitBoard(3951704919769088), BitBoard(7903409839538176), BitBoard(15806819679076352), BitBoard(31613639358152704), BitBoard(63227278716305408), BitBoard(54114388906344448), BitBoard(216739030602088448), BitBoard(505818229730443264), BitBoard(1011636459460886528), BitBoard(2023272918921773056), BitBoard(4046545837843546112), BitBoard(8093091675687092224), BitBoard(16186183351374184448), BitBoard(13853283560024178688), BitBoard(144959613005987840), BitBoard(362258295026614272), BitBoard(724516590053228544), BitBoard(1449033180106457088), BitBoard(2898066360212914176), BitBoard(5796132720425828352), BitBoard(11592265440851656704), BitBoard(4665729213955833856)];
pub const DIAGONALS_NW_SE: [BitBoard; 64] = [BitBoard(9241421688590303745), BitBoard(36099303471055874), BitBoard(141012904183812), BitBoard(550831656968), BitBoard(2151686160), BitBoard(8405024), BitBoard(32832), BitBoard(128), BitBoard(4620710844295151872), BitBoard(9241421688590303745), BitBoard(36099303471055874), BitBoard(141012904183812), BitBoard(550831656968), BitBoard(2151686160), BitBoard(8405024), BitBoard(32832), BitBoard(2310355422147575808), BitBoard(4620710844295151872), BitBoard(9241421688590303745), BitBoard(36099303471055874), BitBoard(141012904183812), BitBoard(550831656968), BitBoard(2151686160), BitBoard(8405024), BitBoard(1155177711073755136), BitBoard(2310355422147575808), BitBoard(4620710844295151872), BitBoard(9241421688590303745), BitBoard(36099303471055874), BitBoard(141012904183812), BitBoard(550831656968), BitBoard(2151686160), BitBoard(577588855528488960), BitBoard(1155177711073755136), BitBoard(2310355422147575808), BitBoard(4620710844295151872), BitBoard(9241421688590303745), BitBoard(36099303471055874), BitBoard(141012904183812), BitBoard(550831656968), BitBoard(288794425616760832), BitBoard(577588855528488960), BitBoard(1155177711073755136), BitBoard(2310355422147575808), BitBoard(4620710844295151872), BitBoard(9241421688590303745), BitBoard(36099303471055874), BitBoard(141012904183812), BitBoard(144396663052566528), BitBoard(288794425616760832), BitBoard(577588855528488960), BitBoard(1155177711073755136), BitBoard(2310355422147575808), BitBoard(4620710844295151872), BitBoard(9241421688590303745), BitBoard(36099303471055874), BitBoard(72057594037927936), BitBoard(144396663052566528), BitBoard(288794425616760832), BitBoard(577588855528488960), BitBoard(1155177711073755136), BitBoard(2310355422147575808), BitBoard(4620710844295151872), BitBoard(9241421688590303745)];
pub const DIAGONALS_NE_SW: [BitBoard; 64] = [BitBoard(1), BitBoard(258), BitBoard(66052), BitBoard(16909320), BitBoard(4328785936), BitBoard(1108169199648), BitBoard(283691315109952), BitBoard(72624976668147840), BitBoard(258), BitBoard(66052), BitBoard(16909320), BitBoard(4328785936), BitBoard(1108169199648), BitBoard(283691315109952), BitBoard(72624976668147840), BitBoard(145249953336295424), BitBoard(66052), BitBoard(16909320), BitBoard(4328785936), BitBoard(1108169199648), BitBoard(283691315109952), BitBoard(72624976668147840), BitBoard(145249953336295424), BitBoard(290499906672525312), BitBoard(16909320), BitBoard(4328785936), BitBoard(1108169199648), BitBoard(283691315109952), BitBoard(72624976668147840), BitBoard(145249953336295424), BitBoard(290499906672525312), BitBoard(580999813328273408), BitBoard(4328785936), BitBoard(1108169199648), BitBoard(283691315109952), BitBoard(72624976668147840), BitBoard(145249953336295424), BitBoard(290499906672525312), BitBoard(580999813328273408), BitBoard(1161999622361579520), BitBoard(1108169199648), BitBoard(283691315109952), BitBoard(72624976668147840), BitBoard(145249953336295424), BitBoard(290499906672525312), BitBoard(580999813328273408), BitBoard(1161999622361579520), BitBoard(2323998145211531264), BitBoard(283691315109952), BitBoard(72624976668147840), BitBoard(145249953336295424), BitBoard(290499906672525312), BitBoard(580999813328273408), BitBoard(1161999622361579520), BitBoard(2323998145211531264), BitBoard(4647714815446351872), BitBoard(72624976668147840), BitBoard(145249953336295424), BitBoard(290499906672525312), BitBoard(580999813328273408), BitBoard(1161999622361579520), BitBoard(2323998145211531264), BitBoard(4647714815446351872), BitBoard(9223372036854775808)];
pub const RANKS: [BitBoard; 8] = [BitBoard(0xFF << 0), BitBoard(0xFF << 8), BitBoard(0xFF << 16), BitBoard(0xFF << 24), BitBoard(0xFF << 32), BitBoard(0xFF << 40), BitBoard(0xFF << 48), BitBoard(0xFF << 56)];
pub const FILES: [BitBoard; 8] = [BitBoard(0x0101010101010101 << 0), BitBoard(0x0101010101010101 << 1), BitBoard(0x0101010101010101 << 2), BitBoard(0x0101010101010101 << 3), BitBoard(0x0101010101010101 << 4), BitBoard(0x0101010101010101 << 5), BitBoard(0x0101010101010101 << 6), BitBoard(0x0101010101010101 << 7)];

impl BitBoard {

    pub fn is_set(&self, sqr: Square) -> bool {
        self.0 & (1 << sqr.0) != 0
    }

    pub fn set(&self, sqr: Square) -> BitBoard {
        BitBoard(self.0 | 1 << sqr.0)
    }

    pub fn unset(&self, sqr: Square) -> BitBoard {
        BitBoard(self.0 & !(1 << sqr.0))
    }

    pub fn toggle(&self, sqr: Square) -> BitBoard {
        BitBoard(self.0 ^ (1 << sqr.0))
    }

    pub fn pop_lsb(&mut self) -> Option<Square> {
        match self {
            BitBoard(0) => None,
            BitBoard(n) => {
                let lss = Square(n.trailing_zeros() as u8);
                self.0 &= self.0 - 1;
                Some(lss)
            }
        }
    }
}

impl Display for BitBoard {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for rank in (0..8).rev() {
            for file in 0..8 {
                let sqr = Square::new(file, rank);
                let _ = match *self & sqr == 0 {
                    true => write!(f, "0")?,
                    false => write!(f, "1")?
                };
            }
            write!(f, "\n")?
        }
        Ok(())
    }
}

impl PartialEq<u64> for BitBoard {
    fn eq(&self, other: &u64) -> bool {
        self.0 == *other
    }
}

// impl PartialEq<BitBoard> for u64 {
//     fn eq(&self, other: &BitBoard) -> bool {
//         *self != other.0
//     }
// }

impl From<Square> for BitBoard {
    fn from(value: Square) -> Self {
        BitBoard(1 << value.0)
    }
}

impl Iterator for BitBoard {
    type Item = Square;

    fn next(&mut self) -> Option<Self::Item> {
        self.pop_lsb()
    }
}

impl Shr<usize> for BitBoard {
    type Output = BitBoard;

    fn shr(self, rhs: usize) -> Self::Output {
        BitBoard(self.0 >> rhs)
    }
}

impl Shl<usize> for BitBoard {
    type Output = BitBoard;

    fn shl(self, rhs: usize) -> Self::Output {
        BitBoard(self.0 << rhs)
    }
}

impl Sub<Square> for BitBoard {
    type Output = BitBoard;

    fn sub(self, rhs: Square) -> Self::Output {
        BitBoard(self.0 - (1 << rhs.0))
    }
}

impl SubAssign<Square> for BitBoard {
    fn sub_assign(&mut self, rhs: Square) {
        self.0 -= 1 << rhs.0;
    }
}

impl BitOr<BitBoard> for BitBoard {
    type Output = BitBoard;

    fn bitor(self, rhs: BitBoard) -> BitBoard {
        BitBoard(self.0 | rhs.0)
    }
}

impl BitOr<Square> for BitBoard {
    type Output = BitBoard;

    fn bitor(self, rhs: Square) -> BitBoard {
        self | BitBoard::from(rhs)
    }
}

impl BitOrAssign<Square> for BitBoard {
    fn bitor_assign(&mut self, rhs: Square) {
        self.0 |= 1 << rhs.0;
    }
}

impl BitXorAssign<Square> for BitBoard {
    fn bitxor_assign(&mut self, rhs: Square) {
        self.0 ^= 1 << rhs.0;
    }
}

impl BitAndAssign<Square> for BitBoard {
    fn bitand_assign(&mut self, rhs: Square) {
        self.0 &= 1 << rhs.0;
    }
}

impl BitOrAssign<BitBoard> for BitBoard {
    fn bitor_assign(&mut self, rhs: BitBoard) {
        self.0 |= rhs.0;
    }
}

impl BitXorAssign<BitBoard> for BitBoard {
    fn bitxor_assign(&mut self, rhs: BitBoard) {
        self.0 ^= rhs.0;
    }
}

impl BitAndAssign<BitBoard> for BitBoard {
    fn bitand_assign(&mut self, rhs: BitBoard) {
        self.0 &= rhs.0;
    }
}

impl BitXor<Square> for BitBoard {
    type Output = BitBoard;

    fn bitxor(self, rhs: Square) -> BitBoard {
        self ^ BitBoard::from(rhs)
    }
}

impl BitXor<BitBoard> for BitBoard {
    type Output = BitBoard;

    fn bitxor(self, rhs: BitBoard) -> BitBoard {
        BitBoard(self.0 ^ rhs.0)
    }
}

impl Not for BitBoard {
    type Output = BitBoard;

    fn not(self) -> BitBoard {
        BitBoard(!self.0)
    }
}

impl BitAnd<BitBoard> for BitBoard {
    type Output = BitBoard;

    fn bitand(self, rhs: BitBoard) -> BitBoard {
        BitBoard(self.0 & rhs.0)
    }
}

impl BitAnd<Square> for BitBoard {
    type Output = BitBoard;

    fn bitand(self, rhs: Square) -> BitBoard {
        self & BitBoard::from(rhs)
    }
}

#[cfg(test)]
mod bitboard_gen_tests {
    use crate::bitboard::BitBoard;
    use crate::piece::Square;

    #[test]
    fn generate_diag_mask() {
        let all = BitBoard(0xFFFFFFFFFFFFFFFF);
        let rd = 1;
        let fd = 1;
        for sqr in all {
            let mut mask = BitBoard(0) | sqr;
            let mut rank: i32 = sqr.rank() as i32 + rd;
            let mut file: i32 = sqr.file() as i32 + fd;
            while rank < 8 && file < 8 {
                mask = mask | Square::new(file as usize, rank as usize);
                rank = rank + rd;
                file = file + fd;
            }
            let mut rank: i32 = sqr.rank() as i32 - rd;
            let mut file: i32 = sqr.file() as i32 - fd;
            while rank >= 0 && file >= 0 {
                mask = mask | Square::new(file as usize, rank as usize);
                rank = rank - rd;
                file = file - fd;
            }
            let mut rank: i32 = sqr.rank() as i32 + rd;
            let mut file: i32 = sqr.file() as i32 - fd;
            while rank < 8 && file < 8 && rank >= 0 && file >= 0 {
                mask = mask | Square::new(file as usize, rank as usize);
                rank = rank + rd;
                file = file - fd;
            }
            let mut rank: i32 = sqr.rank() as i32 - rd;
            let mut file: i32 = sqr.file() as i32 + fd;
            while rank >= 0 && file >= 0 && rank < 8 && file < 8 {
                mask = mask | Square::new(file as usize, rank as usize);
                rank = rank - rd;
                file = file + fd;
            }
            // println!("{}", mask);
            print!("BitBoard({}), ", mask.0);
        }
    }

    #[test]
    fn generate_rook_mask() {
        let all = BitBoard(0xFFFFFFFFFFFFFFFF);
        for sqr in all {
            let mut mask = BitBoard(0) | sqr;
            let mut rank: i32 = sqr.rank() as i32 + 1;
            let mut file: i32 = sqr.file() as i32;
            while rank < 8 && file < 8 {
                mask = mask | Square::new(file as usize, rank as usize);
                rank = rank + 1;
            }
            let mut rank: i32 = sqr.rank() as i32 - 1;
            let mut file: i32 = sqr.file() as i32;
            while rank >= 0 && file >= 0 {
                mask = mask | Square::new(file as usize, rank as usize);
                rank = rank - 1;
            }
            let mut rank: i32 = sqr.rank() as i32;
            let mut file: i32 = sqr.file() as i32 - 1;
            while rank < 8 && file < 8 && rank >= 0 && file >= 0 {
                mask = mask | Square::new(file as usize, rank as usize);
                file = file - 1;
            }
            let mut rank: i32 = sqr.rank() as i32;
            let mut file: i32 = sqr.file() as i32 + 1;
            while rank >= 0 && file >= 0 && rank < 8 && file < 8 {
                mask = mask | Square::new(file as usize, rank as usize);
                file = file + 1;
            }
            // println!("{}", mask);
            print!("BitBoard({}), ", mask.0);
        }
    }

}