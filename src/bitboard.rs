use std::future::poll_fn;
use std::ops::{BitAnd, BitOr, BitXor, Not};
use std::process::Output;
use crate::board::board::Board;
use crate::board::board::positional::Square;

#[derive(Copy, Clone, Debug)]
pub struct BitBoard(pub u64);

pub const KNIGHT_MOVES: [BitBoard; 64] = [BitBoard(132096), BitBoard(329728), BitBoard(659712), BitBoard(1319424), BitBoard(2638848), BitBoard(5277696), BitBoard(10489856), BitBoard(4202496), BitBoard(33816580), BitBoard(84410376), BitBoard(168886289), BitBoard(337772578), BitBoard(675545156), BitBoard(1351090312), BitBoard(2685403152), BitBoard(1075839008), BitBoard(8657044482), BitBoard(21609056261), BitBoard(43234889994), BitBoard(86469779988), BitBoard(172939559976), BitBoard(345879119952), BitBoard(687463207072), BitBoard(275414786112), BitBoard(2216203387392), BitBoard(5531918402816), BitBoard(11068131838464), BitBoard(22136263676928), BitBoard(44272527353856), BitBoard(88545054707712), BitBoard(175990581010432), BitBoard(70506185244672), BitBoard(567348067172352), BitBoard(1416171111120896), BitBoard(2833441750646784), BitBoard(5666883501293568), BitBoard(11333767002587136), BitBoard(22667534005174272), BitBoard(45053588738670592), BitBoard(18049583422636032), BitBoard(145241105196122112), BitBoard(362539804446949376), BitBoard(725361088165576704), BitBoard(1450722176331153408), BitBoard(2901444352662306816), BitBoard(5802888705324613632), BitBoard(11533718717099671552), BitBoard(4620693356194824192), BitBoard(288234782788157440), BitBoard(576469569871282176), BitBoard(1224997833292120064), BitBoard(2449995666584240128), BitBoard(4899991333168480256), BitBoard(9799982666336960512), BitBoard(1152939783987658752), BitBoard(2305878468463689728), BitBoard(1128098930098176), BitBoard(2257297371824128), BitBoard(4796069720358912), BitBoard(9592139440717824), BitBoard(19184278881435648), BitBoard(38368557762871296), BitBoard(4679521487814656), BitBoard(9077567998918656)];
pub const PAWN_CAPTURE_MOVES_WHITE: [BitBoard; 64] = [BitBoard(512), BitBoard(1280), BitBoard(2560), BitBoard(5120), BitBoard(10240), BitBoard(20480), BitBoard(40960), BitBoard(16384), BitBoard(131072), BitBoard(327680), BitBoard(655360), BitBoard(1310720), BitBoard(2621440), BitBoard(5242880), BitBoard(10485760), BitBoard(4194304), BitBoard(33554432), BitBoard(83886080), BitBoard(167772160), BitBoard(335544320), BitBoard(671088640), BitBoard(1342177280), BitBoard(2684354560), BitBoard(1073741824), BitBoard(8589934592), BitBoard(21474836480), BitBoard(42949672960), BitBoard(85899345920), BitBoard(171798691840), BitBoard(343597383680), BitBoard(687194767360), BitBoard(274877906944), BitBoard(2199023255552), BitBoard(5497558138880), BitBoard(10995116277760), BitBoard(21990232555520), BitBoard(43980465111040), BitBoard(87960930222080), BitBoard(175921860444160), BitBoard(70368744177664), BitBoard(562949953421312), BitBoard(1407374883553280), BitBoard(2814749767106560), BitBoard(5629499534213120), BitBoard(11258999068426240), BitBoard(22517998136852480), BitBoard(45035996273704960), BitBoard(18014398509481984), BitBoard(144115188075855872), BitBoard(360287970189639680), BitBoard(720575940379279360), BitBoard(1441151880758558720), BitBoard(2882303761517117440), BitBoard(5764607523034234880), BitBoard(11529215046068469760), BitBoard(4611686018427387904), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0)];
pub const PAWN_CAPTURE_MOVES_BLACK: [BitBoard; 64] = [BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(2), BitBoard(5), BitBoard(10), BitBoard(20), BitBoard(40), BitBoard(80), BitBoard(160), BitBoard(64), BitBoard(512), BitBoard(1280), BitBoard(2560), BitBoard(5120), BitBoard(10240), BitBoard(20480), BitBoard(40960), BitBoard(16384), BitBoard(131072), BitBoard(327680), BitBoard(655360), BitBoard(1310720), BitBoard(2621440), BitBoard(5242880), BitBoard(10485760), BitBoard(4194304), BitBoard(33554432), BitBoard(83886080), BitBoard(167772160), BitBoard(335544320), BitBoard(671088640), BitBoard(1342177280), BitBoard(2684354560), BitBoard(1073741824), BitBoard(8589934592), BitBoard(21474836480), BitBoard(42949672960), BitBoard(85899345920), BitBoard(171798691840), BitBoard(343597383680), BitBoard(687194767360), BitBoard(274877906944), BitBoard(2199023255552), BitBoard(5497558138880), BitBoard(10995116277760), BitBoard(21990232555520), BitBoard(43980465111040), BitBoard(87960930222080), BitBoard(175921860444160), BitBoard(70368744177664), BitBoard(562949953421312), BitBoard(1407374883553280), BitBoard(2814749767106560), BitBoard(5629499534213120), BitBoard(11258999068426240), BitBoard(22517998136852480), BitBoard(45035996273704960), BitBoard(18014398509481984)];
pub const PAWN_MOVES_WHITE: [BitBoard; 64] = [BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(16842752), BitBoard(33685504), BitBoard(67371008), BitBoard(134742016), BitBoard(269484032), BitBoard(538968064), BitBoard(1077936128), BitBoard(2155872256), BitBoard(16777216), BitBoard(33554432), BitBoard(67108864), BitBoard(134217728), BitBoard(268435456), BitBoard(536870912), BitBoard(1073741824), BitBoard(2147483648), BitBoard(4294967296), BitBoard(8589934592), BitBoard(17179869184), BitBoard(34359738368), BitBoard(68719476736), BitBoard(137438953472), BitBoard(274877906944), BitBoard(549755813888), BitBoard(1099511627776), BitBoard(2199023255552), BitBoard(4398046511104), BitBoard(8796093022208), BitBoard(17592186044416), BitBoard(35184372088832), BitBoard(70368744177664), BitBoard(140737488355328), BitBoard(281474976710656), BitBoard(562949953421312), BitBoard(1125899906842624), BitBoard(2251799813685248), BitBoard(4503599627370496), BitBoard(9007199254740992), BitBoard(18014398509481984), BitBoard(36028797018963968), BitBoard(72057594037927936), BitBoard(144115188075855872), BitBoard(288230376151711744), BitBoard(576460752303423488), BitBoard(1152921504606846976), BitBoard(2305843009213693952), BitBoard(4611686018427387904), BitBoard(9223372036854775808), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0)];
pub const PAWN_MOVES_BLACK: [BitBoard; 64] = [BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(1), BitBoard(2), BitBoard(4), BitBoard(8), BitBoard(16), BitBoard(32), BitBoard(64), BitBoard(128), BitBoard(256), BitBoard(512), BitBoard(1024), BitBoard(2048), BitBoard(4096), BitBoard(8192), BitBoard(16384), BitBoard(32768), BitBoard(65536), BitBoard(131072), BitBoard(262144), BitBoard(524288), BitBoard(1048576), BitBoard(2097152), BitBoard(4194304), BitBoard(8388608), BitBoard(16777216), BitBoard(33554432), BitBoard(67108864), BitBoard(134217728), BitBoard(268435456), BitBoard(536870912), BitBoard(1073741824), BitBoard(2147483648), BitBoard(4294967296), BitBoard(8589934592), BitBoard(17179869184), BitBoard(34359738368), BitBoard(68719476736), BitBoard(137438953472), BitBoard(274877906944), BitBoard(549755813888), BitBoard(1099511627776), BitBoard(2199023255552), BitBoard(4398046511104), BitBoard(8796093022208), BitBoard(17592186044416), BitBoard(35184372088832), BitBoard(70368744177664), BitBoard(140737488355328), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0)];
pub const KING_MOVES: [BitBoard; 64] = [BitBoard(770), BitBoard(1797), BitBoard(3594), BitBoard(7188), BitBoard(14376), BitBoard(28752), BitBoard(57504), BitBoard(49216), BitBoard(197123), BitBoard(460039), BitBoard(920078), BitBoard(1840156), BitBoard(3680312), BitBoard(7360624), BitBoard(14721248), BitBoard(12599488), BitBoard(50463488), BitBoard(117769984), BitBoard(235539968), BitBoard(471079936), BitBoard(942159872), BitBoard(1884319744), BitBoard(3768639488), BitBoard(3225468928), BitBoard(12918652928), BitBoard(30149115904), BitBoard(60298231808), BitBoard(120596463616), BitBoard(241192927232), BitBoard(482385854464), BitBoard(964771708928), BitBoard(825720045568), BitBoard(3307175149568), BitBoard(7718173671424), BitBoard(15436347342848), BitBoard(30872694685696), BitBoard(61745389371392), BitBoard(123490778742784), BitBoard(246981557485568), BitBoard(211384331665408), BitBoard(846636838289408), BitBoard(1975852459884544), BitBoard(3951704919769088), BitBoard(7903409839538176), BitBoard(15806819679076352), BitBoard(31613639358152704), BitBoard(63227278716305408), BitBoard(54114388906344448), BitBoard(216739030602088448), BitBoard(505818229730443264), BitBoard(1011636459460886528), BitBoard(2023272918921773056), BitBoard(4046545837843546112), BitBoard(8093091675687092224), BitBoard(16186183351374184448), BitBoard(13853283560024178688), BitBoard(144959613005987840), BitBoard(362258295026614272), BitBoard(724516590053228544), BitBoard(1449033180106457088), BitBoard(2898066360212914176), BitBoard(5796132720425828352), BitBoard(11592265440851656704), BitBoard(4665729213955833856)];

impl BitBoard {

    pub fn from_square(sqr: Square) -> BitBoard {
        BitBoard(1 << sqr.0)
    }

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
                let lss = Square(n.trailing_zeros() as usize);
                self.0 = *n ^ (1 << lss.0);
                Some(lss)
            }
        }
    }
}

impl Iterator for BitBoard {
    type Item = Square;

    fn next(&mut self) -> Option<Self::Item> {
        self.pop_lsb()
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
        self | BitBoard::from_square(rhs)
    }
}

impl BitXor<Square> for BitBoard {
    type Output = BitBoard;

    fn bitxor(self, rhs: Square) -> BitBoard {
        self ^ BitBoard::from_square(rhs)
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
        self & BitBoard::from_square(rhs)
    }
}