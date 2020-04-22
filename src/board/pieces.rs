use crate::board::board::positional::{Square, Rank, File, Move};
use std::path::Iter;
use crate::board::board::{Board, Color, Piece, PieceType};
use crate::board::pieces::PieceIterator::{KING, KNIGHT, ROOK, QUEEN, BISHOP, PAWN};

fn bounds_check(r: u8, f: u8) -> bool {
    f < 0 || r < 0 || f >= 8 || r >= 8
}

fn bounds_check_sqr(sqr: &Square) -> bool {
    let &Square(File(f), Rank(r)) = sqr;
    bounds_check(r, f)
}

#[derive(Debug)]
struct LineOfSightIterator<'a> {
    direction: (i8, i8),
    pos: Square,
    distance: u8,
    piece_color: Color,
    board: &'a Board,
    done: bool,
}

impl<'a> Iterator for LineOfSightIterator<'a> {
    type Item = Move;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None
        }
        let direction = (self.direction.0 * self.distance as i8, self.direction.1 * self.distance as i8);
        let target_square = self.pos + direction;
        self.distance += 1;
        if bounds_check_sqr(&target_square) {
            return None;
        }
        match self.board.get_piece(&target_square) {
            None => Some(Move::new(self.board, &self.pos, &target_square)),
            Some(Piece{color, piece_type}) => {
                if color == self.piece_color {
                    None
                }
                else {
                    self.done = true;
                    Some(Move::new(self.board, &self.pos, &target_square))
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct KingIterator<'a> {
    pos: Square,
    piece_color: Color,
    board: &'a Board,
    direction: u8,
}

impl<'a> Iterator for KingIterator<'a> {
    type Item = Move;

    fn next(&mut self) -> Option<Self::Item> {
        while self.direction < 8 {
            let dir = match self.direction {
                0 => (1, 1),
                1 => (1, 0),
                2 => (1, -1),
                3 => (0, 1),
                4 => (0, -1),
                5 => (-1, 1),
                6 => (-1, 0),
                7 => (-1, -1),
                _ => unreachable!()
            };
            self.direction += 1;
            let target_square = self.pos + dir;
            if bounds_check_sqr(&target_square) {
                continue;
            }
            return match self.board.get_piece(&target_square) {
                None => Some(Move::new(self.board, &self.pos, &target_square)),
                Some(Piece{color, piece_type}) => {
                    if color == self.piece_color {
                        continue;
                    }
                    else {
                        Some(Move::new(self.board, &self.pos, &target_square))
                    }
                }
            }
        }
        None
    }
}

impl<'a> KingIterator<'a> {
    pub fn new(pos: Square, board: &'a Board, piece_color: Color) -> PieceIterator<'a> {
        PieceIterator::KING(Self{pos, board, piece_color, direction: 0})
    }
}

#[derive(Debug)]
pub struct KnightIterator<'a> {
    pos: Square,
    piece_color: Color,
    board: &'a Board,
    direction: u8,
}

impl<'a> Iterator for KnightIterator<'a> {
    type Item = Move;

    fn next(&mut self) -> Option<Self::Item> {
        while self.direction < 8 {
            let dir = match self.direction {
                0 => (1,2),
                1 => (2,1),
                2 => (2,-1),
                3 => (1, -2),
                4 => (-1, -2),
                5 => (-2, -1),
                6 => (-2, 1),
                7 => (-1, 2),
                _ => unreachable!()
            };
            self.direction += 1;
            let target_square = self.pos + dir;
            if bounds_check_sqr(&target_square) {
                continue;
            }
            match self.board.get_piece(&target_square) {
                None => {
                    return Some(Move::new(self.board, &self.pos, &target_square))
                },
                Some(Piece{color, piece_type}) => {
                    if color == self.piece_color {
                        continue;
                    }
                    else {
                        return Some(Move::new(self.board, &self.pos, &target_square));
                    }
                }
            }
        }
        None
    }
}

impl<'a> KnightIterator<'a> {
    pub(crate) fn new(pos: Square, board: &'a Board, piece_color: Color) -> PieceIterator<'a> {
        PieceIterator::KNIGHT(Self{pos, board, piece_color, direction: 0})
    }
}

#[derive(Debug)]
pub struct RookIterator<'a> {
    pos: Square,
    piece_color: Color,
    board: &'a Board,
    direction: u8,
    current_iterator: LineOfSightIterator<'a>
}

impl<'a> RookIterator<'a> {
    pub(crate) fn new(pos: Square, board: &'a Board, piece_color: Color) -> PieceIterator<'a> {
        let fake_iter = LineOfSightIterator{direction: (0,0), pos, board, piece_color, done: true, distance: 0 };
        PieceIterator::ROOK(Self{pos, board, piece_color, direction: 0, current_iterator: fake_iter})
    }
}

impl<'a> Iterator for RookIterator<'a> {
    type Item = Move;

    fn next(&mut self) -> Option<Self::Item> {

        let mut next_move = self.current_iterator.next();
        match next_move {
            Some(m) => return Some(m),
            None => {
                let dir = match self.direction {
                    0 => (0, 1),
                    1 => (0, -1),
                    2 => (1, 0),
                    3 => (-1, 0),
                    _ => return None,
                };
                self.direction += 1;
                self.current_iterator = LineOfSightIterator{direction: dir, pos: self.pos, board: self.board, piece_color: self.piece_color, done: false, distance: 1 };
                self.next()
            }
        }
    }
}

#[derive(Debug)]
pub struct BishopIterator<'a> {
    pos: Square,
    piece_color: Color,
    board: &'a Board,
    direction: u8,
    current_iterator: LineOfSightIterator<'a>
}

impl<'a> BishopIterator<'a> {
    pub(crate) fn new(pos: Square, board: &'a Board, piece_color: Color) -> PieceIterator<'a> {
        let fake_iter = LineOfSightIterator{direction: (0,0), pos, board, piece_color, done: true, distance: 0 };
        PieceIterator::BISHOP(Self{pos, board, piece_color, direction: 0, current_iterator: fake_iter})
    }
}

impl<'a> Iterator for BishopIterator<'a> {
    type Item = Move;

    fn next(&mut self) -> Option<Self::Item> {

        let mut next_move = self.current_iterator.next();
        match next_move {
            Some(m) => return Some(m),
            None => {
                let dir = match self.direction {
                    0 => (1, -1),
                    1 => (1, 1),
                    2 => (-1, 1),
                    3 => (-1, -1),
                    _ => return None,
                };
                self.direction += 1;
                self.current_iterator = LineOfSightIterator{direction: dir, pos: self.pos, board: self.board, piece_color: self.piece_color, done: false, distance: 1 };
                self.next()
            }
        }
    }
}

#[derive(Debug)]
pub struct QueenIterator<'a> {
    pos: Square,
    piece_color: Color,
    board: &'a Board,
    direction: u8,
    current_iterator: LineOfSightIterator<'a>
}

impl<'a> QueenIterator<'a> {
    pub(crate) fn new(pos: Square, board: &'a Board, piece_color: Color) -> PieceIterator<'a> {
        let fake_iter = LineOfSightIterator{direction: (0,0), pos, board, piece_color, done: true, distance: 0 };
        PieceIterator::QUEEN(Self{pos, board, piece_color, direction: 0, current_iterator: fake_iter})
    }
}

impl<'a> Iterator for QueenIterator<'a> {
    type Item = Move;

    fn next(&mut self) -> Option<Self::Item> {

        let mut next_move = self.current_iterator.next();
        match next_move {
            Some(m) => return Some(m),
            None => {
                let dir = match self.direction {
                    0 => (1, -1),
                    1 => (1, 1),
                    2 => (-1, 1),
                    3 => (-1, -1),
                    4 => (0, -1),
                    5 => (0, 1),
                    6 => (1, 0),
                    7 => (-1, 0),
                    _ => return None,
                };
                self.direction += 1;
                self.current_iterator = LineOfSightIterator{direction: dir, pos: self.pos, board: self.board, piece_color: self.piece_color, done: false, distance: 1 };
                self.next()
            }
        }
    }
}

#[derive(Debug)]
pub enum PieceIterator<'a> {
    KNIGHT(KnightIterator<'a>),
    KING(KingIterator<'a>),
    PAWN(PawnIterator<'a>),
    ROOK(RookIterator<'a>),
    QUEEN(QueenIterator<'a>),
    BISHOP(BishopIterator<'a>),
    None
}

impl<'a> Iterator for PieceIterator<'a> {
    type Item = Move;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            PieceIterator::KNIGHT(iter) => iter.next(),
            PieceIterator::KING(iter) => iter.next(),
            PieceIterator::PAWN(iter) => iter.next(),
            PieceIterator::ROOK(iter) => iter.next(),
            PieceIterator::QUEEN(iter) => iter.next(),
            PieceIterator::BISHOP(iter) => iter.next(),
            PieceIterator::None=> Option::None
        }
    }
}

#[derive(Debug)]
pub struct PawnIterator<'a> {
    pos: Square,
    piece_color: Color,
    board: &'a Board,
    index: u8,
}

impl<'a> Iterator for PawnIterator<'a> {
    type Item = Move;

    fn next(&mut self) -> Option<Self::Item> {
        let (unmoved, dir, can_promote) = match self.piece_color {
            Color::WHITE => ((self.pos.1).0 == 1, 1, (self.pos.1).0 == 6),
            Color::BLACK => ((self.pos.1).0 == 6, -1, (self.pos.1).0 == 1),
        };
        let last_index = self.index;
        if can_promote {
            if self.index >= 12 {
                return None
            }
            let dir = ((last_index as i8 / 4) - 1, dir);
            let target_position = self.pos + dir;
            if bounds_check_sqr(&target_position) {
                self.index += 4;
                return self.next();
            }
            let piece_type = match last_index % 4 {
                0 => PieceType::QUEEN,
                1 => PieceType::ROOK,
                2 => PieceType::KNIGHT,
                3 => PieceType::BISHOP,
                _ => unreachable!()
            };
            if dir.0 != 0 {
                if let Some(Piece{piece_type: _, color} ) = self.board.get_piece(&target_position) {
                    if color != self.piece_color {
                        self.index += 1;
                        return Some(Move::new_promotion(self.board, &self.pos, &target_position, &piece_type))
                    }
                }
                self.index += 4;
                return self.next();
            }
            else {
                if let None = self.board.get_piece(&target_position) {
                    self.index += 1;
                    return Some(Move::new_promotion(self.board, &self.pos, &target_position, &piece_type))
                }
                else {
                    self.index += 4;
                    return self.next();
                }
            }
        }
        else {
            if self.index >= 4 {
                return None
            }
            self.index += 1;
            match last_index {
                0 => {
                    return if let None = self.board.get_piece(&(self.pos + (0, dir))) {
                        Some(Move::new(self.board, &self.pos, &(self.pos + (0, dir))))
                    }
                    else {
                        self.index += 1;//Skip the next one too
                        self.next()
                    }
                },
                1 => {
                    if !unmoved {
                        return self.next();
                    }
                    return if let None = self.board.get_piece(&(self.pos + (0, dir+dir))) {
                        Some(Move::new(self.board, &self.pos, &(self.pos + (0, dir+dir))))
                    }
                    else {
                        self.next()
                    }
                },
                2 | 3 => {
                    let target_position = self.pos + ((last_index as i8 - 3) * 2 + 1, dir);
                    if bounds_check_sqr(&target_position) {
                        return self.next();
                    }
                    if let Some(ep_square) = self.board.en_passant_square {
                        if ep_square == target_position {
                            return Some(Move::new(self.board, &self.pos, &target_position))
                        }
                    }
                    if let Some(Piece{piece_type, color} ) = self.board.get_piece(&target_position) {
                        if color != self.piece_color {
                            return Some(Move::new(self.board, &self.pos, &target_position))
                        }
                    }
                    return self.next()
                },
                _ => return None,
            };
        }
        None
    }
}

impl<'a> PawnIterator<'a> {
    pub(crate) fn new(pos: Square, board: &'a Board, piece_color: Color) -> PieceIterator<'a> {
        PieceIterator::PAWN(PawnIterator {pos, board, index: 0, piece_color})
    }
}

#[cfg(test)]
mod pawn_test {
    use crate::board::board::{Board, PieceType};
    use crate::board::board::Color;
    use crate::board::pieces::PawnIterator;
    use crate::board::board::positional::{Square, Move};
    use crate::board::board::positional::MoveAction::Promotion;

    #[test]
    fn pawn_no_capture() {
        let mut board = Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1".to_string()).unwrap();
        let mut iter = PawnIterator::new(Square::new(0, 1), &board,Color::WHITE);
        assert!(iter.next().is_some());
        assert!(iter.next().is_some());
        assert!(iter.next().is_none());
    }

    #[test]
    fn pawn_one_sqr() {
        let mut board = Board::from_fen("rnbqkbnr/pppppppp/8/8/p7/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1".to_string()).unwrap();
        let mut iter = PawnIterator::new(Square::new(0, 1), &board,Color::WHITE);
        assert!(iter.next().is_some());
        assert!(iter.next().is_none());
    }

    #[test]
    fn pawn_one_sqr_capture() {
        let mut board = Board::from_fen("rnbqkbnr/pppppppp/8/8/1p6/p1p5/PPPPPPPP/RNBQKBNR w KQkq - 0 1".to_string()).unwrap();
        let mut iter = PawnIterator::new(Square::new(1, 1), &board,Color::WHITE);
        assert!(iter.next().is_some());
        assert!(iter.next().is_some());
        assert!(iter.next().is_some());
        assert!(iter.next().is_none());
    }

    #[test]
    fn pawn_two_sqr_capture() {
        let mut board = Board::from_fen("rnbqkbnr/pppppppp/8/8/8/p1p5/PPPPPPPP/RNBQKBNR w KQkq - 0 1".to_string()).unwrap();
        let mut iter = PawnIterator::new(Square::new(1, 1), &board,Color::WHITE);
        assert!(iter.next().is_some());
        assert!(iter.next().is_some());
        assert!(iter.next().is_some());
        assert!(iter.next().is_some());
        assert!(iter.next().is_none());
    }

    #[test]
    fn pawn_promote_white() {
        let mut board = Board::from_fen("8/P7/8/kK6/8/8/8/8 w - - 0 1".to_string()).unwrap();
        println!("{}", board);
        let mut iter = PawnIterator::new(Square::new(0, 6), &board,Color::WHITE);
        if let Some(Move{from: _, to: _, move_type: Promotion(piece, None)}) = iter.next() {
            assert_eq!(piece.piece_type, PieceType::QUEEN);
        }
        else {
            assert!(false);
        }
        if let Some(Move{from: _, to: _, move_type: Promotion(piece, None)}) = iter.next() {
            assert_eq!(piece.piece_type, PieceType::ROOK);
        }
        else {
            assert!(false);
        }
        if let Some(Move{from: _, to: _, move_type: Promotion(piece, None)}) = iter.next() {
            assert_eq!(piece.piece_type, PieceType::KNIGHT);
        }
        else {
            assert!(false);
        }
        if let Some(Move{from: _, to: _, move_type: Promotion(piece, None)}) = iter.next() {
            assert_eq!(piece.piece_type, PieceType::BISHOP);
        }
        else {
            assert!(false);
        }
        assert!(iter.next().is_none());
    }

    #[test]
    fn pawn_promote_black() {
        let mut board = Board::from_fen("8/8/8/kK6/8/8/p7/8 w - - 0 1".to_string()).unwrap();
        let mut iter = PawnIterator::new(Square::new(0, 1), &board,Color::BLACK);
        if let Some(Move{from: _, to: _, move_type: Promotion(piece, None)}) = iter.next() {
            assert_eq!(piece.piece_type, PieceType::QUEEN);
        }
        else {
            assert!(false);
        }
        if let Some(Move{from: _, to: _, move_type: Promotion(piece, None)}) = iter.next() {
            assert_eq!(piece.piece_type, PieceType::ROOK);
        }
        else {
            assert!(false);
        }
        if let Some(Move{from: _, to: _, move_type: Promotion(piece, None)}) = iter.next() {
            assert_eq!(piece.piece_type, PieceType::KNIGHT);
        }
        else {
            assert!(false);
        }
        if let Some(Move{from: _, to: _, move_type: Promotion(piece, None)}) = iter.next() {
            assert_eq!(piece.piece_type, PieceType::BISHOP);
        }
        else {
            assert!(false);
        }
        assert!(iter.next().is_none());
    }

    #[test]
    fn pawn_promote_no_capture_black() {
        let mut board = Board::from_fen("8/8/8/kK6/8/8/1p6/1Q6 b - - 0 1".to_string()).unwrap();
        let mut iter = PawnIterator::new(Square::new(1, 1), &board,Color::BLACK);
        assert!(iter.next().is_none());
    }

    #[test]
    fn pawn_promote_capture_black() {
        let mut board = Board::from_fen("8/8/8/kK6/8/8/1p6/RQR5 b - - 0 1".to_string()).unwrap();
        let mut iter = PawnIterator::new(Square::new(1, 1), &board,Color::BLACK);
        assert!(iter.next().is_some());
        assert!(iter.next().is_some());
        assert!(iter.next().is_some());
        assert!(iter.next().is_some());
        assert!(iter.next().is_some());
        assert!(iter.next().is_some());
        assert!(iter.next().is_some());
        assert!(iter.next().is_some());
        assert!(iter.next().is_none());
    }

    #[test]
    fn pawn_promote_no_capture_white() {
        let mut board = Board::from_fen("1q6/1P6/8/kK6/8/8/8/8 w - - 0 1".to_string()).unwrap();
        let mut iter = PawnIterator::new(Square::new(1, 6), &board,Color::WHITE);
        assert!(iter.next().is_none());
    }

    #[test]
    fn pawn_promote_capture_white() {
        let mut board = Board::from_fen("rqr5/1P6/8/kK6/8/8/8/8 w - - 0 1".to_string()).unwrap();
        let mut iter = PawnIterator::new(Square::new(1, 6), &board,Color::WHITE);
        assert!(iter.next().is_some());
        assert!(iter.next().is_some());
        assert!(iter.next().is_some());
        assert!(iter.next().is_some());
        assert!(iter.next().is_some());
        assert!(iter.next().is_some());
        assert!(iter.next().is_some());
        assert!(iter.next().is_some());
        assert!(iter.next().is_none());
    }
}


#[cfg(test)]
mod king_test {
    use crate::board::board::{Board, PieceType};
    use crate::board::board::Color;
    use crate::board::pieces::{PawnIterator, KingIterator};
    use crate::board::board::positional::{Square, Move};
    use crate::board::board::positional::MoveAction::Promotion;

    #[test]
    fn king_all_moves() {
        let mut board = Board::from_fen("8/8/8/1k2K3/8/8/8/8 w - - 0 1".to_string()).unwrap();
        let mut iter = KingIterator::new(Square::new(4, 4), &board,Color::WHITE);
        assert!(iter.next().is_some());
        assert!(iter.next().is_some());
        assert!(iter.next().is_some());
        assert!(iter.next().is_some());
        assert!(iter.next().is_some());
        assert!(iter.next().is_some());
        assert!(iter.next().is_some());
        assert!(iter.next().is_some());
        assert!(iter.next().is_none());
    }

    #[test]
    fn king_captures_back() {
        let mut board = Board::from_fen("8/8/3PPP2/1k1PKP2/3ppp2/8/8/8 w - - 0 1".to_string()).unwrap();
        let mut iter = KingIterator::new(Square::new(4, 4), &board,Color::WHITE);
        println!("{}", board);
        assert!(iter.next().is_some());
        assert!(iter.next().is_some());
        assert!(iter.next().is_some());
        assert!(iter.next().is_none());
    }
}