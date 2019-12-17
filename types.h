#ifndef TYPES_H
#define TYPES_H
//Enums, structs and type definitions for the program

enum PieceSpec { W_PAWN, W_ROOK, W_BISHOP, W_KNIGHT, W_QUEEN, W_KING,
		 B_PAWN, B_ROOK, B_BISHOP, B_KNIGHT, B_QUEEN, B_KING, NONE=-1}; //For zobrist hashing

enum PieceType {  Pawn, Rook, Bishop, Knight, Queen, King,  None=-1 }; //For moves 

enum Flag : uint16_t
{ 
	quiet 	       	   = 0, 
	doublePawnPush     = 1  << 12,     			
	kingCastle         = 2  << 12, 
	queenCastle        = 3  << 12,   			
	capture    	   = 4  << 12, 
	enPassantCapture   = 5  << 12, 			
	knightPromotion    = 8  << 12, 
	bishopPromotion    = 9  << 12,
	rookPromotion      = 10 << 12, 
	queenPromotion     = 11 << 12, 
	knightPromoCapture = 12 << 12, 
	bishopPromoCapture = 13 << 12,
	rookPromoCapture   = 14 << 12, 
	queenPromoCapture  = 15 << 12
};

enum { North, South, East, West, NorthEast, NorthWest, SouthEast, SouthWest };

enum { Rank, File, PosDiag, NegDiag };

enum Colour { WHITE, BLACK };

enum File { FileA, FileB, FileC, FileD, FileE, FileF, FileG, FileH };
enum Rank { Rank1, Rank2, Rank3, Rank4, Rank5, Rank6, Rank7, Rank8 };

enum SQ : int
{
	a1, b1, c1, d1, e1, f1, g1, h1,
	a2, b2, c2, d2, e2, f2, g2, h2, 
	a3, b3, c3, d3, e3, f3, g3, h3,
	a4, b4, c4, d4, e4, f4, g4, h4, 
	a5, b5, c5, d5, e5, f5, g5, h5,
	a6, b6, c6, d6, e6, f6, g6, h6,
	a7, b7, c7, d7, e7, f7, g7, h7,
	a8, b8, c8, d8, e8, f8, g8, h8
};

enum Direction : int { 
	N   =    8, S   =   -8, E   =    1,  W   =   -1, //Basic compass rose 
        NE  =  N+E, NW  =  N+W, SE  =  S+E,  SW  =  S+W, 
        NEE = NE+E, NNE = N+NE, NNW =  N+NW, NWW = NW+W, //Knight Compass rose		
        SWW = SW+W, SSW = S+SW, SSE =  S+SE, SEE = SE+E
}; 

struct Magic
{
	uint64_t mask;  //Sliding piece moves
	uint64_t magic; //Magic number for referencing a move
	uint64_t *ptr;  //Pointer to magic number
	unsigned shift; //Shift necessary for size

	unsigned index(uint64_t occupied) const 
	{ return unsigned(((occupied & mask) * magic) >> shift); }

	//Get attacks with a specific occupied bitboard (mostly for debugging)
	uint64_t attacks(uint64_t occupied) const
	{ return ptr[index(occupied)]; }
};

enum move : uint16_t { MOVE_NONE, MOVE_NULL=65 };

//Node struct for a linked list approach to the movelist
struct MoveNode {
	move mv;
	std::shared_ptr<const MoveNode> next;
	
	MoveNode() : mv(MOVE_NONE), next(NULL) { }

	MoveNode(const move m) : mv(m), next(NULL) { }

	MoveNode( const move m, std::shared_ptr<const MoveNode> const & n) 
		: mv(m), next(n) { }

	MoveNode(std::shared_ptr<const MoveNode> const & n) 
		: mv(MOVE_NONE), next(n) { }
};

//Persistent Linked list.
class MoveList {
	std::shared_ptr<const MoveNode> head;

	explicit MoveList(std::shared_ptr<const MoveNode> n ) 
		: head(n) { }

public:
	MoveList() { }

	MoveList(const move m, MoveList const & n) 
		: head( std::make_shared<MoveNode>( m, n.head ) ) {  }
	
	
	MoveList push_front(move m) const {
		return MoveList(m, *this);
	}

	bool is_empty() const { return !head ; }

	move front() const { 
		assert(!is_empty());
		return head->mv; 
	}

	MoveList popped_front() const { 
		assert(!is_empty());
		return MoveList(head->next); 
	}
};

#endif
