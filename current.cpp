#include <iostream>
#include <array>
#include <cassert>
#include <vector>
#include <algorithm>
#include <random>
#include <cmath>
#include <chrono>
#include <memory>
#include <functional>
#include <climits>
#include <map>
#include <utility>

#include "magicIndices.h"
#include "debugging_helpers.h"
#include "types.h"
#include "evaluate.h"

//Ranks and files. 
constexpr uint64_t FileABB = 0x0101010101010101ULL;
constexpr uint64_t FileBBB = FileABB << 1;
constexpr uint64_t FileCBB = FileABB << 2;
constexpr uint64_t FileDBB = FileABB << 3;
constexpr uint64_t FileEBB = FileABB << 4;
constexpr uint64_t FileFBB = FileABB << 5;
constexpr uint64_t FileGBB = FileABB << 6;
constexpr uint64_t FileHBB = FileABB << 7;

constexpr uint64_t Rank1BB = 0xFF;
constexpr uint64_t Rank2BB = Rank1BB << (8 * 1);
constexpr uint64_t Rank3BB = Rank1BB << (8 * 2);
constexpr uint64_t Rank4BB = Rank1BB << (8 * 3);
constexpr uint64_t Rank5BB = Rank1BB << (8 * 4);
constexpr uint64_t Rank6BB = Rank1BB << (8 * 5);
constexpr uint64_t Rank7BB = Rank1BB << (8 * 6);
constexpr uint64_t Rank8BB = Rank1BB << (8 * 7);

constexpr uint64_t DarkSquares = 0xAA55AA55AA55AA55ULL;
constexpr uint64_t LightSquares = ~DarkSquares;


namespace mailbox {
	PieceSpec board[64];	
}


move gameMoveList[100] = { }; //Store each move for each half ply made. 

constexpr move make_move(SQ from, SQ to) {
	return move((from << 6) + to);
}

constexpr SQ from_sq(move m) {
	return static_cast<SQ>((m >> 6) & 0x3F);
}

constexpr SQ to_sq(move m) {
  return static_cast<SQ>(m & 0x3F);
}

//Helper functions
constexpr bool in_bounds(int sq) 
{ return (sq >= a1 && sq <= h8 ) ? true : false; }

constexpr int makeEasy(int x, int y) 
{assert(x>=0 && x <=7); assert(y>=0 && y<=7); return y*8+x;}

inline void set_bit(uint64_t &bb, int x, int y) { bb |= 1ULL << makeEasy(x,y);}

inline void set_bit(uint64_t &bb, int square) { bb |= 1ULL << square;}

constexpr uint64_t get_bit(uint64_t bb, int x, int y) { return (bb >> makeEasy(x,y)) & 1ULL;}
constexpr uint64_t get_bit(uint64_t bb, int square)   {  return (bb >> square & 1ULL);}
		
inline void clear_bit(uint64_t &bb, int x, int y) { bb &= ~(1ULL << makeEasy(x,y));}
inline void clear_bit(uint64_t &bb, int square ) { bb &= ~(1ULL << square);} 

constexpr uint64_t cleared_bit(uint64_t bb, SQ sq ) { return bb & ~(1ULL << sq); }
  
constexpr int file_of(SQ s) { return int(s & 7); }
constexpr int rank_of(SQ s) { return int(s >> 3); }

//Shifts a bitboard, useful for when a shift might be negative (directions)
inline void shiftd(uint64_t &b, int shift) { shift < 0 ? b >>= std::abs(shift) : b <<= std::abs(shift); }

//Gets a bitboard of the file or rank of a square
constexpr uint64_t get_file(SQ sq) { return FileABB << file_of(sq); }
constexpr uint64_t get_rank(SQ sq) { return Rank1BB << rank_of(sq); }

//Get a bitboard representing a single square
constexpr uint64_t square_bb(SQ sq) { return 1ULL << sq; }

//Completely empty and completely full board, might be usefule for something IDK
uint64_t EmptyBB = 0ULL;
uint64_t FullBB = ~EmptyBB;

constexpr uint64_t (*s)(SQ sq) = &square_bb;

/*White Pieces*/        
constexpr uint64_t WP_BB = s(a2) | s(b2) | s(c2) | s(d2) | s(e2) | s(f2) | s(g2) | s(h2);
constexpr uint64_t WR_BB = s(a1) | s(h1);  
constexpr uint64_t WB_BB = s(c1) | s(f1);  
constexpr uint64_t WN_BB = s(b1) | s(g1);  
constexpr uint64_t WQ_BB = s(d1);  
constexpr uint64_t WK_BB = s(e1);  

//Black pieces
constexpr uint64_t BP_BB = s(a7) | s(b7) | s(c7) | s(d7) | s(e7) | s(f7) | s(g7) | s(h7);
constexpr uint64_t BR_BB = s(a8) | s(h8);                                                
constexpr uint64_t BB_BB = s(c8) | s(f8);                                                
constexpr uint64_t BN_BB = s(b8) | s(g8);                                                
constexpr uint64_t BQ_BB = s(d8);                                                        
constexpr uint64_t BK_BB = s(e8);                                                        

//White and black pieces exclusive, useful for captures
#define WhitePiecesBB (WP_BB | WR_BB | WB_BB | WN_BB | WQ_BB | WK_BB) 
#define BlackPiecesBB (BP_BB | BR_BB | BB_BB | BN_BB | BQ_BB | BK_BB)

//Occupied squares. The union of all pieces. 
#define OBB (WhitePiecesBB | BlackPiecesBB) 
//Empty Squares. The compliment of the occupied squares.
#define EBB ~OBB

//Current time: 137 nanoseconds
constexpr SQ ls1b(uint64_t bb) {
	return static_cast<SQ>(__builtin_ctzll(bb));
}

//returns the location of the LS1B and clears the bit from the btiboard
inline SQ pop_bit(uint64_t &bb)
{
	SQ index = SQ(__builtin_ctzll(bb));
	clear_bit(bb, index);
	return index;
} //side effect, totally dumb function. Fuck this function 

//returns a bitboard with the ls1b popped off. Current time: Roughly 160 nanoseconds
constexpr uint64_t popped_bitBB(const uint64_t bb) { return cleared_bit(bb, ls1b(bb)); }

//Returns a vector of the square locations of all the one bits in a bitboard
inline std::vector<SQ> get_indices(uint64_t bb)
{
	std::vector<SQ> indices;
	uint64_t temp = bb;
	while(temp)
		indices.push_back(pop_bit(temp));	
	return indices;
}

constexpr int count_bits(uint64_t bb){ return __builtin_popcountll(bb); }

template <typename T> constexpr int distance(T s1, T s2) { return std::abs(s2 - s1); }
constexpr int distance(SQ s1, SQ s2) 
{ return std::max(distance(rank_of(s1), rank_of(s2)), distance(file_of(s1), file_of(s2))); }

//Operator overloading for squares, we might want to add other data types to squares (directions, ints)
template<typename T>
constexpr SQ operator+(SQ a, T b) { return static_cast<SQ>(int(a) + int(b)); }

template<typename T>
constexpr SQ operator-(SQ a, T b) { return static_cast<SQ>(int(a) - int(b)); }

template<typename T>
SQ& operator+=(SQ& origin, T add){
	origin = static_cast<SQ>(origin + int(add));
	return origin;
}

template<typename T>
SQ& operator-=(SQ& origin, T add){
	origin = static_cast<SQ>(origin - add);
	return origin;
}

SQ& operator ++(SQ& origin) {
	origin = static_cast<SQ>(origin + 1);
	return origin;	
}

SQ& operator --(SQ& origin) {
	origin = static_cast<SQ>(origin - 1);
	return origin;	
}

//Overloaded for PieceTypes and PieceSpecs, iterating through these things will prove useful
PieceType& operator++(PieceType& origin){
	origin = static_cast<PieceType>(origin + 1);
	return origin;
}

PieceType& operator--(PieceType& origin){
	origin = static_cast<PieceType>(origin - 1);
	return origin;
}

PieceSpec& operator++(PieceSpec& origin){
	origin = static_cast<PieceSpec>(origin + 1);
	return origin;
}

PieceSpec& operator--(PieceSpec& origin){
	origin = static_cast<PieceSpec>(origin - 1);
	return origin;
}

Colour& operator++(Colour& origin) {
	origin = static_cast<Colour>(origin + 1);
	return origin;
}

move& operator++(move& origin) {
	origin = static_cast<move>(origin + 1);
	return origin;
}

template<typename T>
constexpr PieceType operator+(PieceType p, T q) {
	return static_cast<PieceType>(static_cast<int>(p) + static_cast<int>(q));
}

constexpr Colour operator!(Colour us) { 
	return static_cast<Colour>( !static_cast<bool>(us));
}

namespace mailbox {
	
	void update(){
		for(SQ s=a1; s <= h8; ++s)
		{
			if(get_bit(EBB,  s)) board[s] = NONE;
		
			if(get_bit(WP_BB,s)) board[s] = W_PAWN  ;
			if(get_bit(WR_BB,s)) board[s] = W_ROOK  ;
			if(get_bit(WB_BB,s)) board[s] = W_BISHOP;
			if(get_bit(WN_BB,s)) board[s] = W_KNIGHT;
			if(get_bit(WQ_BB,s)) board[s] = W_QUEEN ;
			if(get_bit(WK_BB,s)) board[s] = W_KING  ;

			if(get_bit(BP_BB,s)) board[s] = B_PAWN  ;
			if(get_bit(BR_BB,s)) board[s] = B_ROOK  ;
			if(get_bit(BB_BB,s)) board[s] = B_BISHOP;
			if(get_bit(BN_BB,s)) board[s] = B_KNIGHT;
			if(get_bit(BQ_BB,s)) board[s] = B_QUEEN ;
			if(get_bit(BK_BB,s)) board[s] = B_KING  ;
		}	
	}

	void print()
	{
		for(int y=7; y >=0; --y){
			std::cout << '\n';
			for(int x=0; x<8; ++x)
			{
				int i=makeEasy(x,y);
				switch(board[i])  
				{
					case NONE: std::cout << " . "; break;
			
					case W_PAWN  : std::cout << " P "; break;
					case W_ROOK  : std::cout << " R "; break;
					case W_BISHOP: std::cout << " B "; break;
					case W_KNIGHT: std::cout << " N "; break;
					case W_QUEEN : std::cout << " Q "; break;
					case W_KING  : std::cout << " K "; break;

					case B_PAWN  : std::cout << "*P "; break;
					case B_ROOK  : std::cout << "*R "; break;
					case B_BISHOP: std::cout << "*B "; break;
					case B_KNIGHT: std::cout << "*N "; break;
					case B_QUEEN : std::cout << "*Q "; break;
					case B_KING  : std::cout << "*K "; break;
				}
			}	
		}
		std::cout << '\n';
	}

	void make_move(move m)	{
		PieceSpec piece = board[from_sq(m)];
		board[from_sq(m)] = NONE;
		board[to_sq(m)] = piece;
	}

	void unmake_move(move m, PieceSpec captured) {
		PieceSpec piece = board[to_sq(m)];
		board[to_sq(m)] = captured;
		board[from_sq(m)] = piece;
	}
}

//Just prints the bits in a bitboard in board format. Might come in handy for debugging
void print(uint64_t bb) {
	for(int y=7; y >=0; --y){
		std::cout << '\n';
		for(int x=0; x < 8; ++x)
			std::cout << get_bit(bb, x,y) << "  ";
	}
	std::cout << '\n';
}

void print_uint16(uint16_t uint16) {
	for(int x=15; x >= 0; --x)
		std::cout << get_bit(uint16, x) << "  ";
	std::cout << '\n';
}

//Lookup tables
Magic mRook[64];
Magic mBishop[64];

uint64_t RookTable[64] [4096]; //For every possible Rook move
uint64_t BishopTable[64][4096]; //For every possible bishop move

uint64_t rays[64][8];
uint64_t rules[64][8];
uint64_t pawnAttacks[64][2];
uint64_t pawnRules[64][2];

Direction RookDirections[]   = { N, S, E, W };
Direction BishopDirections[] = { NE, NW, SE, SW };
Direction KnightDirections[] = { NNE, NNW, NWW, NEE, 
				 SSE, SSW, SWW, SEE };
Direction AllDirections[]  = { N,  S,  E,  W,
				 NE, NW, SE, SW};

//Compute the sliding pieces attacks. Accounts for blockers. 
//I stole this from stockfish, I like the idea though so I'm not going to change it
uint64_t slide(uint64_t occupied, Direction direction[], SQ sq)
{
	assert(in_bounds(sq));
	uint64_t rays = EmptyBB;
	//loop through directions
	for(int d = 0; d < 4; ++d) {
		//trace out a directional ray by:
		//adding a direction to the square,
		//when the combination is not equal to one, an overflow has occured
		for(SQ s = sq + direction[d]; in_bounds(s) && distance(s, s - direction[d]) == 1; s += direction[d]){
			uint64_t sqBB = square_bb(s);
			rays |= sqBB;
			if(sqBB & occupied ) break;			
		} 
	}
	return rays;
}

//Initialize rules for each piece for each square
void initRules()
{
	//Initialize the rules for each piece.
	//The array rules isn't concerned with things like captures, checks, pieces in the way etc...
	//Just the most basic rules
	for(SQ sq=a1; sq <= h8; ++sq)
	{
		//knights and kings have pretty specific masks
		//This function computes those masks		
		auto b = [&](Direction d)
		{ 
			uint64_t bb = 1ULL << sq;
		       	auto mask = [&]() { 
				switch(d) {
					case NNE: case SSE: case E: 
					       return ~FileABB;
					case NEE: case SEE: 
					       return ~(FileABB | FileBBB);
					case NNW: case SSW: case W:
					       return ~FileHBB;
					case NWW: case SWW: 
						return ~(FileHBB | FileGBB);
					case N : 
						return ~Rank1BB;
					case S : 
						return ~Rank8BB;
					case NE: 
						return ~(Rank1BB | FileABB);
					case NW: 
						return ~(Rank1BB | FileHBB);
					case SE: 
						return ~(Rank8BB | FileABB);
					default: 
						return ~(Rank8BB | FileHBB);		  
			 	} 
			};
			
			return d < 0 ? ((bb >> std::abs(d)) & mask()) : ((bb << std::abs(d)) & mask());
		};

		rules[sq][Knight] = b(NEE) | b(NNE) | b(NNW) | b(NWW) | b(SEE) | b(SSE) | b(SSW) | b(SWW);		
		rules[sq][King]   = b(N)   | b(S)   | b(E)   | b(W)   | b(NE)  | b(NW)  | b(SE)  | b(SW);

		//White Pawns 
		set_bit(pawnRules[sq][WHITE], sq + N);
		if(sq >= a2 && sq <= h2) set_bit(pawnRules[sq][WHITE], sq + 2*N);
		
		//White Pawn Attack
		set_bit(pawnAttacks[sq][WHITE], sq + NW);
		set_bit(pawnAttacks[sq][WHITE], sq + NE);

		//Black Pawns
		set_bit(pawnRules[sq][BLACK], sq + S);
		if(sq >= a7 && sq <= h7) set_bit(pawnRules[sq][BLACK], sq + 2*S);

		//Black Pawn Attack
		set_bit(pawnAttacks[sq][BLACK], sq + SW);
		set_bit(pawnAttacks[sq][BLACK], sq + SE);
	}
}

//initialize magic arrays. Magic numbers have been pre initialized and stored in "magicIndices.h"
//This function is practically identical to stockfishes init_magics. I want to change it to my own but I don't know how 
void init_magics(Magic magicArray[], Direction directions[], const uint64_t magicNumbers[] ) 
{
	uint64_t b = 0, size = 0, edges,  occupied[4096], reference[4096];
	int epoch[4096] = {0}, cnt = 0;
	for(SQ s = a1; s <= h8; ++s) 
	{
		 edges = ((Rank1BB | Rank8BB) & ~get_rank(s)) | ((FileABB | FileHBB) & ~get_file(s));
		//initialize magic structs members, instead of combuting magic numbers on the fly, 
		//magic numbers have been pre initialized and copy pasted into a seperate file
		Magic &m = magicArray[s];
		m.magic = magicNumbers[s];
		//The mask we are using are the unblocked directional rays (not including edges, we don't care about edges)
		m.mask = slide(EmptyBB, directions, s) & ~edges;

		if(directions == RookDirections)
			s == a1 ? m.ptr = RookTable[s] : m.ptr = magicArray[s-1].ptr + size;
		else 
			s == a1 ? m.ptr = BishopTable[s] : m.ptr = magicArray[s-1].ptr + size;

		//We shift the occupied squares multipied by the magic down by 64 - number of bits in the sliding piece
		//This ensures that we only keep the important bits, while saving a lot of space
		m.shift = 64 - count_bits(m.mask);	
		
		//(Apparently) carry-rippler adding allows for all important bits to be computed
		//The intersection of these bits and the relevent rays for our piece will be our blockers
		b = size = 0;
		do
		{
			occupied[size] = b;
			reference[size] = slide(b, directions, s) ;
			size++;
			//adding the 2's compliment to a number gives the compliment of the number, except for some extra bits.
			//The extra bits added to the compliment must be on one of the 'rays', thats the only place there is space. 
			//Therefore the intersection of this number and the mask will only contain those extra bits.
			//Which works perfectectly for simulating blockers.
			b = (b - m.mask) & m.mask;

		}while(b);	

		unsigned i;
		for(++cnt, i=0; i < size; ++i){
			unsigned idx = m.index(occupied[i]);
			//Keeping epoch array ensures that collisions don't occur without re computing
			if(epoch[idx] < cnt){
				epoch[idx] = cnt;
				m.ptr[idx] = reference[i];
			}
			else if(m.ptr[idx] != reference[i])
				break;
		}	
		
	}
}

Flag flag(const move m);

//Array for easy access to each bitboard for each piece
class MasterBoard
{	
	uint64_t board[2][6];
	PieceType last_capture;
	MoveList history;

	//Only to be called on the first ply 
	void init() 
	{
		board[WHITE][Pawn  ] = WP_BB;
		board[WHITE][Rook  ] = WR_BB;
		board[WHITE][Knight] = WN_BB;
		board[WHITE][Bishop] = WB_BB;
                board[WHITE][Queen ] = WQ_BB;
		board[WHITE][King  ] = WK_BB;
		board[BLACK][Pawn  ] = BP_BB;
		board[BLACK][Rook  ] = BR_BB;
	        board[BLACK][Knight] = BN_BB;
	        board[BLACK][Bishop] = BB_BB;
                board[BLACK][Queen ] = BQ_BB;
                board[BLACK][King  ] = BK_BB;
	}

	constexpr uint64_t our_squares(Colour us, uint64_t pBB, PieceType p = Pawn  ) const {
		return p <= King ? our_squares(us, pBB | board[us][p], p + 1) : pBB;
	}

	PieceType what_piece(move m, Colour us) const {
		for(PieceType piece = Pawn; piece <= King; ++piece) 
			if (board[us][piece] & square_bb(from_sq(m)))
				return piece;
		return None;
	}

	PieceType what_piece( SQ location, Colour us ) const {
		for(PieceType piece = Pawn; piece <= King; ++piece) 
			if (board[us][piece] & square_bb(location))
				return piece;
		return None;
	}
	
	explicit MasterBoard( const move m, Colour const us, PieceType const piece, MasterBoard const & mb ) {	  
		switch ( flag (m) ) {
			case capture:
				last_capture = what_piece ( to_sq ( m ), !us );
				break;
			case quiet:
				last_capture = None;
				break;
			default: break;
		}
		for(PieceType p = Pawn; p <= King; ++p) {
			this->board[us][p] = mb.board[us][p];
			this->board[!us][p] = mb.board[!us][p];
		}
		set_bit( this->board[us][piece], to_sq(m) );
		//Clear the captured piece from its bitboard
		if ( flag ( m ) == capture ) { 
			PieceType captured_p = what_piece( to_sq(m), !us );
			last_capture = captured_p;
			clear_bit ( this->board[!us][captured_p], to_sq(m) );
		}
		clear_bit( this->board[us][piece], from_sq(m) );	
	}

public:

	MasterBoard() : last_capture(None) { init(); } 

	MasterBoard n_make_move( move m, Colour us ) {
		return MasterBoard(m, us, what_piece(m,us), *this);		
	}

 	void make_move(move m, Colour us) {
		switch ( flag (m) ) {
			case capture:
				last_capture = what_piece ( to_sq ( m ), !us );
				break;
			case quiet:
				last_capture = None;
				break;
			default: break;
		}
		set_bit( board[us][what_piece(m, us)], to_sq(m) );
		clear_bit( board[us][what_piece(m, us)], from_sq(m) );
	}

	void unmake_move(move m, Colour us) {
		set_bit( board[us][what_piece(to_sq(m),us)], from_sq(m) );
		clear_bit( board[us][what_piece(from_sq(m),us)], to_sq(m) );
		if ( flag(m) == capture ) 
			set_bit( board[!us][last_capture], to_sq(m) );
	}

	PieceType last_captured_p() {
		return last_capture;
	}

	constexpr uint64_t our_guys(Colour us) const { 
		return our_squares(us, board[us][Pawn] );
	}

	constexpr uint64_t occupied() const { return our_guys(WHITE) | our_guys(BLACK) ; }

	uint64_t operator()(Colour us, PieceType piece) const { return board[us][piece]; }

}masterboard;

namespace mailbox {
	void update(MasterBoard masterboard){
		for (SQ s = a1; s <= h8; ++s ) board[s] = NONE;
		for (PieceType p = Pawn; p <= King; ++ p) {
			uint64_t whites = masterboard(WHITE, p), 
				 blacks = masterboard(BLACK, p);
			auto ps = [](const PieceType p, const Colour us) -> const PieceSpec {
					switch ( p ) {
						case Pawn :
							return us == WHITE ? W_PAWN : B_PAWN;
						case Rook :
							return us == WHITE ? W_ROOK : B_ROOK ;
						case Bishop :
							return us == WHITE ? W_BISHOP : B_BISHOP;
						case Knight :
							return us == WHITE ? W_KNIGHT : B_KNIGHT;
						case Queen :
							return us == WHITE ? W_QUEEN : B_QUEEN;
						case King :
							return us == WHITE ? W_KING : B_KING;
						default: 
							break;
					}
					return NONE;
				};
			while ( whites ) {
				SQ location = pop_bit(whites);
				board[location] = ps(p, WHITE);
			}
			while ( blacks ) {
				SQ location = pop_bit( blacks );
				board[location] = ps(p, BLACK);
			}
		}	
	}
}

constexpr move operator+(const move m, const Flag f) { 
	uint16_t tempm = static_cast<uint16_t>(m), tempf = static_cast<uint16_t>(f);
	return static_cast<move>(tempm + tempf); 
}

void operator+=(move &m, const Flag f) { 
	uint16_t tempm = static_cast<uint16_t>(m), tempf = static_cast<uint16_t>(f);
	m = static_cast<move>(tempm + tempf);
}

Flag flag(const move m) { 
	return static_cast<Flag>( m & queenPromoCapture );
}

inline uint64_t pawn_attacks(SQ origin, Colour us, MasterBoard board) {
	return pawnAttacks[origin][us] & board.our_guys(!us); 
}

inline uint64_t en_passant(const SQ origin, const Colour us, const int ply, MasterBoard board) {
	if(!ply) return 0ULL;	

	return ( (flag(gameMoveList[ply]) == doublePawnPush ) 
	    && (  board(!us, Pawn) & ( square_bb(origin + W) | square_bb(origin + E) ) ) ) ? 
		 square_bb(to_sq(gameMoveList[ply]) + (us ? S : N)) : 0ULL;
}


//Handles pawns. 
//TODO: promotions

inline uint64_t pawns( SQ origin, Flag f, Colour us, const int ply, MasterBoard board) {
	uint64_t movesBB = pawnRules[origin][us] ;
	//If the pawn is blocked on its first move
	if((movesBB & ~board.occupied()) < movesBB)
	        us == WHITE ? clear_bit(movesBB, origin + 2*N) : clear_bit(movesBB, origin + 2*S);

	return  f == capture 	      ? pawn_attacks(origin, us, board) :
		f == enPassantCapture ? en_passant(origin, us, ply, board) : movesBB; 
}

bool can_castle_kingside(SQ origin, Colour us, MasterBoard board) {
	return true;			
}

uint64_t castle(SQ origin, Colour us, MasterBoard board) {
	if (can_castle_kingside(origin, us, board)) {
		return 0;
	}
	return 0;
}

//Returns the set of all moves for a given piece on a given square 
//Current time: ~20 nanoseconds
//TODO: Support the rest of the flags.
//TODO: Get rid of emptySquares, we can get these with ~board.occupied()

inline uint64_t moves_set(PieceType piece, SQ origin, Flag f, Colour us, 
			  const int ply, MasterBoard board) 
{
	return piece == Rook   ? mRook  [origin].attacks(board.occupied()) :
	       piece == Bishop ? mBishop[origin].attacks(board.occupied()) :
   	       piece == Queen  ? mRook  [origin].attacks(board.occupied()) | mBishop[origin].attacks(board.occupied()) : 
	       piece == Pawn   ? pawns(origin, f, us, ply, board) : rules[origin][piece];
}


//Returns a flag, deduced from a move that has already been made.
//It is up to the user (future me) to ensure that the move made is possible given the board.
constexpr Flag flag(const move m, const PieceType piece, const uint64_t oppsBB) {
	int difference = static_cast<int>(from_sq(m) - to_sq(m));
	const bool goingNE = !(difference % NE), goingNW = !(difference % NW ); //SE = -NE, SW = -NW
	if(piece == Pawn ) {
		if(distance(from_sq(m), to_sq(m)) == 2)
			return doublePawnPush;
		//A pawn should never be allowed to move diagonally without intersecting an opposing piece
		else 
		if(!(square_bb(to_sq(m)) & oppsBB) && (goingNE || goingNW)) 
			return enPassantCapture;
		//TODO: promotions
		//TODO: Global movelist, ply index, each move made is stored, Check movesmade[ply-1] == doublePawnPush for EnPassant
	}
	
	//Making the assumption that the moves bitboard always returns capture squares when its supposed to
	if ( square_bb ( to_sq( m ) ) & oppsBB ) 
		return capture;

	//Should be impossible for the king to move more than two squares in any other situation.
	if(piece == King && distance(from_sq(m), to_sq(m)) > 1 ) 
		return distance(from_sq(m), to_sq(m)) == 2 ? kingCastle : queenCastle;
	
	return quiet;
}

//Returns the piece on the from square of the current board. Deduces the piece
//Roughly 50 ns
constexpr PieceType piece(SQ from, PieceSpec board[]) {
	if( board[from] == NONE )
		return None;
	else return 
	(  board[from] >= W_PAWN && board[from] <= W_QUEEN ) ? 
	static_cast<PieceType>( board[from] ) :
	static_cast<PieceType>(	static_cast<int>(board[from] ) - 6 );
}

//The least significant 1 valued bit from the range of available quiet moves
constexpr move ls1b_quiet_move( SQ from,  uint64_t quiets) {
	return make_move(from, ls1b(quiets)) + quiet;
}

//Same but capture 
constexpr move ls1b_capture_move(SQ from, uint64_t attacked ) {
	return make_move(from, ls1b(attacked)) + capture;
}

//Each bit in moveset is an available move for a given origin (from).
//Specifies whatever type the move is via a flag.
//Returns the move.

inline move ls1b_move(const SQ from, const uint64_t moveset, 
		      const Colour us, const PieceType piece,
		      MasterBoard board) 
{
	move m = make_move(from, ls1b(moveset));
	return m + flag( m, piece, board.our_guys(!us));
}

//Recursive function returning a movelist whose head points to the last available move
//possible for a given piece and board.
//Current time: roughly (timeof(push_front) + 50 )*pop_count(moveset) nanoseconds 

inline MoveList moves( const MoveList & movelist, const SQ from, 
		       const uint64_t moveset, const Colour us, 
		       const PieceType piece, MasterBoard board) 
{
	return moveset ? moves( 
		movelist.push_front( ls1b_move(from, moveset, us, piece, board) ), //Head now points to a new move
		from, popped_bitBB(moveset), us, piece, board
	) : movelist; 
}

inline uint64_t move_set_union(PieceType piece, SQ from, Colour us, 
			       const int ply, MasterBoard board) 
{
	return (moves_set(piece, from, quiet,   	 us, ply, board ) & ~board.occupied())
	     | (moves_set(piece, from, capture, 	 us, ply, board ) & board.our_guys(!us) )
	     | (moves_set(piece, from, enPassantCapture, us, ply, board ) & ~board.occupied());
}

//Returns a list of available, psuedo legal moves for a single board state. 
//Board -> list of possible moves
//Current time: ~22'000 nanoseconds 

MoveList generatePsuedoLegal( const Colour us, MoveList &movelist, 
			      const int ply, MasterBoard board)  
{
	for(PieceType piece = Pawn; piece <= King; ++piece) {
		uint64_t copy = board(us, piece);
		while(copy) {
			SQ from = pop_bit(copy);	
			uint64_t moveset = move_set_union(piece, from, us, ply, board);
			movelist = moves(movelist, from, moveset, us, piece, board);
		}
	}
	return movelist;
}

void init_all()
{
	initRules();
	init_magics(mRook, RookDirections, magicRook);
	init_magics(mBishop, BishopDirections, magicBishop);
}

namespace mailbox {
	void update_print(move m) {
		update();
		make_move(m);
		print();
	} 

	void make_unmake_print(move m, PieceSpec captured) {
		make_move(m);
		print();
		unmake_move(m, captured);
	}
}

void print_moves( MoveList const & movelist ) {
	mailbox::make_unmake_print(movelist.front(), NONE);
	std::cout << bullshit_function(flag(movelist.front())) << '\n';
	if(!movelist.popped_front().is_empty()) 
		print_moves(movelist.popped_front()); 
}

int evaluate(MasterBoard board, Colour us) {
	int score = 0;
	for( PieceType p = Pawn; p <= King; ++p ) {
		uint64_t copy = board(us,p);
		while(copy) {
			score += pieceScore(p, us, pop_bit(copy));
		}
	}
	return score;
}

int alpha_beta_max ( MasterBoard board, int depth, int alpha, int beta ); 
int alpha_beta_min ( MasterBoard board, int depth, int alpha, int beta );

int alpha_beta_max ( MasterBoard board, int depth, int alpha, int beta ) 
{
	MoveList movelist;
	Colour us = WHITE;
	if( depth == 0 ) 
		return (evaluate ( board, us ) + evaluate( board, !us ) );  
	int value = INT_MIN;
	for ( movelist = generatePsuedoLegal ( us, movelist, 1, board ); 
			!movelist.popped_front().is_empty(); 
			movelist = movelist.popped_front() ) 
	{
		board.make_move( movelist.front(), BLACK );
		int  n_value = alpha_beta_min ( 
				board, 
				depth-1, alpha, beta );
		board.unmake_move( movelist.front(), BLACK );
		if ( n_value > value ) 
			value = n_value;
		if ( n_value >= alpha ) 
			return value;
		if ( n_value > beta ) 
			beta = n_value;
	}
	return value;
}

int alpha_beta_min ( MasterBoard board, int depth, int alpha, int beta ) 
{ 
	MoveList movelist;
	Colour us = BLACK;
	if( depth == 0 ) 
		return -1*(evaluate ( board, us ) + evaluate( board, !us ) ); 
	int value = INT_MAX;
	for ( movelist = generatePsuedoLegal ( us, movelist, 1, board ); 
			!movelist.popped_front().is_empty(); 
			movelist = movelist.popped_front() ) 
	{
		board.make_move( movelist.front(), BLACK );
		int  n_value = alpha_beta_max ( 
				board, 
				depth-1, alpha, beta );
		board.unmake_move( movelist.front(), BLACK );
		if ( n_value < value ) 
			value = n_value;
		if ( n_value <= alpha ) 
			return value;
		if ( n_value < beta ) 
			beta = n_value;
	}	
	return value;
}

move best_move ( MasterBoard board, int ply, int depth, Colour us ) {
	std::map<const move, const int> move_score;
	MoveList moves;
	for ( moves = generatePsuedoLegal( us, moves, ply, board ); 
			!moves.popped_front().is_empty(); 
			moves = moves.popped_front() ) 
	{
		int score = us == WHITE ? alpha_beta_max(board.n_make_move(moves.front(), us), 
							depth, INT_MIN, INT_MAX)
			    		: alpha_beta_min(board.n_make_move(moves.front(), us), 
							depth, INT_MIN, INT_MAX);
		move_score.insert( { moves.front(), score } );
	}
	auto x = us == WHITE ? std::max_element ( move_score.begin(), move_score.end(), 
						[](const std::pair<move,int> &p1,  const std::pair<move,int> &p2) { 
							return p1.second < p2.second; } )
			     : std::min_element ( move_score.begin(), move_score.end(), 
						[](const std::pair<move,int> &p1,  const std::pair<move,int> &p2) { 
							return p1.second > p2.second; } );
	return x->first; 
}

using namespace std::chrono;

int main()
{
	init_all();
	mailbox::update();

	MasterBoard board;

	MoveList moves;
	
	Colour us = WHITE;

	for ( int i = 0; ; ++i ) {
		move m = best_move ( board, i, 6, us );
		board = board.n_make_move ( m, us );
		std::cout << bullshit_function ( flag ( m ) ) << '\n'
			  << bullshit_function ( board.last_captured_p () ) << '\n';
		mailbox::update( board );
		mailbox::print();
		us = !us;
	}
	
	return 0;
}
