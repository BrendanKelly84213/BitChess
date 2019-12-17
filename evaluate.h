#include "types.h"

//Material strength

const int P_Str = 100;
const int N_Str = 320;
const int B_Str = 330;
const int R_Str = 500;
const int Q_Str = 900;
const int K_Str = 20000;

//Piece Position Strength
//The goodness of a position is dependent on the piece and the pieces position
//We'll just hard code relative strength indices for each piece and position
//Black table is white table flipped upside down

int pawn_table[]= { 
 0,  0,  0,  0,  0,  0,  0,  0,
50, 50, 50, 50, 50, 50, 50, 50,
10, 10, 20, 30, 30, 20, 10, 10,
 5,  5, 10, 25, 25, 10,  5,  5,
 0,  0,  0, 20, 20,  0,  0,  0,
 5, -5,-10,  0,  0,-10, -5,  5,
 5, 10, 10,-20,-20, 10, 10,  5,
 0,  0,  0,  0,  0,  0,  0,  0
};

int rook_table[]= { 
 0,  0,  0,  0,  0,  0,  0,  0,
 5, 10, 10, 10, 10, 10, 10,  5,
-5,  0,  0,  0,  0,  0,  0, -5,
-5,  0,  0,  0,  0,  0,  0, -5,
-5,  0,  0,  0,  0,  0,  0, -5,
-5,  0,  0,  0,  0,  0,  0, -5,
-5,  0,  0,  0,  0,  0,  0, -5,
 0,  0,  0,  5,  5,  0,  0,  0
  
};


int bishop_table[]= {
-20,-10,-10,-10,-10,-10,-10,-20,
-10,  0,  0,  0,  0,  0,  0,-10,
-10,  0,  5, 10, 10,  5,  0,-10,
-10,  5,  5, 10, 10,  5,  5,-10,
-10,  0, 10, 10, 10, 10,  0,-10,
-10, 10, 10, 10, 10, 10, 10,-10,
-10,  5,  0,  0,  0,  0,  5,-10,
-20,-10,-10,-10,-10,-10,-10,-20,
};


int knight_table[]={
-50,-40,-30,-30,-30,-30,-40,-50,
-40,-20,  0,  0,  0,  0,-20,-40,
-30,  0, 10, 15, 15, 10,  0,-30,
-30,  5, 15, 20, 20, 15,  5,-30,
-30,  0, 15, 20, 20, 15,  0,-30,
-30,  5, 10, 15, 15, 10,  5,-30,
-40,-20,  0,  5,  5,  0,-20,-40,
-50,-40,-30,-30,-30,-30,-40,-50,

 };

int king_table[]={
-30,-40,-40,-50,-50,-40,-40,-30,
-30,-40,-40,-50,-50,-40,-40,-30,
-30,-40,-40,-50,-50,-40,-40,-30,
-30,-40,-40,-50,-50,-40,-40,-30,
-20,-30,-30,-40,-40,-30,-30,-20,
-10,-20,-20,-20,-20,-20,-20,-10,
 20, 20,  0,  0,  0,  0, 20, 20,
 20, 30, 10,  0,  0, 10, 30, 20

};

int queen_table[]={
-20,-10,-10, -5, -5,-10,-10,-20,
-10,  0,  0,  0,  0,  0,  0,-10,
-10,  0,  5,  5,  5,  5,  0,-10,
 -5,  0,  5,  5,  5,  5,  0, -5,
  0,  0,  5,  5,  5,  5,  0, -5,
-10,  5,  5,  5,  5,  5,  0,-10,
-10,  0,  5,  0,  0,  0,  0,-10,
-20,-10,-10, -5, -5,-10,-10,-20
};

int score(PieceType piece, Colour us, SQ s, int score_table[], int piece_str) {
	return us == WHITE ? score_table[h8-s] * piece_str : score_table[s] * piece_str;
}

int pieceScore(PieceType piece, Colour us, SQ s) {
	switch(piece) {
		case Pawn :    return score(piece, us, s, pawn_table,   P_Str);
		case Rook :    return score(piece, us, s, rook_table,   R_Str);
		case Bishop :  return score(piece, us, s, bishop_table, B_Str);
		case Knight :  return score(piece, us, s, knight_table, N_Str);
		case Queen :   return score(piece, us, s, queen_table,  Q_Str);
		case King :    return score(piece, us, s, king_table,   K_Str);
		case None: return 0;
	}
	return 0;
}

