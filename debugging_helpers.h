#ifndef DEBUGGING_HELPERS_H
#define DEBUGGING_HELPERS_H

#include "types.h"

//Bullshit function for debugging
std::string bullshit_function(PieceSpec piece)
{
	switch(piece) 
	{
		case W_PAWN:   return "W_PAWN:   ";
		case W_ROOK:   return "W_ROOK:   ";
		case W_KNIGHT: return "W_KNIGHT: ";
		case W_BISHOP: return "W_BISHOP: ";
		case W_QUEEN:  return "W_QUEEN:  ";
		case W_KING:   return "W_KING:   ";

		case B_PAWN:   return "B_PAWN:    ";
		case B_ROOK:   return "B_ROOK:    ";
		case B_KNIGHT: return "B_KNIGHT:  ";
		case B_BISHOP: return "B_BISHOP:  ";
		case B_QUEEN:  return "B_QUEEN:   ";
		case B_KING:   return "B_KING:    ";
		

		case NONE: break; 
	}
	return "NONE";
}

std::string bullshit_function(PieceType piece)
{
	switch(piece)
	{
		case Pawn:     return "Pawn:    ";
	        case Rook:     return "Rook:    ";
                case Knight:   return "Knight:  ";
		case Bishop:   return "Bishop:  ";
		case Queen:    return "Queen:   ";
		case King:     return "King:    ";
		
		case None: break;
	}
	return " NONE";
}


//God I hate this function so much 
std::string bullshit_function(Flag flag)
{
	switch(flag) 
	{
		case quiet 	       	:break;

		case doublePawnPush     :return "doublePawnPush     ";
		case kingCastle         :return "kingCastle         ";
		case queenCastle        :return "queenCastle        ";
		case capture    	:return "capture    	    ";
		case enPassantCapture   :return "enPassantCapture   ";
		case knightPromotion    :return "knightPromotion    ";
		case bishopPromotion    :return "bishopPromotion    ";
		case rookPromotion      :return "rookPromotion      ";
		case queenPromotion     :return "queenPromotion     ";
		case knightPromoCapture :return "knightPromoCapture ";
		case bishopPromoCapture :return "bishopPromoCapture ";
	        case rookPromoCapture   :return "rookPromoCapture   ";
                case queenPromoCapture  :return "queenPromoCapture  ";
	}
	return "quiet 	       ";
}

#endif
