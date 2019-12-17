int alpha_beta_max ( MasterBoard board, MoveList movelist,  int depth, int alpha, int beta ); 
int alpha_beta_min ( MasterBoard board, MoveList movelist, int depth, int alpha, int beta );

int alpha_beta_max ( MoveList movelist, int depth, int alpha, int beta ) 
{
	Colour us = WHITE;
	if( depth == 0 ) 
		return -evaluate ( board, us ); 
	int value = INT_MIN;
	for ( movelist = generatePsuedoLegal ( us, movelist, 1, board ); 
			!movelist.popped_front().is_empty(); 
			movelist = movelist.popped_front() ) 
	{
		int  n_value = alpha_beta_min ( movelist, depth-1, alpha, beta );
		if ( n_value > value ) 
			value = n_value;
		if ( n_value >= alpha ) 
			return value;
		if ( n_value > beta ) 
			beta = n_value;
	}
	return value;
}

int alpha_beta_min ( MoveList movelist, int depth, int alpha, int beta ) 
{ 
	Colour us = BLACK;
	if( depth == 0 ) { 
		return evaluate ( board, us ); 
		std::cout << "nice " << '\n';
	}
	int value = INT_MAX;
	for ( movelist = generatePsuedoLegal ( us, movelist, 1, board ); 
			!movelist.popped_front().is_empty(); 
			movelist = movelist.popped_front() ) 
	{
		int  n_value = alpha_beta_max ( movelist, depth-1, alpha, beta );
		if ( n_value < value ) 
			value = n_value;
		if ( n_value <= alpha ) 
			return value;
		if ( n_value < beta ) 
			beta = n_value;
	}
	return value;
}
