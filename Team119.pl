
main:- write('Welcome to Pro-Wordle') , nl, 
			write('-----------------------'), nl , 
			build_kb , nl, 
			write('Done building the words database ....'),nl,
			play.

build_kb:- write('Please enter a word and its category on seperate lines:'), 
			read(X), 
			(X\=done,
			read(Y), nl,
			assert(word(X,Y)),
			build_kb	);
			(X = done , !).	


is_category(C):- word(A,C).

categories(L):- setof(C,is_category(C),L).

available_length(L):- word(W,_) , atom_chars(W,List), length(List,L).

pick_word(W,L,C):- word(W,C) , string_length(W,L).

correct_letters([],L2,[]).

correct_letters([H|T1],L2,[H1|TL]):-(\+member(H,T1)),(member(H,L2)),H1=H,correct_letters(T1,L2,TL).

correct_letters([H|T1],L2,CL):- ((\+ member(H,L2));(member(H,T1),member(H,L2))),correct_letters(T1,L2,CL).

correct_positions([],[],[]).

correct_positions([],[_|_],[]).

correct_positions([_|_],[],[]).

correct_positions([H|T1],[H|T2],[H|T3]):-
	correct_positions(T1,T2,T3).

correct_positions([H|T1],[H2|T2],PL):-	
	H\=H2,correct_positions(T1,T2,PL).

choose_category(C):- read(X),is_category(X) ,C = X; 
				  (write('This category does not exist.') , nl ,
				  write('Choose a category: ') , nl ,
				  choose_category(C)).

choose_length(L,C):- read(X),available_length(X),word(W,C),string_length(W,X) ,L = X
				; write('There are no words of this length.') ,nl, write('Choose a length:'),
				nl , choose_length(L,C).


play:- write('The available categories are: '),categories(N),write(N),nl,
	   write('Choose a category: '),nl,choose_category(C),nl,
	   write('Choose a length: '),nl,choose_length(L,C),
	   write('Game started. You have ') , Z is L + 1 , write(Z), write(' guesses.') ,nl,nl, Y is Z -1 ,
	   pick_word(W,L,C),write('Enter a word composed of '),write(L), write(' letters:'), read(X),playHelper(Y,W,Z,X).

playHelper(L,W,1,X):- X\== W ,write('You Lost!'), !.

playHelper(L,W,N,X):- X == W , write('You Won') , !.

playHelper(L,W,N,X):- X\==W, string_length(X,L), atom_chars(W,L1) , atom_chars(X,L2)
				  , correct_letters(L1,L2,CL)
				  , write('Correct letters are: ') , write(CL) ,nl
				  ,correct_positions(L1,L2,PL),write('Correct letters in correct positions are: '),write(PL), N1 is N - 1
				  ,nl, write('Remaining guesses are ') , write(N1),nl,nl
				  ,write('Enter a word composed of ') , write(L) , write(' letters:') 
				  ,nl, read(X1),nl ,playHelper(L,W,N1,X1).


playHelper(L,W,N,X):- \+ string_length(X,L), write('Word is not composed of ') , 
				  write(L) , write(' letters. Try again.'), nl , write('Remaining guesses are ') 
				  , write(N),nl,nl,write('Enter a word composed of ') , write(L) , write(' letters:') 
				  ,nl,nl, read(X1), nl,playHelper(L,W,N,X1).

		  			    
















