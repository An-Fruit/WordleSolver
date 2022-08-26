:- use_module(library(readutil)).
:- use_module(library(lists)).

play :-
    N = 5,
    File = "wordle.txt",
    read_words(File,N,Words),
    guess(Words,['.','.','.','.','.'],[],[],6).
    


%@param 3 is the number of guesses left
%
%If you have 1 or more guesses left, call smartstr to find the optimal guess G
%print G and read player response from console. If it matches, success.
%Otherwise, decrease number of guesses and use the response to update the grn/yel/gry lists and continue

guess(_,_,_,_,0):- write('Program Failed'), nl.
guess(Dict,Grn,Yel,Gry,N):-
    N > 0,
    valid_guesses(Dict,Grn,Yel,Gry,[],Candidates),
    chooseword(G,Candidates,N),
    select(G,Candidates,Candidates1),
    write('Guess: '), writeln(G),
    read(Response0),
    string_chars(Response0,Response),
    (Response == ['g','g','g','g','g'] ->  
    writeln('You chose the correct word, congratulations!');
    N1 is N - 1,
    updatelists(Response,G,Grn,Yel,Gry,Grn1,Yel1,Gry1),
    writeln(Grn1),
    writeln(Yel1),
    writeln(Gry1),
    guess(Candidates1, Grn1, Yel1, Gry1, N1)).

valid_guesses([],_Grn,_Yel,_Gry,Candidates, Candidates).
valid_guesses([Word|Words],Grn,Yel,Gry,Candidates,FinalCandidates):-
    (correct_pos(Word,Grn),
     correct_char(Word,Yel),
     not_in_word(Word,Gry) ->

    append(Candidates,[Word],Candidates1),
    valid_guesses(Words,Grn,Yel,Gry,Candidates1,FinalCandidates);
    valid_guesses(Words,Grn,Yel,Gry,Candidates,FinalCandidates)).

chooseword(X,L,6):- X = [a,d,i,e,u].
chooseword(X,L,N):-
    N < 6,
    L \= [],
    select(X,L,_).
     

updatelists(InputArray,Word,Grn,Yel,Gry,Grn1,Yel1,Gry1):-
    update_grn(InputArray,Word,Grn,Grn1),
    append_yel(InputArray,Word,Yel,Yel1),
    append_gry(InputArray,Word,Gry,Gry1).

%fully functional
update_grn([], [], R, R).
update_grn([C|Cs], [G|Gs], [R|Rs], [U|Us]):-
    (C == 'g' ->
    U = G;
    U = R ),
    update_grn(Cs, Gs, Rs, Us).

%fully functional  
append_yel([],[],Final,Final).
append_yel([C|Cs],[G|Gs],Yel, FinalYel):-
    (   C == 'y' ->
    	append(Yel,[G],Yel1),
    	append_yel(Cs,Gs,Yel1,FinalYel) ;
    	append_yel(Cs,Gs,Yel,FinalYel)). 

%fully functional   
append_gry([],[],Final,Final).
append_gry([C|Cs],[G|Gs],Gry, FinalGry):-
    (   C == 'r' ->
    	append(Gry,[G],Gry1),
    	append_gry(Cs,Gs,Gry1,FinalGry);
    	append_gry(Cs,Gs,Gry,FinalGry)).

%@param 1 is the guess in the form of a list ex: [w,o,r,d]
%@param2 is the 'Green' List, where unfilled characters are represented by a '.'
%@succeeds if param1 matches param2 in terms of filled characters
correct_pos([],[]).
correct_pos([C|Cs],[C2|CorrectPos]) :-
        (C == C2 ; C2 == '.'),
        correct_pos(Cs,CorrectPos).

%@param 1 is the guess in the form of a list ex: [w,o,r,d]
%@param 2 is the list of characters that must be in the word
%@succeeds if all characters are in the word
correct_char(_,[]).
correct_char(Guess,[C|Cs]):-
    memberchk(C,Guess),
    correct_char(Guess,Cs).

%@param 1 is the guess in the form of a list ex: [w,o,r,d]
%@param 2 is the list of characters that are not in the word
%@succeeds if none of the characters are in the word
not_in_word(_,[]).
not_in_word(Guess,[C|Cs]) :-
        \+ memberchk(C,Guess),
        not_in_word(Guess,Cs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%								 		%
%		Technical Methods for reading input and printing 		%
%										%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

append( [], X, X).                                   
append( [X | Y], Z, [X | W]) :- append( Y, Z, W).
   
%@param File: the name of the file to be read in in the format of a string
%@param Len: the Length of the words
%@param Words: list of words to be returned in the format [w,o,r,d,s]
%functioning
read_words(File,Len, Words) :-
    	%the file is read into the string Str
        read_file_to_string(File,Str,[]),
    	%Str is split based on newlines into separate words
        split_string(Str,"\n", "",WordsRead),
    	%Looks for 5 letter words out of WordsRead and puts them in Words1
        include(strlen(Len),WordsRead,Words1),
    	%Changes Words1 into a list of chars Words
        maplist(string_chars,Words1,Words).

%returns the length of a string
%functioning
strlen(Len,Word) :-
        string_length(Word,Len).