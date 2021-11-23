main:- doudou_search.       % main entry point

doudou_search:-
    init_dynamic_facts,    

    write('Looking for Dou Dou'),nl,
    write('Looking for Dou Dou is a simple Prolog adventure game.'),nl,
    write('It takes full advantage of Prolog''s '),nl,
    write('such as recursion, '),nl,
    write('You are me when I was 6 years old. You were given a '),nl,
    write('stuffed mouse when you were a toddler.  Its name is Dou Dou.'),nl,
    write('You cannot seem to find Dou Dou anywhere around.'),nl,
    write('It''s late and you''re tired, but you can''t go to sleep'),nl,
    write('without Dou Dou.  You set your mind on finding Dou Dou.'),nl,
    nl,
    write('Your adventure is controlled by using English simple natural'),nl,
    write('language commands that describe the action you wish to take.'),nl,
    write('You can go to other rooms, look at your surroundings, look in things'),nl,
    write('take things, drop things, eat things, check'),nl,
    write('your inventory, and turn some things on and off.'),nl,
    nl,
    write('Hit any key to continue.'),get0(_),
    write('Type "help" if you need more help on mechanics.'),nl,
    write('Type "hint" if you want a big hint.'),nl,
    write('Type "quit" if you give up.'),nl,
    nl,
    write('Enjoy the search.'),nl,

    look,                   % give a look before starting the game
    command_loop.

% command_loop - repeats until either the doudou is found or the
%     player types quit

command_loop:-
    repeat,
    get_command(X),
    do(X),
    (doudoufound; X == quit).

% do - matches the input command with the predicate which carries out
%     the command.  More general approaches which might work in the

do(goto(X)):-goto(X),!.
do(nshelp):-nshelp,!.
do(hint):-hint,!.
do(inventory):-inventory,!.
do(take(X)):-take(X),!.
do(drop(X)):-drop(X),!.
do(eat(X)):-eat(X),!.
do(look):-look,!.
do(turn_on(X)):-turn_on(X),!.
do(turn_off(X)):-turn_off(X),!.
do(look_in(X)):-look_in(X),!.
do(quit):-quit,!.

% These are the breakers of the game

doudoufound:-
    have(doudou),        
    write('Congratulations, you saved Dou Dou!.'),nl,
    write('Now you can sleep tight xx.'),nl,nl
.

quit:-
    write('Giving up?  It''s going to be a scary night'),nl,
    write('and when you get Dou Dou it''s not going'),nl,
    write('to smell right.'),nl,nl
.

% The help command

nshelp:-
    write('Use simple English sentences to enter commands.'),nl,
    write('The commands can cause you to:'),nl,
    nl,
    write('   go to a room          (ex. go to the office)'),nl,
    write('   look around           (ex. look)'),nl,
    write('   look in something     (ex. look in the desk)'),nl,
    write('   take something        (ex. take the apple)'),nl,
    write('   drop something        (ex. drop the apple)'),nl,
    write('   eat something         (ex. eat the apple)'),nl,
    write('   turn something on     (ex. turn on the light)'),nl,
    write('   inventory your things (ex. inventory)'),nl,
    nl,
    write('Terser commands and synonyms are usually accepted.'),nl,
    write('Hit any key to continue.'),nl,
    get0(_),
    look
.

hint:-
    write('You need to get to the dark, scary laundry room, and you can''t'),nl,
    write('unless you get some light.  You can''t turn on the laundry room'),nl,
    write('light, but there is a flash lightin the desk in the'),nl,
    write('living room you could use.'),nl,nl,
    look
.

init_dynamic_facts:-
    assertz(location(desk,'living room')),
    assertz(location(apple,kitchen)),
    assertz(location(flashlight,desk)),
    assertz(location(door,hall)),
    assertz(location(tv,'living room')),
    assertz(location(buffet,'dining room')),
    assertz(location('washing machine','laundry room')),
    assertz(location(doudou,'washing machine')),
    assertz(location(table,kitchen)),
    assertz(location(crackers,desk)),
    assertz(location(broccoli,kitchen)),
    assertz(here('living room')),
    assertz(turned_off(flashlight)),
    assertz(have(empty)),
    assertz(turned_on(a))
.

furniture(desk).
furniture('washing machine').
furniture(table).
furniture(door).
furniture(tv).
furniture(buffet).

edible(apple).
edible(crackers).

tastes_yucky(zucchini).


%%%%%%%%%%%%%%%%%%% ROOMS SECTION %%%%%%%%%%%%%%%%%%%%

% Rooms definition
room(kitchen).
room('laundry room').
room(hall).
room('dining room').
room('living room').

% Room connections
door(hall, kitchen).
door(hall, 'living room').
door('dining room', kitchen).
door(kitchen, 'laundry room').
door('living room', 'dining room').

% door connection
connect(X,Y):-
    door(Y,X);
    door(X,Y)
.

% List of things in a room
list_things(Place) :-  
    location(X, Place),
    tab(3),
    write(X),
    nl,
    fail
.
% Clause to always get true at the very end
list_things(_).

% List of connections of a room
list_connections(Place) :-
    connect(Place, X),
    tab(3),
    write(X),
    nl,
    fail
.
list_connections(_).

% Look around the room
look :-
    here(Place),
    write('You are in the '), write(Place), nl,
    write('You see:'), nl,
    list_things(Place),
    write('You can go to:'), nl,
    list_connections(Place)
.

% Moving between rooms
can_go(Place):- 
    here(X),
    connect(X, Place)
.
can_go(_):-
    write('There is no way for you to get there from here.'), nl,
    fail
.

goto(Place):-  
    can_go(Place),
    move(Place),
    look
.

move(Place):-
    retract(here(_)),
    asserta(here(Place))
.



%%%%%%%%%%%%%%%%%% THINGS IN ROOM %%%%%%%%%%%%%%%%%%



%%%%%%%%%   INVENTORY MANAGEMENT %%%%%%%%%

%%%% Taking things %%%%
% Checks if a Thing exists in current Room
is_here(Thing):-
    here(Here),
    contains(Thing,Here),
    !
. 

is_here(Thing):-
    respond(['There is no ',Thing,' here :(']),
    fail
.

% Checks whether a thing in current room is contained in another thing
contains(Thing,Here):-             
    location(Thing,Here)
.           

contains(Thing,Here):-
    location(Thing,X),
    contains(X,Here)
.

% Checks all of the above to try and take something
take(Thing):-
    is_here(Thing),
    is_takable(Thing),
    move(Thing,have),
    respond(['You now have the ',Thing])
.

move(Thing,have):-
    retract(location(Thing,_)),
    asserta(have(Thing))
.


% Checks if a thing is a furniture so the player cannot take it
is_takable(Thing):-                
    furniture(Thing),
    respond(['Sorry, you really aren''t strong enough to pick up a ',Thing]),
    !,
    fail
.

is_takable(_).

%%%%% Dropping things %%%%%%
% Drops in current room
drop(Thing):-
    have(Thing),                     
    here(Here),                      
    retract(have(Thing)),
    asserta(location(Thing,Here)),
    respond(['You have dropped the ',Thing])
.
drop(Thing):-
    respond(['You don''t even have the ',Thing])
.

%%%% Inventory listing %%%%%
inventory:-
    have(_),                         % make sure you have at least one thing
    write('You have: '),nl,
    list_possessions
.

inventory:-
    write('You have nothing lol'),nl
.

list_possessions:-
    have(X),
    tab(3),write(X),nl,
    fail
.

list_possessions.

%%%%%% Eating things %%%%%%%%%

% Check if you have the thing
eat(Thing):-
    have(Thing),
    eat_edible(Thing)
.
eat(Thing):-
    respond(['You don''t have the ',Thing])
.

% Check if it is edible, eat it and delete it from your things
eat_edible(Thing):-
    edible(Thing),
    retract(have(Thing)),
    respond(['That ',Thing,' was good'])
.

% Check if it's yucky
eat_edible(Thing):-
    tastes_yucky(Thing),
    respond(['There''s no way you''re eating this, you hate ',Thing])
.

% Check if it is not even edible
eat_edible(Thing):-
    respond(['Are you nuts?! You can''t eat a ',Thing])
.

%%% Things contained in things %%%%
look_in(Thing):-
    location(_,Thing),
    write('The '),write(Thing),write(' contains:'),nl,
    list_things(Thing)
.

look_in(Thing):-
    respond(['There is nothing in the ',Thing])
.

%%% Turning on and off %%%
turn_on(light):-
  respond(['You can''t reach the switch and there''s nothing to stand on']).
turn_on(Thing):-
  have(Thing),
  turn_on2(Thing).
turn_on(Thing):-
  respond(['You don''t have the ',Thing]).

turn_on2(Thing):-
  turned_on(Thing),
  respond([Thing,' is already on, can''t you see?']).
turn_on2(Thing):-
  turned_off(Thing),
  retract(turned_off(Thing)),
  asserta(turned_on(Thing)),
  respond([Thing,' turned on']).
turn_on2(Thing):-
  respond(['You can''t turn a ',Thing,' on']).


% If you have a thing that can turn off
turn_off(Thing):-
    have(Thing),
    turn_off2(Thing)
.

% If you don't 
turn_off(Thing):-
    respond(['You don''t have the ',Thing])
.

% checks to see if Thing is turned off
turn_off2(Thing):-
    turned_off(Thing),
    respond([Thing,' is already off, are you blind?'])
.

turn_off2(Thing):-
    turned_on(Thing),
    retract(turned_on(Thing)),
    asserta(turned_off(Thing)),
    respond([Thing,' turned off'])
.

turn_off2(Thing):-
    respond(['You can''t turn a ',Thing,' off, sorry bro']).


%% Finding Dou Dou %%

% You need a flashlight 
puzzle(goto(cellar)):-
    have(flashlight),
    turned_on(flashlight),!
.

% If you dont have the flashlight
puzzle(goto(cellar)):-
    write('You can''t go to the cellar because it''s dark in the'),nl,
    write('cellar, and you''re afraid of the dark. Maybe try finding'),nl,
    write('something that lights up?'),nl,
    !,fail
.

puzzle(_).

%%%%%%%%%%%%%%% NATURAL LANGUAGE FROM HELL %%%%%%%%%%%%%%%
%% Mixture of literals and variables
respond([]):-
    write('.'),nl,nl
.
respond([H|T]):-
    write(H),
    respond(T)
.

%% Inputting commands %%

% Command listener 
get_command(C):-
    readlist(L),        % reads a sentence and puts [it,in,list,form]
    command(X,L,[]),    % call the grammar for command
    C =.. X,! % make the command list a structure
.          
get_command(_):-
    respond(['Sorry, I don''t understand, try again or type help']),
    fail
.

% Three types of commands
command([Pred,Arg]) --> verb(Type,Pred),nounphrase(Type,Arg).
command([Pred]) --> verb(control,Pred). % Single word argument
command([goto,Arg]) --> noun(go_place,Arg). % Could work with just the name of the room

% Three types of verbs as well, all accept synonyms
verb(go_place,goto) --> go_verb.
verb(thing,V) --> int_verb(V).
verb(control,V) --> ctrl_verb(V).

% Go to another room
go_verb --> [go].
go_verb --> [go,to].

% Interactions verbs
int_verb(take) --> [take].
int_verb(take) --> [pick,up].
int_verb(drop) --> [drop].
int_verb(drop) --> [put].
int_verb(drop) --> [put,down].
int_verb(eat) --> [eat].
int_verb(turn_on) --> [turn,on].
int_verb(turn_on) --> [switch,on].
int_verb(turn_off) --> [turn,off].
int_verb(look_in) --> [look,in].
int_verb(look_in) --> [look].
int_verb(look_in) --> [open].

% Controller verbs
ctrl_verb(inventory) --> [inventory].
ctrl_verb(inventory) --> [i].
ctrl_verb(look) --> [look].
ctrl_verb(look) --> [look,around].
ctrl_verb(look) --> [l].
ctrl_verb(quit) --> [quit].
ctrl_verb(quit) --> [exit].
ctrl_verb(quit) --> [end].
ctrl_verb(quit) --> [bye].
ctrl_verb(nshelp) --> [help].
ctrl_verb(hint) --> [hint].

%% Subject part of the sentence
nounphrase(Type,Noun) --> det,noun(Type,Noun).
nounphrase(Type,Noun) --> noun(Type,Noun).

det --> [the].
det --> [a].

% WHen a place is a noun + special case for the rooms
noun(go_place,R) --> [R], {room(R)}.
noun(go_place,'dining room') --> [dining,room].
noun(go_place,'living room') --> [living,room].
noun(go_place,'laundry room') --> [laundry,room].

noun(thing,T) --> [T], {location(T,_)}.
noun(thing,T) --> [T], {have(T)}.
noun(thing,flashlight) --> [flash,light].
noun(thing,'washing machine') --> [washing,machine].
noun(thing,'dirty clothes') --> [dirty,clothes].

% The word light preceeded by a room name
noun(thing,light) --> [X,light], {room(X)}.
% Wants to turn on their own flashlight
noun(thing,flashlight) --> [light], {have(flashlight)}.
% Current room light
noun(thing,light) --> [light].

readlist(L):-
    write('> '),
    read_word_list(L)
.

read_word_list([W|Ws]) :-
    get0(C),
    readword(C, W, C1),       % Read word starting with C, C1 is first new
    restsent(C1, Ws), !
.      % character - use it to get rest of sentence

restsent(C,[]) :- lastword(C), !. % Nothing left if hit last-word marker
restsent(C,[W1|Ws]) :-
    readword(C,W1,C1),        % Else read next word and rest of sentence
    restsent(C1,Ws)
.

readword(C,W,C1) :-         % Some words are single characters
    single_char(C),           % i.e. punctuation
    !, 
    name(W, [C]),             % get as an atom
    get0(C1)
.
readword(C, W, C1) :-
    is_num(C),                % if we have a number --
    !,
    number_word(C, W, C1, _)
. % convert it to a genuine number
readword(C,W,C2) :-         % otherwise if character does not
    in_word(C, NewC),         % delineate end of word - keep
    get0(C1),                 % accumulating them until 
    restword(C1,Cs,C2),       % we have all the word     
    name(W, [NewC|Cs])
.       % then make it an atom
readword(C,W,C2) :-         % otherwise
    get0(C1),       
    readword(C1,W,C2)
.        % start a new word

restword(C, [NewC|Cs], C2) :-
    in_word(C, NewC),
    get0(C1),
    restword(C1, Cs, C2)
.
restword(C, [], C).


single_char(0',).
single_char(0';).
single_char(0':).
single_char(0'?).
single_char(0'!).
single_char(0'.).


in_word(C, C) :- C >= 0'a, C =< 0'z.
in_word(C, L) :- C >= 0'A, C =< 0'Z, L is C + 32.
in_word(0'',0'').
in_word(0'-,0'-).

% Have character C (known integer) - keep reading integers and build
% up the number until we hit a non-integer. Return this in C1,
% and return the computed number in W.

number_word(C, W, C1, Pow10) :- 
    is_num(C),
    !,
    get0(C2),
    number_word(C2, W1, C1, P10),
    Pow10 is P10 * 10,
    W is integer(((C - 0'0) * Pow10) + W1)
.
number_word(C, 0, C, 0.1).


is_num(C) :-
    C =< 0'9,
    C >= 0'0.

% These symbols delineate end of sentence

lastword(10).   % end if new line entered
lastword(0'.).
lastword(0'!).
lastword(0'?).