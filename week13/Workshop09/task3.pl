% Kaung Khant Lin, ID: 6540131
/* expanded_adventure.pl
    Expanded Adventure Game (map-accurate + available-moves shown)

    Map summary (locations only):
    valley -> forest
    forest -> left: riverbank, right: cave, forward: mountaintrail
    riverbank -> forward: bridge, right: waterfall
    waterfall -> forward: shrine -> forward -> bridge
    bridge -> left: cliff (death), forward: maze(0)
    cave -> forward: maze(0)
    mountaintrail -> forward: snowfield -> forward: fork
    maze(0) -> left: maze(1), right: maze(2)
    maze(1) -> left: maze(3), right: fork
    maze(2) -> right: maze(0) (loop), left: swamp
    maze(3) -> left: maze(0), right: maze(2)
    swamp -> forward: hermithut -> forward: fork
    fork -> left: trap (death), right: mountaintop (treasure)

    Entities (placed at startup):
    - ogre at maze(3)
    - treasure at mountaintop

    Legal commands: forward.  left.  right.  look.
  */

:- dynamic at/2.   % at(Entity,Location)

% Descriptions

description(valley, 'You are in a pleasant valley, with a trail ahead.').
description(forest, 'You are in a dense forest. Paths branch left, right, and forward.').
description(riverbank, 'You are at a riverbank. A fragile bridge lies forward; a roaring waterfall is to the right.').
description(cave, 'You are in a dark cave. Water drips from the ceiling.').
description(waterfall, 'You stand before a roaring waterfall; a hidden path seems to lead behind it.').
description(shrine, 'You are in a hidden shrine. Carvings whisper: "Beware the looping paths."').
description(bridge, 'You are on a fragile bridge. It creaks beneath your feet.').
description(cliff, 'You are teetering on the edge of a cliff.').
description(mountaintrail, 'You are on a steep mountain trail. The air grows colder as you climb.').
description(snowfield, 'You are in a snowy field. A harsh wind bites at your face.').
description(maze(_), 'You are in a maze of twisty trails, all alike.').
description(swamp, 'You are in a foul swamp. The ground bubbles suspiciously.').
description(hermithut, 'You reach a lonely hut. An old hermit looks up from his pipe.').
description(fork, 'You are at a fork in the path. Danger one way, treasure the other.').
description(trap, 'A hidden trap snaps shut!').
description(mountaintop, 'You are on the mountaintop. A treasure glitters here.').

% Helper: available moves + printing

available_moves(Loc, Dirs) :-
    findall(Dir, connect(Loc, Dir, _), Dirs).

print_moves([]) :- write('No exits.'), nl.
print_moves([D]) :- write(D), nl.
print_moves([D1,D2|Rest]) :-
    write(D1), write(', '),
    print_moves([D2|Rest]).

% Report location + available moves (called on arrival and on illegal move)
report :-
    at(you, Loc),
    description(Loc, Text),
    write(Text), nl,
    available_moves(Loc, Dirs),
    ( Dirs = [] ->
        write('Available moves: none'), nl
    ;
        write('Available moves: '),
        print_moves(Dirs)
    ).

% A simple 'look' command to reprint current location & available moves
look :- report.


% Map connections (locations only)
% connect(Current,Direction,Next)

connect(valley, forward, forest).

connect(forest, left, riverbank).
connect(forest, right, cave).
connect(forest, forward, mountaintrail).

connect(riverbank, forward, bridge).
connect(riverbank, right, waterfall).

connect(waterfall, forward, shrine).
connect(shrine, forward, bridge).

connect(bridge, left, cliff).
connect(bridge, forward, maze(0)).

connect(cave, forward, maze(0)).

connect(mountaintrail, forward, snowfield).
connect(snowfield, forward, fork).

% Maze system (loops)
connect(maze(0), left, maze(1)).
connect(maze(0), right, maze(2)).

connect(maze(1), left, maze(3)).
connect(maze(1), right, fork).

connect(maze(2), right, maze(0)).   % loop back
connect(maze(2), left, swamp).

connect(maze(3), left, maze(0)).
connect(maze(3), right, maze(2)).

connect(swamp, forward, hermithut).
connect(hermithut, forward, fork).

connect(fork, left, trap).
connect(fork, right, mountaintop).


% Movement: perform move only if connect/3 exists

move(Dir) :-
    at(you, Loc),
    connect(Loc, Dir, Next),   % only succeed if this direction exists
    retract(at(you, Loc)),
    assert(at(you, Next)),
    report,
    !.
move(_) :-
    % illegal move: show current location and allowed moves (no guessing)
    report,
    write('That is not a legal move. Choose one of the "Available moves."'), nl.

% Shorthand directions
forward :- move(forward).
left    :- move(left).
right   :- move(right).

% Hazards and win condition
% All set at(you,done) on game end

gameover :-
    at(you, Loc),
    retract(at(you, Loc)),
    assert(at(you, done)).

% Ogre kills if you are at same location as the ogre entity
ogre :-
    at(ogre, Loc),
    at(you, Loc),
    write('An ogre ambushes you and you die!'), nl,
    gameover,
    !.
ogre.

% Trap location (lose)
trap :-
    at(you, trap),
    write('You are trapped and cannot escape! Game over.'), nl,
    gameover,
    !.
trap.

% Cliff: falling death
cliff :-
    at(you, cliff),
    write('You slip and fall off the cliff. You die.'), nl,
    gameover,
    !.
cliff.

% Swamp: instant sink (death)
swamp :-
    at(you, swamp),
    write('You sink into the swamp. You perish.'), nl,
    gameover,
    !.
swamp.

% Snowfield: blizzard death
snowfield :-
    at(you, snowfield),
    write('A sudden blizzard overwhelms you. You freeze.'), nl,
    gameover,
    !.
snowfield.

% Treasure: winning condition (treasure entity placed at mountaintop)
treasure :-
    at(treasure, Loc),
    at(you, Loc),
    write('There is a treasure here!'), nl,
    write('Congratulations — you win!'), nl,
    gameover,
    !.
treasure.

% Main loop

main :-
    at(you, done),
    write('Thanks for playing.'), nl,
    !.

main :-
    write('\nNext move -- '),
    read(Move),
    call(Move),             % move/command (or illegal)
    ogre, trap, cliff, swamp, snowfield, treasure,
    main.

% Game startup

go :-
    retractall(at(_,_)),               % clear previous state
    assert(at(you, valley)),           % player start
    assert(at(ogre, maze(3))),         % place ogre as entity (not a map node)
    assert(at(treasure, mountaintop)),% place treasure
    write('Welcome to the expanded adventure game.'), nl,
    write('Legal moves: forward, left, right. Use look. to re-check location.'), nl,
    write('End each command with a period.'), nl, nl,
    report,
    main.
% -------------------------