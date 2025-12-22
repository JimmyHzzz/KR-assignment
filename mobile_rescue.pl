/*
   
    How to start this game:

    set_prolog_flag(encoding, utf8).
    consult('mobile_rescue.pl').
    consult('pyperplan_runner.pl').
    start.
*/

:- use_module(library(random)).
:- use_module(pyperplan_runner).
:- use_module(library(filesex)).

% === Generate Relative Path ===
project_dir(Dir) :-
    source_file(start, File),
    file_directory_name(File, Dir).

project_file(Rel, Abs) :-
    project_dir(Dir),
    directory_file_path(Dir, Rel, Abs).

% === Pyperplan Configuration ===
pyperplan_exe('pyperplan.exe').
pyperplan_domain('adversary_domain.pddl').

:- dynamic
    i_am_at/1,        % player cuurent locatoin
    at/2,             % where object at
    holding/1,        % inventory
    door_state/2,     % door_state(Room, locked/unlocked)
    locker_state/1,   % locker_state(locked/unlocked)
    locker_password/1, %  locker 6 digit password
    torch_on/0,       % flashlight on
    turn/1,           % current turn number
    phone_in/1,       % where phone at
    visited/1,        % if corridor is visited
    game_over/0,      % game over flag
    guard_at/1,       % where guard at
    guard_mode/1,     % guard moede(patrol/chaser)
    guard_last_moved_turn/1,  % last turn guard moved
    last_guard_moved/0,       % if guard has moved in last turn
    guard_patrol_index/1.     % index for patrol route

:- discontiguous take/1.


% start game
start :-
    reset_game_state,
    instructions,
    look.

% show instructions
instructions :-
    nl,
    write('[Mobile Rescue]'), nl,
    write('You are a Chinese high school student who has just handed in your smartphone.'), nl,
    write('In midnight, you sneaked into the teaching building and tried to swap your smartphone with a dummy phone.'), nl, nl,
    write('1. Actions'), nl,
    write('  start.              -- Restart Game'), nl,
    write('  look.               -- Observe the surroundings'), nl,
    write('  go(Room).           -- Move to an adjacent room, e.g. go(corridor).'), nl,
    write('  wait.               -- Wait for a round and do nothing'), nl,
    write('  listen.             -- Listen to the footsteps of the guard'), nl,
    write('  take(Item).         -- Take item, e.g. take(torch).'), nl,
    write('  drop(Item).         -- Drop item'), nl,
    write('  inventory.          -- Check inventory'), nl,
    write('  search(Thing).      -- Seerch an object, e.g. search(mat).'), nl,
    write('  unlock(Room).       -- Unlock a door with a key, e.g. unlock(office).'), nl,
    write('  lock(Room).         -- Lock a door with a key, e.g. lock(office).'), nl,
    write('  open(locker).       -- Open a locker (password required)'), nl,
    write('  use(torch).         -- Turn on/off the flashlight'), nl,
    write('  escape.             -- Escape from the gate'), nl,
    write('  help.               -- Check instructions'), nl,
    write('  halt.               -- Exit Prolog'), nl, nl,
    write('2. Rooms in this game'), nl,
    write('  gate        -- Where the player is spawned and escapes'), nl,
    write('  corridor    -- A corridor that connects to all rooms'), nl,
    write('  office      -- Office (Your cellphone might be kept here)'), nl,
    write('  storage     -- Storage (Your cellphone might be kept here)'), nl,
    write('  classroom   -- Classrooom'), nl,
    write('  security    -- Security'), nl, nl,
    write('3. Goal: Swap your cellphone with a dummy phone'), nl,
    write('Keep other things as they were, and leave safely from the gate'), nl, nl,
    write('4. Adversary Behaviour:'), nl,
    write('  The guard will patrol the teaching building according to the following fixed route:'), nl,
    write('    security -> corridor -> storage -> corridor -> office'), nl,
    write('    -> corridor -> classroom -> corridor -> security -> ...'), nl,
    write('  The adversay will freeze for 3 rounds whenever enters a new room (corridor excluded)'), nl,
    write('  When you have obtained your phone, the guard is aware of where you at, and starts to chase after you'), nl, nl,
    write('5. Rounds each action costs:'), nl,
    write('  Actions that cost 1 round:'), nl,
    write('    go, wait, take, drop, unlock, lock, open(locker), ...'), nl,
    write('  Actions that cost 0 rounds: look, listen, search, inventory, read_notebook, help, ...'), nl, nl.

help :-
    instructions.

% reset game state
reset_game_state :-
    retractall(i_am_at(_)),
    retractall(at(_,_)),
    retractall(holding(_)),
    retractall(door_state(_,_)),
    retractall(locker_state(_)),
    retractall(locker_password(_)),
    retractall(torch_on),
    retractall(turn(_)),
    retractall(phone_in(_)),
    retractall(visited(_)),
    retractall(game_over),

    retractall(guard_at(_)),
    retractall(guard_mode(_)),
    retractall(guard_patrol_index(_)),
    retractall(guard_last_moved_turn(_)),
    retractall(last_guard_moved),

    init_world,
    init_phone_location.

% initialization
init_world :-
    % player state
    assert(i_am_at(gate)),
    assert(holding(dummy_phone)),
    assert(holding(torch)),
    assert(holding(phone)),

    % door state
    assert(door_state(office, locked)),
    assert(door_state(storage, locked)),

    assert(locker_state(locked)),
    % generate random 6-digit password with 0 paddings
    random_between(0, 999999, PNum),
    format(string(PassStr), "~|~`0t~d~6+", [PNum]),
    assert(locker_password(PassStr)),

    % game state
    assert(turn(0)),
    assert(guard_last_moved_turn(0)),

    assert(at(mat, corridor)),
    assert(at(desk, office)),
    assert(at(drawer, office)),
    assert(at(locker, storage)),

    assert(at(confiscated_comics, drawer)),
    assert(at(confiscated_magazines, drawer)),
    assert(at(other_phone1, locker)),
    assert(at(other_phone2, locker)),
    assert(at(other_phone3, locker)),

    assert(at(storage_key, desk)),
    assert(at(notebook, desk)),

    init_guard_random.

% random phone location
init_phone_location :-
    random_between(0, 1, R),
    ( R = 0 ->
        assert(phone_in(drawer)),
        assert(at(phone, drawer))
    ;   assert(phone_in(locker)),
        assert(at(phone, locker))
    ).

% define room connectivity

path(gate, corridor).
path(corridor, gate).

path(corridor, office).
path(office, corridor).

path(corridor, classroom).
path(classroom, corridor).

path(corridor, storage).
path(storage, corridor).

path(corridor, security).
path(security, corridor).

path(storage, security).
path(security, storage).

% define which rooms are dark and need flashlight

needs_light(corridor).
needs_light(office).
needs_light(storage).

can_see(Room) :-
    needs_light(Room), !,
    torch_on.
can_see(_Room).

% look and describe actions

look :-
    i_am_at(Place),
    ( can_see(Place) ->
        describe(Place),
        nl,
        notice_objects_at(Place)
    ;   write('It is dark here, try using a flashlight.'), nl
    ),
    nl.

describe(gate) :-
    write('You are standing in front of the teaching building,'), nl,
    (   visited(gate) -> true
    ;   write('you nned to go to the corridor to find clues about your phone.'), nl,
        assert(visited(gate))
    ).

describe(corridor) :-
    write('You have reached the corridor, the classroom in on your left side,'), nl,
    write('The office and the storage room lie at the end of the corridor'), nl,
    write('Besides them is the security room'), nl,
    (   visited(corridor) -> true
    ;   write('There is a slightly worn mat at the door of the office'), nl,
        write('It seems there is something under the mat'), nl,
        assert(visited(corridor))
    ).

describe(office) :-
    write('This is the office room'), nl,
    write('There is a desk with a drawer in the office room'), nl.

describe(classroom) :-
    write('This is the classroom'), nl,
    write('There is only empty chairs and desks here, nothing seems relevant to your phone'), nl.

describe(storage) :-
    write('This is the storage room, filled with odds and ends'), nl,
    write('You can see there is a locker with a note on it, it says \'Smartphones\''), nl.

describe(security) :-
    write('This is the security room'), nl.

describe(_) :-
    write('Nothing special here'), nl.

notice_objects_at(Place) :-
    at(X, Place),
    write('Here is a '), write(X), write('.'), nl,
    fail.
notice_objects_at(_).

/*--------------------------------------
  移动：go/1
  --------------------------------------*/

go(Room) :-
    game_running,
    i_am_at(Here),
    ( Room = Here ->
        write('You are already here'), nl, !
    ; Room = security -> % security room
        (   path(Here, security)
        ->  (   guard_at(security) % guard inside
            ->  retract(i_am_at(Here)),
                assert(i_am_at(security)),
                lose('You stepped into the security room while the guard is inside. He caught you immediately.')
            ;   retract(i_am_at(Here)),
                assert(i_am_at(security)), % guard outside
                write('You entered the security room while the guard is on his patrol.'), nl,
                (   guard_at(R)
                ->  format('The log book says the guard has just left for ~w. ~n', [R])
                ;   true
                ),
                update_turn,
                look,
                check_lose
            )
        ;   write('You can\'t reach there from here.'), nl
        )
    ; path(Here, Room) ->
        ( door_blocked(Room) ->
            write('The door is locked, you need a key'), nl
        ; retract(i_am_at(Here)),
          assert(i_am_at(Room)),
          update_turn,
          look,
          check_lose
        )
    ; write('You can\'t reach there from here'), nl
    ).

% if door is locked
door_blocked(office) :-
    door_state(office, locked), !.
door_blocked(storage) :-
    door_state(storage, locked), !.
door_blocked(_Room) :- fail.

% wait and listen actions

wait :-
    game_running,
    update_turn,
    write('You held your breathe and waited for a while.'), nl,
    look,
    check_lose.

listen :-
    game_running,
    (   guard_at(Room) ->
        current_guard_mode(Mode),
        (   Mode = patrol
        ->  turn(N), % patrol mode
            NextTurn is N + 1,
            guard_last_moved_turn(Last),
            patrol_route(Route),
            guard_patrol_index(I),
            length(Route, Len),
            NextI is (I + 1) mod Len,
            nth0(NextI, Route, NextRoom),
            (   (   Room = corridor,
                    NextTurn > Last
                ;   Room \= corridor,
                    NextTurn >= Last + 3
                ) % guard can move next turn
            ->  format('You can hear footsteps from the ~w, it\'s like someone is heading towards the ~w. ~n',
                       [Room, NextRoom])
            ;   format('You listened carefully, you can hear footsteps from the ~w. ~n', [Room])
            ) % chaser mode
        ;   format('Foorsteps from the ~w are getting closer, you feel nervous. ~n', [Room])
        )
    ;   write('It\'s quiet here.'), nl
    ).

/*--------------------------------------
  take and drop actions
  --------------------------------------*/

% items that are too heavy to be picked up
unpickable(desk).
unpickable(drawer).
unpickable(locker).
unpickable(mat).

% if item is reachable
reachable(Item, Place) :-
    at(Item, Place)
 ;  ( at(drawer, Place),  at(Item, drawer) )
 ;  ( at(locker, Place), at(Item, locker) ).

take(X) :-
    game_running,
    holding(X),
    write('You already have it in your invetory.'), nl, !.

take(Item) :-
    game_running,
    unpickable(Item),
    write('You can\'t pick this thing up.'), nl, !.

confiscated_comic(confiscated_comics).
confiscated_comic(confiscated_magazines).

confiscated_phones(other_phone1).
confiscated_phones(other_phone2).
confiscated_phones(other_phone3).

take(Item) :-
    game_running,
    confiscated_comic(Item),
    i_am_at(Place),
    reachable(Item, Place),
    retract(at(Item, _)),
    assert(holding(Item)),
    update_turn,
    write('You have picked up '), write(Item), write('.'), nl,
    write('Feels weird. If you don\'t return it, the teachers might found out tomorrow......'), nl,
    check_lose, !.

take(Item) :-
    game_running,
    confiscated_phones(Item),
    i_am_at(Place),
    reachable(Item, Place),
    retract(at(Item, _)),
    assert(holding(Item)),
    update_turn,
    write('You have picked up someone else\'s cellphone, '), write(Item), write('.'), nl,
    write('Feels weird. If you don\'t return it, the teachers might found out tomorrow......'), nl,
    check_lose, !.

take(X) :-
    game_running,
    i_am_at(Place),
    reachable(X, Place),
    retract(at(X, _)),
    assert(holding(X)),
    update_turn,
    write('You have picked up '), write(X), write('.'), nl,
    check_lose, !.

take(X) :-
    game_running,
    write('You can\'t pick up '), write(X), write(' here'), nl.

drop(X) :-
    holding(X),
    i_am_at(Place),
    retract(holding(X)),
    assert(at(X, Place)),
    update_turn,
    write('You have dropped down '), write(X), write('.'), nl,
    check_lose, !.

drop(_) :-
    write('You can\'t drop something you don\'t have.'), nl.

inventory :-
    ( holding(_) ->
        write('You are now holding:'), nl,
        list_holding
    ;   write('Nothing in your hands now.'), nl
    ).

list_holding :-
    holding(X),
    write('  - '), write(X), nl,
    fail.
list_holding.

% search, unlock, open, and use actions

% see what's inside a container
print_contents(Container) :-
    (   at(Item, Container),
        write('  - '), write(Item), nl,
        fail
    ;   true
    ).

search(mat) :-
    i_am_at(corridor),
    at(mat, corridor),
    ( at(office_key, corridor) ->
        write('There\'s nothing here'), nl
    ;   assert(at(office_key, corridor)),
        write('Under the mat, you found the (office_key).'), nl
    ), !.

search(desk) :-
    i_am_at(office),
    ( at(storage_key, desk) ; at(notebook, desk) ),
    ( at(storage_key, desk) ->
        retract(at(storage_key, desk)),
        assert(at(storage_key, office))
    ; true ),
    ( at(notebook, desk) ->
        retract(at(notebook, desk)),
        assert(at(notebook, office))
    ; true ),
    write('You searched the desk and found the (storage_key) and a (notebook).'), nl, !.

search(desk) :-
    i_am_at(office),
    write('Nothing\'s new here.'), nl, !.

search(drawer) :-
    i_am_at(office),
    ( phone_in(drawer),
      at(phone, drawer) ->
        retract(at(phone, drawer)),
        assert(at(phone, office)),
        write('You opened the drawer and found your phone here!'), nl
    ;   write('You opened the drawer. It\'s filled with homeworks and magazines. But your phone is not here.'), nl
    ),
    write('Thing inside the drawer'), nl,
    print_contents(drawer),
    !.

search(notebook) :-
    game_running,
    i_am_at(Place),
    can_see(Place),
    (   holding(notebook)
    ;   reachable(notebook, Place)
    ),
    locker_password(PassStr),
    write('You searched the (notebook), it says'), nl,
    format('"Locker Password: ~w"~n', [PassStr]),
    !.

search(_) :-
    write('Nothing intersting here'), nl.

unlock(office) :-
    i_am_at(corridor),
    door_state(office, locked),
    holding(office_key),
    retract(door_state(office, locked)),
    assert(door_state(office, unlocked)),
    update_turn,
    write('You have unlocked the office door'), nl,
    check_lose, !.

unlock(storage) :-
    i_am_at(corridor),
    door_state(storage, locked),
    holding(storage_key),
    retract(door_state(storage, locked)),
    assert(door_state(storage, unlocked)),
    update_turn,
    write('You have unlocked the storage room'), nl,
    check_lose, !.

unlock(_) :-
    write('You don\'t have the correct key to this room or you are not at the door'), nl.

lock(office) :-
    game_running,
    i_am_at(corridor),
    door_state(office, unlocked),
    holding(office_key),
    retract(door_state(office, unlocked)),
    assert(door_state(office, locked)),
    update_turn,
    write('You locked the office door carefully without making a noise'), nl,
    check_lose, !.

lock(storage) :-
    game_running,
    i_am_at(corridor),
    door_state(storage, unlocked),
    holding(storage_key),
    retract(door_state(storage, unlocked)),
    assert(door_state(storage, locked)),
    update_turn,
    write('You locked the storage door carefully without making a noise'), nl,
    check_lose, !.

lock(_) :-
    game_running,
    write('You don\'t have the correct key to this room or you are not at the door'), nl.

open(locker) :-
    i_am_at(storage),
    locker_state(unlocked),
    write('The locker is opened, with a list of things:'), nl,
    print_contents(locker),
    !.

open(locker) :-
    i_am_at(storage),
    locker_state(locked),
    write('Please enter the 6 digit password:'), nl,
    read(UserInput),
    term_string(UserInput, PassString),
    (   correct_password(PassString) ->
        retract(locker_state(locked)),
        assert(locker_state(unlocked)),
        update_turn,
        write('Bingo, the locker is now unlocked. You opened the locker eagerly.'), nl,
        ( phone_in(locker),
          at(phone, locker) ->
            retract(at(phone, locker)),
            assert(at(phone, storage)),
            write('Your phone is here!'), nl
        ; true ),
        write('What\'s in this locker:'), nl,
        print_contents(locker),
        check_lose
    ;   write('Wrong Password!'), nl
    ),
    !.

open(_) :-
    write('You can\'t open this thing.'), nl.

% check password
correct_password(PassString) :-
    locker_password(PassString).

% use flashlight
use(torch) :-
    holding(torch),
    (   torch_on -> % already on
        retract(torch_on),
        write('You have already turned on the flashlight.'), nl
    ;   assert(torch_on),
        write('With the flashlight on, you can see things better.'), nl,
        look
    ),
    !.

use(torch) :-
    write('You don\'t have the flashlight now.'), nl.

% game rules

game_running :-
    \+ game_over, !.
game_running :-
    write('Game\'s over.'), nl,
    write('To start a new game, type start. Or use halt. to exit.'), nl,
    fail.

% check guard can move this turn
should_guard_move(Turn) :-
    guard_last_moved_turn(Last),
    (   guard_at(corridor)
    ->  Turn > Last
    ;   Turn >= Last + 3 % guard in other rooms needs to freeze for 3 rounds
    ),
    retract(guard_last_moved_turn(Last)),
    assert(guard_last_moved_turn(Turn)).

update_turn :-
    retract(turn(N)),
    N1 is N + 1,
    assert(turn(N1)),
    (   should_guard_move(N1)
    ->  retractall(last_guard_moved),
        assert(last_guard_moved),
        adversary_step
    ;   retractall(last_guard_moved)
    ),
    check_guard_catch_player.

check_lose :-
    turn(T),
    T > 60, !,
    lose('You\'ve stayed too long in the teaching building, it\'s now morning and teachers are coming......').
check_lose :- true.

% === Adversary Logic ===

% only chases player in the building
player_in_building :-
    i_am_at(R),
    R \= gate.

% swith to chaser mode if player has phone and is in building
current_guard_mode(chaser) :-
    holding(phone),
    player_in_building, !.
% initialize as patrol mode
current_guard_mode(patrol).

adversary_step :-
    game_over, !.
adversary_step :-
    current_guard_mode(Mode),
    maybe_update_guard_mode(Mode), % check for possible update
    (   Mode = patrol
    ->  guard_patrol_step % patrol step
    ;   plan_for_guard(chaser, Plan), % pddl
        execute_guard_action(Plan)
    ).

maybe_update_guard_mode(Mode) :-
    guard_mode(Mode), !.
maybe_update_guard_mode(Mode) :-
    retractall(guard_mode(_)),
    assert(guard_mode(Mode)).

% patrol route
patrol_route([
    security,
    corridor,
    storage,
    corridor,
    classroom,
    corridor,
    office,
    corridor
]).

% random spawn spot
init_guard_random :-
    patrol_route(Route),

    sort(Route, UniqueRooms),
    random_member(StartRoom, UniqueRooms),

    % pick random room
    findall(I, nth0(I, Route, StartRoom), Indices),
    random_member(StartI, Indices),

    assert(guard_at(StartRoom)),
    assert(guard_mode(patrol)),
    assert(guard_patrol_index(StartI)).

guard_patrol_step :-
    guard_patrol_index(I),
    patrol_route(Route),
    length(Route, Len),
    nth0(I, Route, From),
    NextI is (I + 1) mod Len,
    nth0(NextI, Route, To),
    guard_at(From),
    retract(guard_at(From)),
    assert(guard_at(To)),
    retract(guard_patrol_index(I)),
    assert(guard_patrol_index(NextI)),
    !.
guard_patrol_step :- true.

% update pddl problem
write_chaser_problem(File) :-
    i_am_at(PRoom),
    guard_at(GRoom),
    open(File, write, Out),
    format(Out, "(define (problem phone-heist-chaser-dyn)~n", []),
    format(Out, "  (:domain phone-heist-adversary)~n~n", []),
    format(Out, "  (:objects~n", []),
    format(Out, "    gate corridor office classroom storage security - location~n", []),
    format(Out, "  )~n~n", []),
    format(Out, "  (:init~n", []),
    format(Out, "    (guard-at ~w)~n", [GRoom]),
    format(Out, "    (player-at ~w)~n", [PRoom]),
    format(Out, "    (adj gate corridor)~n", []),
    format(Out, "    (adj corridor gate)~n", []),
    format(Out, "    (adj corridor office)~n", []),
    format(Out, "    (adj office corridor)~n", []),
    format(Out, "    (adj corridor classroom)~n", []),
    format(Out, "    (adj classroom corridor)~n", []),
    format(Out, "    (adj corridor storage)~n", []),
    format(Out, "    (adj storage corridor)~n", []),
    format(Out, "    (adj storage security)~n", []),
    format(Out, "    (adj security storage)~n", []),
    format(Out, "  )~n~n", []),
    format(Out, "  (:goal (guard-at ~w))~n", [PRoom]),
    format(Out, ")~n", []),
    close(Out).

% solve pddl problem
plan_for_guard(chaser, Plan) :-
    pyperplan_exe(ExeRel),
    pyperplan_domain(DomainRel),

    project_file(ExeRel, ExeAbs),
    project_file(DomainRel, DomainAbs),
    project_file('adversary_problem.pddl', DynProblemAbs),

    write('DEBUG: writing dynamic chaser problem: '), writeln(DynProblemAbs),
    write_chaser_problem(DynProblemAbs),
    write('DEBUG: calling pyperplan...'), nl,
    catch(run_pyperplan_soln(ExeAbs, DomainAbs, DynProblemAbs, Plan),
          Error,
          ( write('DEBUG: pyperplan error: '), writeln(Error), Plan = [] )).

execute_guard_action([]) :- !.
execute_guard_action([Action | Rest]) :-
    % execute first action of the plan
    write('DEBUG: Executing guard action:'), write(Action), nl,
    write('DEBUG:Remaining plan:'), write(Rest), nl,
    (   Action = move_guard(From, To) ->
        execute_move_guard(From, To)
    ;   true
    ).

execute_move_guard(From, To) :-
    guard_at(From),
    retract(guard_at(From)),
    assert(guard_at(To)),
    !.
execute_move_guard(_, _) :- true.

% catch player
% chaser
execute_catch_player(planner) :-
    retractall(last_guard_moved),
    lose('You have been caught by the guard!').

% patrol
execute_catch_player(guard_move) :-
    retractall(last_guard_moved),
    lose('The guard found you on his patrol!').

% player entered the room where the guard is
execute_catch_player(player_move) :-
    lose('You walked straight into the guard!').

check_guard_catch_player :-
    i_am_at(Room),
    guard_at(Room),
    !,
    current_guard_mode(Mode),
    (   Mode = patrol
    ->  (   last_guard_moved
        ->  execute_catch_player(guard_move)
        ;   execute_catch_player(player_move)
        )
    ;   Mode = chaser
    ->  (   last_guard_moved
        ->  execute_catch_player(planner)
        ;   execute_catch_player(player_move)
        )
    ).
check_guard_catch_player :- true.

% === Escape Logic ===

% check if all the items are back to good places
item_back_ok(Item, GoodPlace) :-
    \+ holding(Item),
    (   \+ at(Item, _)
    ;   (   is_list(GoodPlace)
        ->  member(P, GoodPlace), at(Item, P)
        ;   at(Item, GoodPlace)
        )
    ).

% keys missing
missing_keys :-
    \+ item_back_ok(office_key, corridor)
 ;  \+ item_back_ok(storage_key, [desk, office])
 ;  \+ item_back_ok(notebook, [desk, office]).

% doors left unocked
unlocked_doors :-
    door_state(office, unlocked)
 ;  door_state(storage, unlocked).
% missing confiscated items
missing_confiscated_items :-
    ( holding(Item),
      ( confiscated_comic(Item)
      ; confiscated_phones(Item)
      )
    )
 ;  ( confiscated_comic(C), \+ at(C, drawer) )
 ;  ( confiscated_phones(P), \+ at(P, locker) ).

escape :-
    game_running,
    i_am_at(gate),
    missing_keys, !,
    lose('The second day, the keys are missing and the teacher found out it was you.').

escape :-
    game_running,
    i_am_at(gate),
    missing_confiscated_items, !,
    lose('The second day, the teacher found out the confiscated items are missing, and somehow knew it was you.').

escape :-
    game_running,
    i_am_at(gate),
    unlocked_doors, !,
    lose('One of the teachers noticed that the doors are left unlocked, you were soon considered to be the biggest suspect.').

escape :-
    game_running,
    i_am_at(gate),
    ( win_condition ->
        win
    ;   write('It\'s too dangerous to leave right now. There must be something you haven\'t done yet.'), nl
    ),
    !.

escape :-
    game_running,
    write('You can\'t leave now.'), nl.

win_condition :-
    holding(phone),
    \+ holding(dummy_phone),
    ( phone_in(drawer),
      at(dummy_phone, office)
    ; phone_in(locker),
      at(dummy_phone, storage)
    ).

win :-
    assert(game_over),
    nl,
    write('You have obtained your phone and escaped from the gate.'), nl,
    write('No one will ever know that you have swapped your smartphone with a dummy phone.'), nl,
    write('The second day, the teacher checked all the confiscated items as regular, and found nothing wrong.'), nl, nl,
    write('-- Perfect Escape'), nl,
    write('End of the game, enter start. for a new game, or use halt. to exit Prolog.'), nl.

lose(Reason) :-
    assert(game_over),
    nl,
    write('You failed:'), nl,
    write(Reason), nl, nl,
    write('End of the game, enter start. for a new game, or use halt. to exit Prolog.'), nl.
