% Usage (Windows example):
% ?- run_pyperplan_soln('D:/Anaconda/Scripts/pyperplan.exe', 'domain.1.pddl', 'problem.1.pddl', Plan),
%    maplist(writeln, Plan).
% Pyperplan is a lightweight planner. So negation (i.e. not) in preconditions is not allowed.

:- module(pyperplan_runner,
          [ run_pyperplan_soln/4        % +Exe,+DomainPDDL,+ProblemPDDL,-Actions
          , read_plan_file/2            % +SolnFile,-Actions
          ]).

:- use_module(library(process)).
:- use_module(library(readutil)).
:- use_module(library(pcre)).

%% run_pyperplan_soln(+Exe, +DomainPDDL, +ProblemPDDL, -Actions:list) is det.
%  Calls pyperplan (no redirection). Expects solver to write ProblemPDDL+'.soln'.
%  After the process exits, reads and parses that .soln file.
run_pyperplan_soln(Exe, Domain, Problem, Actions) :-
    soln_path_(Problem, SolnFile),
    setup_call_cleanup(
        process_create(Exe, [Domain, Problem],
                       [ stdout(null),              % do not capture logs
                         stderr(pipe(Err)),         % capture errors
                         process(PID)
                       ]),
        read_string(Err, _, Stderr),
        close(Err)
    ),
    process_wait(PID, exit(Status)),
    ( exists_file(SolnFile)
    -> read_plan_file(SolnFile, Actions)
    ;  ( Status = exit(0)
       -> throw(error(existence_error(file, SolnFile), _))
       ;  format(user_error, "~s", [Stderr]),
          throw(error(pyperplan_failed(Status), _))
       )
    ).

%% read_plan_file(+SolnFile, -Actions:list) is det.
%  Reads a pyperplan .soln file and parses action lines like:
%     "0: (MOVE A B) [1]"
%  into Prolog terms:
%     move(a,b).
read_plan_file(SolnFile, Actions) :-
    ( exists_file(SolnFile)
    -> setup_call_cleanup(
           open(SolnFile, read, In, [encoding(utf8)]),
           read_string(In, _, Content),
           close(In)
       ),
       parse_plan_text_(Content, Actions)
    ;  throw(error(existence_error(file, SolnFile), _))
    ).

% --- helpers ---------------------------------------------------------------

soln_path_(Problem, SolnFile) :-
    atom_string(Problem, PStr),
    string_concat(PStr, ".soln", SStr),
    atom_string(SolnFile, SStr).

parse_plan_text_(Text, Actions) :-
    split_string(Text, "\n", "\r", Lines0),
    include(has_paren_, Lines0, Lines),
    maplist(line_to_action_term_, Lines, Actions).

has_paren_(Line) :- sub_string(Line, _, _, _, "(").

% Extract the first "(...)" group and convert to a functor:
% "(move a b)" or "0: (load box truck) [1]" -> move(a,b) / load(box,truck)
line_to_action_term_(Line, Term) :-
    ( re_matchsub("\\(([^\\)]+)\\)", Line, Dict, [])
    -> Str = Dict.1,
       string_lower(Str, Low),
       split_string(Low, " ", " \t", Parts),
       maplist(atom_string, Atoms, Parts),
       Atoms = [Name|Args],
       Term =.. [Name|Args]
    ;  atom_string(Term, Line)   % fallback: leave line as atom
    ).
