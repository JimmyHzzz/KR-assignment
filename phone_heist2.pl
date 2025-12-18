/*
   启动游戏方式：
    set_prolog_flag(encoding, utf8).
    consult('E:/KR-assignment/phone_heist.pl').
    start.
*/

:- use_module(library(random)).
:- use_module(pyperplan_runner).
:- use_module(library(filesex)).

% === 项目相对路径工具 ===
project_dir(Dir) :-
    source_file(start, File),
    file_directory_name(File, Dir).

project_file(Rel, Abs) :-
    project_dir(Dir),
    directory_file_path(Dir, Rel, Abs).

% === Pyperplan 配置（相对路径）===
pyperplan_exe('pyperplan.exe').
pyperplan_domain('adversary_domain.pddl').

:- dynamic
    i_am_at/1,                 % 玩家当前所在房间
    at/2,                      % at(物品, 位置)
    holding/1,                 % 玩家身上的物品
    door_state/2,              % door_state(Room, locked/unlocked)
    locker_state/1,            % locker_state(locked/unlocked)
    locker_password/1,         % 本局储物柜六位数密码（字符串）
    torch_on/0,                % 手电是否打开
    turn/1,                    % 动作数
    phone_in/1,                % phone_in(drawer) 或 phone_in(locker)
    visited/1,                 % 经过走廊不重复提醒
    game_over/0,               % 判断是否已经结束

    guard_at/1,                % 守卫当前位置
    guard_mode/1,              % 守卫模式: patrol 或 chaser
    guard_last_moved_turn/1,   % 守卫上一次行动的回合
    last_guard_moved/0,        % 本回合守卫是否刚刚行动过
    guard_patrol_index/1,      % 守卫当前在巡逻路线中的第几步

    guard_cached_plan/1,       % 缓存的追捕计划（列表）
    guard_plan_for_player/1.   % 缓存计划对应的玩家位置（用于判定是否需要重规划）

:- discontiguous take/1.

/*--------------------------------------
  入口：start/0 & instructions/0
  --------------------------------------*/

start :-
    reset_game_state,
    instructions,
    look.

instructions :-
    nl,
    write('【夺回手机大作战】'), nl,
    write('你是高三学生「可汗」，手机被班主任没收。'), nl,
    write('深夜，你翻墙潜入教学楼，准备拿回自己的手机，并用备用机调包。'), nl, nl,
    write('可用命令：'), nl,
    write('  start.              -- 重新开始游戏'), nl,
    write('  look.               -- 查看当前位置'), nl,
    write('  go(Room).           -- 前往相邻房间，例如 go(corridor).'), nl,
    write('  wait.               -- 原地等待一回合，让时间悄悄流逝'), nl,
    write('  listen.             -- 竖起耳朵听听周围有没有动静'), nl,
    write('  take(Item).         -- 拿起物品，例如 take(torch).'), nl,
    write('  drop(Item).         -- 放下物品'), nl,
    write('  inventory.          -- 查看你现在携带的东西'), nl,
    write('  search(Thing).      -- 搜索某个东西，例如 search(mat).'), nl,
    write('  unlock(Room).       -- 用钥匙开房门，例如 unlock(office).'), nl,
    write('  lock(Room).         -- 离开后把门重新锁好，例如 lock(office).'), nl,
    write('  open(locker).       -- 尝试打开储物柜（需要密码）'), nl,
    write('  use(torch).         -- 开关手电筒'), nl,
    write('  escape.             -- 在 gate 处尝试带着手机离开'), nl,
    write('  help.               -- 随时查看本帮助信息'), nl,
    write('  halt.               -- 退出 Prolog'), nl, nl,
    write('本游戏可探索的地点包括：'), nl,
    write('  gate        —— 教学楼门口（起点与撤离点）'), nl,
    write('  corridor    —— 三楼走廊（连接所有房间）'), nl,
    write('  office      —— 班主任办公室（可能藏有你的手机）'), nl,
    write('  storage     —— 储物室（统一保管手机的柜子,也可能藏有你的手机）'), nl,
    write('  classroom   —— 教室'), nl,
    write('  security    —— 值班室'), nl, nl,
    write('游戏目标：拿回自己的手机 phone，用 dummy_phone 调包，'), nl,
    write('保持其他物品原样，并从 gate 安全离开。'), nl, nl,
    write('【保安巡逻规则】'), nl,
    write('  值班老师会在教学楼里巡逻，按如下路线循环移动：'), nl,
    write('    security -> corridor -> storage -> corridor -> office'), nl,
    write('    -> corridor -> classroom -> corridor -> security -> ...'), nl,
    write('  老师会在房间里待3个回合，移动到走廊上不会停留'), nl,
    write('  当你拿到自己的手机后，值班老师会更加警觉，并尝试追捕你。'), nl, nl,
    write('【回合与时间规则】'), nl,
    write('  会消耗时间并推进回合的动作（以是否改变场上人物位置及物品状态为准）包括：'), nl,
    write('    go, wait, take, drop, unlock, lock, open(locker) 等'), nl,
    write('  以下动作不会消耗时间：look, listen, search, inventory, read_notebook, help 等。'), nl, nl.

help :-
    instructions.

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
    retractall(guard_cached_plan(_)),
    retractall(guard_plan_for_player(_)),

    init_world,
    init_phone_location.

init_world :-
    assert(i_am_at(gate)),
    assert(holding(dummy_phone)),
    assert(holding(torch)),

    assert(door_state(office, locked)),
    assert(door_state(storage, locked)),

    assert(locker_state(locked)),
    % 每局随机生成六位数密码（字符串，含前导0）
    random_between(0, 999999, PNum),
    format(string(PassStr), "~|~`0t~d~6+", [PNum]),
    assert(locker_password(PassStr)),

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

init_phone_location :-
    random_between(0, 1, R),
    ( R = 0 ->
        assert(phone_in(drawer)),
        assert(at(phone, drawer))
    ;   assert(phone_in(locker)),
        assert(at(phone, locker))
    ).

/*--------------------------------------
  地图：path/2
  --------------------------------------*/

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

/*--------------------------------------
  needs_light/1
  --------------------------------------*/

needs_light(corridor).
needs_light(office).
needs_light(storage).

can_see(Room) :-
    needs_light(Room), !,
    torch_on.
can_see(_Room).

/*--------------------------------------
  look/0, describe/1, notice_objects_at/1
  --------------------------------------*/

look :-
    i_am_at(Place),
    ( can_see(Place) ->
        describe(Place),
        nl,
        notice_objects_at(Place)
    ;   write('这里一片漆黑，什么也看不清。也许需要手电筒。'), nl
    ),
    nl.

describe(gate) :-
    write('你站在教学楼的大门(gate)，楼道灯昏黄，外面是空荡荡的操场。'), nl,
    (   visited(gate) -> true
    ;   write('你需要走上三楼的走廊，因为班主任办公室在那里。'), nl,
        assert(visited(gate))
    ).

describe(corridor) :-
    write('你来到三楼走廊(corridor)，两边是锁着的教室门。'), nl,
    write('尽头有：班主任办公室(office)、储物室(storage)，'), nl,
    write('以及写着"值班室"的房间(security)。'), nl,
    (   visited(corridor) -> true
    ;   write('在班主任办公室的门口铺着一块有点旧的地毯(mat)，'), nl,
        write('根据你平常的观察，你知道办公室门的钥匙就藏在地毯下面。'), nl,
        assert(visited(corridor))
    ).

describe(office) :-
    write('这里是班主任的办公室(office)。'), nl,
    write('班主任的桌子(desk)就在你眼前，桌边有个抽屉(drawer)。'), nl.

describe(classroom) :-
    write('这里是一间教室(classroom)，空荡荡的桌椅和写满公式的黑板。'), nl,
    write('有些课本和练习册散落在桌上，看起来和你的手机无关。'), nl.

describe(storage) :-
    write('这里是储物室(storage)，堆满了纸箱和旧设备。'), nl,
    write('你隐隐约约看见一排铁皮储物柜(locker)靠墙排着，其中一个上面贴着“手机统一保管柜”。'), nl.

describe(security) :-
    write('这里是值班室(security)，里面有手机被拿走后的报警装置。'), nl.

describe(_) :-
    write('这里看起来很普通。'), nl.

notice_objects_at(Place) :-
    at(X, Place),
    write('这里有一个 '), write(X), write('。'), nl,
    fail.
notice_objects_at(_).

/*--------------------------------------
  移动：go/1
  --------------------------------------*/

go(Room) :-
    game_running,
    i_am_at(Here),
    ( Room = Here ->
        write('你已经在这里了。'), nl, !
    ; Room = security ->
        % 必须检查合法 path，不能任何地方直达
        (   path(Here, security)
        ->  (   guard_at(security)
            ->  retract(i_am_at(Here)),
                assert(i_am_at(security)),
                lose('你推门走进了值班室，保安正坐在椅子上，抬头就看见了你。')
            ;   retract(i_am_at(Here)),
                assert(i_am_at(security)),
                write('你轻轻推开值班室的门，里面的椅子是空的，保安似乎已经出去巡逻了。'), nl,
                (   guard_at(R)
                ->  format('值班桌上的巡逻记录本上写着：保安刚刚朝 ~w 的方向离开。~n', [R])
                ;   true
                ),
                update_turn,
                look,
                check_lose
            )
        ;   write('你没法直接到那个地方。'), nl
        )
    ; path(Here, Room) ->
        ( door_blocked(Room) ->
            write('门好像锁着，你需要一把钥匙。'), nl
        ; retract(i_am_at(Here)),
          assert(i_am_at(Room)),
          update_turn,
          look,
          check_lose
        )
    ; write('你没法直接到那个地方。'), nl
    ).

door_blocked(office) :-
    door_state(office, locked), !.
door_blocked(storage) :-
    door_state(storage, locked), !.
door_blocked(_Room) :- fail.

/*--------------------------------------
  wait/0, listen/0
  --------------------------------------*/

wait :-
    game_running,
    update_turn,
    write('你屏住呼吸，原地等待了一会儿。'), nl,
    look,
    check_lose.

listen :-
    game_running,
    (   guard_at(Room) ->
        current_guard_mode(Mode),
        (   Mode = patrol
        ->  turn(N),
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
                )
            ->  format('你竖起耳朵，似乎从 ~w 的方向传来隐约的脚步声，像是有人正准备朝 ~w 的方向走去。~n',
                       [Room, NextRoom])
            ;   format('你竖起耳朵，似乎从 ~w 的方向传来隐约的脚步声。~n', [Room])
            )
        ;   format('你竖起耳朵，~w 方向的脚步声越来越近，让你有点不安。~n', [Room])
        )
    ;   write('教学楼一片寂静，你暂时没有察觉到其他人的动静。'), nl
    ).

/*--------------------------------------
  物品：take/1, drop/1, inventory/0
  --------------------------------------*/

% 不可拿起的固定物
unpickable(desk).
unpickable(drawer).
unpickable(locker).
unpickable(mat).

reachable(Item, Place) :-
    at(Item, Place)
 ;  ( at(drawer, Place),  at(Item, drawer) )
 ;  ( at(locker, Place),  at(Item, locker) ).

take(X) :-
    game_running,
    holding(X),
    write('你已经拿着它了。'), nl, !.

take(Item) :-
    game_running,
    unpickable(Item),
    write('这个东西太大/固定在原处，你没法把它拿起来。'), nl, !.

forbidden_comic(confiscated_comics).
forbidden_comic(confiscated_magazines).

forbidden_phone(other_phone1).
forbidden_phone(other_phone2).
forbidden_phone(other_phone3).

take(Item) :-
    game_running,
    forbidden_comic(Item),
    i_am_at(Place),
    reachable(Item, Place),
    retract(at(Item, _)),
    assert(holding(Item)),
    update_turn,
    write('你拿起了 '), write(Item), write('。'), nl,
    write('总觉得这样做有点危险，如果不放回去，明早可能会被发现……'), nl,
    check_lose, !.

take(Item) :-
    game_running,
    forbidden_phone(Item),
    i_am_at(Place),
    reachable(Item, Place),
    retract(at(Item, _)),
    assert(holding(Item)),
    update_turn,
    write('你偷偷拿起了别人的手机 '), write(Item), write('。'), nl,
    write('如果不放回原位，明早统一清点时一定会出事。'), nl,
    check_lose, !.

take(X) :-
    game_running,
    i_am_at(Place),
    reachable(X, Place),
    retract(at(X, _)),
    assert(holding(X)),
    update_turn,
    write('你拿起了 '), write(X), write('。'), nl,
    check_lose, !.

take(X) :-
    game_running,
    write('你在这里看不到 '), write(X), write('。'), nl.

drop(X) :-
    holding(X),
    i_am_at(Place),
    retract(holding(X)),
    assert(at(X, Place)),
    update_turn,
    write('你放下了 '), write(X), write('。'), nl,
    check_lose, !.

drop(_) :-
    write('你手上没有这个东西。'), nl.

inventory :-
    ( holding(_) ->
        write('你现在拿着：'), nl,
        list_holding
    ;   write('你手上什么也没有。'), nl
    ).

list_holding :-
    holding(X),
    write('  - '), write(X), nl,
    fail.
list_holding.

/*--------------------------------------
  搜索 & 解锁 & 打开 & 使用
  --------------------------------------*/

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
        write('你又掀了一遍地毯，下面已经没有别的东西了。'), nl
    ;   assert(at(office_key, corridor)),
        write('你掀开地毯，在下面摸到了一把小钥匙(office_key)。'), nl
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
    write('你翻了翻桌上的资料，找到了一把储物室的钥匙(storage_key)和一本密码本(notebook)。'), nl, !.

search(desk) :-
    i_am_at(office),
    write('你又翻了一遍桌子，没找到新的东西。'), nl, !.

search(drawer) :-
    i_am_at(office),
    ( phone_in(drawer),
      at(phone, drawer) ->
        retract(at(phone, drawer)),
        assert(at(phone, office)),
        write('你拉开抽屉，里面有一堆作业本和——你的手机(phone)！'), nl
    ;   write('你拉开抽屉，里面有一堆作业本和一些被没收的东西。看样子你的手机并不在这里！'), nl
    ),
    write('抽屉里目前还有：'), nl,
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
    write('你翻了翻密码本(notebook)，上面写着：'), nl,
    format('"手机柜密码：~w。不要忘了。\"~n', [PassStr]),
    !.

search(_) :-
    write('你随便翻找了一下，但没有什么特别的收获。'), nl.

unlock(office) :-
    i_am_at(corridor),
    door_state(office, locked),
    holding(office_key),
    retract(door_state(office, locked)),
    assert(door_state(office, unlocked)),
    update_turn,
    write('你用办公室钥匙打开了办公室的门。'), nl,
    check_lose, !.

unlock(storage) :-
    i_am_at(corridor),
    door_state(storage, locked),
    holding(storage_key),
    retract(door_state(storage, locked)),
    assert(door_state(storage, unlocked)),
    update_turn,
    write('你用储物室钥匙打开了储物室的门。'), nl,
    check_lose, !.

unlock(_) :-
    write('你似乎不在正确的门口，或者你没有对应的钥匙。'), nl.

lock(office) :-
    game_running,
    i_am_at(corridor),
    door_state(office, unlocked),
    holding(office_key),
    retract(door_state(office, unlocked)),
    assert(door_state(office, locked)),
    update_turn,
    write('你轻手轻脚地把办公室的门重新锁好。'), nl,
    check_lose, !.

lock(storage) :-
    game_running,
    i_am_at(corridor),
    door_state(storage, unlocked),
    holding(storage_key),
    retract(door_state(storage, unlocked)),
    assert(door_state(storage, locked)),
    update_turn,
    write('你轻手轻脚地把储物室的门重新锁好。'), nl,
    check_lose, !.

lock(_) :-
    game_running,
    write('现在不适合锁门：要么门本来就锁着，要么你不在门口，或者你没有对应的钥匙。'), nl.

open(locker) :-
    i_am_at(storage),
    locker_state(unlocked),
    write('柜门已经是开着的，里面有：'), nl,
    print_contents(locker),
    !.

open(locker) :-
    i_am_at(storage),
    locker_state(locked),
    write('储物柜有一个六位数密码锁。请输入密码：'), nl,
    read(UserInput),
    term_string(UserInput, PassString),
    (   correct_password(PassString) ->
        retract(locker_state(locked)),
        assert(locker_state(unlocked)),
        update_turn,
        write('咔哒一声，密码锁弹开了。你拉开柜门。'), nl,
        ( phone_in(locker),
          at(phone, locker) ->
            retract(at(phone, locker)),
            assert(at(phone, storage)),
            write('在一排手机中，你一眼就认出了自己的那一部(phone)。'), nl
        ; true ),
        write('柜子里目前还有：'), nl,
        print_contents(locker),
        check_lose
    ;   write('密码不对。'), nl
    ),
    !.

open(_) :-
    write('你现在打不开这个东西。'), nl.

correct_password(PassString) :-
    locker_password(PassString).

use(torch) :-
    holding(torch),
    (   torch_on ->
        retract(torch_on),
        write('你关掉了手电筒。'), nl
    ;   assert(torch_on),
        write('你打开了手电筒，视野一下子清晰了许多。'), nl,
        look
    ),
    !.

use(torch) :-
    write('你手上没有手电筒。'), nl.

/*--------------------------------------
  回合 & 失败检测
  --------------------------------------*/

game_running :-
    \+ game_over, !.
game_running :-
    write('这局游戏已经结束了。'), nl,
    write('请先输入 start. 开始新的一局，或者输入 halt. 退出。'), nl,
    fail.

should_guard_move(Turn) :-
    guard_last_moved_turn(Last),
    (   guard_at(corridor)
    ->  Turn > Last
    ;   Turn >= Last + 3
    ),
    retract(guard_last_moved_turn(Last)),
    assert(guard_last_moved_turn(Turn)).

% ✅ 核心改动：每次推进回合，若满足 chaser 条件，立刻规划并缓存（即使守卫这回合不行动）
update_turn :-
    retract(turn(N)),
    N1 is N + 1,
    assert(turn(N1)),

    ensure_chaser_plan_cached,   

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
    lose('你在教学楼里摸索得太久，天色已经发白，楼下传来开门声……').
check_lose :- true.

/*--------------------------------------
  守卫（巡逻 + 追捕）
  --------------------------------------*/

% 只有“玩家在教学楼内(不含 gate) 且拿着 phone”才 chaser
player_in_building :-
    i_am_at(R),
    R \= gate.

current_guard_mode(chaser) :-
    holding(phone),
    player_in_building, !.
current_guard_mode(patrol).

% ✅ 新增：确保追捕计划已缓存（玩家一拿到手机就会触发：因为 take/drop/go 等都会走 update_turn）
ensure_chaser_plan_cached :-
    current_guard_mode(chaser),
    i_am_at(PRoom),
    guard_cached_plan(Plan),
    Plan \= [],
    guard_plan_for_player(PRoom),
    !.
ensure_chaser_plan_cached :-
    current_guard_mode(chaser),
    i_am_at(PRoom),
    (   plan_for_guard(chaser, NewPlan)
    ->  true
    ;   NewPlan = []
    ),
    retractall(guard_cached_plan(_)),
    assert(guard_cached_plan(NewPlan)),
    retractall(guard_plan_for_player(_)),
    assert(guard_plan_for_player(PRoom)),
    !.
ensure_chaser_plan_cached :- true.

adversary_step :-
    game_over, !.

adversary_step :-
    current_guard_mode(Mode),
    maybe_update_guard_mode(Mode),
    (   Mode = patrol
    ->  guard_patrol_step
    ;   % chaser：这里只负责按缓存走一步（缓存由 update_turn 提前计算）
        ( guard_cached_plan(Plan) -> true ; Plan = [] ),
        execute_guard_action(Plan),
        (   Plan = [_|Rest]
        ->  retractall(guard_cached_plan(_)),
            assert(guard_cached_plan(Rest))
        ;   true
        )
    ).

maybe_update_guard_mode(Mode) :-
    guard_mode(Mode), !.
maybe_update_guard_mode(Mode) :-
    retractall(guard_mode(_)),
    assert(guard_mode(Mode)).

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

% 守卫随机出生（对“不同房间”均匀随机），巡逻路线不变
init_guard_random :-
    patrol_route(Route),
    sort(Route, UniqueRooms),
    random_member(StartRoom, UniqueRooms),
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

execute_catch_player(planner) :-
    retractall(last_guard_moved),
    lose('你被值班老师发现了：他径直朝你走来，把你逮了个正着。').

execute_catch_player(guard_move) :-
    retractall(last_guard_moved),
    lose('你被值班老师发现了：他巡逻到这里时，正好看见了你。').

execute_catch_player(player_move) :-
    lose('你刚走进房间，就和正在巡逻的值班老师迎面撞上了。').

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

/*--------------------------------------
  escape / win / lose
  --------------------------------------*/

% 让 item_back_ok/2 支持第二参数为“单个位置”或“位置列表”
item_back_ok(Item, GoodPlace) :-
    \+ holding(Item),
    (   \+ at(Item, _)
    ;   (   is_list(GoodPlace)
        ->  member(P, GoodPlace), at(Item, P)
        ;   at(Item, GoodPlace)
        )
    ).

bad_keys :-
    \+ item_back_ok(office_key, corridor)
 ;  \+ item_back_ok(storage_key, [desk, office])
 ;  \+ item_back_ok(notebook, [desk, office]).

bad_doors :-
    door_state(office, unlocked)
 ;  door_state(storage, unlocked).

bad_forbidden_items :-
    ( holding(Item),
      ( forbidden_comic(Item)
      ; forbidden_phone(Item)
      )
    )
 ;  ( forbidden_comic(C), \+ at(C, drawer) )
 ;  ( forbidden_phone(P), \+ at(P, locker) ).

escape :-
    game_running,
    i_am_at(gate),
    bad_keys, !,
    lose('第二天老师发现钥匙或密码本不在平时的位置，觉得非常可疑，最后查到了你头上。').

escape :-
    game_running,
    i_am_at(gate),
    bad_forbidden_items, !,
    lose('第二天老师清点被没收的物品时发现数量不对或摆放位置异常，你很快就成为最大嫌疑人。').

escape :-
    game_running,
    i_am_at(gate),
    bad_doors, !,
    lose('你离开后，有老师发现办公室或储物室的门没有上锁，立刻起了疑心，很快查到了你头上。').

escape :-
    game_running,
    i_am_at(gate),
    ( win_condition ->
        win
    ;   write('你总觉得还有什么没处理好，现在离开太冒险了。'), nl
    ),
    !.

escape :-
    game_running,
    write('你现在还不在可以离开的地方。'), nl.

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
    write('你抱着自己的手机，悄悄回到了楼梯间(gate)。'), nl,
    write('你确认备用机已经放回原来的位置，一切看起来和之前没有任何区别。'), nl,
    write('第二天早晨，老师打开抽屉/柜子，只看到"一部"手机安静地躺在那里。'), nl,
    write('而你，正坐在教室里若无其事地刷着屏幕。'), nl, nl,
    write('—— 完美脱身。'), nl,
    write('游戏结束，如需再玩一局，请输入 start. 重新开始，或输入 halt. 退出 Prolog。'), nl.

lose(Reason) :-
    assert(game_over),
    nl,
    write('你失败了：'), nl,
    write(Reason), nl, nl,
    write('游戏结束，如需再玩一局，请输入 start. 重新开始，或输入 halt. 退出 Prolog。'), nl.

where_is_guard :-
    guard_at(R),
    turn(T),
    format('回合 ~w：守卫在 ~w~n', [T, R]).
