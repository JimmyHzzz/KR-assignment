# 深夜夺回手机文本冒险游戏 —— 代码说明文档

> 目标读者：需要基于本代码写报告 / 做展示的同学  
> 目的：帮助快速理解 Prolog 游戏逻辑 + 守卫追逐（PDDL 规划）整体设计。

---

## 1. 游戏与技术整体概览

### 1.1 游戏背景与目标

- 主角：高三学生「可汗」  
- 故事：  
  - 白天手机被班主任没收  
  - 深夜翻墙潜入教学楼，悄悄拿回手机  
  - 用备用机调包，避免被发现  
  - 保证现场几乎恢复原样，再安全从 `gate` 撤离

- 地图地点（对应 PDDL 中的 `location` 类型）：
  - `gate`        —— 教学楼门口（游戏起点 & 终点）
  - `corridor`    —— 三楼走廊
  - `office`      —— 班主任办公室
  - `storage`     —— 储物室（手机统一保管柜）
  - `classroom`   —— 教室
  - `security`    —— 值班室（守卫起点）

### 1.2 技术结构一览

- 语言：
  - **Prolog**：负责游戏主逻辑（世界状态 + 回合制 +交互）
  - **PDDL + Pyperplan**：负责守卫在「追捕模式」下的自动规划
- 核心特性：
  - 回合制系统：玩家的部分动作会推进 `turn/1`
  - 状态完全由 Prolog 动态事实表示（位置、物品、门锁、光照、守卫等）
  - 守卫有两种模式：
    - `patrol`：固定路线巡逻 + 停留规则
    - `chaser`：玩家拿到 `phone` 后进入追捕模式，调用 PDDL 规划“最短路径抓人”
  - Prolog 每次根据当前真实局面 **动态生成 PDDL problem 文件**，调用 pyperplan 求解

---

## 2. 文件与模块关系

### 2.1 Prolog 主文件：`phone_heist.pl`

负责：

- 游戏启动与初始化：
  - `start/0`、`reset_game_state/0`、`init_world/0`
- 地图移动：`path/2`、`go/1`
- 回合推进：`turn/1`、`update_turn/0`
- 守卫逻辑（重点）：`guard_at/1`、`guard_mode/1`、`adversary_step/0`
- PDDL 规划接口：`write_chaser_problem/1`、`plan_for_guard/2`、`execute_guard_action/1`
- 逃脱与结局：`escape/0`、`win/0`、`lose/1`
- 环境交互：
  - 搜索：`search/1`
  - 拾取/放置：`take/1`、`drop/1`
  - 门与柜：`unlock/1`、`lock/1`、`open/1`
  - 光源：`use(torch)`
  - 可见性：`can_see/1`、`needs_light/1`

### 2.2 PDDL 域文件：`adversary_domain.pddl`

负责抽象描述守卫追逐逻辑：

- 类型：`location`
- 谓词：`guard-at`、`player-at`、`adj`、`captured`
- 动作：
  - `move_guard(from,to)`
  - `catch_player(room)`

### 2.3 动态 PDDL Problem：`adversary_problem.pddl`

- Prolog 根据当前世界状态生成
- 包含：
  - 守卫位置
  - 玩家位置
  - 地图邻接关系
  - `(captured)` 作为目标

---

## 3. 世界状态建模（Prolog 动态事实）

整个游戏世界由一组动态事实维护，如：

- 玩家位置：`i_am_at/1`
- 守卫位置与模式：`guard_at/1`、`guard_mode/1`
- 物品位置：`at(Item,Place)`
- 玩家背包：`holding(Item)`
- 门和柜的锁状态：`door_state/2`、`locker_state/1`
- 光照状态：`torch_on`
- 回合数：`turn/1`
- 守卫巡逻路线下标：`guard_patrol_index/1`
- 守卫行动频率控制：`guard_last_moved_turn/1`、`last_guard_moved/0`
- 游戏结束标记：`game_over/0`

Prolog 的动态数据库使得所有状态都可以随事件实时更新。

---

## 4. 地图与可见性系统

### 4.1 地图连接（`path/2`）

例如：

```prolog
path(gate, corridor).
path(corridor, office).
path(corridor, storage).
path(storage, security).
```

移动通过 `go(Room)` 实现，自动检查：

- 是否存在路径
- 门是否上锁
- 是否进入 `security`（值班室有特殊处理）

### 4.2 光照与手电筒

部分区域（如 `corridor`、`office`、`storage`）需要打开手电筒才能看清：

```prolog
needs_light(corridor).
```

在 `look/0` 中：

- 若 `not(torch_on)` 且是暗房间 → 显示黑暗提示
- 否则展示房间描述和可见物品

---

## 5. 玩家动作与交互设计

### 5.1 会“推进回合”的动作

这些动作必须触发 `update_turn/0`：

- `go(Room)`
- `wait`
- `take(Item)` / `drop(Item)`
- `unlock(Room)` / `lock(Room)`
- `open(locker)`（输入密码）
- 对抽屉/柜子取到真正手机等事件

### 5.2 不推进回合的动作

- `look`
- `listen`
- `inventory`
- `search(Thing)`
- `read_notebook`
- `help`

### 5.3 Search 逻辑（谜题设计）

- `search(mat)` → 找到 `office_key`
- `search(desk)` → 将 `storage_key` 和 `notebook` 放到 `office`
- `search(drawer)`：
  - 若手机在抽屉：将 `phone` 移到 `office`
  - 否则展示抽屉内物品

### 5.4 密码与柜子

`open(locker)` 要求输入密码 `"666666"`：

- 若密码正确：
  - 解锁储物柜
  - 若 `phone_in(locker)` → 将手机移到 `storage`

### 5.5 手电筒与视野

`use(torch)` 切换手电开关，并刷新视野。

---

## 6. 回合制系统与守卫行动触发

### 6.1 回合推进机制

核心：

```prolog
update_turn :-
    turn(N), N1 is N+1,
    should_guard_move(N1),
    adversary_step,
    check_guard_catch_player.
```

每个推进回合的动作都会：

1. 让 `turn` 自增
2. 判断守卫是否要移动
3. 若移动 → 执行巡逻 / 规划行动
4. 最后检查玩家是否被抓

### 6.2 守卫移动频率

规则：

- 在`corridor`：守卫只停留 1 回合，下回合必走
- 在其他房间：守卫每 3 回合移动一次

通过 `guard_last_moved_turn/1` 控制。

---

## 7. 守卫逻辑（巡逻模式）

### 7.1 模式判定

```prolog
current_guard_mode(patrol) :- \+ holding(phone).
current_guard_mode(chaser) :- holding(phone).
```

玩家一旦拿到 `phone` → 守卫进入追捕模式。

### 7.2 巡逻路线（FSM）

```prolog
[ security, corridor, storage, corridor,
  classroom, corridor, office, corridor ]
```

通过 `guard_patrol_index/1` 实现循环。

### 7.3 巡逻行为

每次守卫需要移动时：

```prolog
guard_patrol_step :-
    guard_patrol_index(I),
    next step → To,
    update guard_at(To) & guard_patrol_index.
```

---

## 8. 守卫追捕模式（核心）—— Prolog + PDDL

这是该项目最有价值的 AI 规划融合部分。

### 8.1 PDDL 域（`adversary_domain.pddl`）

核心动作：

- `move_guard(from,to)`
- `catch_player(r)`

目标：

```lisp
(:goal (captured))
```

### 8.2 动态 Problem 生成

`write_chaser_problem/1` 每次根据当前 Prolog 状态生成：

- 当前守卫位置
- 当前玩家位置
- 全部邻接关系
- `captured` 为目标

生成后写入 `adversary_problem.pddl`。

### 8.3 调用 pyperplan 

```prolog
plan_for_guard(chaser, Plan) :-
    write_chaser_problem(File),
    run_pyperplan_soln(..., Plan).
```

### 8.4 执行计划第一步

```prolog
execute_guard_action([move_guard(F,T)|_]) → 更新守卫位置  
execute_guard_action([catch_player(R)|_]) → 设置 caught_now 标记
```

### 8.5 检查抓捕

`check_guard_catch_player/0` 统一判定，包括：

- 玩家和守卫处于同一房间
- 撞上
- 规划中发生 `catch_player`

---

## 9. 逃脱与胜利条件

### 9.1 胜利条件（必须全部满足）

```prolog
win_condition :-
    holding(phone),
    \+ holding(dummy_phone),
    dummy_phone 在原手机位置对应的房间,
    所有钥匙/密码本归位,
    所有违禁物品归位,
    所有门重新锁好.
```

### 9.2 Escape 流程（在 gate）

1. 检查钥匙/密码本位置  
2. 检查违禁物品  
3. 检查门锁  
4. 检查是否调包成功  
5. 满足全部条件 → `win`

否则 → `lose(Reason)`。

---

## 10. 调试辅助

`where_is_guard.` 显示：

```
回合 N：守卫在 Room
```

可用于写报告时展示守卫路径与规划行为。

---

## 11. 报告撰写建议（可直接引用）

### 11.1 建议章节结构

1. **引言**
2. **知识表示（KR）**
   - 世界状态建模（Prolog 动态事实）
   - 环境结构（path/2）
   - 守卫状态与模式
3. **规划与推理机制**
   - 回合系统
   - 巡逻有限状态机（FSM）
   - PDDL 追捕规划（Domain + Dynamic Problem）
4. **Prolog 与 PDDL 的集成**
   - 如何实时生成 PDDL Problem
   - 如何解析规划结果
5. **系统设计特点**
   - 潜行机制、光照系统、谜题设计
   - 逃脱条件的公平性
6. **实验展示**
   - 两种手机放置地点的游玩过程
   - 守卫巡逻 → 追捕的行为变化
7. **总结与未来扩展**
