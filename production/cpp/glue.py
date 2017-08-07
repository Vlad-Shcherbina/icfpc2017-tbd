from typing import NamedTuple, Dict, Tuple

from production.cpp import stuff
from production.bot_interface import *
from production import json_format


def state_from_setup_req(req, futures):
    return dict(
            punters=req.punters,
            my_id=req.punter,
            settings=req.settings.raw_settings,
            my_futures=[[k, v] for k, v in futures.items()],
            map=req.map.raw_map,
            all_past_moves=[])


def story_from_state(state) -> Story:
    return Story(
            punters=state['punters'],
            my_id=state['my_id'],
            settings=json_format.parse_settings(state['settings']),
            map=json_format.parse_map(state['map']),
            my_futures=dict(state['my_futures']),
            moves=[json_format.parse_move(m) for m in state['all_past_moves']])


def reconstruct_board(story: Story) -> stuff.Board:
    # Pack site IDs to contiguous range.
    pack = {}
    unpack = []
    for s in story.map.g:
        pack[s] = len(unpack)
        unpack.append(s)

    adj = [[] for _ in story.map.g]
    for u, ws in story.map.g.items():
        for w in ws:
            adj[pack[u]].append(pack[w])

    mines = [pack[m] for m in story.map.mines]

    board = stuff.Board(adj, mines)

    board.set_futures(
        story.my_id, {pack[k]: pack[v] for k, v in story.my_futures.items()})

    for move in story.moves:
        if isinstance(move, ClaimMove):
            board.claim_river(
                move.punter, pack[move.source], pack[move.target])
        elif isinstance(move, SplurgeMove):
            for source, target in zip(move.route, move.route[1:]):
                board.claim_river(
                    move.punter, pack[source], pack[target])
        elif isinstance(move, OptionMove):
            board.option_river(
                move.punter, pack[move.source], pack[move.target])

    board.pack = pack
    board.unpack = unpack
    return board


class ProbInfo(NamedTuple):
    reach_prob: Dict[int, Dict[int, float]]  # mine -> site -> prob
    cut_prob_grad: Dict[Tuple[int, int], float]
    # both (u, v) and (v, u) are included
    # howewer, this thing is sparse and some zero edges are not included


def compute_prob_info(
        cut_prob: Dict[Tuple[int, int], float],
        with_options: bool,
        board: stuff.Board,
        my_id: int) -> ProbInfo:
    pack = board.pack
    unpack = board.unpack

    cut_prob = {(pack[u], pack[v]): p for (u, v), p in cut_prob.items()}
    if with_options:
        for u, v in cut_prob:
            if board.claimed_by(u, v) not in (-1, my_id):
                cut_prob[u, v] = 1.0

    reach_prob = {}
    cut_prob_grad = {}
    for mine in board.mines:
        mine = unpack[mine]
        rp = stuff.ReachProb(board, my_id, pack[mine], cut_prob, with_options)
        reach_prob[mine] = {unpack[k]: v for k, v in enumerate(rp.reach_prob)}
        for (u, v), grad in rp.get_cut_prob_grad().items():
            k = unpack[u], unpack[v]
            cut_prob_grad.setdefault(k, 0.0)
            cut_prob_grad[k] += grad

    t = {k[::-1]: v for k, v in cut_prob_grad.items()}
    cut_prob_grad.update(t)

    return ProbInfo(reach_prob=reach_prob, cut_prob_grad=cut_prob_grad)
