from typing import NamedTuple

from production.cpp import stuff
from production.bot_interface import *


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

    board.pack = pack
    board.unpack = unpack
    return board


class ProbInfo(NamedTuple):
    reach_prob: Dict[int, Dict[int, float]]  # mine -> site -> prob
    cut_prob_grad: Dict[Tuple[int, int], float]
    # both (u, v) and (v, u) are included
    # howewer, this thing is sparse and some zero edges are not included


def compute_prob_info(story: Story, board: stuff.Board, my_id: int) -> ProbInfo:
    pack = board.pack
    unpack = board.unpack

    reach_prob = {}
    cut_prob_grad = {}
    for mine in story.map.mines:
        cut_prob = 1.0 - 1.0 / story.punters
        rp = stuff.ReachProb(board, my_id, pack[mine], cut_prob)
        reach_prob[mine] = {unpack[k]: v for k, v in enumerate(rp.reach_prob)}
        for (u, v), grad in rp.get_cut_prob_grad().items():
            k = unpack[u], unpack[v]
            cut_prob_grad.setdefault(k, 0.0)
            cut_prob_grad[k] += grad

    t = {k[::-1]: v for k, v in cut_prob_grad.items()}
    cut_prob_grad.update(t)

    return ProbInfo(reach_prob=reach_prob, cut_prob_grad=cut_prob_grad)
