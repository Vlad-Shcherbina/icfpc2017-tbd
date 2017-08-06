from production.cpp import stuff
from production.bot_interface import *


def reconstruct_board(story: Story):
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
