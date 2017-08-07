import copy

from production.bot_interface import *
from production.json_format import parse_map, parse_move
from production.cpp_bot import glue


class FirstMoveBot(Bot):
    """Claims the first available river (lexicographically)."""

    def setup(self, req: SetupRequest) -> SetupResponse:
        if req.settings.futures:
            not_mines = set(req.map.g) - set(req.map.mines)
            futures = dict(zip(sorted(req.map.mines), sorted(not_mines)))
        else:
            futures = {}

        state = glue.state_from_setup_req(req, futures)

        return SetupResponse(ready=req.punter, state=state, futures=futures)


    def gameplay(self, req: GameplayRequest) -> GameplayResponse:
        state = req.state
        state['all_past_moves'] += req.raw_moves
        story = glue.story_from_state(state)
        board = glue.reconstruct_board(story)

        last_move = state.get('debug_last_move')
        if last_move:
            [move] = [move for move in req.raw_moves if parse_move(move).punter == story.my_id]
            assert last_move in move

        rivers = []
        for u, adj in enumerate(board.adj):
            for v in adj:
                if board.claimed_by(u, v) < 0:
                    rivers.append((board.unpack[u], board.unpack[v]))

        if rivers:
            source, target = min(rivers)
            move = ClaimMove(punter=req.state['my_id'], source=source, target=target)
            state['debug_last_move'] = 'claim'
        else:
            move = PassMove(punter=state['my_id'])
            state['debug_last_move'] = 'pass'

        return GameplayResponse(move=move, state=state)
