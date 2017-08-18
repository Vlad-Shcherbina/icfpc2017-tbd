import copy
from random import randrange

from production.bot_interface import *
from production.json_format import parse_map, parse_move, parse_settings
from production.cpp_bot import glue


class RandMoveBot(Bot):
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

        settings = parse_settings(state['settings'])

        last_move = state.get('debug_last_move')
        if last_move:
            [move] = [move for move in req.raw_moves if parse_move(move).punter == story.my_id]
            assert last_move in move, (last_move, move)

        move = None
        if settings.splurges:
            if last_move == 'pass':
                # try to splurge, fall back to claim/pass if unsuccessful
                for u, adj in enumerate(board.adj):
                    unclaimed_adj = [v for v in adj if board.claimed_by(u, v) < 0]
                    if len(unclaimed_adj) >= 2:
                        v1, v2, *_ = unclaimed_adj
                        move = SplurgeMove(punter=story.my_id,
                                route=(board.unpack[v1], board.unpack[u], board.unpack[v2]))
                        state['debug_last_move'] = 'splurge'
                        break
            elif last_move == 'claim':
                # force pass so we can splurge next turn
                move = PassMove(punter=story.my_id)
                state['debug_last_move'] = 'pass'

        if move is None:
            # Try to claim or option
            rivers = []

            # TODO: reuse Story.remaining_options() once fixed
            options_left = state['punters'] if settings.options else 0

            for u, adj in enumerate(board.adj):
                for v in adj:
                    if board.optioned_by(u, v) == story.my_id:
                        options_left -= 1
                    rivers.append((min(board.unpack[u], board.unpack[v]), 
                            max(board.unpack[u], board.unpack[v])))
            if rivers:
                #source, target = min(rivers)
                source, target = rivers[randrange(len(rivers))]
                u, v = board.pack[source], board.pack[target]
                if board.claimed_by(u, v) < 0:
                    move = ClaimMove(punter=story.my_id, source=source, target=target)
                    state['debug_last_move'] = 'claim'
                elif options_left < 1 or board.claimed_by(u, v) == story.my_id:
                    pass
                elif board.optioned_by(u, v) < 0:
                    move = OptionMove(punter=story.my_id, source=source, target=target)
                    state['debug_last_move'] = 'option'

        if move is None:
            move = PassMove(punter=story.my_id)
            state['debug_last_move'] = 'pass'

        return GameplayResponse(move=move, state=state)

