import copy
from random import randrange

from production.bot_interface import *
from production.json_format import parse_map, parse_move, parse_settings
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
            assert last_move in move, (last_move, move)

        move = None
        if story.settings.splurges:
            if last_move == 'pass':
                # try to splurge, fall back to claim/pass if unsuccessful
                for u, adj in enumerate(board.adj):
                    unclaimed_adj = [v for v in adj if board.claimed_by(u, v) < 0]
                    if len(unclaimed_adj) >= 2:
                        v1, v2, *_ = unclaimed_adj
                        move = SplurgeMove(punter=story.my_id,
                                route=(board.unpack[v1], board.unpack[u], board.unpack[v2]))
                        break
            elif last_move == 'claim':
                # force pass so we can splurge next turn
                move = PassMove(punter=story.my_id)

        if move is None:
            # Try to claim or option
            free_rivers = []
            claimed_rivers = []
            my_options = (len(board.mines) - board.remaining_options(story.my_id)
                                if story.settings.options else 0)

            for u, adj in enumerate(board.adj):
                for v in adj:
                    optioned_by = board.optioned_by(u, v)
                    claimed_by = board.claimed_by(u, v)

                    if claimed_by < 0:
                        free_rivers.append((board.unpack[u], board.unpack[v]))
                    elif optioned_by < 0 and claimed_by != story.my_id:
                        claimed_rivers.append((board.unpack[u], board.unpack[v]))

            if (    story.settings.options and
                    my_options < len(board.mines) and
                    len(claimed_rivers) > my_options * 3 * story.punters):
                source, target = claimed_rivers[0]
                move = OptionMove(punter=story.my_id, source=source, target=target)
            elif free_rivers:
                source, target = free_rivers[0]
                move = ClaimMove(punter=story.my_id, source=source, target=target)

        if move is None:
            move = PassMove(punter=story.my_id)

        state['debug_last_move'] = move.key()
        return GameplayResponse(move=move, state=state)

