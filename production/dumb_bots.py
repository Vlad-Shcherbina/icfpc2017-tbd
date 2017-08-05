import copy

from production.bot_interface import *
from production.json_format import parse_map, parse_move


class FirstMoveBot(Bot):
    """Claims the first available river (lexicographically).

    GameState format:
    {'punters': 142,
     'my_id': 42,
     'settings': <in their format>
     'map': <in their format>
     'all_past_moves': <list of moves in their format>}
    """

    # TODO: Tracking my_id and current game state is common for all bots
    # and should be factored out. Predicting scores goes there as well.

    def setup(self, req: SetupRequest) -> SetupResponse:
        state = dict(
            punters=req.punters,
            my_id=req.punter,
            settings=req.settings.raw_settings,
            map=req.map.raw_map,
            all_past_moves=[])
        if req.settings.futures:
            not_mines = set(req.map.g) - set(req.map.mines)
            futures = dict(zip(sorted(req.map.mines), sorted(not_mines)))
        else:
            futures = {}
        return SetupResponse(ready=req.punter, state=state, futures=futures)

    def gameplay(self, req: GameplayRequest) -> GameplayResponse:
        map = parse_map(req.state['map'])

        new_state = copy.deepcopy(req.state)
        new_state['all_past_moves'] += req.raw_moves

        rivers = set((u, w) for u, ws in map.g.items() for w in ws)
        for move in new_state['all_past_moves']:
            move = parse_move(move)
            if isinstance(move, ClaimMove):
                rivers.remove((move.source, move.target))
                rivers.remove((move.target, move.source))

        if rivers:
            source, target = min(rivers)
            move = ClaimMove(
                punter=req.state['my_id'],
                source=source, target=target)
        else:
            move = PassMove(punter=req.state['my_id'])


        return GameplayResponse(move=move, state=new_state)
