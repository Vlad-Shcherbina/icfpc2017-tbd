import copy

from production.bot_interface import *
from production.json_format import parse_map, parse_move


class FirstMoveBot(Bot):
    """Claims the first available river (lexicographically).

    GameState format:
    {'punters': 142,
     'my_id': 42,
     'map': <in their format>
     'all_past_moves': <list of moves in their format>}
    """

    # TODO: Tracking my_id and current game state is common for all bots
    # and should be factored out. Predicting scores goes there as well.

    def setup(self, req: SetupRequest) -> SetupResponse:
        state = dict(
            punters=req.punters,
            my_id=req.punter,
            map=req.map.raw_map,
            all_past_moves=[])
        return SetupResponse(ready=req.punter, state=state)

    def gameplay(self, req: GameplayRequest) -> GameplayResponse:
        map = parse_map(copy.deepcopy(req.state['map']))

        rivers = set((u, w) for u, ws in map.g.items() for w in ws)
        for move in req.state['all_past_moves']:
            move = parse_move(copy.deepcopy(move))
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

        new_state = copy.deepcopy(req.state)
        new_state['all_past_moves'] += req.raw_moves

        return GameplayResponse(move=move, state=new_state)

    def score(self, req: ScoreRequest):
        pass
