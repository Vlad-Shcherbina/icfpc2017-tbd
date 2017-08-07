import copy
import random
import logging
from typing import Dict
from math import log

from production.bot_interface import *
from production.cpp import stuff as cpp
from production.cpp import glue
from production import visualization, json_format


class CppBot(Bot):
    """
    GameState format:
    {'punters': 142,
     'my_id': 42,
     'settings': <in their format>
     'my_futures': [[1, 2], [10, 20], ...]
     'map': <in their format>
     'all_past_moves': <list of moves in their format>}
    """

    def setup(self, req: SetupRequest) -> SetupResponse:
        story = Story(
            punters=req.punters,
            my_id=req.punter,
            settings=req.settings,
            map=req.map,
            my_futures={},
            moves=[])

        futures = {}
        if req.settings.futures:
            board = glue.reconstruct_board(story)
            pack = board.pack
            unpack = board.unpack

            #cut_prob = 1 - 1 / story.punters
            cut_prob = 0.4  # optimistic  TODO
            cut_prob = {
                (u, v): cut_prob for u, vs in req.map.g.items() for v in vs}
            pi = glue.compute_prob_info(cut_prob, False, board, story.my_id)

            dist = board.dist
            for mine in story.map.mines:
                payoff = {}
                for site in story.map.g:
                    d = dist[pack[mine]][pack[site]]
                    if d < 0:
                        continue
                    p = pi.reach_prob[mine][site]
                    q = d * d * d * (p - (1 - p))
                    if q > 0:
                        payoff[site] = q
                if payoff:
                    futures[mine] = max(payoff, key=payoff.get)

        state = glue.state_from_setup_req(req, futures)
        return SetupResponse(ready=req.punter, state=state, futures=futures)


    def gameplay(self, req: GameplayRequest) -> GameplayResponse:
        state = req.state
        state['all_past_moves'] += req.raw_moves
        story = glue.story_from_state(state)
        board = glue.reconstruct_board(story)
        pack = board.pack

        last_move = state.get('debug_last_move')
        if last_move and json_format.REPORT_UNKNOWN_FIELDS:
            [move] = [move for move in req.raw_moves if json_format.parse_move(move).punter == story.my_id]
            assert last_move in move, ('server rejected the move', last_move, move)

        logging.info(f'remaining options: {story.remaining_options()}')

        predicted_score = {}
        for punter in range(req.state['punters']):
            predicted_score[punter] = board.base_score(punter)
        logging.info(f'my id: {story.my_id}, predicted score: {predicted_score}')

        cut_prob = 1 - 1 / story.punters
        cut_prob = {
            (u, v): cut_prob for u, vs in story.map.g.items() for v in vs}
        pi = glue.compute_prob_info(cut_prob, story.remaining_options() > 0, board, story.my_id)
        #logging.info(f'*********** {cut_prob_grad}')

        # ignore option moves for now TODO
        # cut_prob_grad = {
        #     (u, v): g
        #     for (u, v), g in pi.cut_prob_grad.items()
        #     if board.claimed_by(pack[u], pack[v]) == -1}
        cut_prob_grad = pi.cut_prob_grad

        if cut_prob_grad:
            source, target = min(cut_prob_grad, key=cut_prob_grad.get)
            #logging.info(f'*** {source}, {target}')

            cl = board.claimed_by(pack[source], pack[target])
            if cl == -1:
                move = ClaimMove(
                    punter=req.state['my_id'],
                    source=source, target=target)
            else:
                assert cl != req.state['my_id']
                assert board.optioned_by(pack[source], pack[target]) == -1
                assert story.remaining_options() > 0
                move = OptionMove(
                    punter=req.state['my_id'],
                    source=source, target=target)
        else:
            move = PassMove(punter=req.state['my_id'])

        state['debug_last_move'] = move.key()
        return GameplayResponse(move=move, state=state)


def render_prob_field(story: Story, size=600, individual_mine_probs=False):
    vis = visualization.Visualization(size, size)
    vis.draw_story(story)
    base_im = vis.get_image()

    vis = visualization.Visualization(size // 2, size // 2)
    vis.draw_background()
    vis.adjust_to_map(story.map)

    board = glue.reconstruct_board(story)

    cut_prob = 1 - 1 / story.punters
    cut_prob = {
        (u, v): cut_prob for u, vs in story.map.g.items() for v in vs}
    pi = glue.compute_prob_info(cut_prob, story.remaining_options() > 0, board, story.my_id)

    a = min(0, min(pi.cut_prob_grad.values()))
    b = max(0, max(pi.cut_prob_grad.values()))

    vis.draw_text((15, 15), f'white: {-a}')
    vis.draw_text((15, 30), f'black: {-b}')

    for u, vs in story.map.g.items():
        for v in vs:
            if (u, v) not in pi.cut_prob_grad:
                continue
            t = (pi.cut_prob_grad[u, v] - a) / (b - a + 1e-6)
            c = int(255 * (1 - t))
            vis.draw_edge(
            story.map.site_coords[u],
            story.map.site_coords[v],
            color=(c, c, c), width=3)

    grad_im = vis.get_image()


    if individual_mine_probs:
        prob_im = visualization.Visualization(1, 1).get_image()
        for reach_prob in pi.reach_prob.values():
            prob_im = visualization.hstack(
                prob_im,
                render_one_prob_field(story, reach_prob, size // 2))
    else:
        max_probs = {}
        for site in story.map.g:
            max_probs[site] = max(
                reach_prob[site] for reach_prob in pi.reach_prob.values())
        prob_im = render_one_prob_field(story, max_probs, size // 2)

    if vis.width < 2 * vis.height:
        return visualization.hstack(
            base_im, visualization.vstack(prob_im, grad_im))
    else:
        return visualization.vstack(
            base_im, visualization.hstack(prob_im, grad_im))

def render_one_prob_field(story, probs, size):
    vis = visualization.Visualization(size, size)
    vis.draw_background()
    vis.draw_map(story.map)
    for site in story.map.g:
        vis.draw_point(
            story.map.site_coords[site],
            color=prob_palette(probs[site]), size=6)

    for i, p in enumerate([1, 0.3, 0.1, 0.03, 0.01, 0.003, 0.001]):
        vis.draw_text((5, 15 * i + 5), f'p={p}', color=prob_palette(p))
    return vis.get_image()


def prob_palette(p):
    assert 0 <= p <= 1

    def hz(a, b, x):
        return (log(x) - log(a)) / (log(b) - log(a))

    if p > 0.1:
        t = hz(0.1, 1.0, p)
        return (255, 255, int(255 * t))
    if p > 0.01:
        t = hz(0.01, 0.1, p)
        return (255, int(255 * t), 0)
    if p > 0.001:
        t = hz(0.001, 0.01, p)
        return (int(255 * t), 0, 0)
    return (0, 0, 0)
