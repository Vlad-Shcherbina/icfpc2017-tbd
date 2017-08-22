# !python3

# source:
# https://github.com/Vlad-Shcherbina/icfpc2016-tbd/blob/master/production/render.py

from collections import namedtuple, Counter
from typing import Tuple, List
from random import randrange
from math import sqrt, atan, pi, cos, sin

from PIL import Image, ImageDraw

from production.bot_interface import *
from production.json_format import parse_map, parse_move
from production.cpp import glue

river_color = (100, 100, 100)
site_color = (200, 200, 200)
mine_color = (255, 255, 255)
back_color = (0, 0, 60)
text_color = (255, 255, 255)
me_color = (0, 255, 0)

mine_size = 4
site_size = 1
river_width = 1
claimed_width = 2
me_width = 3


DEFAULT_CLRS = 6
LEFT_MARGIN = 70  # for legend
NOISE_CURV = 2    # the more coeff, the flatter edges


def angle(x, y):
    if x > 1.e-6:    phi = atan(y / x)
    elif x < -1.e-6: phi = pi + atan(y / x)
    else:            phi = pi/2 if y > 0 else -pi/2
    return phi

def arc_params(p1, p2):
    if p1[1] > p2[1] : p1, p2 = p2, p1
    vx, vy = (p2[0] - p1[0]) / 2, (p2[1] - p1[1]) / 2   # initial edge vector
           
    R = sqrt((vx ** 2 + vy ** 2) * (NOISE_CURV ** 2 + 1)) # raduis of circle
    
    center_x = vy * NOISE_CURV + (p1[0] + p2[0])/2        # center of circle
    center_y = -vx * NOISE_CURV + (p1[1] + p2[1])/2
    alpha1 = angle(p1[0] - center_x, p1[1] - center_y)    # starting angle
    alpha2 = angle(p2[0] - center_x, p2[1] - center_y)    # ending angle


    if abs(alpha2 - alpha1) > pi: 
        alpha1, alpha2 = max(alpha1, alpha2) - pi*2, min(alpha1, alpha2)
    else:
        alpha1, alpha2 = min(alpha1, alpha2), max(alpha1, alpha2)
    return (center_x, center_y, R, alpha1, alpha2)


class Visualization:
    """ Draws one image of game state or a pile of them.

    Example:
    img = Visualization(800, 800).draw_state(somegamestate).get_image()
    """
    punter_colors = [(255, 205, 80),
                    (80, 205, 245),
                    (240, 80, 240),
                    (240, 120, 110),
                    (145, 155, 155),
                    (125, 205, 105)]

    def __init__(self, width=800, height=800, curved=None):
        # while drawing map width will be reset.
        # If no map is drawn, set curved param explicitly!
        self.width = width
        self.height = height
        self.curved = curved
        self.scale = 1

        self.back_commands = []
        self.site_commands = []
        self.river_commands = []
        self.fore_commands = []


    def draw_background(self):
        def draw_command(img):
            img = Image.new('RGBA', (self.width, self.height))
            draw = ImageDraw.Draw(img)
            draw.polygon(((0, 0), (0, self.height), (self.width, self.height), (self.width, 0)),
                         fill=back_color)
            return img
        self.back_commands.append(draw_command)


    def draw_point(self, coord: Tuple[float, float], color=site_color, size=site_size, outline=None):
        coord = self.get_coord(coord)
        if outline is None:
            outline = color
        def draw_command(img):
            draw = ImageDraw.Draw(img)
            draw.ellipse(
                    (
                        coord[0] - size/2,
                        coord[1] - size/2,
                        coord[0] + size/2,
                        coord[1] + size/2
                    ),
                    fill=color,
                    outline=outline
                )
            return img
        self.site_commands.append(draw_command)


    def draw_site(self,  coord: Tuple[float, float]):
        self.draw_point(coord, color=site_color, size=site_size) # just in case

    def draw_mine(self, coord: Tuple[float, float]):
        self.draw_point(coord, color=(0, 0, 0), size=2*mine_size)
        self.draw_point(coord, color=mine_color, size=mine_size) 


    def draw_edge(
            self,
            p1: Tuple[float, float],
            p2: Tuple[float, float],
            color=river_color,
            width=river_width,
            no_curve=False):
        if self.curved and not no_curve: 
            self.draw_curved_edge(p1, p2, color, width)
            return
        p1 = self.get_coord(p1)
        p2 = self.get_coord(p2)

        def draw_command(img):
            draw = ImageDraw.Draw(img)
            draw.line((p1, p2), fill=color, width=width)
            return img
        self.river_commands.append(draw_command)


    def draw_curved_edge(
            self,
            p1: Tuple[float, float],
            p2: Tuple[float, float],
            color=river_color,
            width=river_width):
        p1 = self.get_coord(p1)
        p2 = self.get_coord(p2)
        center_x, center_y, R, alpha1, alpha2 = arc_params(p1, p2)
        a = (alpha2 + alpha1) / 2
        shift = (int(sin(a) / sin(pi/8)), int(cos(a) / cos(pi/8)))

        def draw_command(img):
            draw = ImageDraw.Draw(img)
            for i in range(-width//2, width//2):
                draw.arc((center_x - R + shift[0] * i, 
                          center_y - R + shift[1] * i, 
                          center_x + R + shift[0] * i, 
                          center_y + R + shift[1] * i), 
                          alpha1 / pi * 180, alpha2 / pi * 180,
                          fill=color)


            return img
        self.river_commands.append(draw_command)

    def draw_text(self, p: Tuple[float, float], text: str, color=text_color):
        def draw_command(img):
            draw = ImageDraw.Draw(img)
            draw.text(p, text, fill=color)
            return img
        self.fore_commands.append(draw_command)


    @classmethod
    def set_punters(cls, n):
        if n <= DEFAULT_CLRS: return
        cls.punter_colors[DEFAULT_CLRS:] = []
        threshold = 100 * 3 / n

        def colors_differ(clr1, clr2):
            return sum(abs(clr1[i] - clr2[i]) for i in range(3)) > threshold

        def randcolor(): return randrange(1, 6) * 30 + 100

        for _ in range(n - DEFAULT_CLRS):
            while True:
                color = (randcolor(), randcolor(), randcolor())
                if all(colors_differ(color, old) for old in cls.punter_colors):
                    cls.punter_colors.append(color)
                    break


    def draw_legend(self, legend: List[str], p: Tuple[float, float]=None):
        if not p:
            p = (30, self.height - 30 - len(legend) * 15)
        if len(self.punter_colors) <= len(legend): self.set_punters(len(legend) + 1)
        assert len(legend) <= len(self.punter_colors)
        for p_text, p_color in zip(legend, self.punter_colors):
            if '(me)' in p_text:
                p_color = me_color
            self.draw_text(p, p_text, color=p_color)
            p = (p[0], p[1] + 15)


    def draw_move(self, mv: Move, m: Map, me=False):
        if isinstance(mv, PassMove): return
        elif isinstance(mv, ClaimMove): self.draw_claim(mv, m, me)
        elif isinstance(mv, OptionMove): self.draw_option(mv, m, me)
        elif isinstance(mv, SplurgeMove):
            raise NotImplementedError


    def draw_claim(self, mv: ClaimMove, m: Map, me=False):
        assert isinstance(mv, ClaimMove)   # overkill?
        if len(self.punter_colors) <= mv.punter: self.set_punters(punter + 1)
        self.draw_edge(m.site_coords[mv.source],
                       m.site_coords[mv.target],
                       self.punter_colors[mv.punter] if not me else me_color,
                       width=claimed_width if not me else me_width)


    def draw_option(self, mv: OptionMove, m: Map, me=False):
        assert isinstance(mv, OptionMove)   # overkill?
        p1 = m.site_coords[mv.source]
        p2 = m.site_coords[mv.target]

        mid_p = self.get_midpoint(p1, p2)
        length = 10 * self.scale
        vx, vy = p2[0] - p1[0], p2[1] - p1[1]
        L = sqrt(vx * vx + vy * vy)

        s1 = (mid_p[0] + vy / L * length, mid_p[1] - vx / L * length)
        s2 = (mid_p[0] - vy / L * length, mid_p[1] + vx / L * length)
        color = self.punter_colors[mv.punter] if not me else me_color
        self.draw_edge(s1, s2, color=color, width = 2, no_curve=True)


    def get_midpoint(self, p1, p2):
        if not self.curved:
            return (p1[0] + p2[0]) / 2, (p1[1] + p2[1]) / 2
        center_x, center_y, R, alpha1, alpha2 = arc_params(p1, p2)
        a = (alpha2 + alpha1) / 2
        return (center_x + R * cos(a), center_y + R * sin(a))


    def adjust_to_map(self, m: Map):
        self.adjust_to_map_coords(
                min(p[0] for p in m.site_coords.values()),
                max(p[0] for p in m.site_coords.values()),
                min(p[1] for p in m.site_coords.values()),
                max(p[1] for p in m.site_coords.values())
                )

    def draw_map(self, m: Map):
        self.adjust_to_map(m)
        if self.curved is None: self.curved = (len(m.g) < 70)

        count = 0
        for source in m.g:
            for target in m.g[source]:
                if source < target:
                    self.draw_edge(m.site_coords[source], m.site_coords[target])
        for site in m.site_coords:
            if site in m.mines:
                self.draw_mine(m.site_coords[site])
            else:
                self.draw_site(m.site_coords[site])


    def get_x(self, x: float) -> int:
        return int(x) + LEFT_MARGIN

    def get_y(self, y: float) -> int:
        return int(y) + LEFT_MARGIN

    def get_coord(self, p: Tuple[float, float]) -> Tuple[int, int]:
        return (self.get_x(p[0]), self.get_y(p[1]))


    def adjust_to_map_coords(self, x_min, x_max, y_min, y_max):
        border_coeff = 0.05
        W, H = (x_max - x_min + 1e-6), (y_max - y_min + 1e-6)
        self.scale = H / self.height * (1 - 2*border_coeff)
        self.width = int(self.height * W / H)
        canvas_width = self.width * (1 - 2 * border_coeff)
        canvas_height = self.height * (1 - 2 * border_coeff)
        self.width += LEFT_MARGIN


        def get_x(x):
            return int((x - x_min) / W * canvas_width
                       + self.width * border_coeff
                       + LEFT_MARGIN)
        def get_y(y):
            return int((y - y_min) / H * canvas_height
                        + self.height * border_coeff)

        self.get_x = get_x
        self.get_y = get_y


    def draw_state(self, state, height=None):
        if height: self.height = height
        self.draw_background()
        m = parse_map(state['map'])
        self.draw_map(m)

        self.set_punters(state['punters'])
        me = state['my_id']
        legend = [f'[{i}]' for i in range(state['punters'])]
        legend[me] += ' (me)'
        self.draw_legend(legend)

        for mv_raw in state['all_past_moves']:
            mv = parse_move(mv_raw)
            self.draw_move(mv, m, me=(me==mv.punter))

    def draw_story(self, story: Story):
        self.draw_background()
        self.draw_map(story.map)

        board = glue.reconstruct_board(story)
        pack = board.pack
        unpack = board.unpack

        legend = [f'[{i}]' for i in range(story.punters)]

        if story.score:
            for k, v in story.score.items():
                legend[k] += f' score={v}'
        else:
            predicted_score = {}
            for punter in range(story.punters):
                legend[punter] += f' ~{board.base_score(punter)}'

        pass_cnt = Counter()
        option_cnt = Counter()
        for i, move in enumerate(story.moves):
            if isinstance(move, OptionMove):
                option_cnt[move.punter] += 1
            # Don't count pass moves in the first turn
            # (they send placeholder pass moves for each player).
            if i < story.punters:
                continue
            # Also, weirdly, they don't send the very last moves
            # in the score request, and replace them with passes instead.
            if story.score and i >= len(story.moves) - story.punters:
                continue
            if isinstance(move, PassMove):
                pass_cnt[move.punter] += 1
        for i in range(len(legend)):
            if pass_cnt[i]:
                legend[i] += f' {pass_cnt[i]}p'
            if option_cnt[i]:
                legend[i] += f' {option_cnt[i]}o'

        legend[story.my_id] += ' (me)'
        self.draw_legend(legend)

        for move in story.moves:
            if isinstance(move, SplurgeMove):
                for source, target in zip(move.route, move.route[1:]):
                    if board.claimed_by(pack[source], pack[target]) < 0:
                        self.draw_move(OptionMove(move.punter, source, target),
                                         story.map, 
                                         me=move.punter==story.my_id)
                    else:
                        self.draw_move(ClaimMove(move.punter, source, target),
                                         story.map, 
                                         me=move.punter==story.my_id)
            else:
                self.draw_move(move, story.map, me=move.punter==story.my_id)

        for source, target in story.my_futures.items():
            color = (255, 0, 0)
            if pack[target] in board.reachable_by_claimed(story.my_id, pack[source]):
                color = (0, 255, 0)

            self.draw_edge(
                story.map.site_coords[source],
                story.map.site_coords[target],
                color=color, width=1, no_curve=True)
            self.draw_point(
                story.map.site_coords[target],
                color=None, outline=color, size=10)

    def get_image(self) -> Image.Image:
        img = Image.new('RGBA', (self.width, self.height))
        for command_set in [self.back_commands,
                            self.river_commands,
                            self.site_commands,
                            self.fore_commands]:
            for draw_command in command_set:
                img = draw_command(img)
        return img


def hstack(im1, im2):
    im = Image.new(
        'RGBA', (im1.size[0] + im2.size[0], max(im1.size[1], im2.size[1])))
    im.paste(im1, (0, 0) + im1.size)
    im.paste(im2, (im1.size[0], 0, im1.size[0] + im2.size[0], im2.size[1]))
    return im


def vstack(im1, im2):
    im = Image.new(
        'RGBA', (max(im1.size[0], im2.size[0]), im1.size[1] + im2.size[1]))
    im.paste(im1, (0, 0) + im1.size)
    im.paste(im2, (0, im1.size[1], im2.size[0],  im1.size[1] + im2.size[1]))
    return im


def main():
    import json
    from production import utils

    # V for Visualization ^^
    v = Visualization(width=1000, height=1000)
    # fast track: with single json-entry inside of state
    d = utils.project_root() / 'julie_scratch' / 'allpastmoves_example.json'
    with open(utils.project_root() / 'julie_scratch' / 'allpastmoves_example.json') as datafile:
        state = json.load(datafile)

    v.draw_state(state)
    img = v.get_image()
    img.save(utils.project_root() / 'outputs' / 'state_foo.png')

    # by element
    v = Visualization(width=1000, height=1000)
    v.draw_background()
    p1, p2 = (100, 10), (10, 100)
    v.draw_edge(p1, p2)
    v.draw_point(p1)
    v.draw_point(p2, color=mine_color, size=6)

    # draw map
    d = utils.project_root() / 'maps' / 'official_map_samples' / 'randomMedium.json'
    m = parse_map(json.loads(d.read_text()))
    v.draw_map(m)

    # set punters
    v.set_punters(10)
    v.draw_legend([f'Punter {i}' for i in range(10)])

    # make move
    mv = parse_move(json.loads('{"claim":{"punter":1,"source":34,"target":56}}'))
    v.draw_move(mv, m)
    mv = parse_move(json.loads('{"claim":{"punter":2,"source":82,"target":74}}'))
    v.draw_move(mv, m, me=True)

    mv = parse_move(json.loads('{"option":{"punter":5,"source":82,"target":74}}'))
    v.draw_move(mv, m, me=False)

    mv = parse_move(json.loads('{"pass":{"punter":0}}'))
    v.draw_move(mv, m)

    # save image
    img = v.get_image()
    img.save(utils.project_root() / 'outputs' / 'foo.png')

    # curved visualization: set curved=None for default 
    # (curved for small, straight for big).
    v = Visualization(curved=True)
    d = utils.project_root() / 'maps' / 'official_map_samples' / 'lambda.json'
    m = parse_map(json.loads(d.read_text()))
    v.draw_background()
    v.draw_map(m)
    img = v.get_image()
    img.save(utils.project_root() / 'outputs' / 'lambda.png')


if __name__ == '__main__':
    main()
