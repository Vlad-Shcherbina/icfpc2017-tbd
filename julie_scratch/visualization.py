# !python3

# source:
# https://github.com/Vlad-Shcherbina/icfpc2016-tbd/blob/master/production/render.py

from PIL import Image, ImageDraw
from production.bot_interface import *
from collections import namedtuple
from typing import Tuple
from random import randrange

river_color = (111, 111, 111)
site_color = (255, 255, 255)
mine_color = (255, 50, 50)
back_color = (60, 60, 60)
text_color = (255, 255, 255)

mine_size = 4
site_size = 2

punter_colors = [(255, 245, 80),
                 (80, 255, 245),
                 (255, 80, 245),
                 (255, 120, 110),
                 (145, 155, 155),
                 (125, 255, 105)]

DEFAULT_CLRS = 6

def set_punter_colors(n):
    if n <= DEFAULT_CLRS: return
    punter_colors[DEFAULT_CLRS:] = []
    threshold = 100 * 3 / n

    def colors_differ(clr1, clr2):
        return sum(abs(clr1[i] - clr2[i]) for i in range(3)) > threshold

    def randcolor(): return randrange(1, 6) * 20 + 150

    for _ in range(n - DEFAULT_CLRS):
        while True:
            color = (randcolor(), randcolor(), randcolor())
            if all(colors_differ(color, old) for old in punter_colors):
                punter_colors.append(color)
                break


class Visualization:
    """ Draws one image of game state or a pile of them.

    Example:
    img = Visualization(800, 800).draw_state(somegamestate).get_image()
    """

    def __init__(self, width=800, height=800):
        # while drawing map width will be reset.
        self.width = width
        self.height = height

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


    def draw_point(self, coord: Tuple[float, float], color=site_color, size=site_size):
        coord = self.get_coord(coord)
        def draw_command(img):
            draw = ImageDraw.Draw(img)
            draw.ellipse(
                    (
                        coord[0] - size/2, 
                        coord[1] - size/2,
                        coord[0] + size/2,
                        coord[1] + size/2
                    ),
                    color,
                    color
                )
            return img
        self.site_commands.append(draw_command)


    def draw_site(self,  coord: Tuple[float, float]):
        self.draw_point(coord, color=site_color, size=site_size) # just in case

    def draw_mine(self, coord: Tuple[float, float]):
        self.draw_point(coord, color=mine_color, size=mine_size) # just in case


    def draw_edge(
            self,
            p1: Tuple[float, float],
            p2: Tuple[float, float],
            color=river_color):
        p1 = self.get_coord(p1)
        p2 = self.get_coord(p2)

        def draw_command(img):
            draw = ImageDraw.Draw(img)
            draw.line((p1, p2), fill=color, width=3)
            return img
        self.river_commands.append(draw_command)


    def draw_text(self, p: Tuple[float, float], text: str, color=text_color):
        def draw_command(img):
            draw = ImageDraw.Draw(img)
            draw.text(p, text, fill=color)
            return img
        self.fore_commands.append(draw_command)


    def draw_legend(self, punters=DEFAULT_CLRS, p: Tuple[float, float]=None):
        if not p:
            p = (30, self.height - 30 - punters * 15)
        for i, punter in enumerate(punter_colors[:punters]):
            self.draw_text(p, "Punter " + str(i), color=punter)
            p = (p[0], p[1] + 15)


    def draw_move(self, mv: Move, m: Map):
        if isinstance(mv, PassMove): return
        assert isinstance(mv, ClaimMove)
        if len(punter_colors) <= mv.punter: set_punter_colors(punter + 1)
        self.draw_edge(m.site_coords[mv.source],
                       m.site_coords[mv.target],
                       punter_colors[mv.punter])


    def draw_map(self, m: Map):
        self.adjust_to_map_coords(
                min(p[0] for p in m.site_coords.values()),
                max(p[0] for p in m.site_coords.values()),
                min(p[1] for p in m.site_coords.values()),
                max(p[1] for p in m.site_coords.values())
                )

        for source in m.g:
            for target in m.g[source]:
                self.draw_edge(m.site_coords[source], m.site_coords[target])
        for site in m.site_coords:
            if site in m.mines:
                self.draw_mine(m.site_coords[site])
            else:
                self.draw_site(m.site_coords[site])


    def get_x(self, x: float) -> int:
        return int(x)

    def get_y(self, y: float) -> int:
        return int(y)

    def get_coord(self, p: Tuple[float, float]) -> Tuple[int, int]:
        return (self.get_x(p[0]), self.get_y(p[1]))


    def adjust_to_map_coords(self, x_min, x_max, y_min, y_max):
        border_coeff = 0.05
        W, H = (x_max - x_min), (y_max - y_min)
        self.width = int(self.height * W / H)  # set same ratio as given
        canvas_width = self.width * (1 - 2 * border_coeff)
        canvas_height = self.height * (1 - 2 * border_coeff)

        def get_x(x):
            return int((x - x_min) / W * canvas_width + self.width * border_coeff)
        def get_y(y):
            return int((y - y_min) / H * canvas_height + self.height * border_coeff)

        self.get_x = get_x
        self.get_y = get_y



    def get_image(self) -> Image.Image:
        img = Image.new('RGBA', (self.width, self.height))
        for command_set in [self.back_commands,
                            self.river_commands,
                            self.site_commands,
                            self.fore_commands]:
            for draw_command in command_set:
                img = draw_command(img)
        return img
        


def main():
    import json
    from production import utils
    from production.json_format import parse_map, parse_move

    # V for Visualization ^^
    v = Visualization(width=1000, height=1000)
    v.draw_background()
    p1, p2 = (10, 20), (400, 100)
    v.draw_edge(p1, p2)
    v.draw_point(p1)
    v.draw_point(p2, color=mine_color, size=6)

    # draw map
    d = utils.project_root() / 'maps' / 'official_map_samples' / 'gothenburg-sparse.json'
    m = parse_map(json.loads(d.read_text()))
    v.draw_map(m)

    # set punters
    set_punter_colors(10)
    v.draw_legend(10)

    # make move
    mv = parse_move(json.loads('{"claim":{"punter":1,"source":7,"target":1}}'))
    v.draw_move(mv, m)

    # save image
    img = v.get_image()
    img.save('foo.png')

if __name__ == '__main__':
    main()
