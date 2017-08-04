# !python3

# source:
# https://github.com/Vlad-Shcherbina/icfpc2016-tbd/blob/master/production/render.py

from PIL import Image, ImageDraw
from production.bot_interface import Map
#from typing import NamedTuple
from collections import namedtuple

Point = namedtuple('Point', 'x y')
river_color = (120, 140, 220)
site_color = (255, 255, 255)
mine_color = (255, 50, 50)
back_color = (60, 60, 60)

#class Map(NamedTuple):
#    g: Graph
#    mines: Set[int]

    # Dunno if we can rely on these, but it's nice for visualization.
#    site_coords: Dict[int, Tuple[float, float]]

class Visualization:


    def __init__(self, width=800, height=800):
        self.draw_commands = []
        self.width = width
        self.height = height



    def draw_background(self):
        def draw_command(img):
            img = Image.new('RGBA', (self.width, self.height))
            draw = ImageDraw.Draw(img)
            draw.polygon(((0, 0), (0, self.height), (self.width, self.height), (self.width, 0)),
                         fill=back_color)
            return img
        self.draw_commands.append(draw_command)

    def draw_point(self, coord: (float, float), color=site_color, size=4):
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
        self.draw_commands.append(draw_command)

    def draw_edge(self, p1: (float, float), p2: (float, float), color=river_color):
        p1 = self.get_coord(p1)
        p2 = self.get_coord(p2)

        def draw_command(img):
            draw = ImageDraw.Draw(img)
            draw.line((p1, p2), fill=color, width=3)
            return img
        self.draw_commands.append(draw_command)


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
                self.draw_point(m.site_coords[site], color=mine_color, size=4)
            else:
                self.draw_point(m.site_coords[site], color=site_color, size=2)


    def get_x(self, x: float) -> int:
        return int(x)


    def get_y(self, y: float):
        return int(y)


    def get_coord(self, p: (float, float)) -> (int, int):
        return (self.get_x(p[0]), self.get_y(p[1]))


    def adjust_to_map_coords(self, x_min, x_max, y_min, y_max):
        border_coeff = 0.05
        W, H = (x_max - x_min), (y_max - y_min)
        self.width = int(self.height * W / H)  # set same ratio as given
        canvas_width = self.width * (1 - 2 * border_coeff)
        canvas_height = self.height * (1 - 2 * border_coeff)

        def get_x(x):
            return int(x / W * canvas_width + self.width * border_coeff)
        def get_y(y):
            return int(y / H * canvas_height + self.height * border_coeff)

        self.get_x = get_x
        self.get_y = get_y




    def get_image(self, size=None) -> Image:
        if size: 
            self.size = size

        img = Image.new('RGBA', (self.width, self.height))
        for draw_command in self.draw_commands:
            img = draw_command(img)
        return img
        


def main():
    import json
    from production import utils
    from production.json_format import parse_map

    # V for Visualization ^^
    v = Visualization()
    d = utils.project_root() / 'maps' / 'official_map_samples' / 'gothenburg-sparse.json'
    m = parse_map(json.loads(d.read_text()))

    v.draw_background()
    p1, p2 = (10, 20), (400, 100)
    v.draw_edge(p1, p2)
    v.draw_point(p1)
    v.draw_point(p2, color=mine_color, size=6)

    v.draw_map(m)
    img = v.get_image()
    img.save('foo.png')

if __name__ == '__main__':
    main()
