# !python3

import json
from pprint import pprint
from collections import deque
import logging; logger = logging.getLogger(__name__)

# Get some statistics for given map.
# Printing stuff out! Not for production.

def bfs(sites, executor, start, end=None, found=None):
    # better alg got from 
    # https://github.com/Vlad-Shcherbina/icfpc2017-tbd/blob/master/production/cpp/stuff.cpp
    if found is None:
        found = { ID : False for ID in sites}
    frontier = [start]
    found[start] = True
    depth = 0
    while frontier:
        depth += 1
        new_frontier = []
        for site in frontier:
            for neighbour in sites[site]:
                if found[neighbour]: continue
                found[neighbour] = True
                executor.do(neighbour, depth)
                new_frontier.append(neighbour)
        frontier = new_frontier


def farthest(sites, site):
    class farthestcounter:
        def __init__(self, sites):
            self.max = -1
        def do(self, site, depth):
            self.max = depth

    counter = farthestcounter(sites)
    bfs(sites, counter, site)
    return counter.max


def print_connected_components(sites, mines):
    # very dirty...
    components = []
    useless_components = []
    class componentcounter:
        def __init__(self, mines):
            self.count = 0
            self.has_mine = False
            self.mines = mines
        def do(self, site, depth):
            self.count += 1
            if site in self.mines:
                self.has_mine = True

    visited = { ID : False for ID in sites }
    for site in visited:
        if visited[site]: continue
        if len(sites[site]) == 0: continue   # excluding zeros
        counter = componentcounter(mines)
        bfs(sites, counter, site, found=visited)
        if counter.has_mine:
            components.append(counter.count)
        else:
            useless_components.append(counter.count)

    logger.info('Connected components: ' + str(components))
    logger.info('Useless components: ' + str(useless_components))
    max_dist = sum_dist = 0
    for site in sites:
        dist = farthest(sites, site)
        sum_dist += dist
        max_dist = max(dist, max_dist)
    logger.info('Average dist: %.2f | Max dist: %d' % (sum_dist / len(sites), max_dist))
    


def main():
    official_maps = {
                        'boston-sparse' : None,
                        'edinburgh-sparse' : None,
                        'gothenburg-sparse' : None,
                        'nara-sparse' : None,
                        'oxford2-sparse-2' : None,
                        'oxford-center-sparse' : None,
                        'randomMedium' : None,
                        'randomSparse' : None,
                        'van-city-sparse' : None
                    }


    for name in official_maps:
        with open('../maps/official_map_samples/' + name + '.json') as mapfile:
            data = json.load(mapfile)
            official_maps[name] = data

    # Data!
    for name, data in official_maps.items():
        datagraph = {'name' : name , 'sites' : {}, 'mines' : []}

        logger.info('\n\n' + name + '\n')
        logger.info(
              'Sites: ' + str(len(data['sites'])) +
              ' | rivers: ' + str(len(data['rivers'])) + 
              ' | mines: ' + str(len(data['mines']))
             )

        for site in data['sites']:
            datagraph['sites'][site['id']] = []

        for river in data['rivers']:
            datagraph['sites'][river['source']].append(river['target'])
            datagraph['sites'][river['target']].append(river['source'])
            
        degree_count = {}
        for site in datagraph['sites'].values():
            d = len(site)
            if d not in degree_count: degree_count[d] = 0
            degree_count[d] += 1

        logger.info(
              'min: ' + str(min(degree_count.keys())) + 
              ' | max: ' + str(max(degree_count.keys())) +
              ' | average: ' + '%.1f' % (
                        sum(d * n for d, n in degree_count.items())
                        / sum(degree_count.values()))
              )

        line1 = '\n['
        line2 = '['
        for x in sorted(degree_count):
            line1 += '{:>5}'.format(x)
            line2 += '{:>5}'.format(degree_count[x])
        line1 += ' ]'
        line2 += ' ]\n'
        logger.info(line1 + '\n' + line2)
        print_connected_components(datagraph['sites'], data['mines'])
        


if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG, format='%(message)s')
    main()