# !python3

"""
Calculates scores for given history of moves. No futures yet.
"""

import json
from typing import List, Set, NamedTuple, Dict  #, Callable, Tuple, Union,
from collections import deque

import logging; logger = logging.getLogger(__name__)

from production.json_format import parse_map, parse_move
from production.bot_interface import *


#========== UNION FIND ALGORITHM FOR CONNECTED COMPONENTS ============

class Union:
    """Union find tree node."""

    def __init__(self, value):
        self.value = value
        self.parent = self
        self.level = 0

    def root(self):
        current = self
        while not current.parent.value == current.value: 
            current = current.parent
        return current

    def connected(self, other):
        return self.root == other.root

    @staticmethod
    def unite(u1, u2):
        root1, root2 = u1.root(), u2.root()
        if root1.level < root2.level:
            root1, root2 = root2, root1
        root2.parent = root1
        root1.level = max(root1.level, root2.level + 1)
        

def union_find(g: Graph) -> Dict[int, int]:
    """Splits graph to connected components.

    Returns { site : rootsite } dictionary, where rootsite is random
    vertex in connected component,  same for all sites in this component.

    [2]----[0]----[1]    [3]

    >>> u = union_find({ 0 : [1, 2], 1 : [0], 2 : [0], 3 : []})
    >>> u[0] == u[1]
    True

    >>> u[0] == u[3]
    False
    """
    unions = { V : Union(V) for V in g }
    for V in g:
        for W in g[V]:
            if not unions[W].connected(unions[V]):
                Union.unite(unions[V], unions[W])
    for V in unions:
        unions[V] = unions[V].root().value
    return unions


#============================== SCORES ===============================

def get_claimed_rivers(g: Graph, 
                   punters: int, 
                   past_moves: List[Move]) -> List[Graph]:
    """
    Return list of graphs, where graph i contains only edges claimed by punter i.
    """
    graphs = []
    for i in range(punters):
        graphs.append({ site : set() for site in g })

    for mv in past_moves:
        if not isinstance(mv, ClaimMove): continue
        graphs[mv.punter][mv.source].add(mv.target)
        graphs[mv.punter][mv.target].add(mv.source)

    return graphs


def score_for_mine(mine: int, 
                   g: Graph,
                   unions: Dict[int, int]) -> int:
    """Given full graphs and punter's connected unions, counts score for one mine.

    Note, that connected unions are calculated not for general graph g,
    but for partial "claimed" punter's graph.
    Connected unions can be found once for all mines, so they are
    passed instead of graph of claimed rivers.

    >>> score_for_mine(0, { 0: [1], 1: [0]}, { 0: [1], 1: [1]})
    1
    """
    found = { site : False for site in g }
    min_path = { site : -1 for site in g }

    queue = deque([mine])
    min_path[mine] = 0
    found[mine] = True
    connected_count = 1  # how many sites reachable by 
    score = 0

    while queue:
        if connected_count == 0: 
            break  # no more sites reachable by punter (stop search early)
        current = queue.popleft()
        if unions[current] == unions[mine]: 
            connected_count -= 1
            score += min_path[current] ** 2
        for neigh in g[current]:
            if not found[neigh]:
                found[neigh] = True
                assert min_path[neigh] == -1, min_path[neigh]
                min_path[neigh] = min_path[current] + 1
                queue.append(neigh)
                if unions[neigh] == unions[mine]:
                    connected_count += 1
    return score


def score_for_graph(claimed: Graph, g: Graph, mines: Set) -> int:
    unions = union_find(claimed)
    return sum(score_for_mine(mine, g, unions) for mine in mines)


def main():
    logtext = '{'
    from production.utils import project_root
    with open(project_root() / 'julie_scratch' / 'allpastmoves_example.json') as datafile:
    #with open('my_allpastmoves.json') as datafile:
        data = json.load(datafile)

    m = parse_map(data['map'])
    punters = data['punters']
    my_id = data['my_id']
    past_moves = list(parse_move(x) for x in data['all_past_moves'])

    # create graphs of claimed rivers
    claimed_rivers = get_claimed_rivers(m.g, punters, past_moves)
    for i, claimed in enumerate(claimed_rivers):
        score = score_for_graph(claimed, m.g, m.mines)
        logtext += '(' + str(i) + ' : ' + str(score) + ') '
    logtext += '}\n'

    # score for individual mine - create connection unions for punter first
    unions = union_find(claimed_rivers[my_id])
    mine = next(iter(m.mines))
    score = score_for_mine(mine, m.g, unions)
    logtext += 'My score for mine ' + str(mine) + ': ' + str(score)
    logger.debug(logtext)


if __name__ == '__main__':
    import doctest
    doctest.testmod()
    logging.basicConfig(level=logging.DEBUG, format='%(message)s')
    main()