from production.cpp import stuff as cpp


def make_adj():
    # 0 - 1 - 2
    #      \ /
    #       3      4
    edges = [
        (0, 1),
        (1, 2),
        (1, 3),
        (2, 3),
    ]
    adj = [[] for _ in range(5)]
    for u, v in edges:
        adj[u].append(v)
        adj[v].append(u)
    return adj


def test_all_dists_from():
    assert cpp.all_dists_from(make_adj(), 0) == [0, 1, 2, 2, -1]
    assert cpp.all_dists_from(make_adj(), 2) == [2, 1, 0, 1, -1]
    assert cpp.all_dists_from(make_adj(), 4) == [-1, -1, -1, -1, 0]


def test_stuff():
    board = cpp.Board(make_adj(), [])
    assert board.reachable_by_claimed(42, 1) == [1]

    board.claim_river(43, 0, 1)
    assert board.reachable_by_claimed(42, 1) == [1]

    board.claim_river(42, 1, 2)
    board.option_river(42, 3, 2)
    assert sorted(board.reachable_by_claimed(42, 1)) == [1, 2, 3]

    board.pack = {1: 2}
    assert isinstance(board.pack, dict)
    assert board.pack == {1: 2}


def test_reach_prob():
    board = cpp.Board(make_adj(), [0, 1, 2, 3, 4])
    board.set_futures(42, {0: 3})
    board.claim_river(42, 0, 1)
    #board.claim_river(43, 1, 2)

    cut_prob = {
        (0, 1): 0.8,
        (1, 0): 0.8,
        (1, 2): 0.8,
        (2, 1): 0.8,
        (1, 3): 0.8,
        (3, 1): 0.8,
        (2, 3): 0.8,
        (3, 2): 0.8,
    }

    rp = cpp.ReachProb(board, 42, 0, cut_prob, False)

    print(rp.get_cut_prob_grad())
    print(rp.reach_prob)

    #assert False


if __name__ == '__main__':
    import sys, pytest
    pytest.main([__file__] + sys.argv[1:])
