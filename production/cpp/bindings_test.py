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
    board.claim_river(42, 3, 2)
    assert sorted(board.reachable_by_claimed(42, 1)) == [1, 2, 3]


if __name__ == '__main__':
    import sys, pytest
    pytest.main([__file__] + sys.argv[1:])
