#include <pybind11/pybind11.h>
#include <pybind11/stl.h>
namespace py = pybind11;

#include <vector>
#include <map>
#include <algorithm>
#include <numeric>

#include "pretty_printing.h"
#include "debug.h"

using namespace std;


// -1 if unreachable
vector<int> all_dists_from(const vector<vector<int>> &adj, int start) {
    vector<int> d(adj.size(), -1);
    d[start] = 0;
    int step = 1;
    vector<int> frontier = {start};
    while (!frontier.empty()) {
        vector<int> new_frontier;
        for (int u : frontier) {
            for (int v : adj[u]) {
                if (d[v] == -1) {
                    d[v] = step;
                    new_frontier.push_back(v);
                }
            }
        }
        frontier = move(new_frontier);
        step++;
    }
    return d;
}


struct Board {
    Board(const vector<vector<int>> &adj, const vector<int> &mines)
    : adj(adj), mines(mines), dist(adj.size()) {
        for (int m : mines)
            dist[m] = all_dists_from(adj, m);
    }

    void claim_river(int owner, int u, int v) {
        pair<int, int> k(min(u, v), max(u, v));
        assert(claimed_by.count(k) == 0);
        claimed_by[k] = owner;
    }

    vector<int> reachable_by_claimed(int owner, int start) const {
        vector<bool> visited(adj.size(), false);
        visited[start] = true;
        vector<int> result = {start};
        vector<int> worklist = {start};
        while (!worklist.empty()) {
            int u = worklist.back();
            worklist.pop_back();
            for (int v : adj[u]) {
                if (visited[v])
                    continue;
                pair<int, int> k(min(u, v), max(u, v));
                auto it = claimed_by.find(k);
                if (it == end(claimed_by) || it->second != owner)
                    continue;

                visited[v] = true;
                worklist.push_back(v);
                result.push_back(v);
            }
        }
        return result;
    }

    int base_score(int owner) const {
        int result = 0;
        for (int mine : mines) {
            for (int v : reachable_by_claimed(owner, mine)) {
                int d = dist[mine][v];
                assert(d != -1);
                result += d * d;
            }
        }
        return result;
    }

    vector<int> mines;
    vector<vector<int>> adj;
    map<pair<int, int>, int> claimed_by;
    vector<vector<int>> dist;  // dist[u][v] is only computed if u is a mine
};


class DSU {
public:
    vector<int> up;
    vector<int> rank;

    DSU(int size) : up(size), rank(size, 0) {
        iota(begin(up), end(up), 0);
    }

    int find(int x) {
        if (up.at(x) != x) {
            up.at(x) = find(up.at(x));
        }
        return up.at(x);
    }

    void merge(int x, int y) {
        int x_root = find(x);
        int y_root = find(y);

        assert(x_root != y_root);

        if (rank.at(x_root) < rank.at(y_root)) {
            up.at(x_root) = y_root;
        } else if (rank.at(x_root) > rank.at(y_root)) {
            up.at(y_root) = x_root;
        } else {
            up.at(y_root) = x_root;
            rank.at(x_root)++;
        }
    }
};


// https://en.wikipedia.org/wiki/Topological_sorting#Depth-first_search
struct TopologicalSort {
    TopologicalSort(const vector<vector<int>> &adj)
    : adj(adj), mark(adj.size(), 0) {
        for (int i = 0; i < mark.size(); i++) {
            if (mark[i] == 0)
                visit(i);
        }
        reverse(begin(order), end(order));
    }

    void visit(int n) {
        if (mark[n] == 2)
            return;
        if (mark[n] == 1)
            assert(false && "not a DAG");
        mark[n] = 1;
        for (int m : adj[n])
            visit(m);
        mark[n] = 2;
        order.push_back(n);
    }

    vector<int> mark;  // 0 - unmarked, 1 - temporary, 2 - permanent
    const vector<vector<int>> &adj;
    vector<int> order;
};

vector<double> residual_products(const vector<double> &xs) {
    // TODO: linear time
    vector<double> result(xs.size(), 1.0);
    for (int i = 0; i < xs.size(); i++) {
        for (int j = 0; j < xs.size(); j++) {
            if (i != j)
                result[i] *= xs[j];
        }
    }
    return result;
}


struct ReachProb {
    ReachProb(const Board &board, int owner, int mine, double cut_prob) {
        DSU dsu(board.adj.size());
        for (auto kv : board.claimed_by) {
            if (kv.second == owner) {
                dsu.merge(kv.first.first, kv.first.second);
            }
        }
        // debug2(owner, mine);
        // debug(dsu.up);

        vector<vector<int>> cluster_adj(board.adj.size());
        int u = 0;
        for (const auto &a : board.adj) {
            for (int v : a) {
                pair<int, int> k(min(u, v), max(u, v));
                if (board.claimed_by.count(k) == 0)
                    cluster_adj[dsu.find(u)].push_back(dsu.find(v));
            }
            u++;
        }

        vector<int> cluster_dist = all_dists_from(cluster_adj, dsu.find(mine));
        // debug(cluster_dist);

        vector<vector<int>> dag_cluster_adj(board.adj.size());
        u = 0;
        for (const auto &a : cluster_adj) {
            for (int v : a) {
                if (make_pair(cluster_dist[u], u) < make_pair(cluster_dist[v], v))
                    dag_cluster_adj[u].push_back(v);
            }
            u++;
        }
        // debug(dag_cluster_adj);
        for (auto &a : dag_cluster_adj) {
            set<int> t(begin(a), end(a));
            a = vector<int>(begin(t), end(t));
        }
        // debug(dag_cluster_adj);

        auto order = TopologicalSort(dag_cluster_adj).order;
        // debug(order);

        vector<vector<pair<int, int>>> incoming_edges_by_cluster(board.adj.size());
        u = 0;
        for (const auto &a : board.adj) {
            const auto &dca = dag_cluster_adj[dsu.find(u)];
            for (int v : a) {
                if (!binary_search(begin(dca), end(dca), dsu.find(v)))
                    continue;
                pair<int, int> k(min(u, v), max(u, v));
                if (board.claimed_by.count(k) == 0)
                    incoming_edges_by_cluster[dsu.find(v)].emplace_back(u, v);
            }
            u++;
        }
        // debug(incoming_edges_by_cluster);

        vector<double> p(board.adj.size(), -42.0);

        for (int cluster : order) {
            const auto &incoming_edges = incoming_edges_by_cluster[cluster];
            if (cluster == dsu.find(mine)) {
                p[cluster] = 1.0;
            } else {
                double f = 1.0;
                for (auto uv : incoming_edges) {
                    assert(p[dsu.find(uv.first)] != -42.0);
                    f *= 1 - p[dsu.find(uv.first)] * (1 - cut_prob);
                }
                p[cluster] = 1 - f;
            }
        }

        // debug(p);

        vector<int> reward(board.adj.size());
        for (int i = 0; i < reward.size(); i++)
            reward[i] = board.dist[mine][i] * board.dist[mine][i];

        // debug(reward);

        vector<double> p_grad(board.adj.size());
        double expected = 0.0;
        for (int i = 0; i < reward.size(); i++) {
            expected += reward[i] * p[dsu.find(i)];
            p_grad[dsu.find(i)] += reward[i];
        }
        // debug(expected);

        // debug(p_grad);

        map<pair<int, int>, double> cut_prob_grad;
        reverse(begin(order), end(order));

        for (int cluster : order) {
            if (cluster == dsu.find(mine))
                continue;
            const auto &incoming_edges = incoming_edges_by_cluster[cluster];

            vector<double> xs;
            for (auto uv : incoming_edges) {
                xs.push_back(1 - p[dsu.find(uv.first)] * (1 - cut_prob));
            }
            vector<double> rxs = residual_products(xs);

            int i = 0;
            for (auto uv : incoming_edges) {
                cut_prob_grad[uv] -= p[dsu.find(uv.first)] * rxs[i] * p_grad[cluster];
                p_grad[dsu.find(uv.first)] += (1 - cut_prob) * rxs[i] * p_grad[cluster];
                i++;
            }
        }

        // debug(cut_prob_grad);
        // debug(p_grad);

        this->cut_prob_grad = cut_prob_grad;
    }

    map<pair<int, int>, double> get_cut_prob_grad() const {
        map<pair<int, int>, double> result;
        for (auto kv : cut_prob_grad) {
            int u = kv.first.first;
            int v = kv.first.second;
            pair<int, int> k(min(u, v), max(u, v));
            result[k] += kv.second;
        }
        return result;
    }

    map<pair<int, int>, double> cut_prob_grad;
};


PYBIND11_PLUGIN(stuff) {
    py::module m("stuff");

    m.def("all_dists_from", &all_dists_from);

    py::class_<Board>(m, "Board")
        .def(py::init<const vector<vector<int>>&, const vector<int>&>())
        .def(py::init<const Board&>())
        .def("claim_river", &Board::claim_river)
        .def("reachable_by_claimed", &Board::reachable_by_claimed)
        .def("base_score", &Board::base_score)
    ;

    py::class_<ReachProb>(m, "ReachProb")
        .def(py::init<const Board&, int, int, double>())
        .def("get_cut_prob_grad", &ReachProb::get_cut_prob_grad)
    ;

    return m.ptr();
}
