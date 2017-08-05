#include <pybind11/pybind11.h>
#include <pybind11/stl.h>
namespace py = pybind11;

#include <vector>
#include <map>
#include <algorithm>

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

    return m.ptr();
}
