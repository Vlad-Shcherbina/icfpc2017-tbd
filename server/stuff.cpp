#include <pybind11/pybind11.h>
#include <pybind11/stl.h>
namespace py = pybind11;

#include <vector>
#include <map>
#include <algorithm>
#include <numeric>
#include <iostream>

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


struct Scoreboard {
    Scoreboard(const vector<vector<int>> &adj, const vector<int> &mines)
    : adj(adj), mines(mines), dist(adj.size()) {
        for (int m : mines)
            dist[m] = all_dists_from(adj, m);
    }

    void set_futures(int owner, const map<int, int> &futures) {
        assert(futures_by_player.count(owner) == 0);
        futures_by_player[owner] = futures;
    }

    int claimed_by(int u, int v) const {
        pair<int, int> k(min(u, v), max(u, v));
        auto it = claimed_by_map.find(k);
        if (it == end(claimed_by_map)) return -1;
        return it->second;
    }

    int optioned_by(int u, int v) const {
        pair<int, int> k(min(u, v), max(u, v));
        auto it = optioned_by_map.find(k);
        if (it == end(optioned_by_map)) return -1;
        return it->second;
    }

    void set_claims(const vector<vector<int>> &claims) {
        for (auto a : claims) {
            int owner = a[0];
            pair<int, int> k(min(a[1], a[2]), max(a[1], a[2]));
            assert(claimed_by_map.count(k) == 0);
            claimed_by_map[k] = owner;
        }
    }

    void set_options(const vector<vector<int>> &options) {
        for (auto a : options) {
            int owner = a[0];
            pair<int, int> k(min(a[1], a[2]), max(a[1], a[2]));
            assert(optioned_by_map.count(k) == 0);
            optioned_by_map[k] = owner;
        }
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
                if (claimed_by(u, v) != owner && optioned_by(u, v) != owner)
                    continue;

                visited[v] = true;
                worklist.push_back(v);
                result.push_back(v);
            }
        }
        return result;
    }

    int base_score(int owner, map<int, vector<int>> reachables) const {
        int result = 0;
        for (int mine : mines) {
            for (int v : reachables[mine]) {
                int d = dist[mine][v];
                assert(d != -1);
                result += d * d;
            }
        }
        return result;
    }

    vector<vector<int>> totals(int players) const {
        vector<vector<int>> scores;
        scores.resize(players);
        for (int p = 0; p < players; p++) {
            map<int, vector<int>> reachables;
            for (int mine : mines) {
                reachables[mine] = reachable_by_claimed(p, mine);
            }
            scores[p].push_back(base_score(p, reachables));
            if (futures_by_player.find(p) == futures_by_player.end())
                continue;
            for (const auto &f : futures_by_player.at(p)) {
                int d = dist[f.first][f.second];
                auto sites = reachables[f.first];
                if (std::find(sites.begin(), sites.end(), f.second) != sites.end())
                    scores[p].push_back(d * d * d);
                else
                    scores[p].push_back(-d * d * d);
            }
        }
        return scores;
    }

    vector<int> mines;
    vector<vector<int>> adj;
    map<int, map<int, int>> futures_by_player;
    map<pair<int, int>, int> claimed_by_map;
    map<pair<int, int>, int> optioned_by_map;
    vector<vector<int>> dist;  // dist[u][v] is only computed if u is a mine

};


PYBIND11_PLUGIN(stuff) {
    py::module m("stuff");

    m.def("all_dists_from", &all_dists_from);

    py::class_<Scoreboard>(m, "Scoreboard")
        .def(py::init<const vector<vector<int>>&, const vector<int>&>())
        .def(py::init<const Scoreboard&>())
        .def("set_futures", &Scoreboard::set_futures)
        .def("set_claims", &Scoreboard::set_claims)
        .def("set_options", &Scoreboard::set_options)
        //.def("reachable_by_claimed", &Scoreboard::reachable_by_claimed)
        //.def("base_score", &Scoreboard::base_score)
        //.def("claimed_by", &Scoreboard::claimed_by)
        //.def("optioned_by", &Scoreboard::optioned_by)
        //.def_readonly("adj", &Scoreboard::adj)
        //.def_readonly("mines", &Scoreboard::mines)
        //.def_readonly("dist", &Scoreboard::dist)
        .def("totals", &Scoreboard::totals)
    ;

    return m.ptr();
}
