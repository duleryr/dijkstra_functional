#include <iostream>
#include <vector>
#include <numeric>
#include <algorithm>
#include <functional>
#include <deque>
#include <cassert>

auto prim = [] (auto t) {return std::get<0>(t);};
auto segu = [] (auto t) {return std::get<1>(t);};
auto terc = [] (auto t) {return std::get<2>(t);};

auto origen = [] (auto t) {return std::get<0>(t);};
auto destino = [] (auto t) {return std::get<1>(t);};
auto costo = [] (auto t) {return std::get<2>(t);};

auto sacar = [] (auto list, auto x) 
{list->erase(std::remove_if(list->begin(), list->end(), [x] (auto i) {return (i == x);}), list->end());};

auto unicos (auto tab) {
    if (tab.size() == 0) {
        return std::deque<int>();
    }
    auto x = tab[0];
    tab.pop_front();
    sacar(&tab, x);
    auto res = unicos(tab);
    res.push_front(x);
    return res;
}

auto vertices (auto tab) {
    if (tab.size() == 0) {
        return std::deque<int>();
    } 
    std::tuple<int, int, int> x = tab[0];
    tab.pop_front();
    auto res = vertices(tab);
    res.push_front(destino(x));
    res.push_front(origen(x));
    return unicos(res);
};

auto computable = [] (auto cs, auto ncs, std::tuple<int,int,int> t) -> bool
{return std::find(cs.begin(), cs.end(), origen(t)) != cs.end() && std::find(ncs.begin(), ncs.end(), destino(t)) != ncs.end();};

auto computables = [] (auto cs, auto ncs, auto tab)
{tab.erase(std::remove_if(tab.begin(), tab.end(), [cs, ncs] (auto t) {return !computable(cs, ncs, t);}), tab.end()); return tab;};

auto costoini (int a, auto tab) {
    if (tab.size() == 0) {
        return std::deque<std::tuple<int,int,int>>();
    }
    auto x = tab[0];
    tab.pop_front();
    if (x == a) {
        auto res = costoini(a, tab);
        res.push_front(std::make_tuple(a,0,a));
        return res;
    } else {
        auto res = costoini(a, tab);
        res.push_front(std::make_tuple(x,1000,-1));
        return res;
    }
}

int getcosto (auto tab, int o) {
    if (tab.size() == 0) {
        return 0;
    }
    auto x = tab[0];
    tab.pop_front();
    if (origen(x) == o) {
        return destino(x);
    } else {
        return getcosto(tab, o);
    }
}

auto difver = [] (auto t1, auto t2) -> bool
{return origen(t1) != origen(t2);};

auto updatecos(auto tab, std::tuple<int,int,int> t) {
    auto a = origen(t);
    auto b = destino(t);
    auto c = costo(t);
    if (getcosto(tab, a) + c < getcosto(tab, b)) {
        tab.erase(std::remove_if(tab.begin(), tab.end(), [b] (auto t) {return !difver(t, std::make_tuple(b,0,0));}), tab.end());
        auto res = tab;
        res.push_front(std::make_tuple(b, getcosto(tab, a) + c, a));
        return res;
    } else {
        return tab;
    }
}

auto updatetodos(auto cost, auto tab) {
    if (tab.size() == 0) {
        return cost;
    }
    auto c = tab[0];
    tab.pop_front();
    return updatetodos(updatecos(cost, c), tab);
}

auto menordista = [] (auto t1, auto t2)
{return (destino(t1) < destino(t2)) ? t1 : t2;};

int getNewBest(auto cost, auto vnc) {
    auto filter = [vnc] (auto x) {return std::find(vnc.begin(), vnc.end(), prim(x)) == vnc.end();};
    cost.erase(std::remove_if(cost.begin(), cost.end(), filter), cost.end());
    auto a = cost[0];
    cost.pop_front();
    auto res = std::accumulate(cost.begin(), cost.end(), a, menordista);
    return prim(res);
}

auto dijaux (auto as, auto t) {
    if (segu(t).size() == 0) {
        return terc(t);
    }
    auto vcs = prim(t);
    auto vncs = segu(t);
    auto csts = terc(t);
    auto nuevocsts = updatetodos(csts, computables(vcs, vncs, as));
    auto m = getNewBest(nuevocsts, vncs);
    vcs.push_front(m);
    sacar(&vncs, m);
    auto arg = std::make_tuple(vcs,vncs,nuevocsts);
    return dijaux(as, arg);
}

auto dijkstra (auto as, int init) {
    std::deque<int> t1(1, init);
    std::deque<int> t2 = vertices(as);
    sacar(&t2, init);
    std::deque<std::tuple<int,int,int>> t3 = costoini(init, vertices(as));
    auto arg = std::make_tuple(t1,t2,t3);
    return dijaux(as, arg);
}

void testSacar() {
    std::vector<int> vec(5);
    std::iota(vec.begin(), vec.end(), 2);
    sacar(&vec, 3);
    assert(vec.size() == 4);
    assert(vec[0] == 2);
    assert(vec[1] == 4);
    assert(vec[2] == 5);
    assert(vec[3] == 6);
}

void testUnicos() {
    std::deque<int> vec = {1, 1, 2, 3, 1, 4, 3, 4, 5, 5};
    std::deque<int> univec = unicos(vec);
    assert(univec.size() == 5);
    assert(univec[0] == 1);
    assert(univec[1] == 2);
    assert(univec[2] == 3);
    assert(univec[3] == 4);
    assert(univec[4] == 5);
}

void testCostoIni() {
    std::deque<int> array = {1, 2, 3, 4};
    auto tab = costoini(2, array);
    assert(prim(tab[0]) == 1);
    assert(segu(tab[0]) == 1000);
    assert(terc(tab[0]) == -1);
    assert(prim(tab[1]) == 2);
    assert(segu(tab[1]) == 0);
    assert(terc(tab[1]) == 2);
    assert(prim(tab[2]) == 3);
    assert(segu(tab[2]) == 1000);
    assert(terc(tab[2]) == -1);
    assert(prim(tab[3]) == 4);
    assert(segu(tab[3]) == 1000);
    assert(terc(tab[3]) == -1);
}

void testVertices () {
    std::deque<std::tuple<int, int, int>> graph1;
    graph1.push_back(std::make_tuple(1,2,10));
    graph1.push_back(std::make_tuple(1,3,100));
    graph1.push_back(std::make_tuple(2,4,50));
    graph1.push_back(std::make_tuple(4,3,10));
    graph1.push_back(std::make_tuple(1,5,30));
    graph1.push_back(std::make_tuple(5,4,60));
    graph1.push_back(std::make_tuple(5,3,60));
    std::deque<int> a = vertices(graph1);
    assert(a.size() == 5);
    assert(a[0] == 1);
    assert(a[1] == 2);
    assert(a[2] == 3);
    assert(a[3] == 4);
    assert(a[4] == 5);
}

void testGetCosto() {
    std::deque<std::tuple<int, int, int>> graph1;
    graph1.push_back(std::make_tuple(1,2,10));
    graph1.push_back(std::make_tuple(1,3,100));
    graph1.push_back(std::make_tuple(2,4,50));
    graph1.push_back(std::make_tuple(4,3,10));
    graph1.push_back(std::make_tuple(1,5,30));
    graph1.push_back(std::make_tuple(5,4,60));
    graph1.push_back(std::make_tuple(5,3,60));

    assert(getcosto(graph1, 1) == 2);
    assert(getcosto(graph1, 2) == 4);
    assert(getcosto(graph1, 4) == 3);
    assert(getcosto(graph1, 5) == 4);
}

void testComputable (){
    std::deque<int> c = {1, 2, 3};
    std::deque<int> nc = {4, 5};
    assert(computable(c, nc, std::make_tuple(1, 1, 10)) == false);
    assert(computable(c, nc, std::make_tuple(1, 2, 10)) == false);
    assert(computable(c, nc, std::make_tuple(1, 3, 10)) == false);
    assert(computable(c, nc, std::make_tuple(1, 4, 10)) == true);
    assert(computable(c, nc, std::make_tuple(1, 5, 15)) == true);
    assert(computable(c, nc, std::make_tuple(2, 3, 10)) == false);
    assert(computable(c, nc, std::make_tuple(2, 4, 10)) == true);
    assert(computable(c, nc, std::make_tuple(2, 5, 10)) == true);
    assert(computable(c, nc, std::make_tuple(3, 3, 10)) == false);
    assert(computable(c, nc, std::make_tuple(3, 6, 10)) == false);
    assert(computable(c, nc, std::make_tuple(3, 2, 10)) == false);

    std::deque<int> cs = {1, 2, 3, 4};
    std::deque<int> ncs = {2, 4, 5, 6};
    assert(computable(cs, ncs, std::make_tuple(1, 2, 10)) == true);
    assert(computable(cs, ncs, std::make_tuple(1, 3, 10)) == false);
    assert(computable(cs, ncs, std::make_tuple(2, 4, 10)) == true);
    assert(computable(cs, ncs, std::make_tuple(4, 3, 10)) == false);
}

void testComputables() {
    std::deque<int> cs = {1, 2, 3, 4};
    std::deque<int> ncs = {2, 4, 5, 6};
    std::deque<std::tuple<int, int, int>> graph;
    graph.push_back(std::make_tuple(1,2,10));
    graph.push_back(std::make_tuple(1,3,10));
    graph.push_back(std::make_tuple(2,4,10));
    graph.push_back(std::make_tuple(4,3,10));
    graph = computables(cs, ncs, graph);
    assert(graph.size() == 2);
    assert(std::get<0>(graph[0]) == 1);
    assert(std::get<1>(graph[0]) == 2);
    assert(std::get<2>(graph[0]) == 10);
    assert(std::get<0>(graph[1]) == 2);
    assert(std::get<1>(graph[1]) == 4);
    assert(std::get<2>(graph[1]) == 10);
}

void testGetNewBest() {
    std::deque<std::tuple<int, int, int>> graph1;
    graph1.push_back(std::make_tuple(1,2,10));
    graph1.push_back(std::make_tuple(1,3,100));
    graph1.push_back(std::make_tuple(2,4,50));
    graph1.push_back(std::make_tuple(4,3,10));
    graph1.push_back(std::make_tuple(1,5,30));
    graph1.push_back(std::make_tuple(5,4,60));
    graph1.push_back(std::make_tuple(5,3,60));
    std::deque<int> vnc = {1,2,3,4,5};
    assert(getNewBest(graph1, vnc) == 1);
    vnc.pop_front();
    assert(getNewBest(graph1, vnc) == 5);
    vnc.pop_back();
    assert(getNewBest(graph1, vnc) == 4);
    vnc.pop_back();
    assert(getNewBest(graph1, vnc) == 2);
}

void testUpdateCos() {
    std::deque<std::tuple<int, int, int>> graph1;
    graph1.push_back(std::make_tuple(1,2,10));
    graph1.push_back(std::make_tuple(1,3,100));
    graph1.push_back(std::make_tuple(2,4,50));
    graph1.push_back(std::make_tuple(4,3,10));
    graph1.push_back(std::make_tuple(1,5,30));
    graph1.push_back(std::make_tuple(5,4,60));
    graph1.push_back(std::make_tuple(5,3,60));
    std::tuple<int,int,int> t = std::make_tuple(1,10,1);
    assert(updatecos(graph1, t) == graph1);
    std::tuple<int,int,int> u = std::make_tuple(1,4,0);
    assert(prim(updatecos(graph1, u)[0]) == 4);
    assert(segu(updatecos(graph1, u)[0]) == 2);
    assert(terc(updatecos(graph1, u)[0]) == 1);
    std::tuple<int,int,int> v = std::make_tuple(1,5,0);
    assert(updatecos(graph1, v).size() == 6);
    assert(prim(updatecos(graph1, v)[0]) == 5);
    assert(segu(updatecos(graph1, v)[0]) == 2);
    assert(terc(updatecos(graph1, v)[0]) == 1);
}

void testDifver() {
    assert(difver(std::make_tuple(1,2,3), std::make_tuple(1,4,3)) == false);
    assert(difver(std::make_tuple(1,2,3), std::make_tuple(2,2,3)) == true);
}

void testMenordista() {
    assert(std::get<0>(menordista(std::make_tuple(2,2,3), std::make_tuple(1,4,3))) == 2);
    assert(std::get<1>(menordista(std::make_tuple(2,2,3), std::make_tuple(1,4,3))) == 2);
    assert(std::get<2>(menordista(std::make_tuple(2,2,3), std::make_tuple(1,4,3))) == 3);
    assert(std::get<0>(menordista(std::make_tuple(2,5,3), std::make_tuple(1,4,3))) == 1);
    assert(std::get<1>(menordista(std::make_tuple(2,5,3), std::make_tuple(1,4,3))) == 4);
    assert(std::get<2>(menordista(std::make_tuple(2,5,3), std::make_tuple(1,4,3))) == 3);
}

auto print_graph = [] (auto graph) 
{std::for_each(graph.begin(), graph.end(), [] (auto i) {std::cout << prim(i) << " " << segu(i) << " " << terc(i) << std::endl;});};

void testDijaux() {
    std::deque<std::tuple<int, int, int>> graph1;
    graph1.push_back(std::make_tuple(1,2,10));
    graph1.push_back(std::make_tuple(1,3,100));
    graph1.push_back(std::make_tuple(2,4,50));
    graph1.push_back(std::make_tuple(4,3,10));
    graph1.push_back(std::make_tuple(1,5,30));
    graph1.push_back(std::make_tuple(5,4,60));
    graph1.push_back(std::make_tuple(5,3,60));

    std::deque<int> t1 = {1};
    std::deque<int> t2 = {2,3,4,5};
    auto t = std::make_tuple(t1,t2,costoini(1, vertices(graph1)));
    //print_graph(dijaux(graph1, t));
}

void unitTests() {
    testSacar();
    testUnicos();
    testComputable();
    testComputables();
    testUpdateCos();
    testCostoIni();
    testGetCosto();
    testGetNewBest();
    testDifver();
    testMenordista();
    testVertices();
    testDijaux();
}

int main() {
    unitTests();
    std::deque<std::tuple<int, int, int>> graph1;
    graph1.push_back(std::make_tuple(1,2,10));
    graph1.push_back(std::make_tuple(1,3,100));
    graph1.push_back(std::make_tuple(2,4,50));
    graph1.push_back(std::make_tuple(4,3,10));
    graph1.push_back(std::make_tuple(1,5,30));
    graph1.push_back(std::make_tuple(5,4,60));
    graph1.push_back(std::make_tuple(5,3,60));
    std::deque<std::tuple<int, int, int>> graph2;
    graph2.push_back(std::make_tuple(1,2,2));
    graph2.push_back(std::make_tuple(1,4,1));
    graph2.push_back(std::make_tuple(2,3,3));
    graph2.push_back(std::make_tuple(2,5,10));
    graph2.push_back(std::make_tuple(3,1,4));
    graph2.push_back(std::make_tuple(3,4,5));
    graph2.push_back(std::make_tuple(4,3,2));
    graph2.push_back(std::make_tuple(4,5,2));
    graph2.push_back(std::make_tuple(4,6,8));
    graph2.push_back(std::make_tuple(4,7,4));
    graph2.push_back(std::make_tuple(5,7,6));
    graph2.push_back(std::make_tuple(7,6,1));
    std::cout << "dijkstra graph1 from 1:" << std::endl;    
    print_graph(dijkstra(graph1, 1));
    std::cout << "dijkstra graph1 from 2:" << std::endl;    
    print_graph(dijkstra(graph1, 2));
    std::cout << "dijkstra graph1 from 3:" << std::endl;    
    print_graph(dijkstra(graph1, 3));
    std::cout << "dijkstra graph2 from 1:" << std::endl;    
    print_graph(dijkstra(graph2, 1));
}
