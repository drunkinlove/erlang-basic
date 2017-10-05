-module(fibo).

-export([fibo/1]).

fibo(N) when N > 0 -> auxfibo(N, 0, 1).

auxfibo(0, F1, _F2) -> F1;
auxfibo(N, F1, F2) -> auxfibo (N-1, F2, F1+F2).