# **Brokerage Simulation**
## Team Name: JAME Street


**NetID and names:** Ethan Hersch esh87, Jerry Chen jwc279, Aarav Khanna ak2246, Mohammad Islam mai54

Install opam dependencies (should take a few minutes):
```
opam install ppx_deriving tls-lwt cohttp cohttp-async cohttp-lwt cohttp-lwt-unix cohttp-lwt-jsoo cohttp-top lwt_ppx
```

To use operate and try out the UI
```
dune build
make play
```

To view stock prices based on their ticker names
```
dune build
make view_stock
```

Final project for CS3110