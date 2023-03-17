Team Name: JAME Street
Project Name: Brokerage Simulation

NetID and names: Ethan Hersch esh87, Jerry Chen jwc279, Aarav Khanna ak2246, Mohammad Islam mai54

Install opam dependencies (should take a few minutes):
```
opam install ppx_deriving tls-lwt cohttp cohttp-async cohttp-lwt cohttp-lwt-unix cohttp-lwt-jsoo cohttp-top lwt_ppx
```

To use operate and try out the UI
```
dune build exec ./main.exe 
```

To run operate
'''
cd operate
dune exec ./main.exe
'''

Final project for CS3110
