# XProf demo configuration guide

First run python script to generate test data:
```sh
python3 data_gen.py > bigdata.csv
```
Then get dependencies from mix a try running the app:
```sh
mix deps.get
iex -S mix
```
There might be a linker error due to hdr_histogram using obsolete(?) lib: liberl-interface.
The solution for me was to open the appropriate Makefile:
```sh
emacs ./deps/hdr_histogram/c_src/Makefile
```
and change one line:
```Makefile
LDLIBS += -L $(ERL_INTERFACE_LIB_DIR) -lerl_interface -lei
```
to:
```Makefile
LDLIBS += -lei
```
