parallel_singlecore_file = parallel_singlecore.ml
parallel_multicore_file  = parallel_multicore.ml

ocaml_switch_multicore   =5.2.0+dev10-2024-05-02

parallel_module := $(shell \
	if [ $(shell ocamlc --version) = $(ocaml_switch_multicore) ]; \
	then echo $(parallel_multicore_file); \
	else echo $(parallel_singlecore_file); fi \
)

all:
	cp $(parallel_module) parallel.ml

clean:
	rm -f parallel.ml