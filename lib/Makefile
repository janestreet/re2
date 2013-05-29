RE2_HOME=libre2/
LIBRE2=$(RE2_HOME)/obj/libre2.a

CXXFLAGS += -I $(RE2_HOME)/re2

all: libre2_stubs.a dllre2_stubs.so

dllre2_stubs.so libre2_stubs.a: stubs.o $(LIBRE2)
	ocamlmklib -oc re2_stubs stubs.o $(LIBRE2) -lstdc++

stubs.o: stubs.cpp stubs.h util.h enum_x_macro.h
	g++ -O2 -DPIC -fPIC -g -pipe -DCAML_NAME_SPACE -Wall -Wextra -Wunused -Werror \
	-Wcast-align -Wpointer-arith -Wfloat-equal -Wlarger-than-512 -Wredundant-decls \
	-Winit-self -Wformat=2 -Wuninitialized -Winline -Wconversion -I. -I../../../include \
	-I$(RE2_HOME) -I$(ocaml-version-selected-include-path) -c stubs.cpp

#stubs.o: %.o: %.cpp %.h

.PHONY: $(LIBRE2)
$(LIBRE2):
	make -C $(RE2_HOME) CXXFLAGS='-Wall -O3 -g -pthread -fPIC' obj/libre2.a

