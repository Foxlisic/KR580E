LIB = /usr/share/verilator/include
INC = $(LIB)/verilated.cpp $(LIB)/verilated_threads.cpp
OBJ = obj_dir/Vkr580__ALL.a

all: late
	g++ -Ofast -o kr580e -I$(LIB) kr580e.cc $(INC) $(OBJ) -lSDL2
	./kr580e > kr580.log
late:
	verilator --threads 1 -cc kr580.v
	cd obj_dir && make -f Vkr580.mk
clean:
	rm -rf kr580e kr580e.exe
