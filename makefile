
CXX = g++

PURE = Pure.h Common.h List.h

all : ex

ex : ${PURE} examples.cpp 
	${CXX} examples.cpp -std=c++11 -Wall -Wextra -O4 -o ex 

run : ex
	./ex 
