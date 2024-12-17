NAME = kal
CFILE = kal
CPP = clang++-20

# -stdlib=libc++ : for std::print
CFLAGS0 = -g3 -pthread -fexperimental-library -std=c++26 # -stdlib=libc++

# optimizations use the SIMD/AVX
CFLAGS1 = -O3 -march=native -mavx2 

# safety 
CFLAGS2 = -Wall -Wextra -pedantic -fsanitize=address # -Werror

# llvm 
CFLAG3 = llvm-config-20 --cxxflags

CFLAGS = $(CFLAGS0) $(CFLAGS1) $(CFLAGS2)

# -static-libstdc++ : to prevent ASAN: alloc-dealloc-mismatch on exception
LDLIBS_LLVM = `llvm-config-20 --ldflags --system-libs --libs core orcjit native`

# https://stackoverflow.com/a/62528308/678491
# https://stackoverflow.com/a/30468391/678491
LDFLAGS = -Wl,--export-dynamic # -rdynamic # -Xlinker --export-dynamic

LDLIBS = $(LDLIBS_LLVM) $(LDFLAGS) -static-libstdc++ 

# get the top most directory
# DIR = $(shell python -c "import os; print(sorted([x for x in os.listdir() if x[0]!='.'])[0])")

$(NAME): $(CFILE).o
	@$(CPP) -o $(NAME) $(CFILE).o $(CFLAGS) $(LDLIBS) # -v
	@./$(NAME) < ex1.kal


$(CFILE).o: $(CFILE).cpp
	# @mkdir -p out
	@$(CPP) -o $(CFILE).o -c $(CFILE).cpp $(CFLAGS)


