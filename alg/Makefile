# Every C project should include Makfile.
# Take this simple one, using implicit '%.c: %.o' rules.
# I dont use '.a' files, but building them is convinient.
all: sc.o fe.o ge.o sign.o verify.o key_exchange.o
libed25519.a: sc.o fe.o ge.o sign.o verify.o key_exchange.o
	$(AR) -rcs $@ $^
