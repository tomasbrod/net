# Every C project should include Makfile.
# Take this simple one, using implicit '%.c: %.o' rules.
# I dont use '.a' files, but building them is convinient.
all: libed25519.a libsha512.a
libed25519.a: sc.o fe.o ge.o sign.o verify.o key_exchange.o
	$(AR) -rcs $@ $^
libsha512.a: sha512.o
	$(AR) -rcs $@ $^
