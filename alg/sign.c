#include "ed25519.h"
#include "openssl/sha.h"
#include "ge.h"
#include "sc.h"


void ed25519_sign(unsigned char *signature, const unsigned char *message, size_t message_len, const unsigned char *public_key, const unsigned char *private_key) {
    SHA512_CTX hash;
    unsigned char hram[64];
    unsigned char r[64];
    ge_p3 R;


    SHA512_Init(&hash);
    SHA512_Update(&hash, private_key + 32, 32);
    SHA512_Update(&hash, message, message_len);
    SHA512_Final(r, &hash);

    sc_reduce(r);
    ge_scalarmult_base(&R, r);
    ge_p3_tobytes(signature, &R);

    SHA512_Init(&hash);
    SHA512_Update(&hash, message, message_len);
    SHA512_Update(&hash, signature, 32);
    SHA512_Update(&hash, public_key, 32);
    SHA512_Final(hram, &hash);

    sc_reduce(hram);
    sc_muladd(signature + 32, hram, private_key, r);
}
