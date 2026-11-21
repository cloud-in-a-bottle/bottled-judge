#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <openssl/crypto.h>
#include <openssl/evp.h>
#include <openssl/rand.h>

CAMLprim value bj_random(value length) {
  CAMLparam1(length);
  if (Long_val(length) < 0 || Long_val(length) > 65536) caml_invalid_argument("Random length");
  CAMLlocal1(result);
  result = caml_alloc_string(Long_val(length));
  if (RAND_bytes((unsigned char *)String_val(result), Long_val(length)) != 1) caml_failwith("Random failed");
  CAMLreturn(result);
}

CAMLprim value bj_sha256(value input) {
  CAMLparam1(input);
  CAMLlocal1(result);
  result = caml_alloc_string(32);
  unsigned int length = 0;
  if (EVP_Digest(String_val(input), caml_string_length(input), (unsigned char *)String_val(result), &length,
                 EVP_sha256(), NULL) != 1 || length != 32) caml_failwith("SHA256 failed");
  CAMLreturn(result);
}

CAMLprim value bj_scrypt(value password, value salt) {
  CAMLparam2(password, salt);
  CAMLlocal1(result);
  result = caml_alloc_string(32);
  if (EVP_PBE_scrypt(String_val(password), caml_string_length(password),
      (const unsigned char *)String_val(salt), caml_string_length(salt),
      16384, 8, 1, 67108864, (unsigned char *)String_val(result), 32) != 1)
    caml_failwith("Scrypt failed");
  CAMLreturn(result);
}

static EVP_PKEY *private_key(value secret) {
  if (caml_string_length(secret) != 32) caml_invalid_argument("Invalid private key");
  EVP_PKEY *key = EVP_PKEY_new_raw_private_key(EVP_PKEY_ED25519, NULL,
      (const unsigned char *)String_val(secret), 32);
  if (!key) caml_failwith("Invalid private key");
  return key;
}

CAMLprim value bj_ed25519_public(value secret) {
  CAMLparam1(secret);
  CAMLlocal1(result);
  EVP_PKEY *key = private_key(secret);
  result = caml_alloc_string(32);
  size_t length = 32;
  int ok = EVP_PKEY_get_raw_public_key(key, (unsigned char *)String_val(result), &length);
  EVP_PKEY_free(key);
  if (ok != 1 || length != 32) caml_failwith("Public key failed");
  CAMLreturn(result);
}

CAMLprim value bj_ed25519_sign(value secret, value message) {
  CAMLparam2(secret, message);
  CAMLlocal1(result);
  EVP_PKEY *key = private_key(secret);
  EVP_MD_CTX *context = EVP_MD_CTX_new();
  if (!context) { EVP_PKEY_free(key); caml_failwith("Signing failed"); }
  result = caml_alloc_string(64);
  size_t length = 64;
  int ok = EVP_DigestSignInit(context, NULL, NULL, NULL, key) == 1 &&
    EVP_DigestSign(context, (unsigned char *)String_val(result), &length,
                   (const unsigned char *)String_val(message), caml_string_length(message)) == 1;
  EVP_MD_CTX_free(context);
  EVP_PKEY_free(key);
  if (!ok || length != 64) caml_failwith("Signing failed");
  CAMLreturn(result);
}

CAMLprim value bj_ed25519_verify(value public, value message, value signature) {
  CAMLparam3(public, message, signature);
  if (caml_string_length(public) != 32 || caml_string_length(signature) != 64) CAMLreturn(Val_false);
  EVP_PKEY *key = EVP_PKEY_new_raw_public_key(EVP_PKEY_ED25519, NULL,
      (const unsigned char *)String_val(public), 32);
  if (!key) CAMLreturn(Val_false);
  EVP_MD_CTX *context = EVP_MD_CTX_new();
  if (!context) { EVP_PKEY_free(key); caml_failwith("Verification failed"); }
  int ok = EVP_DigestVerifyInit(context, NULL, NULL, NULL, key) == 1 &&
    EVP_DigestVerify(context, (const unsigned char *)String_val(signature), 64,
      (const unsigned char *)String_val(message), caml_string_length(message)) == 1;
  EVP_MD_CTX_free(context);
  EVP_PKEY_free(key);
  CAMLreturn(Val_bool(ok));
}

CAMLprim value bj_crypto_equal(value left, value right) {
  CAMLparam2(left, right);
  if (caml_string_length(left) != caml_string_length(right)) CAMLreturn(Val_false);
  CAMLreturn(Val_bool(CRYPTO_memcmp(String_val(left), String_val(right), caml_string_length(left)) == 0));
}
