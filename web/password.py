import hashlib
import hmac
import secrets
import sys


mode = sys.argv[1]
data = sys.stdin.buffer.read(512)

if mode == 'hash':
    salt = secrets.token_bytes(16)
    digest = hashlib.scrypt(data, salt=salt, n=16384, r=8, p=1, dklen=32, maxmem=67108864)
    print('scrypt$' + salt.hex() + '$' + digest.hex())
elif mode == 'verify':
    stored, sep, password = data.partition(b'\n')
    try:
        algorithm, salt, digest = stored.decode('ascii').split('$')
        if not sep or algorithm != 'scrypt' or len(salt) != 32 or len(digest) != 64:
            raise ValueError
        result = hashlib.scrypt(password, salt=bytes.fromhex(salt), n=16384, r=8, p=1,
                                dklen=32, maxmem=67108864)
        print('yes' if hmac.compare_digest(result, bytes.fromhex(digest)) else 'no')
    except ValueError:
        print('no')
else:
    raise SystemExit(2)
