import hashlib
import http.client
import os
import pathlib
import re
import secrets
import sys
import time

from cryptography.exceptions import InvalidSignature
from cryptography.hazmat.primitives import serialization
from cryptography.hazmat.primitives.asymmetric.ed25519 import Ed25519PrivateKey, Ed25519PublicKey


domain_pattern = re.compile(r'[a-z0-9]+(?:[a-z0-9-]*[a-z0-9])?(?:\.[a-z0-9]+(?:[a-z0-9-]*[a-z0-9])?)+\Z')
id_pattern = re.compile(r'[0-9]{13}-[0-9a-f]{16}\Z')
key_pattern = re.compile(r'[0-9a-f]{64}\Z')
signature_pattern = re.compile(r'[0-9a-f]{128}\Z')
private_file = pathlib.Path(os.environ['OPENHOST_APP_DATA_DIR']) / 'keys' / 'private'


def key():
    value = private_file.read_bytes()
    if len(value) != 32:
        raise ValueError('Invalid instance key')
    return Ed25519PrivateKey.from_private_bytes(value)


def initialize():
    if not private_file.exists():
        value = Ed25519PrivateKey.generate().private_bytes(
            serialization.Encoding.Raw, serialization.PrivateFormat.Raw, serialization.NoEncryption())
        temp = private_file.with_name('.private.' + secrets.token_hex(8))
        with os.fdopen(os.open(temp, os.O_CREAT | os.O_EXCL | os.O_WRONLY, 0o600), 'wb') as output:
            output.write(value)
            output.flush()
            os.fsync(output.fileno())
        try:
            os.link(temp, private_file)
        except FileExistsError:
            pass
        finally:
            temp.unlink()
    key()


def public():
    value = key().public_key().public_bytes(serialization.Encoding.Raw, serialization.PublicFormat.Raw)
    print(value.hex())


def endpoint(name):
    if not domain_pattern.fullmatch(name) or len(name) > 120:
        raise ValueError('Invalid instance domain')
    if os.environ.get('JUDGE_ALLOW_HTTP_PEERS') == '1':
        return http.client.HTTPConnection(name, 8080, timeout=5)
    return http.client.HTTPSConnection(name, timeout=5)


def discover(name):
    connection = endpoint(name)
    try:
        connection.request('GET', '/c/instance-key')
        response = connection.getresponse()
        value = response.read(66)
        if response.status != 200 or len(value) != 65 or not value.endswith(b'\n'):
            raise ValueError('Invalid peer key response')
        text = value[:-1].decode('ascii')
        if not key_pattern.fullmatch(text):
            raise ValueError('Invalid peer key')
        return Ed25519PublicKey.from_public_bytes(bytes.fromhex(text))
    finally:
        connection.close()


def message(method, path, author, csrf, timestamp, request, content_type, body):
    return ('\n'.join((method, path, author, csrf, timestamp, request, content_type,
                       hashlib.sha256(body).hexdigest()))).encode('ascii')


def verify():
    headers = os.environ
    name = headers.get('HTTP_X_JUDGE_PEER', '')
    author = headers.get('HTTP_X_JUDGE_AUTHOR', '')
    csrf = headers.get('HTTP_X_JUDGE_CSRF', '')
    timestamp = headers.get('HTTP_X_JUDGE_TIME', '')
    request = headers.get('HTTP_X_JUDGE_REQUEST', '')
    signature = headers.get('HTTP_X_JUDGE_SIGNATURE', '')
    method = headers.get('REQUEST_METHOD', '')
    path = headers.get('REQUEST_URI', '').split('?', 1)[0]
    body = sys.stdin.buffer.read(131073)
    if (headers.get('QUERY_STRING', '') or len(body) > 131072 or
            abs(time.time() - int(timestamp)) > 300 or
            not signature_pattern.fullmatch(signature) or
            not domain_pattern.fullmatch(name) or len(name) > 120 or
            (author and (author.count('@') != 1 or not author.endswith('@' + name) or
                         not re.fullmatch(r'[a-z0-9_-]{3,24}', author.split('@', 1)[0]))) or
            (not author and csrf) or (author and not re.fullmatch(r'[0-9a-f]{32}', csrf)) or
            (method == 'POST' and not id_pattern.fullmatch(request)) or
            (method == 'GET' and request) or method not in ('GET', 'POST')):
        raise ValueError('Invalid request')
    signed = message(method, path, author, csrf, timestamp, request,
                     headers.get('CONTENT_TYPE', ''), body)
    discover(name).verify(bytes.fromhex(signature), signed)
    print('ok')


def send(check=False):
    name = os.environ['FED_PEER']
    connection = endpoint(name)
    method = os.environ['FED_METHOD']
    path = os.environ['FED_PATH']
    body = sys.stdin.buffer.read(131073)
    if len(body) > 131072:
        raise ValueError('Request too large')
    timestamp = str(int(time.time()))
    author = os.environ['FED_AUTHOR']
    csrf = os.environ['FED_CSRF']
    request = os.environ['FED_REQUEST']
    signature = key().sign(message(method, path, author, csrf, timestamp, request,
                                    os.environ['FED_CONTENT_TYPE'] if method == 'POST' else '', body)).hex()
    headers = {'X-Judge-Peer': os.environ.get('JUDGE_DOMAIN', 'judge.1.bottle.cloud'),
               'X-Judge-Author': author, 'X-Judge-Csrf': csrf,
               'X-Judge-Time': timestamp, 'X-Judge-Request': request,
               'X-Judge-Signature': signature}
    if method == 'POST':
        headers['Content-Type'] = os.environ['FED_CONTENT_TYPE']
    try:
        connection.request(method, path, body if method == 'POST' else None, headers)
        response = connection.getresponse()
        content = response.read(4194305)
        if len(content) > 4194304:
            raise ValueError('Response too large')
        if check:
            if response.status != 200:
                raise ValueError('Contest unavailable')
            print('ok')
        else:
            output(response.status, response.reason, response.getheaders(), content)
    finally:
        connection.close()


def output(status, reason, headers, content):
    out = sys.stdout.buffer
    out.write(f'Status: {status} {reason}\r\n'.encode('ascii'))
    for header, value in headers:
        if header.lower() in ('content-type', 'location', 'refresh') and '\r' not in value and '\n' not in value:
            out.write(f'{header}: {value}\r\n'.encode('latin-1'))
    out.write(b'Cache-Control: no-store\r\nReferrer-Policy: no-referrer\r\nX-Content-Type-Options: nosniff\r\n')
    out.write(b"Content-Security-Policy: default-src 'none'; style-src 'self'; script-src 'none'; form-action 'self'; base-uri 'none'; frame-ancestors 'none'\r\n\r\n")
    out.write(content)


if __name__ == '__main__':
    action = sys.argv[1]
    if action == 'init':
        initialize()
    elif action == 'public':
        public()
    elif action == 'verify':
        try:
            verify()
        except (OSError, ValueError, KeyError, UnicodeError, OverflowError,
                InvalidSignature, http.client.HTTPException):
            raise SystemExit(2)
    elif action in ('send', 'check'):
        try:
            send(action == 'check')
        except (OSError, ValueError, KeyError, UnicodeError, http.client.HTTPException):
            if action == 'check':
                raise SystemExit(2)
            output(502, 'Bad Gateway', [('Content-Type', 'text/plain; charset=utf-8')], b'Contest host unavailable')
