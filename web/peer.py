import hashlib
import hmac
import http.client
import os
import re
import sys
import time
import urllib.parse


domain_pattern = re.compile(r'[a-z0-9]+(?:[a-z0-9-]*[a-z0-9])?(?:\.[a-z0-9]+(?:[a-z0-9-]*[a-z0-9])?)+\Z')
id_pattern = re.compile(r'[0-9]{13}-[0-9a-f]{16}\Z')
hex_pattern = re.compile(r'[0-9a-f]{64}\Z')


def peer(name):
    if not domain_pattern.fullmatch(name) or len(name) > 120:
        raise ValueError('Invalid peer')
    values = {}
    with open(os.path.join(os.environ['OPENHOST_APP_DATA_DIR'], 'peers', name), encoding='ascii') as source:
        for line in source:
            if ': ' in line:
                key, value = line.rstrip('\n').split(': ', 1)
                values[key] = value
    if not hex_pattern.fullmatch(values['key']):
        raise ValueError('Invalid key')
    return values['url'], bytes.fromhex(values['key'])


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
            not hex_pattern.fullmatch(signature) or
            (author and (author.count('@') != 1 or not author.endswith('@' + name) or
                         not re.fullmatch(r'[a-z0-9_-]{3,24}', author.split('@', 1)[0]))) or
            (not author and csrf) or (author and not re.fullmatch(r'[0-9a-f]{32}', csrf)) or
            (method == 'POST' and not id_pattern.fullmatch(request)) or
            (method == 'GET' and request) or method not in ('GET', 'POST')):
        raise ValueError('Invalid request')
    _, key = peer(name)
    signed = message(method, path, author, csrf, timestamp, request,
                     headers.get('CONTENT_TYPE', ''), body)
    if not hmac.compare_digest(hmac.new(key, signed, hashlib.sha256).hexdigest(), signature):
        raise ValueError('Invalid signature')
    print('ok')


def send():
    name = os.environ['FED_PEER']
    url, key = peer(name)
    parsed = urllib.parse.urlsplit(url)
    if ((parsed.scheme != 'https' or parsed.netloc != name) and
            not (os.environ.get('JUDGE_ALLOW_HTTP_PEERS') == '1' and parsed.scheme == 'http')):
        raise ValueError('Invalid peer URL')
    if parsed.path or parsed.query or parsed.fragment:
        raise ValueError('Invalid peer URL')
    method = os.environ['FED_METHOD']
    path = os.environ['FED_PATH']
    body = sys.stdin.buffer.read(131073)
    if len(body) > 131072:
        raise ValueError('Request too large')
    timestamp = str(int(time.time()))
    author = os.environ['FED_AUTHOR']
    csrf = os.environ['FED_CSRF']
    request = os.environ['FED_REQUEST']
    signature = hmac.new(key, message(method, path, author, csrf, timestamp, request,
                                      os.environ['FED_CONTENT_TYPE'] if method == 'POST' else '', body), hashlib.sha256).hexdigest()
    headers = {'X-Judge-Peer': os.environ.get('JUDGE_DOMAIN', 'judge.1.bottle.cloud'),
               'X-Judge-Author': author, 'X-Judge-Csrf': csrf,
               'X-Judge-Time': timestamp, 'X-Judge-Request': request,
               'X-Judge-Signature': signature}
    if method == 'POST':
        headers['Content-Type'] = os.environ['FED_CONTENT_TYPE']
    connection = (http.client.HTTPSConnection if parsed.scheme == 'https' else http.client.HTTPConnection)(
        parsed.netloc, timeout=15)
    try:
        connection.request(method, path, body if method == 'POST' else None, headers)
        response = connection.getresponse()
        content = response.read(4194305)
        if len(content) > 4194304:
            raise ValueError('Response too large')
        output(response.status, response.reason, response.getheaders(), content)
    finally:
        connection.close()


def output(status, reason, headers, content):
    out = sys.stdout.buffer
    out.write(f'Status: {status} {reason}\r\n'.encode('ascii'))
    for key, value in headers:
        if key.lower() in ('content-type', 'location', 'refresh') and '\r' not in value and '\n' not in value:
            out.write(f'{key}: {value}\r\n'.encode('latin-1'))
    out.write(b'Cache-Control: no-store\r\nReferrer-Policy: no-referrer\r\nX-Content-Type-Options: nosniff\r\n')
    out.write(b"Content-Security-Policy: default-src 'none'; style-src 'self'; script-src 'none'; form-action 'self'; base-uri 'none'; frame-ancestors 'none'\r\n\r\n")
    out.write(content)


if __name__ == '__main__':
    if sys.argv[1] == 'verify':
        try:
            verify()
        except (OSError, ValueError, KeyError, UnicodeError, OverflowError):
            raise SystemExit(2)
    elif sys.argv[1] == 'send':
        try:
            send()
        except (OSError, ValueError, KeyError, UnicodeError, http.client.HTTPException):
            output(502, 'Bad Gateway', [('Content-Type', 'text/plain; charset=utf-8')], b'Contest host unavailable')
