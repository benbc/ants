import socket, struct

_RECV_SIZE = 1024
_HEADER_FORMAT = '!H'

class Socket:
    def __init__(self, address, port):
        self._address, self._port = address, port
        self._socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

    def send(self, message):
        packet = struct.pack(_HEADER_FORMAT, len(message)) + message
        self._socket.sendall(packet)

    def receive(self):
        return _receive_header(self._socket, '')

    def __enter__(self):
        self._socket.connect((self._address, self._port))
        return self

    def __exit__(self, type, value, traceback):
        self._socket.close()

def _receive_header(socket, packet):
    if len(packet) >= 2:
        header = packet[:2]
        message = packet[2:]
        (length,) = struct.unpack(_HEADER_FORMAT, header)
        return _receive_message(socket, length, message)
    return _receive_header(socket, packet + socket.recv(_RECV_SIZE))

def _receive_message(socket, length, message):
    if len(message) >= length:
        return message[:length]
    return _receive_message(socket,
                            length,
                            message + socket.recv(_RECV_SIZE))
