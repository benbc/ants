import socket

class Socket:
    def __init__(self, address, port):
        self._socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self._socket.connect((address, port))

    def send(self, message):
        self._socket.sendall(message)

    def receive(self):
        parts = []
        while True:
            part = self._socket.recv(1024)
            if not part:
                break
            parts.append(part)
        return ''.join(parts)

    def close(self):
        self._socket.close()
