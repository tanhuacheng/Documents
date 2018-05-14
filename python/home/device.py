import threading
import socket

class Device(object):

    def __init__(self, bindport=20000, backlog=2):
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socket.bind(('', bindport))
        self.socket.listen(backlog)

        self.server = []
        self.thread = threading.Thread(target=self.accept)
        self.thread.start()

    def accept(self):
        while True:
            client_sock, client_addr = self.socket.accept()
            t = threading.Thread(target=self.handle, args=(client_sock, client_addr))
            self.server.append({'thread': t, 'sock': client_sock, 'addr': client_addr})
            t.start()

    def handle(self, client_sock, client_addr):
        print('Got connection from ', client_addr)
        while True:
            msg = client_sock.recv(8192)
            if not msg:
                print('msg:', msg)
                break
            client_sock.send(msg)


device = Device(bindport=20001)
