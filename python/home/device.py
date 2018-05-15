import threading
import socket

class TCPServer(object):

    def __init__(self, address=('', 20001), backlog=511):
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.bind(address)
        sock.listen(backlog)

        self.__t_accept = threading.Thread(target=self.__accept, args=(sock,))
        self.__t_serves = []

        self.__start = True
        self.__t_accept.start()

    def __accept(self, sock):
        sock.settimeout(0.2)
        while self.__start:
            try:
                c_sock, c_addr = sock.accept()
                t = threading.Thread(target=self.__handle, args=(c_sock, c_addr))
                self.__t_serves.append({'thread': t, 'sock': c_sock, 'addr': c_addr})
                t.start()
            except socket.timeout:
                pass
        for serv in self.__t_serves:
            serv['thread'].join()
        sock.close()

    def __handle(self, sock, addr):
        self.on_connected(sock, addr)
        sock.settimeout(0.2)
        while self.__start:
            try:
                msg = sock.recv(8192)
                if not msg:
                    self.on_disconnected(sock, addr)
                    break
                self.on_received(sock, addr, msg)
            except socket.timeout:
                pass
        sock.close()

    def send(self, sock, msg):
        sock.send(msg)

    def shutdown(self):
        self.__start = False
        self.__t_accept.join()

    def on_connected(self, sock, addr):
        pass

    def on_disconnected(self, sock, addr):
        pass

    def on_received(self, sock, addr, msg):
        pass


class Devices(TCPServer):

    def on_connected(self, sock, addr):
        print('connected', addr)

    def on_disconnected(self, sock, addr):
        print('disconnected', addr)

    def on_received(self, sock, addr, msg):
        print('received', addr, msg)
        self.send(sock, msg)


dev = Devices()

import time
time.sleep(30)
dev.shutdown()
