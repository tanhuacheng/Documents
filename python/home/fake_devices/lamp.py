import sys
import time
import readline
import socket
import threading

SERV_ADDR = ('127.0.0.1', 20001)

status = 0
lock = threading.Lock()

def light_on(sock, feed=False):
    global status

    lock.acquire()
    if not status or feed:
        status = 1
        sock.send(b'status 1')
    lock.release()

def light_off(sock, feed=False):
    global status

    lock.acquire()
    if status or feed:
        status = 0
        sock.send(b'status 0')
    lock.release()

def sock_recv(sock):
    msg = sock.recv(8192)
    if not msg:
        return
    if msg == 'request 1':
        light_on(sock, feed=True)
    elif msg == 'request 0':
        light_off(sock, feed=True)

while True:
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    try:
        sock.connect(SERV_ADDR)
    except Exception as e:
        print('disconnected:', e)
        sock.close()
        time.sleep(5)
        continue

    print('connected')
    t_recv = threading.Thread(target=sock_recv, args=(sock,))
    t_recv.start()

    while True:
        try:
            msg = input('> ')
        except:
            sock.close()
            sys.exit(0)

        if 'on' == msg:
            light_on(sock)
        elif 'off' == msg:
            light_off(sock)
        elif 'status' == msg:
            lock.acquire()
            print(status)
            lock.release()
