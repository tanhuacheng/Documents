#!/usr/bin/python3
# -*- coding:utf-8

import sqlite3

def main():
    conn = sqlite3.connect('test.db')
    curs = conn.cursor()

    #  curs.execute('create table user (id varchar(20) primary key, name varchar(20))')
    #  curs.execute('insert into user (id, name) values (\'1\', \'tanhc\')')
    print(curs.rowcount)
    #  print(curs.execute('select * from user where id=?', ('1', )))
    print(curs.fetchall())

    curs.close()
    conn.commit()
    conn.close()


if __name__ == '__main__':
    main()
