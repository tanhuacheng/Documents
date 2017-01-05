#! /usr/bin/python3
# -*- coding: utf-8 -*-

counter = 100   # 整型
miles = 100.0   # 浮点
name = 'tanhc'  # 字符串

print(counter)
print(miles)
print(name)


a = b = c = 1
print(a, b, c)

a, b, c = 1, 2, 'tanhc'
print(a, b, c)

a, b = b, a
print(a, b)


# 标准数据类型
# 1. Number         数字
# 2. String         字符串
# 3. List           列表
# 4. Tuple          元组
# 5. Sets           集合
# 6. Dictionary     字典


# 1. Number 数字

# int float bool complex
a = 1; b = 2.0; c = False; d = 1 + 2j
print(type(a), type(b), type(c), type(d))

# bool 类型的值是 0 或 1, 可以和数字运算
print(False * 1, True * 2.0, 2 + True)

# 删除对象
a = 1
print(a)
del a
# print(a) # NameError: name 'a' is not defined

# 运算 // 整除(向下取整, 不同于 C), ** 乘方, % 求余(结果不小于零, 不同于 C)
print(2 / 4, 2 // 4, 17 % 3, 2 ** 5)


# 2. String 字符串
# 用 ' 或 " 括起来, 使用 \ 转义特殊字符
# 截取 str[s:e] 索引从 0 开始 -1 表示倒数第一个位置
# + 是连字符 * 表示复制字符串多次
str = 'tanhc'
print(str[0:-1])
print(str[3:])
print('hello, ' + str)
print(str * 2)

# 使用 \ 转义特殊字符, 如果不要则在前面加一个 r
print('tanhc\n')
print(r'tanhc\n')

# ''' 或 """ 可以跨越多行
str = '''hi
hello'''
print(str)

# 字符串不能改变
str = 'hkllo'
# str[1] = 'i' # TypeError: 'str' object does not support item assignment


# 3. List 列表
# 写在 [] 之间, 用 , 分开, 可以是不同的类型
list = ['abcd', 786, 2.23, 'tanhc', 70.2]
print(list)
print(list[0])
print(list[0:4])
print(list * 2)
list.append('345')
print(list)
list2 = [1, 2]
print(list + list2)

# List 可以改变
list[0] = '456'
print(list)
print(list.pop(), list)

# 4. Tuple 元组
# 写在 () 中, 类似于 List, 但是不能修改
tuple = ('abcd', 786, 2.23, 'tanhc', 70.2)
print(tuple)
# tuple[0] = '456' # TypeError: 'tuple' object does not support item assignment
# Tuple 虽然不能修改, 但是可以包含可变的对象
tuple = tuple + (20, ) # 如果只有一个元素, 需要在元素后添加逗号
print(tuple)
# 字符串可以看成特殊的 Tuple


# 5. Sets 集合
# 无序不重复的系列
# 写在 {} 之间或使用 set() 函数创建, 但是创建空集合只能用 set(), 因为 {} 用来创建空的字典
student = {'Tom', 'Jim', 'Tom', 'Jack'}
print(student) # 重复的元素被自动删除

# 成员测试
print('Tom' in student)
print('tanhc' in student)
a = set('abracadabra')
b = set('alacazam')
print('a:', a, 'b:', b)
print('a-b:', a - b) # 差集
print('a&b:', a & b) # 交集
print('a|b:', a | b) # 并集
print('a^b:', a ^ b) # 不同时存在的元素


# 6. Dictionary 字典
# 无序的对象集合, 通过键来存取
# {key : value}, key 必须是不可变类型, 且必须是唯一的

print('Dictionary')
d = dict([('one', 1)])
print(d)
d = {x: x**2 for x in (1, 2, 3)}
print(d)
d = dict(one=1, tow=2)
print(d)

sets = set()
print(sets)
d = {}
print(d)
d['one'] = '1s'
print(d)
print(d['one'])
print(d.keys(), d.values())

