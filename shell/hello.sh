#! /bin/bash

# $1 第一输入参数
# $* 所有输入参数
# $# 输入参数数量
# shift n移除参数
# $? 函数返回结果

# 变量
# 赋值: 变量名=值
# 读取: $变量名
a="hello, world"
echo A is: $a
num=2
echo "this is the ${num}nd"
echo "this is the $num nd"

# 流程控制
# if
#
# if ...; then
#    ...
# elif ...; then
#    ...
# else
#    ...
# fi
#
# [ -f filename ] 是否是一个文件
# [ -x filename ] 文件是否存在并且有执行权限
# [ -n $var ] 判断 $var 变量是否有值
# [ $a = $b ] 判断 $a 和 $b 是否相等
# man test 可以查看所有测试表达式
if [ -f hello.sh ] && [ 1 = 1 ]; then
    echo yes
else
    echo no
fi
if [ $SHELL = "/bin/bash" ]; then
    echo yes
fi
[ -f /etc/shadow ] && echo "this computer uses shadow passwors"
mailfolder=/var/spool/mail/james
#[ -r $mailfolder ] || { echo "can not read $mailfolder"; exit 1; }

# case
#
# case ... in
# ...) do something here ;;
# ...) do something here ;;
# esac
var=tanhc
case $var in
    tanhc) echo i am tanhuacheng;;
esac

# select
echo "What is your favourite OS?"
select var in "Linux" "Gnu Hurd" "Free BSD" "Other"; do
    break
done
echo "You have selected $var"

# while ...; do
#     ...
# done
#
# for var in ...; do
#     ...
# done
# break continue 类似于 C
for var in A B C; do
    echo var is $var
done

