#!/usr/bin/python3

import requests
import requests.packages.urllib3.util.ssl_
requests.packages.urllib3.util.ssl_.DEFAULT_CIPHERS = 'ALL'

headers = {
    'accept': 'application/json, text/javascript, */*; q=0.01',
    'accept-encoding': 'gzip, deflate, br',
    'accept-language': 'zh-CN,zh;q=0.9',
    'content-length': '441',
    'content-type': 'application/x-www-form-urlencoded; charset=UTF-8',
    'cookie': 'Hm_lvt_3836b350548df7f66804c70386c27cc3=1525422945; _ga=GA1.2.941066533.1525422946; _gid=GA1.2.680509448.1525422946; Hm_lpvt_3836b350548df7f66804c70386c27cc3=1525423876',
    'origin': 'https://www.btdx8.com',
    'referer': 'https://www.btdx8.com/down.php?69efHc2cArxL6cL014ESZyiyQce4vfjIqpSGAZC1s94S0MwEjAqUKNumG8evRu59CkxLJXoZSHTJlQw+jo4pWxCGP7xRdoJLkwE53nmdFRiu7IjDQAXjfUTJ+69J7IjnzGJACmhkPEKv6E7Xxsu/r1xvXA5NEMfDYwaCtsoLOUz7V6Qr',
    'user-agent': 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Ubuntu Chromium/65.0.3325.181 Chrome/65.0.3325.181 Safari/537.36',
    'x-requested-with': 'XMLHttpRequest',
}

data = {
    'file_id': '111919',
    'fc': 'f783KH/HkFdrDXaBk3Duh2Ars/uGeSH+On27Jc8MP9OoCtsRiBoy/uSPGcIJNkIafDQ2li3Mz6CyNqOmVyIkj8WjGMk8/fqavTyFgw2hL8098UlSd9NlGfdryx4If9DkcSEV+QVKTzqr5hq/eNk0c1rBjhKUMS36/l6vueuDU9y2Pm8X+mFe/x0vZT72VjHxzjHcxOpySNYTXIDBGWfLtBjhA08ExJvPKAP9EByUlWP+06bjhNoeqLzydmMEWSUPLmrthpEBs0IG+bCf1nH2g8ssugcwhlP+OaZSo2z+k4Na82GVscvViSDTjTaJGk7JZFDzeu4daYMcQqWtLZN77oiYjFBzYoDX+q6/SFUqlbW8lQXehtASIIrfrMTdKCWNy9g6Rt4',
}

r = requests.post('https://www.btdx8.com/calldown/calldown.php', data=data, headers=headers)
#  print(r)
#  print(r.text)
