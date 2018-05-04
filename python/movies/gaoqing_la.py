#!/usr/bin/python3
# -*- coding:utf-8

import requests
import re
import time
from html.parser import HTMLParser


class GaoQingLaParser(HTMLParser):

    def __init__(self):
        super().__init__()

        self.start = False
        self.results = []

    def handle_starttag(self, tag, attrs):
        if not self.start:
            if 'ul' == tag and attrs:
                for attr in attrs:
                    if 'id' == attr[0] and 'post_container' == attr[1]:
                        self.start = True
            return

        if 'a' == tag and attrs:
            title = None
            url = None

            for attr in attrs:
                if 'title' == attr[0]:
                    title = attr[1]
                if 'href' == attr[0]:
                    url = attr[1]
                if title and url:
                    break

            if title and url:
                for res in self.results:
                    if title == res['title']:
                        res['url'] = url
                        break
                else:
                    self.results.append({'title': title, 'url': url})

    def handle_startendtag(self, tag, attrs):
        if not self.start:
            return

        if 'img' == tag and attrs:
            title = None
            img = None

            for attr in attrs:
                if 'alt' == attr[0]:
                    title = attr[1]
                if 'src' == attr[0]:
                    img = attr[1]
                if title and img:
                    break

            if title and img:
                for res in self.results:
                    if title == res['title']:
                        res['img'] = img
                        break
                else:
                    self.results.append({'title': title, 'img': img})

    def handle_endtag(self, tag):
        if self.start and 'ul' == tag:
            self.start = False
            raise Exception('end parse')


class MagnetParser(HTMLParser):

    def __init__(self):
        super().__init__()

        self.touch = False
        self.results = []

    def handle_starttag(self, tag, attrs):
        if 'a' == tag and attrs:
            for attr in attrs:
                if 'href' == attr[0]:
                    if re.search(r'magnet:', attr[1]):
                        self.touch = True
                        self.results.append({'magnet': attr[1]})

    def handle_data(self, data):
        if self.touch:
            self.results[-1]['title'] = data

    def handle_endtag(self, tag):
        self.touch = False


url = 'http://gaoqing.la/'
try:
    r = requests.get(url)
except:
    r = None

results = []
if r and r.status_code >= 200 and r.status_code <= 299 and len(r.text) > 4000:
    html_parser = GaoQingLaParser()
    try:
        html_parser.feed(r.text)
    except:
        pass
    results = html_parser.results

for res in results:
    try:
        r = requests.get(res['url'])
    except:
        r = None
    if r and r.status_code >= 200 and r.status_code <= 299 and len(r.text) > 2000:
        magnet_parser = MagnetParser()
        magnet_parser.feed(r.text)
        res['magnets'] = magnet_parser.results
    else:
        res['magnets'] = []
    time.sleep(32)

# [{'title': xxx, 'url': xxx, 'img': xxx, 'magnets': [{'title': xxx, 'magnet': xxx}, ...]}, ...]
