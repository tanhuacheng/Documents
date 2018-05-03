#!/usr/bin/python3

import requests
import re
from HTMLParser import HTMLParser


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

for res in results[1:]:
    try:
        r = requests.get(res['url'])
    except:
        r = None
    if r:
        pos = re.search(r'magnet:.+"', r.text).span()
        print(r.text[pos[0]:pos[1]-1])
        break

#  <p><span style="color: #ff0000;"><a style="color: #ff0000;" href="magnet:?xt=urn:btih:db15889169126e378150f90e31563c94dfed7f8d&amp;dn=Birds.Without.Names.2017.JAPANESE.1080p.BluRay.x264.DTS-WiKi&amp;tr=http%3A%2F%2Ftracker.trackerfix.com%3A80%2Fannounce&amp;tr=udp%3A%2F%2F9.rarbg.me%3A2710&amp;tr=udp%3A%2F%2F9.rarbg.to%3A2710">Birds.Without.Names.2017.JAPANESE.1080p.BluRay.x264.DTS-WiKi</a></span></p>
