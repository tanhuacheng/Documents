#!/usr/bin/python3

import requests
import HTMLParser


class MyHTMLParser(HTMLParser.HTMLParser):

    def __init__(self):
        super().__init__()

        self.start = 0
        self.end = False
        self.results = []

    def handle_starttag(self, tag, attrs):
        if self.end:
            return

        if 0 == self.start and \
                'div' == tag and \
                attrs and len(attrs) == 1 and 'class' == attrs[0][0] and 'mainleft' == attrs[0][1]:
            self.start = 1
        elif 0 == self.start:
            return
        else:
            self.start += 1

        if 'a' == tag and attrs:
            title = None
            href = None

            for attr in attrs:
                if 'title' == attr[0]:
                    title = attr[1]
                if 'href' == attr[0]:
                    href = attr[1]

            if title and href:
                for res in self.results:
                    if title == res['title']:
                        res['href'] = href
                        break
                else:
                    self.results.append({'title': title, 'href': href})

    def handle_startendtag(self, tag, attrs):
        if self.end or not self.start:
            return

        if 'img' == tag and attrs:
            alt = None
            src = None

            for attr in attrs:
                if 'alt' == attr[0]:
                    alt = attr[1]
                if 'src' == attr[0]:
                    src = attr[1]

            if alt and src:
                for res in self.results:
                    if alt == res['title']:
                        res['img'] = src
                        break
                else:
                    self.results.append({'title': alt, 'img': src})

    def handle_endtag(self, tag):
        if self.start:
            self.start -= 1
            if not self.start:
                self.end = True


url = 'http://gaoqing.la/'
r = requests.get(url)
if not r or r.status_code < 200 or r.status_code > 299:
    print('requests.get({0}) failure'.format(url))
else:
    html_parser = MyHTMLParser()
    html_parser.feed(r.text)

    i = 0
    for res in html_parser.results:
        i += 1
        print(i, res['title'])
