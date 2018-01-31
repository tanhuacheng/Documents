#!/usr/bin/python3
# -*- coding:utf-8

from PyQt5 import QtWidgets, QtCore


class TreeWidget(QtWidgets.QTreeWidget):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        self.setHeaderHidden(True)
        self.setRootIsDecorated(False)
        self.setSelectionMode(QtWidgets.QAbstractItemView.SingleSelection)
        self.setAnimated(True)
        self.setIndentation(6)
        self.setFocusPolicy(QtCore.Qt.NoFocus)

        self.setStyleSheet("""
            QTreeWidget {
                background-color:rgb(239,239,239,255);
            }
            QTreeWidget::item {
                height: 48;
            }
            QTreeView::item:!has-children {
                margin : 0px 0px 0px 0px;
                padding: 0px 0px 0px 6px;
                background-color:rgb(239,239,239,255);
            }
            QTreeView::item:!has-children:hover {
                margin : 0px 0px 0px 0px;
                padding: 0px 0px 0px 6px;
                background: qlineargradient(x1:0, y1:0, x2:0, y2:1, stop:0 #e7effd, stop:1 #cbdaf1);
            }
            QTreeView::item:!has-children:selected {
                margin : 0px 0px 0px 0px;
                padding: 0px 0px 0px 6px;
                color:lightgray;
                background-color:rgb(27,29,30,255);
            }
        """)
