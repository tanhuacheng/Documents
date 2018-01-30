# -*- coding:utf-8

from PyQt5 import QtWidgets, QtGui, QtCore
from config import *


class Navigation(QtWidgets.QMainWindow):

    def __init__(self, parent):
        super().__init__()
        self.parent = parent
        self.setMaximumWidth(NAVIGATION_MAX_WIDTH)

        self.tree = QtWidgets.QTreeWidget()
        self.tree.setHeaderHidden(True)
        self.tree.expandAll()
        self.tree.setRootIsDecorated(False)
        self.tree.setSelectionMode(QtWidgets.QAbstractItemView.SingleSelection)
        self.tree.setIndentation(12)
        self.tree.hideColumn(1)
        self.tree.setFocusPolicy(QtCore.Qt.NoFocus)

        self.tree.setStyleSheet("""
            QTreeWidget::item {
                height: 48;
            }
            QTreeView::item:!has-children {
                margin : 0px 0px 0px 0px;
                padding: 0px 0px 0px 15px;
                background-color:rgb(243,243,243,255);
            }
            QTreeView::item:!has-children:hover {
                margin : 0px 0px 0px 0px;
                padding: 0px 0px 0px 15px;
                background:
                    qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1, stop: 0 #e7effd, stop: 1 #cbdaf1);
            }
            QTreeView::item:!has-children:selected {
                color:lightgray;
                margin : 0px 0px 0px 0px;
                padding: 0px 0px 0px 15px;
                /* background-color:rgb(30,39,45,255); */
                background-color:rgb(49,68,77,255);
            }
        """)

        self.items = []
        font = QtGui.QFont()
        font.setPixelSize(18)
        for config in NAV_ITEMS:
            self.items.append(TreeWidgetItem(self.tree, config))
            self.items[-1].setFont(0, font)

        self.current_item = self.items[0]
        self.tree.setCurrentItem(self.current_item)
        self.current_item.update_icon()
        self.parent.on_nav_item_pressed(self.current_item.id)

        self.tree.itemPressed.connect(self.on_item_pressed)
        self.setCentralWidget(self.tree)

    def on_item_pressed(self, item, num):
        if item != self.current_item:
            self.current_item.update_icon()
            item.update_icon()
            self.current_item = item
            self.parent.on_nav_item_pressed(item.id)


class TreeWidgetItem(QtWidgets.QTreeWidgetItem):

    def __init__(self, parent, config):
        super().__init__(parent)
        self.setText(0, config['text'])
        self.icones = config['icones']
        self.id = config['id']
        self.update_icon()

    def update_icon(self):
        icon = QtGui.QIcon(self.icones['active'] if self.isSelected() else self.icones['normal'])
        self.setIcon(0, icon)
