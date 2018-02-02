# -*- coding:utf-8

from PyQt5 import QtWidgets, QtGui, QtCore


class Navigation(QtWidgets.QMainWindow):

    def __init__(self, parent, config):
        super().__init__()
        self.parent = parent
        self.config = config
        self.setMaximumWidth(config['maximum-width'])

        self.tree = TreeWidget()

        self.items = []
        font = QtGui.QFont()
        font.setPixelSize(18)
        for config in config['items']:
            self.items.append(TreeWidgetItem(self.tree, config))
            self.items[-1].setFont(0, font)

        self.current_item = self.items[1]
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
