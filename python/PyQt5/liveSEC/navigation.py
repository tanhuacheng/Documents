# -*- coding:utf-8

from PyQt5 import QtWidgets, QtGui, QtCore


class QNavigation(QtWidgets.QWidget):

    def __init__(self, config):
        super().__init__()
        self.config = config
        self.setMinimumWidth(config['minimum-width'])

        self.callback_current_item_changed = None

        self.layout = QtWidgets.QGridLayout()
        self.layout.setContentsMargins(0, 0, 0, 0)

        self.tree = self.TreeWidget(config['tree'])
        self.tree.itemSelectionChanged.connect(self.on_item_selection_changed)

        self.items = []
        for item_config in config['items']:
            self.items.append(self.TreeWidgetItem(self.tree, item_config))
            font = QtGui.QFont()
            font.setPixelSize(item_config['font-pixel-size'])
            self.items[-1].setFont(0, font)

        self.current_item = self.items[0]
        self.tree.setCurrentItem(self.current_item)

        self.layout.addWidget(self.tree, 0, 0, 1, 1)
        self.setLayout(self.layout)

    def on_item_selection_changed(self):
        self.current_item.update_icon()
        self.current_item = self.tree.selectedItems()[0]
        self.current_item.update_icon()
        if self.callback_current_item_changed:
            self.callback_current_item_changed(self, self.current_item.config['id'])

    def set_current_item(self, item_id):
        if item_id != self.current_item.config['id']:
            for item in self.items:
                if item_id == item.config['id']:
                    self.tree.setCurrentItem(item)


    class TreeWidget(QtWidgets.QTreeWidget):

        def __init__(self, config):
            super().__init__()
            self.config = config
            self.setHeaderHidden(True)
            self.setRootIsDecorated(False)
            self.setSelectionMode(QtWidgets.QAbstractItemView.SingleSelection)
            self.setStyleSheet(config['style-sheet'])


    class TreeWidgetItem(QtWidgets.QTreeWidgetItem):

        def __init__(self, parent, config):
            super().__init__(parent)
            self.config = config
            self.setText(0, config['text'])
            self.update_icon()

        def update_icon(self):
            icon = QtGui.QIcon(self.config['icones']['active' if self.isSelected() else 'normal'])
            self.setIcon(0, icon)
