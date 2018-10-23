from wtforms import Form, StringField, validators

class AddDeviceForm(Form):
    device = StringField('设备', [validators.DataRequired()])
    remark = StringField('备注')
