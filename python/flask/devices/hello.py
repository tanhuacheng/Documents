from flask import Flask, request, render_template, flash

from forms import AddDeviceForm


def create_app():
    app = Flask(__name__, instance_relative_config=True)
    app.config.from_mapping(SECRET_KEY='dev')

    return app

app = create_app()

@app.route('/', methods=['GET', 'POST'])
def index():
    form = AddDeviceForm(request.form)
    if request.method == 'POST' and form.validate():
        print(form.device.data, form.remark.data)
        flash('ok')

    return render_template('users.html', title='用户管理', form=form, devices=[('1', '2', '3')])

app.run()