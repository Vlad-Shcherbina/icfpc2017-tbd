import io
import os
import json
import functools
import webbrowser

import flask
import jinja2

from production import match_history
from production import visualization
from production import json_format

PORT = 5000

app = flask.Flask('viewer')
app.secret_key = 'zzz'

template_loader = jinja2.FileSystemLoader(
    os.path.join(os.path.dirname(__file__), 'templates'))
app.jinja_loader = template_loader

app.jinja_env.undefined = jinja2.StrictUndefined

TABLE_TEMPATE = '''
{% extends 'base.html' %}
{% block body %}
<style>
tr:hover {
  background-color: #eee;
}
</style>
<table>
{% for game in games %}
  <tr>
  <td><a href="{{ url_for('view_game', game_id=game.id) }}#0">view</a></td>
  <td>{{ game.time }}</td>
  <td>{{ game.submitter }}</td>
  <td>{{ game.botname }}</td>
  <td>{{ game.extensions }}</td>
  <td>{{ game.mapname }}</td>
  <td style="white-space: nowrap"><b>{{ game.rank + 1}} of {{ game.num_players}}</b></td>
  <td>{{ game.player_names }}</td>
  </tr>
{% endfor %}
</table>
{% endblock %}
'''

@app.route('/')
def index():
    games = match_history.get_games()

    return flask.render_template_string(TABLE_TEMPATE, **locals())


GAME_TEMPATE = '''
{% extends 'base.html' %}
{% block body %}
{{ game }}
<br>
{{ num_turns }} turns
<br>
press left/right
<br>
<img id="im"
     src="zzz"></img>

<script>
"use strict";

let turn_number = 0
window.onhashchange = function() {
    turn_number = parseInt(window.location.hash.substring(1))
    im.src = '/{{ game.id }}/' + turn_number
}
let im = document.getElementById('im')
im.src = '/{{ game.id }}/' + turn_number
document.onkeydown = function checkKey(e) {
    if (e.keyCode == 37) {
        console.log('left')
        if (turn_number > 0) {
            turn_number--;
            im.src = '/{{ game.id }}/' + turn_number
            history.replaceState('', '', '#' + turn_number)
        }
    }
    if (e.keyCode == 39) {
        console.log('right')
        if (turn_number + 1 < {{ num_turns }}) {
            turn_number++;
            im.src = '/{{ game.id }}/' + turn_number
            history.replaceState('', '', '#' + turn_number)
        }
    }
}
</script>
{% endblock %}
'''

@functools.lru_cache()
def get_game_with_replay(id):
    game, = match_history.get_games(where=f'id = {id}', with_replays=True)
    return game

@app.route('/<int:game_id>')
def view_game(game_id):
    game = get_game_with_replay(game_id)
    replay = game.replay.get()
    num_turns = match_history.replay_length(replay)

    match_history.story_from_replay(replay, num_turns - 1)  # to fail early

    return flask.render_template_string(GAME_TEMPATE, **locals())


# https://stackoverflow.com/a/10170635/6335232
def serve_pil_image(pil_img):
    img_io = io.BytesIO()
    pil_img.save(img_io, 'png')
    img_io.seek(0)
    return flask.send_file(img_io, mimetype='image/png')


@app.route('/<int:game_id>/<int:turn_number>')
def view_turn(game_id, turn_number):
    game = get_game_with_replay(game_id)
    replay = game.replay.get()

    story = match_history.story_from_replay(replay, turn_number)

    vis = visualization.Visualization(300, 300)
    vis.draw_story(story)
    im = vis.get_image()

    r = serve_pil_image(im)
    r.headers["Cache-Control"] = "no-cache, no-store, must-revalidate"
    r.headers["Pragma"] = "no-cache"
    r.headers["Expires"] = "0"
    return r


def main():
    app.jinja_env.auto_reload = True

    app.debug = True
    app.run(port=PORT)


if __name__ == '__main__':
    if os.environ.get("WERKZEUG_RUN_MAIN") != 'true':  # don't open on reloads
        webbrowser.open(f'http://127.0.0.1:{PORT}/')
    main()
