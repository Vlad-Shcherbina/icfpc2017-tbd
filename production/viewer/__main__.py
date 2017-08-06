# hack to placate the restarter
import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..'))

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
from production import cpp_bot

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
  <td><a href="{{ url_for('view_game', game_id=game.id) }}?mode=vis#0">vis</a>
      <a href="{{ url_for('view_game', game_id=game.id) }}?mode=grad#0">grad</a></td>
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
<br><br>
press left/right
<br>
<img id="im"
     src="zzz"></img>
<br><br>Timestats
<br>{{ timestats[0] }}
<br>{{ timestats[1] }}
<br>{{ timestats[2] }}
<br>{{ timestats[3] }}

<script>
"use strict";

let turn_number = 0
window.onhashchange = function() {
    turn_number = parseInt(window.location.hash.substring(1))
    im.src = '/{{ game.id }}/' + turn_number + '?mode={{ mode }}'
}
let im = document.getElementById('im')
im.src = '/{{ game.id }}/' + turn_number + '?mode={{ mode }}'

document.addEventListener("keydown", function(e) {
    console.log(e)
    if (e.keyCode == 37) {
        console.log('left')
        if (turn_number > 0) {
            turn_number--;
            im.src = '/{{ game.id }}/' + turn_number + '?mode={{ mode }}'
            history.replaceState('', '', '#' + turn_number)
            e.preventDefault()
        }
    }
    if (e.keyCode == 39) {
        console.log('right')
        if (turn_number + 1 < {{ num_turns }}) {
            turn_number++;
            im.src = '/{{ game.id }}/' + turn_number + '?mode={{ mode }}'
            history.replaceState('', '', '#' + turn_number)
            e.preventDefault()
        }
    }
}, false)
</script>
{% endblock %}
'''

@functools.lru_cache()
def get_game_with_replay(id):
    return match_history.get_game(id)


@app.route('/<int:game_id>')
def view_game(game_id):
    mode = flask.request.args['mode']

    game = get_game_with_replay(game_id)
    replay = game.replay.get()
    num_turns = match_history.replay_length(replay)
    timestats = get_timestatistics(replay)

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
    mode = flask.request.args['mode']

    game = get_game_with_replay(game_id)
    replay = game.replay.get()

    story = match_history.story_from_replay(replay, turn_number)

    if mode == 'vis':
        vis = visualization.Visualization(600, 600)
        vis.draw_story(story)
        im = vis.get_image()
    elif mode == 'grad':
        im = cpp_bot.render_prob_field(story)
    else:
        assert False, mode

    r = serve_pil_image(im)
    r.headers["Cache-Control"] = "no-cache, no-store, must-revalidate"
    r.headers["Pragma"] = "no-cache"
    r.headers["Expires"] = "0"
    return r


def get_timestatistics(replay):
    res = ['', '', '', '']

    timestamps = [r['debug_request_timer'] for r in replay 
                        if 'debug_request_timer' in r and not 'ready' in r]
    if not timestamps:
        res[0] = '...no timestamps in replay.'
        return res

    for r in replay:
        if 'ready' in r:
            assert 'debug_request_timer' in r
            res[0] = 'setup time: ' + f'{r["debug_request_timer"]:.3}'
            break

    i_min = timestamps.index(min(timestamps))
    i_max = timestamps.index(max(timestamps))

    res[1] += f'min: {timestamps[i_min]:.3} at {i_min} | '
    res[1] += f'max: {timestamps[i_max]:.3} at {i_max}'

    res[2] = 'average for quarters:'
    quarter = len(timestamps) // 4
    for i in range(4):
        avg = sum(timestamps[i*quarter : (i+1)*quarter]) / quarter
        res[2] += f': {avg:.3} '

    res[3] = 'average for type:: '
    typed_times = { 'claim' : [], 'pass' : [], 'splurge' : [], 'option' : [] }
    # NB: options inside of sprulges are not counted.
    for r in replay:
        for move in typed_times:
            if move in r: typed_times[move].append(r['debug_request_timer'])
    for move in typed_times:
        if not typed_times[move]: continue
        res[3] += f'{move}: {sum(typed_times[move])/len(typed_times[move]):.3} '
        res[3] += f'({len(typed_times[move])} moves) | '
    res[3] = res[3][:-2]
    return res


def main():
    app.jinja_env.auto_reload = True

    app.debug = True
    app.run(port=PORT)


if __name__ == '__main__':
    if os.environ.get("WERKZEUG_RUN_MAIN") != 'true':  # don't open on reloads
        webbrowser.open(f'http://127.0.0.1:{PORT}/')
    main()
