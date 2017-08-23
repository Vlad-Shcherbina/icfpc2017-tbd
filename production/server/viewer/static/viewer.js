/*
viewer.js is generated from viewer.ts using the following command:
cd production/server/viewer/static
tsc

Don't edit viewer.js manually.
It's under version control to simplify deployment.
Remember to update it when changing viewer.ts.
*/
function drawMap(map, canvas) {
    var ctx = canvas.getContext('2d');
    var minX = 1e6;
    var minY = 1e6;
    var maxX = -1e6;
    var maxY = -1e6;
    for (var _i = 0, _a = map.sites; _i < _a.length; _i++) {
        var site = _a[_i];
        minX = Math.min(minX, site.x);
        minY = Math.min(minY, site.y);
        maxX = Math.max(maxX, site.x);
        maxY = Math.max(maxY, site.y);
    }
    var scale = Math.min((canvas.width - 10) / (maxX - minX), (canvas.height - 10) / (maxY - minY));
    function transformX(x) {
        return canvas.width * 0.5 + (x - (minX + maxX) * 0.5) * scale;
    }
    function transformY(y) {
        return canvas.height * 0.5 + (y - (minY + maxY) * 0.5) * scale;
    }
    var siteById = {};
    for (var _b = 0, _c = map.sites; _b < _c.length; _b++) {
        var site = _c[_b];
        siteById[site.id] = site;
    }
    for (var _d = 0, _e = map.rivers; _d < _e.length; _d++) {
        var river = _e[_d];
        var source = siteById[river.source];
        var target = siteById[river.target];
        ctx.lineWidth = 1.5;
        ctx.beginPath();
        ctx.moveTo(transformX(source.x), transformY(source.y));
        ctx.lineTo(transformX(target.x), transformY(target.y));
        ctx.stroke();
    }
    for (var _f = 0, _g = map.sites; _f < _g.length; _f++) {
        var site = _g[_f];
        ctx.strokeStyle = 'black';
        ctx.fillStyle = 'white';
        ctx.beginPath();
        ctx.arc(transformX(site.x), transformY(site.y), 2, 0, 2 * Math.PI);
        ctx.fill();
        ctx.stroke();
    }
    for (var _h = 0, _j = map.mines; _h < _j.length; _h++) {
        var mineIdx = _j[_h];
        var mine = siteById[mineIdx];
        ctx.strokeStyle = 'white';
        ctx.fillStyle = 'black';
        ctx.beginPath();
        ctx.arc(transformX(mine.x), transformY(mine.y), 6, 0, 2 * Math.PI);
        ctx.fill();
        ctx.stroke();
    }
}
