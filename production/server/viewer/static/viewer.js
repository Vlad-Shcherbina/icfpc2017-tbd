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
    for (var _b = 0, _c = map.rivers; _b < _c.length; _b++) {
        var river = _c[_b];
        var source = map.sites[river.source];
        var target = map.sites[river.target];
        ctx.lineWidth = 1.5;
        ctx.beginPath();
        ctx.moveTo(transformX(source.x), transformY(source.y));
        ctx.lineTo(transformX(target.x), transformY(target.y));
        ctx.stroke();
    }
    for (var _d = 0, _e = map.sites; _d < _e.length; _d++) {
        var site = _e[_d];
        ctx.strokeStyle = 'black';
        ctx.fillStyle = 'white';
        ctx.beginPath();
        ctx.arc(transformX(site.x), transformY(site.y), 2, 0, 2 * Math.PI);
        ctx.fill();
        ctx.stroke();
    }
    for (var _f = 0, _g = map.mines; _f < _g.length; _f++) {
        var mineIdx = _g[_f];
        var mine = map.sites[mineIdx];
        ctx.strokeStyle = 'white';
        ctx.fillStyle = 'black';
        ctx.beginPath();
        ctx.arc(transformX(mine.x), transformY(mine.y), 6, 0, 2 * Math.PI);
        ctx.fill();
        ctx.stroke();
    }
}