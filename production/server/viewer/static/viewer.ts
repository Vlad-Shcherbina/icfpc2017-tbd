/*
viewer.js is generated from viewer.ts using the following command:
cd production/server/viewer/static
tsc

Don't edit viewer.js manually.
It's under version control to simplify deployment.
Remember to update it when changing viewer.ts.
*/

type Site = {
    id: number
    x: number
    y: number
}
type River = {
    source: number
    target: number
}
type GameMap = {
    sites: Site[]
    rivers: River[]
    mines: number[]
}

function drawMap(map: GameMap, canvas: HTMLCanvasElement) {
    const ctx = canvas.getContext('2d')!;
    let minX = 1e6
    let minY = 1e6
    let maxX = -1e6
    let maxY = -1e6
    for (let site of map.sites) {
        minX = Math.min(minX, site.x)
        minY = Math.min(minY, site.y)
        maxX = Math.max(maxX, site.x)
        maxY = Math.max(maxY, site.y)
    }
    let scale = Math.min(
        (canvas.width - 10) / (maxX - minX),
        (canvas.height - 10) / (maxY - minY))
    function transformX(x: number) {
        return canvas.width * 0.5 + (x - (minX + maxX) * 0.5) * scale
    }
    function transformY(y: number) {
        return canvas.height * 0.5 + (y - (minY + maxY) * 0.5) * scale
    }
    let siteById = {}
    for (let site of map.sites)
        siteById[site.id] = site
    for (let river of map.rivers) {
        const source = siteById[river.source]
        const target = siteById[river.target]
        ctx.lineWidth = 1.5
        ctx.beginPath()
        ctx.moveTo(transformX(source.x), transformY(source.y))
        ctx.lineTo(transformX(target.x), transformY(target.y))
        ctx.stroke()
    }
    for (let site of map.sites) {
        ctx.strokeStyle = 'black'
        ctx.fillStyle = 'white'
        ctx.beginPath()
        ctx.arc(transformX(site.x), transformY(site.y), 2, 0, 2 * Math.PI)
        ctx.fill()
        ctx.stroke()
    }
    for (let mineIdx of map.mines) {
        let mine = siteById[mineIdx]
        ctx.strokeStyle = 'white'
        ctx.fillStyle = 'black'
        ctx.beginPath()
        ctx.arc(transformX(mine.x), transformY(mine.y), 6, 0, 2 * Math.PI)
        ctx.fill()
        ctx.stroke()
    }
}
