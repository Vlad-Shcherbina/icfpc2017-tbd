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

type Move = {
    pass?: {
        punter: number
    }
    claim?: {
        punter: number
        source: number
        target: number
    }
    option?: {
        punter: number
        source: number
        target: number
    }
    splurge?: {
        punter: number
        route: number[]
    }
}
type Replay = {
    setup: {
        responses: {
            futures?: {source: number, target: number}[]
        }[]
    }
    moves: Move[]
}

class Vis {
    canvas: HTMLCanvasElement
    ctx: CanvasRenderingContext2D
    map: GameMap
    width: number
    height: number
    minX = 1e6
    minY = 1e6
    maxX = -1e6
    maxY = -1e6
    scale: number
    siteById = {}
    isMine = {}
    constructor(canvas, map: GameMap) {
        this.canvas = canvas
        this.ctx = canvas.getContext('2d')
        this.map = map
        this.width = canvas.width
        this.height = canvas.height

        for (let site of map.sites) {
            this.minX = Math.min(this.minX, site.x)
            this.minY = Math.min(this.minY, site.y)
            this.maxX = Math.max(this.maxX, site.x)
            this.maxY = Math.max(this.maxY, site.y)
        }
        this.scale = Math.min(
            (this.width - 10) / (this.maxX - this.minX),
            (this.height - 10) / (this.maxY - this.minY))

        for (let site of map.sites)
            this.siteById[site.id] = site
        for (let mine of map.mines)
            this.isMine[mine] = true
    }
    transformX(x: number) {
        return this.width * 0.5 + (x - (this.minX + this.maxX) * 0.5) * this.scale
    }
    transformY(y: number) {
        return this.height * 0.5 + (y - (this.minY + this.maxY) * 0.5) * this.scale
    }
}

function drawMapBackground(vis: Vis) {
    const ctx = vis.ctx
    const map = vis.map
    ctx.lineWidth = 1.5
    ctx.strokeStyle = '#aaa'
    for (let river of map.rivers) {
        const source = vis.siteById[river.source]
        const target = vis.siteById[river.target]
        ctx.beginPath()
        ctx.moveTo(vis.transformX(source.x), vis.transformY(source.y))
        ctx.lineTo(vis.transformX(target.x), vis.transformY(target.y))
        ctx.stroke()
    }
    for (let site of map.sites) {
        ctx.beginPath()
        ctx.arc(vis.transformX(site.x), vis.transformY(site.y), 2, 0, 2 * Math.PI)
        ctx.stroke()
    }
}

function drawSites(vis: Vis) {
    const ctx = vis.ctx
    const map = vis.map
    for (let site of map.sites) {
        ctx.fillStyle = 'white'
        ctx.beginPath()
        ctx.arc(vis.transformX(site.x), vis.transformY(site.y), 2, 0, 2 * Math.PI)
        ctx.fill()
    }
    ctx.lineWidth = 1.5
    for (let mineIdx of map.mines) {
        let mine = vis.siteById[mineIdx]
        ctx.strokeStyle = 'white'
        ctx.fillStyle = 'black'
        ctx.beginPath()
        ctx.arc(vis.transformX(mine.x), vis.transformY(mine.y), 6, 0, 2 * Math.PI)
        ctx.fill()
        ctx.stroke()
    }
}

const playerColors = [
    'red',
    'green',
    'blue',
    // TODO
]

// Remove pass moves, expand splurge moves to claim and option moves.
function flattenMoves(moves: Move[]): Move[] {
    let result: Move[] = []
    let claimed = {}
    for (let move of moves) {
        if (move.claim) {
            result.push(move)
            const s = move.claim.source
            const t = move.claim.target
            claimed[`${Math.min(s, t)}-${Math.max(s, t)}`] = true
        }
        if (move.option)
            result.push(move)
        if (move.splurge) {
            for (let i = 1; i < move.splurge.route.length; i++) {
                const s = move.splurge.route[i - 1]
                const t = move.splurge.route[i]
                const key = `${Math.min(s, t)}-${Math.max(s, t)}`
                if (claimed[key]) {
                    result.push({option:
                        {punter: move.splurge.punter, source: s, target: t}})
                } else {
                    result.push({claim:
                        {punter: move.splurge.punter, source: s, target: t}})
                    claimed[key] = true
                }
            }
        }
    }
    return result
}

function drawMoves(vis: Vis, moves: Move[]) {
    let ctx = vis.ctx
    const map = vis.map

    for (let move of moves) {
        if (move.claim) {
            const source = vis.siteById[move.claim.source]
            const target = vis.siteById[move.claim.target]
            ctx.strokeStyle = playerColors[move.claim.punter]
            ctx.lineWidth = 1.5
            ctx.beginPath()
            ctx.moveTo(vis.transformX(source.x), vis.transformY(source.y))
            ctx.lineTo(vis.transformX(target.x), vis.transformY(target.y))
            ctx.stroke()
        }
        if (move.option) {
            const source = vis.siteById[move.option.source]
            const target = vis.siteById[move.option.target]
            ctx.strokeStyle = playerColors[move.option.punter]
            const dx = target.x - source.x
            const dy = target.y - source.y
            ctx.lineWidth = 1.5
            ctx.beginPath()
            ctx.moveTo(
                vis.transformX(source.x),
                vis.transformY(source.y))
            ctx.bezierCurveTo(
                vis.transformX(source.x + 0.5 * dx - 0.525 * dy),
                vis.transformY(source.y + 0.5 * dy + 0.525 * dx),

                vis.transformX(source.x + 0.5 * dx + 0.525 * dy),
                vis.transformY(source.y + 0.5 * dy - 0.525 * dx),

                vis.transformX(target.x),
                vis.transformY(target.y))
            ctx.stroke()
        }
    }
}

function runViewer(map: GameMap, replay: Replay) {
    const canvas = <HTMLCanvasElement>document.getElementById('vis')
    const vis = new Vis(canvas, map)

    vis.ctx.clearRect(0, 0, canvas.width, canvas.height)
    drawMapBackground(vis)
    drawMoves(vis, flattenMoves(replay.moves))
    drawSites(vis)
}
