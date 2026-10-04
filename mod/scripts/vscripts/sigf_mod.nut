// SUPERHOT Takeover: time only moves when you move. White world, red glass turrets, slow visible bullets.
// A fast clock of my own (0.03 s) drives the robots and bullets; the kit clock only starts it.

::HT <- {
	factor = 0.2,        // world speed: 0.12 when you stand still, 1.0 when you run
	physApplied = -1.0,
	lastT = 0.0,
	lastYaw = 0.0,
	runUntil = 0.0,       // the player ran recently (ramming counts even while pressed against a robot)
	foes = [],
	shots = [],
	pending = 0,
	pendingAt = 0.0,
	kills = 0,
	cubeSeen = {},
	hudAt = 0.0,
	say = {},
	slow = true,
	sfxAt = 0.0,
	hudKills = null,
	hudTime = null,
	spawnAt = 0.0,
	voiceAt = 0.0,
	started = false,
	charge = null,
	boostUntil = 0.0,
	boostAct = 1.0,
	hitFlashAt = 0.0,
	homeAt = 0.0,
	homePos = null,
	homeYaw = 90.0
}

::HOT_RED <- "255 30 30"
::HOT_MODEL <- "models/npcs/turret/turret.mdl"
::HOT_SHARDS <- ["models/gibs/glass_shard01.mdl", "models/gibs/glass_shard02.mdl", "models/gibs/glass_shard03.mdl",
	"models/gibs/glass_shard04.mdl", "models/gibs/glass_shard05.mdl", "models/gibs/glass_shard06.mdl"]

// ---------- look ----------

::HotLook <- function() {
	SigfCmd("mat_fullbright 1")
	SigfCmd("gameinstructor_enable 0")
	SigfCmd("fog_override 1")
	SigfCmd("fog_enable 1")
	SigfCmd("fog_color 255 255 255")
	SigfCmd("fog_start 0")
	SigfCmd("fog_end 850")
	SigfCmd("fog_maxdensity 1.0")
}

// ---------- helpers ----------

::HotClamp <- function(v, lo, hi) { return v < lo ? lo : (v > hi ? hi : v) }

::HotRight <- function() {
	local f = SigfForward()
	return Vector(f.y, -f.x, 0)
}

// Chest height point of the player.
::HotChest <- function() { return GetPlayer().GetOrigin() + Vector(0, 0, 40) }

// Floor point under p, or null.
::HotFloor <- function(p) { return ::SigfFloorAt(p) }

// Full-screen flash: the fog turns solid for a moment (red when you are hit, white when glass shatters).
::HotFlash <- function(rgb, sec) {
	SigfCmd("fog_color " + rgb)
	SigfCmd("fog_start 0")
	SigfCmd("fog_end 500")
	SigfCmd("fog_maxdensity 0.6")
	SigfIn(sec, function() {
		SigfCmd("fog_color 255 255 255")
		SigfCmd("fog_start 0")
		SigfCmd("fog_end 850")
		SigfCmd("fog_maxdensity 1.0")
	})
}

// ---------- time ----------

::HotUpdateTime <- function(dt, now) {
	local p = GetPlayer()
	local v = p.GetVelocity()
	local sp = sqrt(v.x * v.x + v.y * v.y)
	local ang = p.GetAngles()
	local dy = ang.y - ::HT.lastYaw
	while (dy > 180.0) dy -= 360.0
	while (dy < -180.0) dy += 360.0
	::HT.lastYaw = ang.y
	local turn = fabs(dy) / (dt > 0.001 ? dt : 0.001)
	if (sp > 120.0) ::HT.runUntil = now + 0.5
	local act = sp / 170.0 + fabs(v.z) / 350.0 + turn / 500.0
	if (now < ::HT.boostUntil && act < ::HT.boostAct) act = ::HT.boostAct
	local target = 0.2 + 0.8 * ::HotClamp(act, 0.0, 1.0)
	::HT.factor += (target - ::HT.factor) * ::HotClamp(dt * 9.0, 0.0, 1.0)
	// whoosh when time drops to a crawl or kicks back in
	if (::HT.slow && ::HT.factor > 0.5 && now > ::HT.sfxAt) { ::HT.slow = false; ::HT.sfxAt = now + 2.0; SigfSound("sigf/speedup.wav") }
	else if (!::HT.slow && ::HT.factor < 0.2 && now > ::HT.sfxAt) { ::HT.slow = true; ::HT.sfxAt = now + 2.0; SigfSound("sigf/slowdown.wav") }
	// cubes and shards follow: physics clock
	local ph = ::HotClamp(::HT.factor, 0.1, 1.0)
	if (fabs(ph - ::HT.physApplied) > 0.04) {
		::HT.physApplied = ph
		SigfCmd("phys_timescale " + ph)
	}
}

// ---------- shards and kills ----------

::HotBurst <- function(pos, count, power) {
	for (local i = 0; i < count; i++) {
		local m = ::HOT_SHARDS[RandomInt(0, 5)]
		if (i % 3 == 2) m = "models/npcs/turret/turret_fx_break_gib" + RandomInt(1, 26) + ".mdl"
		local at = pos + Vector(RandomFloat(-18, 18), RandomFloat(-18, 18), RandomFloat(10, 70))
		local vel = Vector(RandomFloat(-1, 1), RandomFloat(-1, 1), RandomFloat(0.2, 1.4)) * power
		SigfProp(m, at, 7.0, function(e):(vel) {
			SigfColor(e, "255 25 25")
			SigfPush(e, vel)
		}, true)
	}
}

::HotSuperHot <- function() {
	local now = Time()
	::HotFlash("255 255 255", 0.18)
	::HotSay("big", "SUPER HOT", 1.1)
	if (now > ::HT.voiceAt) {
		::HT.voiceAt = now + 2.8
		SigfSound("sigf/superhot.wav")
	}
}

::HotHurt <- function(f, power, from, dmg = 1) {
	if (!f.alive) return
	local now = Time()
	if (now < f.hurtUntil) return
	f.hurtUntil = now + 0.7
	f.hp -= dmg
	::HotBurst(f.pos, f.hp > 0 ? 4 : 16, f.hp > 0 ? 200.0 : power)
	if (f.hp > 0) {
		// first hit: it cracks, flashes white and gets shoved back
		SigfSound("physics/glass/glass_bottle_impact_hard1.wav")
		SigfColor(f.ent, "255 255 255")
		SigfIn(0.3, function():(f) { if (f.alive) SigfColor(f.ent, ::HOT_RED) })
		local away = f.pos - from
		away.z = 0
		if (away.Length() > 1.0) {
			away.Norm()
			local np = ::HotFloor(f.pos + away * 70.0 + Vector(0, 0, 30))
			if (np != null && fabs(np.z - f.pos.z) < 30 && ::SigfFree(f.pos, atan2(away.y, away.x) * 57.29578, 70) >= 1.0) f.pos = np
		}
		return
	}
	f.alive = false
	::HT.kills++
	if (f.ent.IsValid()) f.ent.Destroy()
	SigfSound("sigf/shatter.wav")
	::HotSuperHot()
}

// ---------- enemies ----------

::HotMakeFoe <- function(pos, yaw) {
	::HT.pending++
	::HT.pendingAt = Time()
	SigfProp(::HOT_MODEL, pos, 0.0, function(e):(pos, yaw) {
		::HT.pending--
		// physics prop with motion off: solid like a wall, but the script can slide it around
		EntFireByHandle(e, "DisableMotion", "", 0.0, null, null)
		SigfColor(e, ::HOT_RED)
		SigfScale(e, 1.6)
		EntFireByHandle(e, "AddOutput", "rendermode 1", 0.0, null, null)
		EntFireByHandle(e, "Alpha", "215", 0.0, null, null)
		e.SetAngles(0, yaw, 0)
		::HT.foes.append({ ent = e, pos = pos, yaw = yaw, hp = 2, alive = true, hurtUntil = 0.0,
			cd = RandomFloat(0.6, 2.5), strafe = RandomInt(0, 1) == 0 ? 1.0 : -1.0, flip = RandomFloat(2, 5), phase = RandomFloat(0, 6) })
	})
}

// A spot on the floor in front of the player, dist units away, off to the side by 'side'.
::HotSpot <- function(dist, side) {
	local p = GetPlayer()
	local base = p.GetOrigin() + SigfForward() * dist + ::HotRight() * side
	local g = ::HotFloor(base + Vector(0, 0, 40))
	if (g == null) return null
	local c = p.GetOrigin()
	if (fabs(g.z - c.z) > 40) return null
	// the way there must be free of walls
	if (TraceLine(c + Vector(0, 0, 50), g + Vector(0, 0, 50), null) < 0.98) return null
	return g
}

::HotWave <- function() {
	if (::HT.pending > 0 && Time() - ::HT.pendingAt > 3.0) ::HT.pending = 0   // a spawn that never arrived
	local alive = 0
	foreach (f in ::HT.foes) { if (f.alive) alive++ }
	if (alive + ::HT.pending >= 4) return
	local tries = 0
	while (tries < 14) {
		tries++
		local g = ::HotSpot(RandomFloat(220, 360), RandomFloat(-300, 300))
		if (g == null) continue
		local ok = true
		foreach (f in ::HT.foes) { if (f.alive && (f.pos - g).Length() < 140) ok = false }
		if (!ok) continue
		local dir = GetPlayer().GetOrigin() - g
		::HotMakeFoe(g, atan2(dir.y, dir.x) * 57.29578)
		return
	}
}

::HotMoveFoe <- function(f, gdt, now) {
	local p = GetPlayer()
	local to = p.GetOrigin() - f.pos
	to.z = 0
	local dist = to.Length()
	if (dist < 1.0) return
	local dir = to * (1.0 / dist)
	f.yaw = atan2(dir.y, dir.x) * 57.29578
	local side = Vector(-dir.y, dir.x, 0) * f.strafe
	local want = Vector(0, 0, 0)
	if (dist > 460) want = dir
	else if (dist < 260) want = dir * -1.0
	want = want + side * 0.45
	f.flip -= gdt
	if (f.flip < 0) { f.flip = RandomFloat(2, 5); f.strafe = -f.strafe }
	local len = want.Length()
	if (len > 0.01) {
		want = want * (1.0 / len)
		local yawMove = atan2(want.y, want.x) * 57.29578
		// wall ahead: try other headings
		local tryYaw = yawMove
		local best = ::SigfFree(f.pos, yawMove, 70)
		if (best < 1.0) {
			foreach (turn in [45.0, -45.0, 90.0, -90.0]) {
				local fr = ::SigfFree(f.pos, yawMove + turn, 70)
				if (fr > best) { best = fr; tryYaw = yawMove + turn }
			}
			f.strafe = -f.strafe
		}
		local r = tryYaw / 57.29578
		local np = f.pos + Vector(cos(r), sin(r), 0) * (140.0 * gdt)
		local g = ::HotFloor(np + Vector(0, 0, 30))
		if (g != null && fabs(g.z - f.pos.z) < 40) f.pos = g
	}
}

::HotFoeTick <- function(f, dt, gdt, now) {
	if (!f.alive) return
	if (!f.ent.IsValid()) { f.alive = false; return }
	local eo = f.ent.GetOrigin()
	if ((eo - f.pos).Length() > 70.0) {   // a portal carried it
		local g = ::HotFloor(eo + Vector(0, 0, 20))
		f.pos = g != null ? g : eo
		EntFireByHandle(f.ent, "DisableMotion", "", 0.0, null, null)
	}
	::HotMoveFoe(f, gdt, now)
	f.ent.SetOrigin(f.pos)
	f.ent.SetAngles(sin(now * 5.0 + f.phase) * 7.0, f.yaw, sin(now * 3.3 + f.phase) * 9.0)
	f.cd -= dt * ::HotClamp(::HT.factor, 0.4, 1.0)
	if (f.cd <= 0) {
		f.cd = RandomFloat(3.4, 5.5)
		::HotShoot(f)
	}
	// ram: the player running into it
	local p = GetPlayer()
	local po = p.GetOrigin()
	local d = f.pos - po
	local flat = sqrt(d.x * d.x + d.y * d.y)
	if (flat < 100 && fabs(d.z) < 90 && now < ::HT.runUntil) ::HotHurt(f, 380.0, po)
	// thrown or falling cubes
	foreach (c in SigfNear(f.pos + Vector(0, 0, 40), 75, "prop_weighted_cube")) {
		local id = c.entindex()
		if (id in ::HT.cubeSeen) {
			local s = ::HT.cubeSeen[id]
			local el = now - s.t
			if (el > 0.001 && (c.GetOrigin() - s.pos).Length() / el > 70) { ::HotHurt(f, 420.0, c.GetOrigin(), 2); break }
		}
	}
}

// ---------- bullets ----------

::HotShoot <- function(f) {
	local r = f.yaw / 57.29578
	local from = f.pos + Vector(0, 0, 62) + Vector(cos(r), sin(r), 0) * 24
	local dir = ::HotChest() + Vector(0, 0, 10) - from
	dir.Norm()
	SigfSound("sigf/bulletfire.wav")
	if (RandomInt(0, 3) == 0) SigfSound("npc/turret/different_turret0" + RandomInt(1, 9) + ".wav")
	SigfProp("models/player/ballbot/gib_core_ball.mdl", from, 0.0, function(e):(from, dir, f) {
		EntFireByHandle(e, "DisableMotion", "", 0.0, null, null)
		EntFireByHandle(e, "DisableCollision", "", 0.0, null, null)
		SigfColor(e, "255 0 0")
		SigfScale(e, 0.4)
		::HT.shots.append({ ent = e, pos = from, dir = dir, age = 0.0, owner = f })
	}, true)
}

::HotShotTick <- function(b, gdt, now) {
	b.age += gdt
	if (b.ent.IsValid()) {
		local eo = b.ent.GetOrigin()
		if ((eo - b.pos).Length() > 45.0) b.pos = eo + b.dir * 30.0   // a portal carried it
	}
	b.pos = b.pos + b.dir * (260.0 * gdt)
	if (b.ent.IsValid()) b.ent.SetOrigin(b.pos)
	// wall
	if (TraceLine(b.pos - b.dir * 10, b.pos + b.dir * 10, null) < 1.0 || b.age > 14.0) return false
	// the player
	if ((b.pos - ::HotChest()).Length() < 34) {
		::HotPlayerHit()
		return false
	}
	// other robots (friendly fire)
	if (b.age > 0.3) {
		foreach (f in ::HT.foes) {
			if (!f.alive || (f == b.owner && b.age < 4.0)) continue
			if ((f.pos + Vector(0, 0, 50) - b.pos).Length() < 55) { ::HotHurt(f, 300.0, b.pos); return false }
		}
	}
	return true
}

::HotPlayerHit <- function() {
	if (Time() > ::HT.hitFlashAt) { ::HT.hitFlashAt = Time() + 2.5; ::HotFlash("255 10 10", 0.3) }
	SigfSound("physics/glass/glass_bottle_impact_hard2.wav")
	::HotSay("big", "HIT!", 0.8)
}

// ---------- going back to the start spot (the demo bot never gets stuck against a wall) ----------

::HotGoHome <- function() {
	if (::HT.homePos == null) return
	local p = GetPlayer()
	p.SetOrigin(::HT.homePos)
	p.SetVelocity(Vector(0, 0, 0))
	SigfLook(8, ::HT.homeYaw)
}

// ---------- main clock ----------

::HotTick <- function() {
	local p = GetPlayer()
	if (p == null) return
	local now = Time()
	local dt = now - ::HT.lastT
	::HT.lastT = now
	if (dt <= 0 || dt > 0.5) return
	// a scripted charge (demo): glide at the target at running speed
	if (::HT.charge != null) {
		local c = ::HT.charge
		if (now > c.until || !c.foe.alive) { ::HT.charge = null; ::HT.homeAt = now + 1.6 }
		else {
			local d = c.foe.pos - p.GetOrigin()
			d.z = 0
			local dl = d.Length()
			if (dl > 20) {
				d.Norm()
				local v = p.GetVelocity()
				p.SetVelocity(Vector(d.x * 320.0, d.y * 320.0, v.z))
				if (dl > 110) SigfLookAt(c.foe.pos + Vector(0, 0, 45))
			}
		}
	}
	if (::HT.homeAt > 0 && now > ::HT.homeAt) { ::HT.homeAt = 0.0; ::HotGoHome() }
	::HotUpdateTime(dt, now)
	local gdt = dt * ::HT.factor
	foreach (f in ::HT.foes) ::HotFoeTick(f, dt, gdt, now)
	local keep = []
	foreach (b in ::HT.shots) {
		if (::HotShotTick(b, gdt, now)) keep.append(b)
		else if (b.ent.IsValid()) b.ent.Destroy()
	}
	::HT.shots = keep
	// remember cube positions for the speed test
	for (local c = Entities.FindByClassname(null, "prop_weighted_cube"); c != null; c = Entities.FindByClassname(c, "prop_weighted_cube")) {
		::HT.cubeSeen[c.entindex()] <- { pos = c.GetOrigin(), t = now }
	}
	local alive = []
	foreach (f in ::HT.foes) { if (f.alive) alive.append(f) }
	::HT.foes = alive
	if (now > ::HT.spawnAt) {
		::HT.spawnAt = now + 1.5
		::HotWave()
	}
	if (now > ::HT.hudAt) {
		::HT.hudAt = now + 0.3
		::HotHud(::HT.hudKills, "SHATTERED " + ::HT.kills)
		::HotHud(::HT.hudTime, "TIME " + (::HT.factor * 100.0).tointeger() + "%")
	}
}

// One game_text per HUD line, re-displayed with a new message (no flicker, no pile of entities).
::HotMakeHud <- function(x, y, size) {
	local t = Entities.CreateByClassname("game_text")
	t.__KeyValueFromString("message", " ")
	t.__KeyValueFromString("color", "170 0 0")
	t.__KeyValueFromString("color2", "170 0 0")
	t.__KeyValueFromFloat("x", x)
	t.__KeyValueFromFloat("y", y)
	t.__KeyValueFromInt("effect", 0)
	t.__KeyValueFromFloat("fadein", 0.0)
	t.__KeyValueFromFloat("fadeout", 0.0)
	t.__KeyValueFromFloat("fxtime", 0.0)
	t.__KeyValueFromFloat("holdtime", 0.8)
	t.__KeyValueFromInt("channel", [2, 1, 4, 0, 5, 3][size])
	t.__KeyValueFromInt("spawnflags", 1)
	return t
}

// Big messages (persistent game_text entities, solid red): "big" = SUPER HOT / HIT, "cap" = demo captions, "note" = small note.
::HotSay <- function(slot, msg, sec) {
	if (!(slot in ::HT.say)) {
		local all = { big = [0.3, 4, "255 0 0"], cap = [0.7, 5, "255 0 0"], note = [0.82, 1, "255 0 0"] }
		local cfg = all[slot]
		local t = ::HotMakeHud(-1.0, cfg[0], cfg[1])
		t.__KeyValueFromString("color", cfg[2])
		t.__KeyValueFromString("color2", cfg[2])
		::HT.say[slot] <- t
	}
	local t = ::HT.say[slot]
	t.__KeyValueFromFloat("holdtime", sec)
	::HotHud(t, msg)
}

::HotHud <- function(t, msg) {
	if (t == null || !t.IsValid()) return
	t.__KeyValueFromString("message", msg)
	EntFireByHandle(t, "Display", "", 0.0, null, null)
}

::HotStart <- function() {
	if (::HT.started) return
	::HT.started = true
	::HT.lastT = Time()
	::HT.hudKills = ::HotMakeHud(0.03, 0.84, 3)
	::HT.hudTime = ::HotMakeHud(0.03, 0.9, 2)
	local po = GetPlayer().GetOrigin()
	::HT.homePos = po
	local fw = SigfForward()
	::HT.homeYaw = atan2(fw.y, fw.x) * 57.29578
	local old = Entities.FindByName(null, "hot_clock")
	if (old != null) old.Destroy()
	local t = Entities.CreateByClassname("logic_timer")
	t.__KeyValueFromString("targetname", "hot_clock")
	t.ValidateScriptScope()
	t.GetScriptScope().HotOnTimer <- function() { ::SigfCall("hot tick", ::HotTick) }
	t.ConnectOutput("OnTimer", "HotOnTimer")
	EntFireByHandle(t, "RefireTime", "0.03", 0.0, null, null)
	EntFireByHandle(t, "Enable", "", 0.0, null, null)
}

// ---------- demo helpers (also fun to call by hand) ----------

// Living robot the player can see best: in front, clear line of sight, nearest first (null if none).
::HotNearest <- function(skip = null) {
	local best = null
	local bd = 99999.0
	local p = GetPlayer()
	local po = p.GetOrigin()
	local eye = p.EyePosition()
	local fwd = SigfForward()
	foreach (f in ::HT.foes) {
		if (!f.alive || f == skip) continue
		local d = f.pos - po
		d.z = 0
		local l = d.Length()
		if (l < 60) continue
		if (d.Dot(fwd) / l < 0.55) continue
		if (TraceLine(eye, f.pos + Vector(0, 0, 45), null) < 0.97) continue
		if (l < bd) { bd = l; best = f }
	}
	return best
}

// Run at the nearest robot and ram it.
::HotRam <- function() {
	local f = ::HotNearest()
	if (f == null) return
	::HT.charge = { foe = f, until = Time() + 6.0 }
}

// A Portal 2 portal ring standing on the floor, facing along dir.
::HotRing <- function(pos, dir, blue, life) {
	local yaw = atan2(dir.y, dir.x) * 57.29578
	SigfDynamic(blue ? "models/effects/fakeportalring_blue.mdl" : "models/effects/fakeportalring_orange.mdl", pos + Vector(0, 0, 50), life, function(e):(yaw) {
		e.SetAngles(90, yaw, 0)
	})
}

// Two portals: a robot's bullet goes into the blue one and comes out of the orange one, to the side.
::HotPortalTrick <- function() {
	local a = ::HotNearest()
	if (a == null) return
	local dir = ::HotChest() - a.pos
	dir.z = 0
	dir.Norm()
	local pa = ::HotFloor(a.pos + dir * 150.0 + Vector(0, 0, 40))
	if (pa == null) return
	local right = Vector(dir.y, -dir.x, 0)
	local pb = null
	foreach (side in [-1.0, 1.0, -0.6, 0.6]) {
		local c = ::HotFloor(pa + right * (260.0 * side) + Vector(0, 0, 40))
		if (c != null && fabs(c.z - pa.z) < 20 && TraceLine(pa + Vector(0, 0, 50), c + Vector(0, 0, 50), null) > 0.98) { pb = c; break }
	}
	if (pb == null) return
	SigfPortal(pa, pb, { radius = 95, boost = 1.0, sprite = false }, 14.0)
	::HotRing(pa, dir, true, 14.0)
	::HotRing(pb, dir, false, 14.0)
	a.cd = 0.1
	::HT.boostUntil = Time() + 8.0
	::HT.boostAct = 0.3
}

// Where a cube can start above a robot without being inside the ceiling.
::HotDropPos <- function(f) {
	local fr = TraceLine(f.pos + Vector(0, 0, 60), f.pos + Vector(0, 0, 560), null)
	local h = 60.0 + 500.0 * fr - 40.0
	return f.pos + Vector(0, 0, h > 100.0 ? h : 100.0)
}

// The sky drops a black Companion Cube on the robot most in front of the player, in slow motion.
::HotCubeDrop <- function() {
	local f = ::HotNearest()
	if (f == null) return
	f.cd = 99.0
	SigfLookAt(f.pos + Vector(0, 0, 90))
	::HT.boostUntil = Time() + 5.0
	::HT.boostAct = 0.45
	SigfCube(::HotDropPos(f), 0, 12.0, function(c) { SigfColor(c, "25 25 25") })
}

// ---------- boot ----------

SigfAfter(0.5, function() {
	// the chamber's own turrets would fight the red ones: this is a SUPERHOT room now
	for (local t = Entities.FindByClassname(null, "npc_portal_turret_floor"); t != null; t = Entities.FindByClassname(t, "npc_portal_turret_floor")) EntFireByHandle(t, "Kill", "", 0.0, null, null)
	::HotLook()
	if (::SigfDemoMode) SigfAutoCam(false)
	::HotStart()
})

// Idle stream: the sky drops black Companion Cubes on the robots, a portal pair opens now and then.
if (!::SigfDemoMode) {
	SigfEvery(9.0, function() {
		foreach (f in ::HT.foes) {
			if (!f.alive) continue
			SigfCube(::HotDropPos(f), 0, 14.0, function(c) { SigfColor(c, "25 25 25") })
			break
		}
	})
	SigfEvery(14.0, function() {
		local a = SigfGround()
		local b = SigfGround()
		for (local i = 0; i < 6 && (a - b).Length() < 300; i++) b = SigfGround()
		local d = ::HotChest() - a
		d.z = 0
		if (d.Length() < 1.0) return
		d.Norm()
		SigfPortal(a, b, { radius = 95, boost = 1.0, sprite = false }, 20.0)
		::HotRing(a, d, true, 20.0)
		::HotRing(b, d, false, 20.0)
	})
}

printl("SUPERHOT mod loaded")
