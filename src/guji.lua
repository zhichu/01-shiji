if not modules then modules = { } end modules ["guji"] = {
    version   = 1.001,
    comment   = "companion to p-guji.mkiv",
    author    = "Zhichu Chen",
    copyright = "PRAGMA ADE / ConTeXt Development Team",
    license   = "see context related readme files"
}



local zhujian            = { }
local implement          = interfaces.implement




moduledata = moduledata or { }
moduledata.guji = moduledata.guji or { }


--~ local pdfsetmatrix      = nodes.pool.pdfsetmatrix


local registerotffeature = fonts.handlers.otf.features.register

local setmetatableindex  = table.setmetatableindex
local add_commands       = table.insert

local settings_to_hash   = utilities.parsers.settings_to_hash

local utfchar            = utf.getchar

local nuts               = nodes.nuts
local tonut              = nodes.tonut
local tonode             = nodes.tonode

local insert_node_after  = nuts.insert_after
local insert_node_before = nuts.insert_before
local copy_node          = nuts.copy
local remove_node        = nuts.remove
local traverse_id        = nuts.traverse_id

local getnext            = nuts.getnext
local getprev            = nuts.getprev
local getfont            = nuts.getfont
local getchar            = nuts.getchar
local getid              = nuts.getid
local getattr            = nuts.getattr
local getsubtype         = nuts.getsubtype
local getfield           = nuts.getfield

local setchar            = nuts.setchar

local nodepool           = nuts.pool
local new_glue           = nodepool.glue
local new_kern           = nodepool.kern
local new_penalty        = nodepool.penalty

local nodecodes          = nodes.nodecodes
local skipcodes          = nodes.skipcodes
local glyph_code         = nodecodes.glyph
local glue_code          = nodecodes.glue
local userskip_code      = skipcodes.userskip

local a_scriptstatus     = attributes.private('scriptstatus')
local a_scriptinjection  = attributes.private('scriptinjection')

local categorytonumber   = scripts.categorytonumber
local numbertocategory   = scripts.numbertocategory
local hash               = scripts.hash
local numbertodataset    = scripts.numbertodataset

local fonthashes         = fonts.hashes
local fontdata           = fonthashes.identifiers
local quaddata           = fonthashes.quads
local spacedata          = fonthashes.spaces

local decomposed         = characters.hangul.decomposed



local sin               = math.sin
local cos               = math.cos


local function round(num, idp)
  return tonumber(string.format("%." .. (idp or 6) .. "f", num))
end

local function sind(deg)
  return round(sin(math.rad(deg)))
end

local function cosd(deg)
  return round(cos(math.rad(deg)))
end

--[[
full circle with quadratic Bézier curves
P[0] .. controls(P[1]) .. P[2] ..  ... P[2n]
where P[i] = ( r[i]×sin(i*α), -r[i]×cos(i*α) ),
where α=360/2n, r[odd] = r, and r[even] = R = r/cos(α)

convert quadratic to cubic:
P[0] .. controls((P[0]+2P[1])/3 and (2P[1]+P[2])/3) .. P[2] ...

local function fullcircle(r,n)
  local N = n or 2
  local alpha = 360/(2*N)
  local R = r/cos(math.rad(alpha))
  local path = {" 0 " .. -r .. " m "}
  for i=1,N,1
  do
    table.insert(path, (round(r*sind((2*i-2)*alpha))+2*round(R*sind((2*i-1)*alpha)))/3 .. " " .. (round(-r*cosd((2*i-2)*alpha))+2*round(-R*cosd((2*i-1)*alpha)))/2 .. " " .. (2*round(R*sind((2*i-1)*alpha))+round(r*sind(2*i*alpha)))/3 .. " " .. (2*round(-R*cosd((2*i-1)*alpha))+round(-r*cosd(2*i*alpha)))/3 .. " " .. round(r*sind(2*i*alpha)) .. " " .. round(-r*cosd(2*i*alpha)) .. " c" )
  end
  path = table.concat(path," ")
  return path
end
--]]

local px = 72.0/72.27/65536


local utf8char = utf.char
--~ local utf8char = unicode.utf8.char

local solid_line  = " [] 0 d "
local dashed_line = " [2 1] 0 d "


local function trad_chinese(tfmdata,key,value)
    local rotate_angle, expand_x, expand_y, depth_correction, trace = 0, 1, 1, 0, false
    local  bbox_pen, bbox_rulewd, bbox_color, base_rulewd, base_color, anchor_pen, anchor_rulewd, anchor_color = " 0 J 0 j ", 0, "0 0 0", 0, "0 0 0", " 1 J 1 j ", 0, "0 0 0"
    local anchor = 0
    local spec
    if type(value) == "string" and value ~= "" then
        spec = settings_to_hash(value)
        if spec.baseline then
            depth_correction = tonumber(spec.baseline)
        end
        if spec.anchor then
            spec.anchor = string.lower(spec.anchor)
            anchor = (spec.anchor == "center" ) and 0 or (spec.anchor == "top") and -1 or (spec.anchor == "bottom") and 1 or (spec.anchor == "baseline") and (1-2*depth_correction)
            anchor_rulewd = " 2 w "
            anchor_color = "0 .5 .5" .. " RG "
        end
        if spec.rotate then
            if spec.rotate == "" then
                rotate_angle = 90
            elseif tonumber(spec.rotate) ~= nil then
                rotate_angle = tonumber(spec.rotate)
            end
        end
        if spec.expand then
            local expand_factor = settings_to_hash(spec.expand)
            expand_x = tonumber(expand_factor.x) or 1
            expand_y = tonumber(expand_factor.y) or 1
        end
        if spec.trace then
            spec.trace = string.lower(spec.trace)
            if spec.trace == "true" or spec.trace == "yes" or spec.trace == "on" or spec.trace == "" then
                trace = true
                bbox_rulewd = " 0.5 w "
                bbox_color = " 0 0 .75" .. " RG "
                base_rulewd = " 0.9 w "
                base_color = " .75 0 .75" .. " RG "
            else
            end
        end
    end
    local characters = tfmdata.characters
    local resources  = tfmdata.resources
    local additions  = { }
    local private    = resources.private
    for unicode, old_c in next, characters do
        private = private + 1
        local width  = old_c.width  or 0
        local height = old_c.height or 0
        local depth  = old_c.depth  or 0
        local htsp,wdsp,dpsp = height*px,width*px,depth*px
        local hor_drift = (1+expand_y-2*depth_correction+anchor-anchor*expand_y)*wdsp/2
        depth = 0
        height = width*expand_x
        local commands = { }
        if (private >= 13312 and private <= 19893) or (private >= 19968 and private <= 40891) or (private >= 131072 and private <= 173782) then
            print(utf8char(private) .. " = " .. private)
        end
--~         if characters[unicode]["unicode"] then
--~             print(utf8char(characters[unicode].unicode) .. " = " .. characters[unicode].unicode .. "(0x" .. characters[unicode].tounicode .. ")")
--~         else
--~             print("-------------------nil-----------------------------")
--~         end
        if spec.rotate or spec.expand then
            --[[
            rotate, then expand, after that, shift (1+expand_y)*wdsp/2 to the right the make sure they are aligned at center

            todo:
            add a "method=top|center|baseline|bottom" key, remember to change the corresponding trace pdf literal
            --]]
            add_commands(commands,{ "special", "pdf: q " .. expand_x*cosd(rotate_angle) .. " " .. expand_x*sind(rotate_angle) .. " " .. -expand_y*sind(rotate_angle) .. " " .. expand_y*cosd(rotate_angle) .." " .. hor_drift .. " 0 cm" })
        end
        if trace then
            --[[
            to draw the anchor:
            choose round pen "1 J"
            set color "0 .5 .5 RG"
            set line width "5 w"
            set dash pattern "[] 0 d" (solid)
            make a degenerated line from (0,0) "0 0 m" to (0,0) "0 0 l"
            and draw it "S"
            --]]
            add_commands(commands,{ "special", "pdf: " .. anchor_pen .. anchor_color .. anchor_rulewd .. solid_line .. " 0 " .. wdsp/2-(anchor+depth_correction*2)*wdsp/2 .. " m " .. wdsp .. " " .. wdsp/2-(anchor+depth_correction*2)*wdsp/2 .. " l S " .. bbox_pen .. bbox_color  .. bbox_rulewd .. " 0 " .. -dpsp .. " " .. wdsp .. " " .. (htsp+dpsp) .. " re S " .. base_color .. base_rulewd .. dashed_line .. " 0 0 m " .. wdsp .. " 0 l S" })
        end
            add_commands(commands,{ "slot", 1, private })
        if spec.rotate or spec.expand then
            add_commands(commands,{ "right", -width })
            add_commands(commands,{ "special", "pdf: Q" })
        end
        --[[ -- this one works, but the following ones don't
        add_commands(commands,{ "rule", 500000,2000 })
        --]]
        --[[
        local allowbreak = node.new(node.id('kern'))
        allowbreak.kern = width
        --]]
        --[[
        local allowbreak = node.new(node.id('penalty'))
        allowbreak.penalty = 0
        --]]
        --[[
        local s = node.new("glue_spec")
        local allowbreak = node.new("glue",13)
        s.width = 0
        allowbreak.spec = s
        --]]
        --[[
        local allowbreak = node.new(node.id('glue'))
        allowbreak.spec = node.new(node.id('glue_spec'))
        allowbreak.spec.width = 0
        allowbreak.spec.stretch = 0
        allowbreak.spec.shrink = 0
        --]]
--~         print("allowbreak type = " .. type(allowbreak))
--~         add_commands(commands,{ "node", allowbreak })
        local new_c = { }
        new_c["width"] = width
        new_c["height"] = height
        new_c["depth"] = depth
        new_c["commands"] = commands
        setmetatableindex(new_c,old_c)
        characters[unicode] = new_c
        additions[private]  = old_c
    end
    for k, v in next, additions do
        characters[k] = v
    end
    resources.private = private
end




registerotffeature {
    name        = "wenzi",
    description = "rotate and expand glyphs",
    manipulators = {
        base = trad_chinese,
        node = trad_chinese,
    }
}






local a_linenumber       = attributes.private('linenumber')



















--[[

local function is_cjk_ideo (n)
    -- CJK Ext A
    if n.char >= 13312 and n.char <= 19893 then
	return true
	-- CJK
    elseif n.char >= 19968 and n.char <= 40891 then
	return true
	-- CJK Ext B
    elseif n.char >= 131072 and n.char <= 173782 then
	return true
    else
	return false
    end
end


--~         add_commands(commands,{ "node", allowbreak })

local function allow_all_break (head)
    for n in node.traverse (head) do
        if n.id == node.id ('glyph') then
            local allowbreak = node.new("penalty")
            allowbreak.penalty = 0
            print("allowbreak type is " .. type(allowbreak))
            print("char = " .. n)
            node.insert_after (head, n, allowbreak)
        end
    end
end



moduledata.zhujian = { }
moduledata.zhujian.allow_all_break = allow_all_break

nodes.tasks.appendaction("processors","before","moduledata.zhujian.allow_all_break")

--]]













--[[
show_hyph = function(head)
   while head do
     if head.id == 0 or head.id == 1 then % hlist, vlist
       show_hyph(head.head) % should be head.head in a newer luatex than 0.64
     elseif head.id == 7 then             % disc
       local n = node.new("whatsit","pdf_literal")
       n.mode = 0
       n.data = "q 0.3 w 0 2 m 0 7 l S Q"
       n.next = head.next
       n.prev = head
       head.next = n
       head = n
     end
   head = head.next
   end
   return true
end

moduledata.mystuff = { }
moduledata.mystuff.show_hyph = show_hyph

nodes.tasks.appendaction("finalizers","after","moduledata.mystuff.show_hyph")
nodes.tasks.appendaction("shipouts","after","moduledata.mystuff.show_hyph")
--]]















--[[

local lines              = nodes.lines
local data               = lines.data

local function is_cjk_ideo (n)
    -- CJK Ext A
    if n.char >= 13312 and n.char <= 19893 then
	return true
	-- CJK
    elseif n.char >= 19968 and n.char <= 40891 then
	return true
	-- CJK Ext B
    elseif n.char >= 131072 and n.char <= 173782 then
	return true
    else
	return false
    end
end

local seen={}

local function dump(t,i)
    seen[t]=true
    local s={}
    local n=0
    for k in pairs(t) do
        n=n+1 s[n]=k
    end
    table.sort(s)
    for k,v in ipairs(s) do
        print(i,v)
        v=t[v]
        if type(v)=="table" and not seen[v] then
            dump(v,i.."\t")
        end
    end
end



show_hyph=function(head)
	for n in node.traverse_id(node.id("glyph"), head) do
        if n.char == 12290 then
--~             n.char = 12295
            n.char = 0x25CB
        end
--~ 		if is_cjk_ideo(n) then
--~             dump(data,"")
--~             local linenumber = data[#data].start or 0
--~             if data.entries then
--~                 linenumber = data.entries.linenumber or 0
--~             end
--~             print("Glyph = " .. unicode.utf8.char(n.char) .. "[" .. n.char .. "]")
--~             print("Line number = " .. linenumber)
--~             print("lineno = "..linenumber)
--~             print("\\prevgraf = "..tex.prevgraf)
--~             insert_node_before (head, n, new_penalty(0))
--~             insert_node_after (head, n, new_penalty(0))
--~             local allowbreak = node.new("penalty")
--~             allowbreak.penalty = 0
--~             insert_node_before(head,mynode,new_kern(10pt))
--~ 		end
	end
	return head
end

moduledata.guji.show_hyph = show_hyph

nodes.tasks.appendaction("processors", "before", "moduledata.guji.show_hyph")

--]]















--[[
twolines = function ( head )
  local linecounter = 0

  while head do
    if head.id == 0 then
      linecounter = linecounter + 1
    end

    if linecounter >= showlinescount then
      local lastline=head

      head = head.next
      while head do
        local head_next = head.next
        node.free(head)
        head = head_next
      end

      lastline.next = nil
      return true
    end

    head = head.next
  end
  return true
end


callback.register("post_linebreak_filter",twolines,"twolines")
--]]







--[[
split_pagebox=function()

end

moduledata.guji.split_pagebox = split_pagebox

callback.register('pre_output_filter', moduledata.guji.split_pagebox, "moduledata.guji.split_pagebox")
--]]

































local function set_parameters(font,data)
    -- beware: parameters can be nil in e.g. punk variants
    local quad = quaddata[font]
    full_char_width            = quad
    half_char_width            = quad/2
    quarter_char_width         = quad/4
    inter_char_shrink          = data.inter_char_shrink_factor          * quad
    inter_char_stretch         = data.inter_char_stretch_factor         * quad
    inter_char_half_shrink     = data.inter_char_half_shrink_factor     * quad
    inter_char_half_stretch    = data.inter_char_half_stretch_factor    * quad
    inter_char_quarter_shrink  = data.inter_char_quarter_shrink_factor  * quad
    inter_char_quarter_stretch = data.inter_char_quarter_stretch_factor * quad
    inter_char_hangul_penalty  = data.inter_char_hangul_penalty
end





local inter_char_shrink          = 0
local inter_char_stretch         = 0
local inter_char_half_shrink     = 0
local inter_char_half_stretch    = 0
local inter_char_quarter_shrink  = 0
local inter_char_quarter_stretch = 0

local full_char_width            = 0
local half_char_width            = 0
local quarter_char_width         = 0

local inter_char_hangul_penalty  = 0




local function allow_break(head,current)
    insert_node_after(head,current,new_penalty(inter_char_hangul_penalty))
--~     print("\the\linenumber")
--~     print("lines.data type " .. type(data[1]))
end



local zhujian_1 = {
    jamo_initial     = allow_break,
    korean           = allow_break,
    chinese          = allow_break,
    hiragana         = allow_break,
    katakana         = allow_break,
    half_width_open  = allow_break,
    half_width_close = allow_break,
    full_width_open  = allow_break,
    full_width_close = allow_break,
    full_width_punct = allow_break,
    hyphen           = allow_break,
    non_starter      = allow_break,
    other            = allow_break,
}

local injectors = { -- [previous] [current]
    jamo_final       = zhujian_1,
    korean           = zhujian_1,
    chinese          = zhujian_1,
    hiragana         = zhujian_1,
    katakana         = zhujian_1,
    hyphen           = zhujian_1,
    start            = zhujian_1,
    other            = zhujian_1,
    non_starter      = zhujian_1,
    full_width_open  = zhujian_1,
    half_width_open  = zhujian_1,
    full_width_close = zhujian_1,
    full_width_punct = zhujian_1,
    half_width_close = zhujian_1,
}

local function process(head,first,last)
    if first ~= last then
        local lastfont, previous, last = nil, "start", nil
        while true do
            local upcoming, id = getnext(first), getid(first)
            if id == glyph_code then
                local a = getattr(first,a_scriptstatus)
                local current = numbertocategory[a]
                local action = injectors[previous]
                if action then
                    action = action[current]
                    if action then
                        local font = getfont(first)
                        if font ~= lastfont then
                            lastfont = font
                            set_parameters(font,numbertodataset[getattr(first,a_scriptinjection)])
                        end
                        action(head,first)
                    end
                end
                previous = current
            else -- glue
                local p, n = getprev(first), upcoming
                if p and n then
                    local pid, nid = getid(p), getid(n)
                    if pid == glyph_code and nid == glyph_code then
                        local pa, na = getattr(p,a_scriptstatus), getattr(n,a_scriptstatus)
                        local pcjk, ncjk = pa and numbertocategory[pa], na and numbertocategory[na]
                        if not pcjk                 or not ncjk
                            or pcjk == "korean"     or ncjk == "korean"
                            or pcjk == "other"      or ncjk == "other"
                            or pcjk == "jamo_final" or ncjk == "jamo_initial" then
                            previous = "start"
                        else -- if head ~= first then
                            remove_node(head,first,true)
                            previous = pcjk
                    --    else
                    --        previous = pcjk
                        end
                    else
                        previous = "start"
                    end
                else
                    previous = "start"
                end
            end
            if upcoming == last then -- was stop
                break
            else
                first = upcoming
            end
        end
    end
end

scripts.installmethod {
    name     = "zhujian",
    injector = process,
    datasets = { -- todo: metatables
        default = {
            inter_char_shrink_factor          = 0.50, -- of quad
            inter_char_stretch_factor         = 0.50, -- of quad
            inter_char_half_shrink_factor     = 0.50, -- of quad
            inter_char_half_stretch_factor    = 0.50, -- of quad
            inter_char_quarter_shrink_factor  = 0.50, -- of quad
            inter_char_quarter_stretch_factor = 0.50, -- of quad
            inter_char_hangul_penalty         =   50,
        },
    },
}
