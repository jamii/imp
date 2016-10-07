module Job

using Data
using Query
using JobData
using Base.Test
using BenchmarkTools
import DataFrames
import SQLite

macro query_not(clause)
  quote 
    exists = @query begin 
      exists = true
      $clause
      return (exists::Bool,)
    end
    length(exists[1]) == 0
  end
end

function q1a()
  @query begin 
    info_type.info(it, "top 250 rank")
    movie_info_idx.info_type(mi, it)
    movie_info_idx.movie(mi, t)
    title.title(t, title)
    title.production_year(t, production_year)
    movie_companies.movie(mc, t)
    movie_companies.company_type(mc, ct)
    company_type.kind(ct, "production companies")
    movie_companies.note(mc, note)
    @when !contains(note, "(as Metro-Goldwyn-Mayer Pictures)") &&
      (contains(note, "(co-production)") || contains(note, "(presents)"))
    return (note::String, title::String, production_year::Int64)
  end
end

function q1b()
  @query begin 
    info_type.info(it, "bottom 10 rank")
    movie_info_idx.info_type(mi, it)
    movie_info_idx.movie(mi, t)
    title.title(t, title)
    title.production_year(t, production_year)
    @when 2005 <= production_year <= 2010
    movie_companies.movie(mc, t)
    movie_companies.company_type(mc, ct)
    company_type.kind(ct, "production companies")
    movie_companies.note(mc, note)
    @when !contains(note, "(as Metro-Goldwyn-Mayer Pictures)")
    return (note::String, title::String, production_year::Int64)
  end
end

function q1c()
  @query begin 
    info_type.info(it, "top 250 rank")
    movie_info_idx.info_type(mi, it)
    movie_info_idx.movie(mi, t)
    title.title(t, title)
    title.production_year(t, production_year)
    @when production_year > 2010
    movie_companies.movie(mc, t)
    movie_companies.company_type(mc, ct)
    company_type.kind(ct, "production companies")
    movie_companies.note(mc, note)
    @when !contains(note, "(as Metro-Goldwyn-Mayer Pictures)") &&
      contains(note, "(co-production)")
    return (note::String, title::String, production_year::Int64)
  end
end

function q1d()
  @query begin 
    info_type.info(it, "bottom 10 rank")
    movie_info_idx.info_type(mi, it)
    movie_info_idx.movie(mi, t)
    title.title(t, title)
    title.production_year(t, production_year)
    @when production_year > 2000
    movie_companies.movie(mc, t)
    movie_companies.company_type(mc, ct)
    company_type.kind(ct, "production companies")
    movie_companies.note(mc, note)
    @when !contains(note, "(as Metro-Goldwyn-Mayer Pictures)")
    return (note::String, title::String, production_year::Int64)
  end
end

function q2a()
  @query begin
    keyword.keyword(k, "character-name-in-title")
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.title(t, title)
    movie_companies.movie(mc, t)
    movie_companies.company(mc, cn)
    company_name.country_code(cn, "[de]")
    return (title::String,)
  end
end

function q2b()
  @query begin
    keyword.keyword(k, "character-name-in-title")
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.title(t, title)
    movie_companies.movie(mc, t)
    movie_companies.company(mc, cn)
    company_name.country_code(cn, "[nl]")
    return (title::String,)
  end
end

function q2c()
  @query begin
    keyword.keyword(k, "character-name-in-title")
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.title(t, title)
    movie_companies.movie(mc, t)
    movie_companies.company(mc, cn)
    company_name.country_code(cn, "[sm]")
    return (title::String,)
  end
end

function q2d()
  @query begin
    keyword.keyword(k, "character-name-in-title")
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.title(t, title)
    movie_companies.movie(mc, t)
    movie_companies.company(mc, cn)
    company_name.country_code(cn, "[us]")
    return (title::String,)
  end
end

# "Denish" typo is also in original benchmarks
function q3a()
  infos = Set(["Sweden", "Norway", "Germany", "Denmark", "Swedish", "Denish", "Norwegian", "German"])
  @query begin 
    @when contains(keyword, "sequel")
    keyword.keyword(k, keyword)
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.title(t, title)
    title.production_year(t, production_year)
    @when production_year > 2005
    movie_info.movie(mi, t)
    movie_info.info(mi, info)
    @when info in infos
    return (title::String,)
  end
end

function q3b()
  @query begin 
    @when contains(keyword, "sequel")
    keyword.keyword(k, keyword)
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.title(t, title)
    title.production_year(t, production_year)
    @when production_year > 2010
    movie_info.movie(mi, t)
    movie_info.info(mi, "Bulgaria")
    return (title::String,)
  end
end

# "Denish" typo is also in original benchmarks
function q3c()
  infos = Set(["Sweden","Norway","Germany","Denmark","Swedish","Denish","Norwegian","German","USA","American"])
  @query begin 
    @when contains(keyword, "sequel")
    keyword.keyword(k, keyword)
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.title(t, title)
    title.production_year(t, production_year)
    @when production_year > 1990
    movie_info.movie(mi, t)
    movie_info.info(mi, info)
    @when info in infos
    return (title::String,)
  end
end

function q4a()
  @query begin
    @when contains(keyword, "sequel")
    keyword.keyword(k, keyword)
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.title(t, title)
    title.production_year(t, production_year)
    @when production_year > 2005
    movie_info_idx.movie(mi, t)
    movie_info_idx.info_type(mi, it)
    info_type.info(it, "rating")
    movie_info_idx.info(mi, info)
    @when info > "5.0"
    return (info::String, title::String)
  end
end

function q4b()
  @query begin
    @when contains(keyword, "sequel")
    keyword.keyword(k, keyword)
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.title(t, title)
    title.production_year(t, production_year)
    @when production_year > 2010
    movie_info_idx.movie(mi, t)
    movie_info_idx.info_type(mi, it)
    info_type.info(it, "rating")
    movie_info_idx.info(mi, info)
    @when info > "9.0"
    return (info::String, title::String)
  end
end

function q4c()
  @query begin
    @when contains(keyword, "sequel")
    keyword.keyword(k, keyword)
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.title(t, title)
    title.production_year(t, production_year)
    @when production_year > 1990
    movie_info_idx.movie(mi, t)
    movie_info_idx.info_type(mi, it)
    info_type.info(it, "rating")
    movie_info_idx.info(mi, info)
    @when info > "2.0"
    return (info::String, title::String)
  end
end

function q5a()
  infos = Set(["Sweden","Norway","Germany","Denmark","Swedish","Denish","Norwegian","German"])
  @query begin
    company_type.kind(ct, "production companies")
    movie_companies.company_type(mc, ct)
    movie_companies.note(mc, note)
    @when contains(note, "(theatrical)") && contains(note, "(France)")
    movie_companies.movie(mc, t)
    title.production_year(t, production_year)
    @when production_year > 2005
    title.title(t, title)
    movie_info.movie(mi, t)
    movie_info.info(mi, info)
    @when info in infos
    movie_info.info_type(mi, it) # unused, but it's in original query too
    return (title::String,)
  end
end

function q5b()
  infos = Set(["USA", "America"])
  @query begin
    company_type.kind(ct, "production companies")
    movie_companies.company_type(mc, ct)
    movie_companies.note(mc, note)
    @when contains(note, "(VHS)") && contains(note, "(USA)") && contains(note, "(1994)")
    movie_companies.movie(mc, t)
    title.production_year(t, production_year)
    @when production_year > 2010
    title.title(t, title)
    movie_info.movie(mi, t)
    movie_info.info(mi, info)
    @when info in infos
    movie_info.info_type(mi, it) # unused, but it's in original query too
    return (title::String,)
  end
end

function q5c()
  infos = Set(["Sweden","Norway","Germany","Denmark","Swedish","Denish","Norwegian","German","USA","American"])
  @query begin
    company_type.kind(ct, "production companies")
    movie_companies.company_type(mc, ct)
    movie_companies.note(mc, note)
    @when !contains(note, "(TV)") && contains(note, "(USA)") 
    movie_companies.movie(mc, t)
    title.production_year(t, production_year)
    @when production_year > 1990
    title.title(t, title)
    movie_info.movie(mi, t)
    movie_info.info(mi, info)
    @when info in infos
    movie_info.info_type(mi, it) # unused, but it's in original query too
    return (title::String,)
  end
end

function q6a()
  @query begin
    keyword = "marvel-cinematic-universe"
    keyword.keyword(k, keyword)
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.title(t, title)
    title.production_year(t, production_year)
    @when production_year > 2010
    cast_info.movie(ci, t)
    cast_info.person(ci, n)
    name.name(n, name)
    @when ismatch(r"Downey.*Robert", name)
    return (keyword::String, name::String, title::String)
  end
end

function q6b()
  keywords = Set(["superhero","sequel","second-part","marvel-comics","based-on-comic","tv-special","fight","violence"])
  @query begin
    @when ismatch(r"Downey.*Robert", name)
    name.name(n, name)
    cast_info.person(ci, n)
    cast_info.movie(ci, t)
    title.production_year(t, production_year)
    @when production_year > 2014
    title.title(t, title)
    movie_keyword.movie(mk, t)
    movie_keyword.keyword(mk, k)
    keyword.keyword(k, keyword)
    @when keyword in keywords
    return (keyword::String, name::String, title::String)
  end
end

function q6c()
  @query begin
    keyword = "marvel-cinematic-universe"
    keyword.keyword(k, keyword)
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.title(t, title)
    title.production_year(t, production_year)
    @when production_year > 2014
    cast_info.movie(ci, t)
    cast_info.person(ci, n)
    name.name(n, name)
    @when ismatch(r"Downey.*Robert", name)
    return (keyword::String, name::String, title::String)
  end
end

function q6d()
  keywords = Set(["superhero","sequel","second-part","marvel-comics","based-on-comic","tv-special","fight","violence"])
  @query begin
    @when ismatch(r"Downey.*Robert", name)
    name.name(n, name)
    cast_info.person(ci, n)
    cast_info.movie(ci, t)
    title.production_year(t, production_year)
    @when production_year > 2000
    title.title(t, title)
    movie_keyword.movie(mk, t)
    movie_keyword.keyword(mk, k)
    keyword.keyword(k, keyword)
    @when keyword in keywords
    return (keyword::String, name::String, title::String)
  end
end

function q6e()
  @query begin
    keyword = "marvel-cinematic-universe"
    keyword.keyword(k, keyword)
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.title(t, title)
    title.production_year(t, production_year)
    @when production_year > 2000
    cast_info.movie(ci, t)
    cast_info.person(ci, n)
    name.name(n, name)
    @when ismatch(r"Downey.*Robert", name)
    return (keyword::String, name::String, title::String)
  end
end

function q6f()
  keywords = Set(["superhero","sequel","second-part","marvel-comics","based-on-comic","tv-special","fight","violence"])
  @query begin
    keyword in keywords
    keyword.keyword(k, keyword)
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.title(t, title)
    title.production_year(t, production_year)
    @when production_year > 2000
    cast_info.movie(ci, t)
    cast_info.person(ci, n)
    name.name(n, name)
    return (keyword::String, name::String, title::String)
  end
end

function q7a()
  @query begin
    person_info.note(pi, "Volker Boehm")
    person_info.info_type(pi, it)
    info_type.info(it, "mini biography")
    person_info.person(pi, n)
    name.name(n, name)
    name.name_pcode_cf(n, code)
    @when "A" <= code <= "F"
    name.gender(n, gender)
    @when gender == "m" || (gender == "f" && startswith(name, "B"))
    aka_name.person(an, n)
    aka_name.name(an, aka_name)
    @when contains(aka_name, "a")
    cast_info.person(ci, n)
    cast_info.movie(ci, t)
    title.title(t, title)
    title.production_year(t, production_year)
    @when 1980 <= production_year <= 1995
    movie_link.linked_movie(ml, t)
    movie_link.link_type(ml, lt)
    link_type.link(lt, "features")
    return (name::String, title::String)
  end
end

function q7b()
  @query begin
    person_info.note(pi, "Volker Boehm")
    person_info.info_type(pi, it)
    info_type.info(it, "mini biography")
    person_info.person(pi, n)
    name.name(n, name)
    name.name_pcode_cf(n, code)
    @when "A" <= code <= "F"
    name.gender(n, "m")
    aka_name.person(an, n)
    aka_name.name(an, aka_name)
    @when contains(aka_name, "a")
    cast_info.person(ci, n)
    cast_info.movie(ci, t)
    title.title(t, title)
    title.production_year(t, production_year)
    @when 1980 <= production_year <= 1984
    movie_link.linked_movie(ml, t)
    movie_link.link_type(ml, lt)
    link_type.link(lt, "features")
    return (name::String, title::String)
  end
end

function q7c()
  links = Set(["references", "referenced in", "features", "featured in"])
  @query begin
    info_type.info(it, "mini biography")
    person_info.info_type(pi, it)
    person_info.note(pi, _)
    person_info.info(pi, info)
    person_info.person(pi, n)
    name.name(n, name)
    name.name_pcode_cf(n, code)
    @when "A" <= code <= "F"   
    name.gender(n, gender)
    @when gender == "m" || (gender == "f" && startswith(name, "A"))
    aka_name.person(an, n)
    aka_name.name(an, aka_name)
    @when contains(aka_name, "a") || startswith(aka_name, "A")
    cast_info.person(ci, n)
    cast_info.movie(ci, t)
    title.title(t, title)
    title.production_year(t, production_year)
    @when 1980 <= production_year <= 2010
    movie_link.linked_movie(ml, t)
    movie_link.link_type(ml, lt)
    link_type.link(lt, link)
    @when link in links
    return (name::String, info::String)
  end
end

function q8a()
  @query begin
    cast_info.note(ci, "(voice: English version)")
    cast_info.role(ci, rt)
    role_type.role(rt, "actress")
    cast_info.person(ci, n)
    name.name(n, name)
    @when contains(name, "Yo") && !contains(name, "Yu")
    aka_name.person(an, n)
    aka_name.name(an, aka_name)
    cast_info.movie(ci, t)
    title.title(t, title)
    movie_companies.movie(mc, t)
    movie_companies.note(mc, note)
    @when contains(note, "(Japan)") && !contains(note, "(USA)")
    movie_companies.company(mc, cn)
    company_name.country_code(cn, "[jp]")
    return (aka_name::String, title::String)
  end
end

function q8b()
  @query begin
    cast_info.note(ci, "(voice: English version)")
    cast_info.role(ci, rt)
    role_type.role(rt, "actress")
    cast_info.person(ci, n)
    name.name(n, name)
    @when contains(name, "Yo") && !contains(name, "Yu")
    aka_name.person(an, n)
    aka_name.name(an, aka_name)
    cast_info.movie(ci, t)
    title.production_year(t, production_year)
    @when 2006 <= production_year <= 2007
    title.title(t, title)
    @when startswith(title, "One Piece") || startswith(title, "Dragon Ball Z")
    movie_companies.movie(mc, t)
    movie_companies.note(mc, note)
    @when contains(note, "(Japan)") && !contains(note, "(USA)")
    @when contains(note, "(2006)") || contains(note, "(2007)")
    movie_companies.company(mc, cn)
    company_name.country_code(cn, "[jp]")
    return (aka_name::String, title::String)
  end
end

function q8c()
  @query begin
    role_type.role(rt, "writer")
    cast_info.role(ci, rt)
    cast_info.person(ci, n)
    aka_name.person(an, n)
    aka_name.name(an, aka_name)
    cast_info.movie(ci, t)
    title.title(t, title)
    movie_companies.movie(mc, t)
    movie_companies.company(mc, cn)
    company_name.country_code(cn, "[us]")
    return (aka_name::String, title::String)
  end
end

function q8d()
  @query begin
    role_type.role(rt, "costume designer")
    cast_info.role(ci, rt)
    cast_info.person(ci, n)
    aka_name.person(an, n)
    aka_name.name(an, aka_name)
    cast_info.movie(ci, t)
    title.title(t, title)
    movie_companies.movie(mc, t)
    movie_companies.company(mc, cn)
    company_name.country_code(cn, "[us]")
    return (aka_name::String, title::String)
  end
end

function q9a()
  ci_notes = ["(voice)", "(voice: Japanese version)", "(voice) (uncredited)", "(voice: English version)"]
  @query begin
    role_type.role(rt, "actress")
    cast_info.role(ci, rt)
    cast_info.note(ci, ci_note)
    ci_note in ci_notes 
    cast_info.person(ci, n)
    name.gender(n, "f")
    name.name(n, name)
    @when contains(name, "Ang")
    aka_name.person(an, n)
    aka_name.name(an, aka_name)
    cast_info.person_role(ci, chn)
    char_name.name(chn, char_name)
    cast_info.movie(ci ,t)    
    title.production_year(t, production_year)
    @when 2005 <= production_year <= 2015
    title.title(t, title)
    movie_companies.movie(mc, t)
    movie_companies.company(mc, cn)    
    company_name.country_code(cn, "[us]")
    movie_companies.note(mc, mc_note)
    @when contains(mc_note, "(USA)") || contains(mc_note, "(worldwide)")
    return (aka_name::String, char_name::String, title::String)
  end
end

function q9b()
  @query begin
    role_type.role(rt, "actress")
    cast_info.role(ci, rt)
    cast_info.note(ci, "(voice)")
    cast_info.person(ci, n)
    name.gender(n, "f")
    name.name(n, name)
    @when contains(name, "Angel")
    aka_name.person(an, n)
    aka_name.name(an, aka_name)
    cast_info.person_role(ci, chn)
    char_name.name(chn, char_name)
    cast_info.movie(ci ,t)    
    title.production_year(t, production_year)
    @when 2007 <= production_year <= 2010
    title.title(t, title)
    movie_companies.movie(mc, t)
    movie_companies.company(mc, cn)    
    company_name.country_code(cn, "[us]")
    movie_companies.note(mc, mc_note)
    @when contains(mc_note, "(USA)") || contains(mc_note, "(worldwide)")
    @when ismatch(r"\(200.\)", mc_note)
    return (aka_name::String, char_name::String, name::String, title::String)
  end
end

function q9c()
  ci_notes = ["(voice)", "(voice: Japanese version)", "(voice) (uncredited)", "(voice: English version)"]
  @query begin
    role_type.role(rt, "actress")
    cast_info.role(ci, rt)
    cast_info.note(ci, ci_note)
    ci_note in ci_notes 
    cast_info.person(ci, n)
    name.gender(n, "f")
    name.name(n, name)
    @when contains(name, "An")
    aka_name.person(an, n)
    aka_name.name(an, aka_name)
    cast_info.person_role(ci, chn)
    char_name.name(chn, char_name)
    cast_info.movie(ci ,t)    
    title.title(t, title)
    movie_companies.movie(mc, t)
    movie_companies.company(mc, cn)    
    company_name.country_code(cn, "[us]")
    return (aka_name::String, char_name::String, name::String, title::String)
  end
end

function q9d()
  ci_notes = ["(voice)", "(voice: Japanese version)", "(voice) (uncredited)", "(voice: English version)"]
  @query begin
    role_type.role(rt, "actress")
    cast_info.role(ci, rt)
    cast_info.note(ci, ci_note)
    ci_note in ci_notes 
    cast_info.person(ci, n)
    name.gender(n, "f")
    name.name(n, name)
    aka_name.person(an, n)
    aka_name.name(an, aka_name)
    cast_info.person_role(ci, chn)
    char_name.name(chn, char_name)
    cast_info.movie(ci ,t)    
    title.title(t, title)
    movie_companies.movie(mc, t)
    movie_companies.company(mc, cn)    
    company_name.country_code(cn, "[us]")
    return (aka_name::String, char_name::String, name::String, title::String)
  end
end

function q10a()
  @query begin
    company_name.country_code(cn, "[ru]")
    movie_companies.company(mc, cn)
    movie_companies.movie(mc, t)    
    title.production_year(t, production_year)
    @when production_year > 2005
    cast_info.movie(ci, t)
    cast_info.role(ci, rt)
    role_type.role(rt, "actor")
    cast_info.note(ci, note)
    @when contains(note, "(voice)") && contains(note, "(uncredited)")
    cast_info.person_role(ci, chn)
    char_name.name(chn, char_name)
    title.title(t, title)
    return (char_name::String, title::String)
  end
end

function q10b()
  @query begin
    company_name.country_code(cn, "[ru]")
    movie_companies.company(mc, cn)
    movie_companies.movie(mc, t)    
    title.production_year(t, production_year)
    @when production_year > 2010
    cast_info.movie(ci, t)
    cast_info.role(ci, rt)
    role_type.role(rt, "actor")
    cast_info.note(ci, note)
    @when contains(note, "(producer)")
    cast_info.person_role(ci, chn)
    char_name.name(chn, char_name)
    title.title(t, title)
    return (char_name::String, title::String)
  end
end

function q10c()
  @query begin
    company_name.country_code(cn, "[us]")
    movie_companies.company(mc, cn)
    movie_companies.movie(mc, t)    
    title.production_year(t, production_year)
    @when production_year > 1990
    cast_info.movie(ci, t)
    cast_info.note(ci, note)
    @when contains(note, "(producer)")
    cast_info.person_role(ci, chn)
    char_name.name(chn, char_name)
    title.title(t, title)
    return (char_name::String, title::String)
  end
end

function q11a()
  @query begin
    keyword.keyword(k, "sequel")
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.production_year(t, production_year)
    @when 1950 <= production_year <= 2000
    movie_companies.movie(mc, t)
    movie_companies.company_type(mc, ct)
    company_type.kind(ct, "production companies")
    movie_companies.company(mc, cn)
    company_name.name(cn, name)
    @when contains(name, "Film") || contains(name, "Warner")
    company_name.country_code(cn, code)
    @when code != "[pl]"
    movie_link.movie(ml, t)
    movie_link.link_type(ml, lt)
    link_type.link(lt, link)
    @when contains(link, "follow")
    title.title(t, title)
    @when @query_not movie_companies.note($mc, _)
    return (name::String, link::String, title::String)
  end
end

function q11b()
  @query begin
    keyword.keyword(k, "sequel")
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.production_year(t, Int16(1998))
    title.title(t, title)
    @when contains(title, "Money")
    movie_companies.movie(mc, t)
    movie_companies.company_type(mc, ct)
    company_type.kind(ct, "production companies")
    movie_companies.company(mc, cn)
    company_name.name(cn, name)
    @when contains(name, "Film") || contains(name, "Warner")
    company_name.country_code(cn, code)
    @when code != "[pl]"
    movie_link.movie(ml, t)
    movie_link.link_type(ml, lt)
    link_type.link(lt, link)
    @when contains(link, "follows")
    @when @query_not movie_companies.note($mc, _)
    return (name::String, link::String, title::String)
  end
end

function q11c()
  keywords = ["sequel", "revenge", "based-on-novel"]
  @query begin
    keyword in keywords
    keyword.keyword(k, keyword)
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.production_year(t, production_year)
    @when production_year > 1950
    title.title(t, title)
    movie_companies.movie(mc, t)
    movie_companies.company_type(mc, ct)
    company_type.kind(ct, kind)
    @when kind != "production companies"
    movie_companies.company(mc, cn)
    company_name.name(cn, name)
    @when startswith(name, "20th Century Fox") || startswith(name, "Twentieth Century Fox")
    company_name.country_code(cn, code)
    @when code != "[pl]"
    movie_companies.note(mc, note)
    movie_link.movie(ml, t)
    movie_link.link_type(ml, lt)
    link_type.link(lt, _)
    return (name::String, note::String, title::String)
  end
end

function q11d()
  keywords = ["sequel", "revenge", "based-on-novel"]
  @query begin
    keyword in keywords
    keyword.keyword(k, keyword)
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.production_year(t, production_year)
    @when production_year > 1950
    title.title(t, title)
    movie_companies.movie(mc, t)
    movie_companies.company_type(mc, ct)
    company_type.kind(ct, kind)
    @when kind != "production companies"
    movie_companies.company(mc, cn)
    company_name.name(cn, name)
    company_name.country_code(cn, code)
    @when code != "[pl]"
    movie_companies.note(mc, note)
    movie_link.movie(ml, t)
    movie_link.link_type(ml, lt)
    link_type.link(lt, _)
    return (name::String, note::String, title::String)
  end
end

function q12a()
  genres = ["Drama", "Horror"]
  @query begin
    genre in genres
    movie_info.info(mi, genre)
    movie_info.info_type(mi, it1)
    info_type.info(it1, "genres")
    movie_info.movie(mi, t)
    movie_info_idx.movie(mii, t)
    movie_info_idx.info_type(mii, it2)
    info_type.info(it2, "rating")
    movie_info_idx.info(mii, rating)
    @when rating > "8.0"
    title.production_year(t, production_year)
    @when 2005 <= production_year <= 2008
    movie_companies.movie(mc, t)
    movie_companies.company(mc, cn)
    company_name.country_code(cn, "[us]")
    movie_companies.company_type(mc, ct)
    company_type.kind(ct, "production companies")
    company_name.name(cn, name)
    title.title(t, title)
    return (name::String, rating::String, title::String)
  end
end

function q12b()
  kinds = Set(["production companies", "distributors"])
  @query begin
    info_type.info(it2, "bottom 10 rank")
    movie_info_idx.info_type(mii, it2)
    movie_info_idx.movie(mii, t)
    title.production_year(t, production_year)
    @when production_year > 2000
    title.title(t, title)
    @when startswith(title, "Birdemic") || contains(title, "Movie")
    movie_companies.movie(mc, t)
    movie_companies.company(mc, cn)
    company_name.country_code(cn, "[us]")
    movie_companies.company_type(mc, ct)
    company_type.kind(ct, kind)
    @when kind in kinds
    movie_info.movie(mi, t)
    movie_info.info_type(mi, it1)
    info_type.info(it1, "budget")
    movie_info.info(mi, budget)
    return (budget::String, title::String)
  end
end

function q12c()
  genres = ["Drama", "Horror", "Western", "Family"]
  @query begin
    genre in genres
    movie_info.info(mi, genre)
    movie_info.info_type(mi, it1)
    info_type.info(it1, "genres")
    movie_info.movie(mi, t)
    movie_info_idx.movie(mii, t)
    movie_info_idx.info_type(mii, it2)
    info_type.info(it2, "rating")
    movie_info_idx.info(mii, rating)
    @when rating > "7.0"
    title.production_year(t, production_year)
    @when 2000 <= production_year <= 2010
    movie_companies.movie(mc, t)
    movie_companies.company(mc, cn)
    company_name.country_code(cn, "[us]")
    movie_companies.company_type(mc, ct)
    company_type.kind(ct, "production companies")
    company_name.name(cn, name)
    title.title(t, title)
    return (name::String, rating::String, title::String)
  end
end

function q13a()
  @query begin
    company_name.country_code(cn, "[de]")
    movie_companies.company(mc, cn)
    movie_companies.company_type(mc, ct)
    company_type.kind(ct, "production companies")
    movie_companies.movie(mc, t)
    title.kind(t, kt)
    kind_type.kind(kt, "movie")
    title.title(t, title)
    movie_info_idx.movie(mii, t)
    movie_info_idx.info_type(mii, it1)
    info_type.info(it1, "rating")
    movie_info_idx.info(mii, rating)
    movie_info.movie(mi, t)
    movie_info.info_type(mi, it2)
    info_type.info(it2, "release dates")
    movie_info.info(mi, release_date)
    return (release_date::String, rating::String, title::String)
  end
end

function q13b()
  @query begin
    company_name.country_code(cn, "[us]")
    movie_companies.company(mc, cn)
    movie_companies.company_type(mc, ct)
    company_type.kind(ct, "production companies")
    movie_companies.movie(mc, t)
    title.kind(t, kt)
    kind_type.kind(kt, "movie")
    title.title(t, title)
    @when contains(title, "Champion") || contains(title, "Loser")
    movie_companies.company(mc, cn)
    company_name.name(cn, name)
    movie_info_idx.movie(mii, t)
    movie_info_idx.info_type(mii, it1)
    info_type.info(it1, "rating")
    movie_info_idx.info(mii, rating)
    movie_info.movie(mi, t)
    movie_info.info_type(mi, it2)
    info_type.info(it2, "release dates")
    movie_info.info(mi, _)
    return (name::String, rating::String, title::String)
  end
end

function q13c()
  @query begin
    company_name.country_code(cn, "[us]")
    movie_companies.company(mc, cn)
    movie_companies.company_type(mc, ct)
    company_type.kind(ct, "production companies")
    movie_companies.movie(mc, t)
    title.kind(t, kt)
    kind_type.kind(kt, "movie")
    title.title(t, title)
    @when startswith(title, "Champion") || startswith(title, "Loser")
    movie_companies.company(mc, cn)
    company_name.name(cn, name)
    movie_info_idx.movie(mii, t)
    movie_info_idx.info_type(mii, it1)
    info_type.info(it1, "rating")
    movie_info_idx.info(mii, rating)
    movie_info.movie(mi, t)
    movie_info.info_type(mi, it2)
    info_type.info(it2, "release dates")
    movie_info.info(mi, _)
    return (name::String, rating::String, title::String)
  end
end

function q13d()
  @query begin
    company_name.country_code(cn, "[us]")
    movie_companies.company(mc, cn)
    movie_companies.company_type(mc, ct)
    company_type.kind(ct, "production companies")
    movie_companies.movie(mc, t)
    title.kind(t, kt)
    kind_type.kind(kt, "movie")
    title.title(t, title)
    movie_companies.company(mc, cn)
    company_name.name(cn, name)
    movie_info_idx.movie(mii, t)
    movie_info_idx.info_type(mii, it1)
    info_type.info(it1, "rating")
    movie_info_idx.info(mii, rating)
    movie_info.movie(mi, t)
    movie_info.info_type(mi, it2)
    info_type.info(it2, "release dates")
    movie_info.info(mi, release_date)
    return (name::String, rating::String, title::String)
  end
end

function q14a()
  keywords = ["murder", "murder-in-title", "blood", "violence"]
  countries = Set(["Sweden","Norway","Germany","Denmark","Swedish","Denish","Norwegian","German","USA","American"])
  @query begin
    keyword in keywords
    keyword.keyword(k, keyword)
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.kind(t, kt)
    kind_type.kind(kt, "movie")
    title.production_year(t, production_year)
    @when production_year > 2010
    title.title(t, title)
    movie_info.movie(mi, t)
    movie_info.info_type(mi, it1)
    info_type.info(it1, "countries")
    movie_info.info(mi, country)
    @when country in countries
    movie_info_idx.movie(mii, t)
    movie_info_idx.info_type(mii, it2)
    info_type.info(it2, "rating")
    movie_info_idx.info(mii, rating)
    @when rating < "8.5"
    return (rating::String, title::String)
  end
end

function q14b()
  keywords = ["murder", "murder-in-title"]
  countries = Set(["Sweden","Norway","Germany","Denmark","Swedish","Denish","Norwegian","German","USA","American"])
  @query begin
    keyword in keywords
    keyword.keyword(k, keyword)
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.kind(t, kt)
    kind_type.kind(kt, "movie")
    title.production_year(t, production_year)
    @when production_year > 2010
    title.title(t, title)
    @when contains(title, "murder") || contains(title, "Murder") || contains(title, "Mord")
    movie_info.movie(mi, t)
    movie_info.info_type(mi, it1)
    info_type.info(it1, "countries")
    movie_info.info(mi, country)
    @when country in countries
    movie_info_idx.movie(mii, t)
    movie_info_idx.info_type(mii, it2)
    info_type.info(it2, "rating")
    movie_info_idx.info(mii, rating)
    @when rating > "6.0"
    return (rating::String, title::String)
  end
end

function q14c()
  keywords = ["murder", "murder-in-title", "blood", "violence"]
  countries = Set(["Sweden","Norway","Germany","Denmark","Swedish","Denish","Norwegian","German","USA","American"])
  kinds = Set(["movie", "episode"])
  @query begin
    keyword in keywords
    keyword.keyword(k, keyword)
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.kind(t, kt)
    kind_type.kind(kt, kind)
    @when kind in kinds
    title.production_year(t, production_year)
    @when production_year > 2005
    title.title(t, title)
    movie_info.movie(mi, t)
    movie_info.info_type(mi, it1)
    info_type.info(it1, "countries")
    movie_info.info(mi, country)
    @when country in countries
    movie_info_idx.movie(mii, t)
    movie_info_idx.info_type(mii, it2)
    info_type.info(it2, "rating")
    movie_info_idx.info(mii, rating)
    @when rating < "8.5"
    return (rating::String, title::String)
  end
end


function q15a()
  @query begin
    company_name.country_code(cn, "[us]")
    movie_companies.company(mc, cn)
    movie_companies.note(mc, mc_note)
    @when ismatch(r"\(200.\)", mc_note) && contains(mc_note, "(worldwide)")
    movie_companies.movie(mc, t)
    title.production_year(t, production_year)
    @when production_year > 2000
    title.title(t, title)
    movie_info.movie(mi, t)
    movie_info.info_type(mi, it)
    info_type.info(it, "release dates")
    movie_info.note(mi, mi_note)
    @when contains(mi_note, "internet")
    movie_info.info(mi, release_date)
    @when ismatch(r"USA:.* 200.", release_date)
    movie_keyword.movie(_, t)
    aka_title.movie(_, t)
    return (release_date::String, title::String)
  end
end

function q15b()
  @query begin
    company_name.country_code(cn, "[us]")
    company_name.name(cn, "YouTube")
    movie_companies.company(mc, cn)
    movie_companies.note(mc, mc_note)
    @when ismatch(r"\(200.\)", mc_note) && contains(mc_note, "(worldwide)")
    movie_companies.movie(mc, t)
    title.production_year(t, production_year)
    @when 2005 <= production_year <= 2010
    title.title(t, title)
    movie_info.movie(mi, t)
    movie_info.info_type(mi, it)
    info_type.info(it, "release dates")
    movie_info.note(mi, mi_note)
    @when contains(mi_note, "internet")
    movie_info.info(mi, release_date)
    @when ismatch(r"USA:.* 200.", release_date)
    aka_title.movie(_, t)
    movie_keyword.movie(_, t)
    return (release_date::String, title::String)
  end
end

function q15c()
  @query begin
    company_name.country_code(cn, "[us]")
    movie_companies.company(mc, cn)
    movie_companies.movie(mc, t)
    title.production_year(t, production_year)
    title.title(t, title)
    @when production_year > 1990
    movie_info.movie(mi, t)
    movie_info.info_type(mi, it)
    info_type.info(it, "release dates")
    movie_info.note(mi, mi_note)
    @when contains(mi_note, "internet")
    movie_info.info(mi, release_date)
    @when ismatch(r"USA:.* (199|200).", release_date)
    aka_title.movie(_, t)
    movie_keyword.movie(_, t)
    return (release_date::String, title::String)
  end
end

function q15d()
  @query begin
    company_name.country_code(cn, "[us]")
    movie_companies.company(mc, cn)
    movie_companies.movie(mc, t)
    title.production_year(t, production_year)
    title.title(t, title)
    @when production_year > 1990
    movie_info.movie(mi, t)
    movie_info.info_type(mi, it)
    info_type.info(it, "release dates")
    movie_info.note(mi, mi_note)
    @when contains(mi_note, "internet")
    aka_title.movie(at, t)
    aka_title.title(at, aka_title)
    movie_keyword.movie(_, t)
    return (aka_title::String, title::String)
  end
end

function q16a()
  @query begin
    keyword.keyword(k, "character-name-in-title")
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.episode_nr(t, episode_nr)
    @when 50 <= episode_nr < 100
    title.title(t, title)
    movie_companies.movie(mc, t)
    movie_companies.company(mc, cn)
    company_name.country_code(cn, "[us]")
    cast_info.movie(ci, t)
    cast_info.person(ci, n)
    aka_name.person(an, n)
    aka_name.name(an, aka_name)
    return (aka_name::String, title::String)
  end
end

function q16b()
  @query begin
    keyword.keyword(k, "character-name-in-title")
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.title(t, title)
    movie_companies.movie(mc, t)
    movie_companies.company(mc, cn)
    company_name.country_code(cn, "[us]")
    cast_info.movie(ci, t)
    cast_info.person(ci, n)
    aka_name.person(an, n)
    aka_name.name(an, aka_name)
    return (aka_name::String, title::String)
  end
end

function q16c()
  @query begin
    keyword.keyword(k, "character-name-in-title")
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.episode_nr(t, episode_nr)
    @when episode_nr < 100
    title.title(t, title)
    movie_companies.movie(mc, t)
    movie_companies.company(mc, cn)
    company_name.country_code(cn, "[us]")
    cast_info.movie(ci, t)
    cast_info.person(ci, n)
    aka_name.person(an, n)
    aka_name.name(an, aka_name)
    return (aka_name::String, title::String)
  end
end

function q16d()
  @query begin
    keyword.keyword(k, "character-name-in-title")
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.episode_nr(t, episode_nr)
    @when 5 <= episode_nr < 100
    title.title(t, title)
    movie_companies.movie(mc, t)
    movie_companies.company(mc, cn)
    company_name.country_code(cn, "[us]")
    cast_info.movie(ci, t)
    cast_info.person(ci, n)
    aka_name.person(an, n)
    aka_name.name(an, aka_name)
    return (aka_name::String, title::String)
  end
end

function q17a()
  @query begin
    keyword.keyword(k, "character-name-in-title")
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    cast_info.movie(ci, t)
    cast_info.person(ci, n)
    name.name(n, name)
    @when startswith(name, "B")
    movie_companies.movie(mc, t)
    movie_companies.company(mc, cn)
    company_name.country_code(cn, "[us]")
    return (name::String, name::String)
  end
end

function q17b()
  @query begin
    keyword.keyword(k, "character-name-in-title")
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    cast_info.movie(ci, t)
    cast_info.person(ci, n)
    name.name(n, name)
    @when startswith(name, "Z")
    movie_companies.movie(_, t)
    return (name::String, name::String)
  end
end

function q17c()
  @query begin
    keyword.keyword(k, "character-name-in-title")
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    cast_info.movie(ci, t)
    cast_info.person(ci, n)
    name.name(n, name)
    @when startswith(name, "X")
    movie_companies.movie(_, t)
    return (name::String, name::String)
  end
end

function q17d()
  @query begin
    keyword.keyword(k, "character-name-in-title")
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    cast_info.movie(ci, t)
    cast_info.person(ci, n)
    name.name(n, name)
    @when contains(name, "Bert")
    movie_companies.movie(_, t)
    return (name::String,)
  end
end

function q17e()
  @query begin
    keyword.keyword(k, "character-name-in-title")
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    movie_companies.movie(mc, t)
    movie_companies.company(mc, cn)
    company_name.country_code(cn, "[us]")
    cast_info.movie(ci, t)
    cast_info.person(ci, n)
    name.name(n, name)
    return (name::String,)
  end
end

function q17f()
  @query begin
    keyword.keyword(k, "character-name-in-title")
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    cast_info.movie(ci, t)
    cast_info.person(ci, n)
    name.name(n, name)
    @when contains(name, "B")
    movie_companies.movie(_, t)
    return (name::String,)
  end
end

function q18a()
  ci_notes = ["(producer)", "(executive producer)"]
  @query begin
    ci_note in ci_notes
    cast_info.note(ci, ci_note)
    cast_info.person(ci, n)
    name.gender(n, "m")
    name.name(n, name)
    @when contains(name, "Tim")
    cast_info.movie(ci, t)
    title.title(t, title)
    movie_info.movie(mi, t)
    movie_info.info_type(mi, it1)
    info_type.info(it1, "budget")
    movie_info.info(mi, budget)
    movie_info_idx.movie(mii, t)
    movie_info_idx.info_type(mii, it2)
    info_type.info(it2, "votes")
    movie_info_idx.info(mii, votes)
    return (budget::String, votes::String, title::String)
  end
end

function q18b()
  ci_notes = ["(writer)", "(head writer)", "(written by)", "(story)", "(story editor)"]
  genres = ["Horror", "Thriller"]
  @query begin
    genre in genres
    movie_info.info(mi, genre)
    info_type.info(it1, "genres")
    movie_info.info_type(mi, it1)
    movie_info.movie(mi, t)
    title.production_year(t, production_year)
    @when 2008 <= production_year <= 2014
    movie_info_idx.movie(mii, t)
    movie_info_idx.info_type(mii, it2)
    info_type.info(it2, "rating")
    movie_info_idx.info(mii, rating)
    @when rating > "8.0"
    title.title(t, title)
    cast_info.movie(ci, t)
    cast_info.note(ci, ci_note)
    @when ci_note in ci_notes
    cast_info.person(ci, n)
    name.gender(n, "f")
    return (genre::String, rating::String, title::String)
  end
end

function q19a()
  ci_notes = ["(voice)", "(voice: Japanese version)", "(voice) (uncredited)", "(voice: English version)"]
  @query begin
    ci_note in ci_notes 
    cast_info.note(ci, ci_note)
    cast_info.role(ci, rt)
    role_type.role(rt, "actress")
    cast_info.movie(ci, t)    
    title.production_year(t, production_year)
    @when 2005 <= production_year <= 2009
    cast_info.person(ci, n)
    name.gender(n, "f")
    name.name(n, name)
    @when contains(name, "Ang")
    title.title(t, title)
    movie_companies.movie(mc, t)
    movie_companies.company(mc, cn)    
    company_name.country_code(cn, "[us]")
    movie_companies.note(mc, mc_note)
    @when contains(mc_note, "(USA)") || contains(mc_note, "(worldwide)")
    movie_info.movie(mi, t)
    movie_info.info_type(mi, it)
    info_type.info(it, "release dates")
    movie_info.info(mi, release_date)
    @when ismatch(r"Japan:.*200.", release_date) || ismatch(r"USA:.*200.", release_date)
    aka_name.person(_, n)
    cast_info.person_role(ci, chn)
    char_name.name(chn, _)
    return (name::String, title::String)
  end
end

function q19b()
  @query begin
    cast_info.note(ci, "(voice)")
    cast_info.role(ci, rt)
    role_type.role(rt, "actress")
    cast_info.movie(ci, t)    
    title.production_year(t, production_year)
    @when 2007 <= production_year <= 2008
    cast_info.person(ci, n)
    name.gender(n, "f")
    name.name(n, name)
    @when contains(name, "Angel")
    title.title(t, title)
    @when ismatch(r"Kung.*Fu.*Panda", title)
    movie_companies.movie(mc, t)
    movie_companies.company(mc, cn)    
    company_name.country_code(cn, "[us]")
    movie_companies.note(mc, mc_note)
    @when ismatch(r"\(200.*\)", mc_note)
    @when contains(mc_note, "(USA)") || contains(mc_note, "(worldwide)")
    movie_info.movie(mi, t)
    movie_info.info_type(mi, it)
    info_type.info(it, "release dates")
    movie_info.info(mi, release_date)
    @when ismatch(r"Japan:.*2007", release_date) || ismatch(r"USA:.*2008", release_date)
    aka_name.person(_, n)
    cast_info.person_role(ci, chn)
    char_name.name(chn, _)
    return (name::String, title::String)
  end
end

function q19c()
  ci_notes = ["(voice)", "(voice: Japanese version)", "(voice) (uncredited)", "(voice: English version)"]
  @query begin
    ci_note in ci_notes 
    cast_info.note(ci, ci_note)
    cast_info.role(ci, rt)
    role_type.role(rt, "actress")
    cast_info.movie(ci, t)    
    title.production_year(t, production_year)
    @when production_year > 2000
    cast_info.person(ci, n)
    name.gender(n, "f")
    name.name(n, name)
    @when contains(name, "An")
    title.title(t, title)
    movie_companies.movie(mc, t)
    movie_companies.company(mc, cn)    
    company_name.country_code(cn, "[us]")
    movie_info.movie(mi, t)
    movie_info.info_type(mi, it)
    info_type.info(it, "release dates")
    movie_info.info(mi, release_date)
    @when ismatch(r"Japan:.*200.", release_date) || ismatch(r"USA:.*200.", release_date)
    aka_name.person(_, n)
    cast_info.person_role(ci, chn)
    char_name.name(chn, _)
    return (name::String, title::String)
  end
end

function q19d()
  ci_notes = ["(voice)", "(voice: Japanese version)", "(voice) (uncredited)", "(voice: English version)"]
  @query begin
    ci_note in ci_notes 
    cast_info.note(ci, ci_note)
    cast_info.role(ci, rt)
    role_type.role(rt, "actress")
    cast_info.movie(ci, t)    
    title.production_year(t, production_year)
    @when production_year > 2000
    cast_info.person(ci, n)
    name.gender(n, "f")
    name.name(n, name)
    title.title(t, title)
    movie_companies.movie(mc, t)
    movie_companies.company(mc, cn)    
    company_name.country_code(cn, "[us]")
    movie_info.movie(mi, t)
    movie_info.info_type(mi, it)
    info_type.info(it, "release dates")
    movie_info.info(mi, _)
    aka_name.person(_, n)
    cast_info.person_role(ci, chn)
    char_name.name(chn, _)
    return (name::String, title::String)
  end
end

function q20a()
  keywords = Set(["superhero","sequel","second-part","marvel-comics","based-on-comic","tv-special","fight","violence"])
  @query begin
    keyword in keywords
    keyword.keyword(k, keyword)
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.kind(t, kt)
    kind_type.kind(kt, "movie")
    title.production_year(t, production_year)
    @when production_year > 1950
    title.title(t, title)
    cast_info.movie(ci, t)
    cast_info.person_role(ci, chn)
    char_name.name(chn, char_name)
    @when !contains(char_name, "Sherlock")
    @when ismatch(r"Tony.*Stark", char_name) || ismatch(r"Iron.*Man", char_name)
    cast_info.person(ci, n)
    name.name(n, _)
    complete_cast.movie(cc, t)
    complete_cast.subject(cc, cct1)
    comp_cast_type.kind(cct1, "cast")
    complete_cast.status(cc, cct2)
    comp_cast_type.kind(cct2, cct2_kind)
    @when contains(cct2_kind, "complete")
    return (title::String,)
  end
end

function q20b()
  keywords = Set(["superhero","sequel","second-part","marvel-comics","based-on-comic","tv-special","fight","violence"])
  @query begin
    keyword in keywords
    keyword.keyword(k, keyword)
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.kind(t, kt)
    kind_type.kind(kt, "movie")
    title.production_year(t, production_year)
    @when production_year > 2000
    title.title(t, title)
    cast_info.movie(ci, t)
    cast_info.person_role(ci, chn)
    char_name.name(chn, char_name)
    @when !contains(char_name, "Sherlock")
    @when ismatch(r"Tony.*Stark", char_name) || ismatch(r"Iron.*Man", char_name)
    cast_info.person(ci, n)
    name.name(n, name)
    @when ismatch(r"Downey.*Robert", name)
    complete_cast.movie(cc, t)
    complete_cast.subject(cc, cct1)
    comp_cast_type.kind(cct1, "cast")
    complete_cast.status(cc, cct2)
    comp_cast_type.kind(cct2, cct2_kind)
    @when contains(cct2_kind, "complete")
    return (title::String,)
  end
end

function q20c()
  keywords = Set(["superhero","marvel-comics","based-on-comic","tv-special","fight","violence","magnet","web","claw","laser"])
  @query begin
    keyword in keywords
    keyword.keyword(k, keyword)
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.kind(t, kt)
    kind_type.kind(kt, "movie")
    title.production_year(t, production_year)
    @when production_year > 2000
    title.title(t, title)
    cast_info.movie(ci, t)
    cast_info.person_role(ci, chn)
    char_name.name(chn, char_name)
    @when contains(char_name, "man") || contains(char_name, "Man")
    cast_info.person(ci, n)
    name.name(n, name)
    complete_cast.movie(cc, t)
    complete_cast.subject(cc, cct1)
    comp_cast_type.kind(cct1, "cast")
    complete_cast.status(cc, cct2)
    comp_cast_type.kind(cct2, cct2_kind)
    @when contains(cct2_kind, "complete")
    return (name::String, title::String,)
  end
end

function q21a()
  infos = Set(["Sweden","Norway","Germany","Denmark","Swedish","Denish","Norwegian","German"])
  @query begin
    keyword.keyword(k, "sequel")
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.production_year(t, production_year)
    @when 1950 <= production_year <= 2000
    title.title(t, title)
    movie_companies.movie(mc, t)
    movie_companies.company_type(mc, ct)
    company_type.kind(ct, "production companies")
    movie_companies.company(mc, cn)
    company_name.name(cn, name)
    @when contains(name, "Film") || contains(name, "Warner")
    company_name.country_code(cn, code)
    @when code != "[pl]"
    @when @query_not movie_companies.note($mc, _)
    movie_info.movie(mi, t)
    movie_info.info(mi, info)
    @when info in infos
    movie_link.movie(ml, t)
    movie_link.link_type(ml, lt)
    link_type.link(lt, link)
    @when contains(link, "follow")
    return (name::String, link::String, title::String)
  end
end

function q21b()
  infos = Set(["Germany","German"])
  @query begin
    keyword.keyword(k, "sequel")
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.production_year(t, production_year)
    @when 2000 <= production_year <= 2010
    title.title(t, title)
    movie_companies.movie(mc, t)
    movie_companies.company_type(mc, ct)
    company_type.kind(ct, "production companies")
    movie_companies.company(mc, cn)
    company_name.name(cn, name)
    @when contains(name, "Film") || contains(name, "Warner")
    company_name.country_code(cn, code)
    @when code != "[pl]"
    @when @query_not movie_companies.note($mc, _)
    movie_info.movie(mi, t)
    movie_info.info(mi, info)
    @when info in infos
    movie_link.movie(ml, t)
    movie_link.link_type(ml, lt)
    link_type.link(lt, link)
    @when contains(link, "follow")
    return (name::String, link::String, title::String)
  end
end

function q21c()
  infos = Set(["Sweden","Norway","Germany","Denmark","Swedish","Denish","Norwegian","German","English"])
  @query begin
    keyword.keyword(k, "sequel")
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.production_year(t, production_year)
    @when 1950 <= production_year <= 2010
    title.title(t, title)
    movie_companies.movie(mc, t)
    movie_companies.company_type(mc, ct)
    company_type.kind(ct, "production companies")
    movie_companies.company(mc, cn)
    company_name.name(cn, name)
    @when contains(name, "Film") || contains(name, "Warner")
    company_name.country_code(cn, code)
    @when code != "[pl]"
    @when @query_not movie_companies.note($mc, _)
    movie_info.movie(mi, t)
    movie_info.info(mi, info)
    @when info in infos
    movie_link.movie(ml, t)
    movie_link.link_type(ml, lt)
    link_type.link(lt, link)
    @when contains(link, "follow")
    return (name::String, link::String, title::String)
  end
end

function q22a()
  keywords = ["murder", "murder-in-title", "blood", "violence"]
  kinds = Set(["movie", "episode"])
  countries = Set(["Germany","German","USA","American"])
  @query begin
    keyword in keywords
    keyword.keyword(k, keyword)
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.production_year(t, production_year)
    @when production_year > 2008
    title.kind(t, kt)
    kind_type.kind(kt, kind)
    @when kind in kinds
    movie_companies.movie(mc, t)
    movie_companies.company(mc, cn)
    company_name.country_code(cn, code)
    @when code != "[us]"
    company_name.name(cn, name)
    movie_companies.note(mc, mc_note)
    @when !contains(mc_note, "(USA)") && ismatch(r"\(200.*\)", mc_note)
    title.title(t, title)
    movie_info.movie(mi, t)
    movie_info.info_type(mi, it1)
    info_type.info(it1, "countries")
    movie_info.info(mi, country)
    @when country in countries
    movie_info_idx.movie(mii, t)
    movie_info_idx.info_type(mii, it2)
    info_type.info(it2, "rating")
    movie_info_idx.info(mii, rating)
    @when rating < "7.0"
    return (name::String, rating::String, title::String)
  end
end

function q22b()
  keywords = ["murder", "murder-in-title", "blood", "violence"]
  kinds = Set(["movie", "episode"])
  countries = Set(["Germany","German","USA","American"])
  @query begin
    keyword in keywords
    keyword.keyword(k, keyword)
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.kind(t, kt)
    kind_type.kind(kt, "movie")
    title.production_year(t, production_year)
    @when production_year > 2009
    title.kind(t, kt)
    kind_type.kind(kt, kind)
    @when kind in kinds
    movie_companies.movie(mc, t)
    movie_companies.company(mc, cn)
    company_name.country_code(cn, code)
    @when code != "[us]"
    company_name.name(cn, name)
    movie_companies.note(mc, mc_note)
    @when !contains(mc_note, "(USA)") && ismatch(r"\(200.\)", mc_note)
    title.title(t, title)
    movie_info.movie(mi, t)
    movie_info.info_type(mi, it1)
    info_type.info(it1, "countries")
    movie_info.info(mi, country)
    @when country in countries
    movie_info_idx.movie(mii, t)
    movie_info_idx.info_type(mii, it2)
    info_type.info(it2, "rating")
    movie_info_idx.info(mii, rating)
    @when rating < "7.0"
    return (name::String, rating::String, title::String)
  end
end

function q22c()
  keywords = ["murder", "murder-in-title", "blood", "violence"]
  kinds = Set(["movie", "episode"])
  countries = Set(["Sweden","Norway","Germany","Denmark","Swedish","Danish","Norwegian","German","USA","American"])
  @query begin
    keyword in keywords
    keyword.keyword(k, keyword)
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.production_year(t, production_year)
    @when production_year > 2005
    title.kind(t, kt)
    kind_type.kind(kt, kind)
    @when kind in kinds
    movie_companies.movie(mc, t)
    movie_companies.company(mc, cn)
    company_name.country_code(cn, code)
    @when code != "[us]"
    company_name.name(cn, name)
    movie_companies.note(mc, mc_note)
    @when !contains(mc_note, "(USA)") && ismatch(r"\(200.*\)", mc_note)
    title.title(t, title)
    movie_info.movie(mi, t)
    movie_info.info_type(mi, it1)
    info_type.info(it1, "countries")
    movie_info.info(mi, country)
    @when country in countries
    movie_info_idx.movie(mii, t)
    movie_info_idx.info_type(mii, it2)
    info_type.info(it2, "rating")
    movie_info_idx.info(mii, rating)
    @when rating < "8.5"
    return (name::String, rating::String, title::String)
  end
end

function q22d()
  keywords = ["murder", "murder-in-title", "blood", "violence"]
  kinds = Set(["movie", "episode"])
  countries = Set(["Sweden","Norway","Germany","Denmark","Swedish","Danish","Norwegian","German","USA","American"])
  @query begin
    keyword in keywords
    keyword.keyword(k, keyword)
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.production_year(t, production_year)
    @when production_year > 2005
    title.kind(t, kt)
    kind_type.kind(kt, kind)
    @when kind in kinds
    movie_companies.movie(mc, t)
    movie_companies.company(mc, cn)
    company_name.country_code(cn, code)
    @when code != "[us]"
    company_name.name(cn, name)
    title.title(t, title)
    movie_info.movie(mi, t)
    movie_info.info_type(mi, it1)
    info_type.info(it1, "countries")
    movie_info.info(mi, country)
    @when country in countries
    movie_info_idx.movie(mii, t)
    movie_info_idx.info_type(mii, it2)
    info_type.info(it2, "rating")
    movie_info_idx.info(mii, rating)
    @when rating < "8.5"
    return (name::String, rating::String, title::String)
  end
end

function q23a()
  @query begin
    kind = "movie"
    comp_cast_type.kind(cct, "complete+verified")
    complete_cast.status(cc, cct)
    complete_cast.movie(cc, t)    
    title.production_year(t, production_year)
    @when production_year > 2000
    title.kind(t, kt)
    kind_type.kind(kt, kind)
    title.title(t, title)
    movie_companies.movie(mc, t)
    movie_companies.company(mc, cn)
    company_name.country_code(cn, "[us]")
    movie_companies.company_type(mc, _)
    movie_info.movie(mi, t)
    movie_info.info_type(mi, it)
    info_type.info(it, "release dates")
    movie_info.note(mi, note)
    @when contains(note, "internet")
    movie_info.info(mi, info)
    @when ismatch(r"USA:.* 199.", info) || ismatch(r"USA:.* 200.", info)
    movie_keyword.movie(_, t)
    return (kind::String, title::String)
  end
end

function q23b()
  keywords = ["nerd", "loner", "alienation", "dignity"]
  @query begin
    kind = "movie"
    keyword in keywords
    keyword.keyword(k, keyword)
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.production_year(t, production_year)
    @when production_year > 2000
    title.kind(t, kt)
    kind_type.kind(kt, kind)
    complete_cast.movie(cc, t)    
    complete_cast.status(cc, cct)
    comp_cast_type.kind(cct, "complete+verified")
    title.title(t, title)
    movie_companies.movie(mc, t)
    movie_companies.company(mc, cn)
    company_name.country_code(cn, "[us]")
    movie_companies.company_type(mc, _)
    movie_info.movie(mi, t)
    movie_info.info_type(mi, it)
    info_type.info(it, "release dates")
    movie_info.note(mi, note)
    @when contains(note, "internet")
    movie_info.info(mi, info)
    @when ismatch(r"USA:.* 200.", info)
    return (kind::String, title::String)
  end
end

function q23c()
  kinds = Set(["movie", "tv movie", "video movie", "video game"])
  @query begin
    comp_cast_type.kind(cct, "complete+verified")
    complete_cast.status(cc, cct)
    complete_cast.movie(cc, t)    
    title.production_year(t, production_year)
    @when production_year > 1990
    title.kind(t, kt)
    kind_type.kind(kt, kind)
    @when kind in kinds
    title.title(t, title)
    movie_companies.movie(mc, t)
    movie_companies.company(mc, cn)
    company_name.country_code(cn, "[us]")
    movie_companies.company_type(mc, _)
    movie_info.movie(mi, t)
    movie_info.info_type(mi, it)
    info_type.info(it, "release dates")
    movie_info.note(mi, note)
    @when contains(note, "internet")
    movie_info.info(mi, info)
    @when ismatch(r"USA:.* 199.", info) || ismatch(r"USA:.* 200.", info)
    movie_keyword.movie(_, t)
    return (kind::String, title::String)
  end
end

function q24a()
  keywords = ["hero", "martial-arts", "hand-to-hand-combat"]
  ci_notes = ["(voice)", "(voice: Japanese version)", "(voice) (uncredited)", "(voice: English version)"]
  @query begin
    ci_note in ci_notes 
    cast_info.note(ci, ci_note)
    cast_info.role(ci, rt)
    role_type.role(rt, "actress")
    cast_info.person(ci, n)
    name.gender(n, "f")
    name.name(n, name)
    @when contains(name, "An")
    aka_name.person(an, n)
    aka_name.name(an, aka_name)
    cast_info.person_role(ci, chn)
    char_name.name(chn, char_name)
    cast_info.movie(ci ,t)    
    title.production_year(t, production_year)
    @when production_year > 2010
    movie_keyword.movie(mk, t)
    movie_keyword.keyword(mk, k)
    keyword.keyword(k, keyword)
    @when keyword in keywords
    title.title(t, title)
    movie_companies.movie(mc, t)
    movie_companies.company(mc, cn)    
    company_name.country_code(cn, "[us]")
    movie_info.movie(mi, t)
    movie_info.info_type(mi, it)
    info_type.info(it, "release dates")
    movie_info.info(mi, info)
    @when ismatch(r"Japan:.*201.", info) || ismatch(r"USA:.*201.", info)
    return (char_name::String, name::String, title::String)
  end
end

function q24b()
  keywords = ["hero", "martial-arts", "hand-to-hand-combat", "computer-animated-movie"]
  ci_notes = ["(voice)", "(voice: Japanese version)", "(voice) (uncredited)", "(voice: English version)"]
  @query begin
    company_name.name(cn, "DreamWorks Animation")
    company_name.country_code(cn, "[us]")
    movie_companies.company(mc, cn)   
    movie_companies.movie(mc, t) 
    title.title(t, title)
    @when startswith(title, "Kung Fu Panda")
    title.production_year(t, production_year)
    @when production_year > 2010
    movie_info.movie(mi, t)
    movie_info.info_type(mi, it)
    info_type.info(it, "release dates")
    movie_info.info(mi, info)
    @when ismatch(r"Japan:.*201.", info) || ismatch(r"USA:.*201.", info)
    cast_info.movie(ci, t)    
    cast_info.role(ci, rt)
    role_type.role(rt, "actress")
    cast_info.note(ci, ci_note)
    @when ci_note in ci_notes 
    cast_info.person(ci, n)
    name.gender(n, "f")
    name.name(n, name)
    @when contains(name, "An")
    aka_name.person(an, n)
    aka_name.name(an, aka_name)
    cast_info.person_role(ci, chn)
    char_name.name(chn, char_name)
    movie_keyword.movie(mk, t)
    movie_keyword.keyword(mk, k)
    keyword.keyword(k, keyword)
    @when keyword in keywords
    return (char_name::String, name::String, title::String)
  end
end

function q25a()
  ci_notes = Set(["(writer)","(head writer)","(written by)","(story)","(story editor)"])
  keywords = Set(["murder", "blood", "gore", "death", "female-nudity"])
  @query begin
    genre = "Horror"
    movie_info.info(mi, genre)
    movie_info.info_type(mi, it1)
    info_type.info(it1, "genres")
    movie_info.movie(mi, t)
    movie_info_idx.movie(mii, t)
    movie_info_idx.info_type(mii, it2)
    info_type.info(it2, "votes")
    movie_info_idx.info(mii, votes)
    title.title(t, title)
    movie_keyword.movie(mk, t)
    movie_keyword.keyword(mk, k)
    keyword.keyword(k, keyword)
    @when keyword in keywords
    cast_info.movie(ci, t)
    cast_info.person(ci, n)
    name.gender(n, "m")
    cast_info.note(ci, ci_note)
    @when ci_note in ci_notes
    name.name(n, name)
    return (genre::String, votes::String, name::String, title::String)
  end
end

function q25b()
  ci_notes = Set(["(writer)","(head writer)","(written by)","(story)","(story editor)"])
  keywords = Set(["murder", "blood", "gore", "death", "female-nudity"])
  @query begin
    genre = "Horror"
    movie_info.info(mi, genre)
    movie_info.info_type(mi, it1)
    info_type.info(it1, "genres")
    movie_info.movie(mi, t)
    title.production_year(t, production_year)
    @when production_year > 2010
    title.title(t, title)
    @when startswith(title, "Vampire")
    movie_info_idx.movie(mii, t)
    movie_info_idx.info_type(mii, it2)
    info_type.info(it2, "votes")
    movie_info_idx.info(mii, votes)
    movie_keyword.movie(mk, t)
    movie_keyword.keyword(mk, k)
    keyword.keyword(k, keyword)
    @when keyword in keywords
    cast_info.movie(ci, t)
    cast_info.person(ci, n)
    name.gender(n, "m")
    cast_info.note(ci, ci_note)
    @when ci_note in ci_notes
    name.name(n, name)
    return (genre::String, votes::String, name::String, title::String)
  end
end

function q25c()
  genres = ["Horror","Action","Sci-Fi","Thriller","Crime","War"]
  ci_notes = Set(["(writer)","(head writer)","(written by)","(story)","(story editor)"])
  keywords = Set(["murder", "violence", "blood", "gore", "death", "female-nudity", "hospital"])
  @query begin
    genre in genres
    movie_info.info(mi, genre)
    movie_info.info_type(mi, it1)
    info_type.info(it1, "genres")
    movie_info.movie(mi, t)
    movie_info_idx.movie(mii, t)
    movie_info_idx.info_type(mii, it2)
    info_type.info(it2, "votes")
    movie_info_idx.info(mii, votes)
    title.title(t, title)
    movie_keyword.movie(mk, t)
    movie_keyword.keyword(mk, k)
    keyword.keyword(k, keyword)
    @when keyword in keywords
    cast_info.movie(ci, t)
    cast_info.person(ci, n)
    name.gender(n, "m")
    cast_info.note(ci, ci_note)
    @when ci_note in ci_notes
    name.name(n, name)
    return (genre::String, votes::String, name::String, title::String)
  end
end

function q26a()
  keywords = Set(["superhero","marvel-comics","based-on-comic","tv-special","fight","violence","magnet","web","claw","laser"])
  @query begin
    keyword in keywords
    keyword.keyword(k, keyword)
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.kind(t, kt)
    kind_type.kind(kt, "movie")
    title.production_year(t, production_year)
    @when production_year > 2000
    title.title(t, title)
    movie_info_idx.movie(mii, t)
    movie_info_idx.info_type(mii, it)
    info_type.info(it, "rating")
    movie_info_idx.info(mii, rating)
    @when rating > "7.0"
    cast_info.movie(ci, t)
    cast_info.person_role(ci, chn)
    char_name.name(chn, char_name)
    @when contains(char_name, "man") || contains(char_name, "Man")
    cast_info.person(ci, n)
    name.name(n, name)
    complete_cast.movie(cc, t)
    complete_cast.subject(cc, cct1)
    comp_cast_type.kind(cct1, "cast")
    complete_cast.status(cc, cct2)
    comp_cast_type.kind(cct2, cct2_kind)
    @when contains(cct2_kind, "complete")
    return (char_name::String, rating::String, name::String, title::String)
  end
end

function q26b()
  keywords = Set(["superhero","marvel-comics","based-on-comic","fight"])
  @query begin
    keyword in keywords
    keyword.keyword(k, keyword)
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.kind(t, kt)
    kind_type.kind(kt, "movie")
    title.production_year(t, production_year)
    @when production_year > 2005
    title.title(t, title)
    movie_info_idx.movie(mii, t)
    movie_info_idx.info_type(mii, it)
    info_type.info(it, "rating")
    movie_info_idx.info(mii, rating)
    @when rating > "8.0"
    cast_info.movie(ci, t)
    cast_info.person_role(ci, chn)
    char_name.name(chn, char_name)
    @when contains(char_name, "man") || contains(char_name, "Man")
    cast_info.person(ci, n)
    name.name(n, name)
    complete_cast.movie(cc, t)
    complete_cast.subject(cc, cct1)
    comp_cast_type.kind(cct1, "cast")
    complete_cast.status(cc, cct2)
    comp_cast_type.kind(cct2, cct2_kind)
    @when contains(cct2_kind, "complete")
    return (char_name::String, rating::String, title::String)
  end
end

function q26c()
  keywords = Set(["superhero","marvel-comics","based-on-comic","tv-special","fight","violence","magnet","web","claw","laser"])
  @query begin
    keyword in keywords
    keyword.keyword(k, keyword)
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.kind(t, kt)
    kind_type.kind(kt, "movie")
    title.production_year(t, production_year)
    @when production_year > 2000
    title.title(t, title)
    movie_info_idx.movie(mii, t)
    movie_info_idx.info_type(mii, it)
    info_type.info(it, "rating")
    movie_info_idx.info(mii, rating)
    cast_info.movie(ci, t)
    cast_info.person_role(ci, chn)
    char_name.name(chn, char_name)
    @when contains(char_name, "man") || contains(char_name, "Man")
    cast_info.person(ci, n)
    name.name(n, name)
    complete_cast.movie(cc, t)
    complete_cast.subject(cc, cct1)
    comp_cast_type.kind(cct1, "cast")
    complete_cast.status(cc, cct2)
    comp_cast_type.kind(cct2, cct2_kind)
    @when contains(cct2_kind, "complete")
    return (char_name::String, rating::String, title::String)
  end
end

function q27a()
  kinds = Set(["cast", "crew"])
  mi_infos = Set(["Sweden", "Germany", "Swedish", "German"])
  @query begin
    keyword.keyword(k, "sequel")
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.production_year(t, production_year)
    @when 1950 <= production_year <= 2000
    title.title(t, title)
    movie_companies.movie(mc, t)
    movie_companies.company_type(mc, ct)
    company_type.kind(ct, "production companies")
    movie_companies.company(mc, cn)
    company_name.name(cn, name)
    @when contains(name, "Film") || contains(name, "Warner")
    company_name.country_code(cn, code)
    @when code != "[pl]"
    movie_link.movie(ml, t)
    movie_link.link_type(ml, lt)
    link_type.link(lt, link)
    @when contains(link, "follow")
    movie_info.movie(mi, t)
    movie_info.info(mi, mi_info)
    @when mi_info in mi_infos
    complete_cast.movie(cc, t)
    complete_cast.subject(cc, cct1)
    comp_cast_type.kind(cct1, cct1_kind)
    @when cct1_kind in kinds
    complete_cast.status(cc, cct2)
    comp_cast_type.kind(cct2, "complete")
    @when @query_not movie_companies.note($mc, _)
    return (name::String, link::String, title::String)
  end
end

function q27b()
  kinds = Set(["cast", "crew"])
  mi_infos = Set(["Sweden", "Germany", "Swedish", "German"])
  @query begin
    keyword.keyword(k, "sequel")
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.production_year(t, Int16(1998))
    title.title(t, title)
    movie_companies.movie(mc, t)
    movie_companies.company_type(mc, ct)
    company_type.kind(ct, "production companies")
    movie_companies.company(mc, cn)
    company_name.name(cn, name)
    @when contains(name, "Film") || contains(name, "Warner")
    company_name.country_code(cn, code)
    @when code != "[pl]"
    movie_link.movie(ml, t)
    movie_link.link_type(ml, lt)
    link_type.link(lt, link)
    @when contains(link, "follow")
    movie_info.movie(mi, t)
    movie_info.info(mi, mi_info)
    @when mi_info in mi_infos
    complete_cast.movie(cc, t)
    complete_cast.subject(cc, cct1)
    comp_cast_type.kind(cct1, cct1_kind)
    @when cct1_kind in kinds
    complete_cast.status(cc, cct2)
    comp_cast_type.kind(cct2, "complete")
    @when @query_not movie_companies.note($mc, _)
    return (name::String, link::String, title::String)
  end
end

function q27c()
  mi_infos = Set(["Sweden","Norway","Germany","Denmark","Swedish","Denish","Norwegian","German","English"])
  @query begin
    keyword.keyword(k, "sequel")
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.production_year(t, production_year)
    @when 1950 <= production_year <= 2010
    title.title(t, title)
    movie_companies.movie(mc, t)
    movie_companies.company_type(mc, ct)
    company_type.kind(ct, "production companies")
    movie_companies.company(mc, cn)
    company_name.name(cn, name)
    @when contains(name, "Film") || contains(name, "Warner")
    company_name.country_code(cn, code)
    @when code != "[pl]"
    movie_link.movie(ml, t)
    movie_link.link_type(ml, lt)
    link_type.link(lt, link)
    @when contains(link, "follow")
    movie_info.movie(mi, t)
    movie_info.info(mi, mi_info)
    @when mi_info in mi_infos
    complete_cast.movie(cc, t)
    complete_cast.subject(cc, cct1)
    comp_cast_type.kind(cct1, "cast")
    complete_cast.status(cc, cct2)
    comp_cast_type.kind(cct2, kind)
    @when startswith(kind, "complete")
    @when @query_not movie_companies.note($mc, _)
    return (name::String, link::String, title::String)
  end
end

function q28a()
  keywords = ["murder", "murder-in-title", "blood", "violence"]
  countries = Set(["Sweden","Norway","Germany","Denmark","Swedish","Denish","Norwegian","German","USA","American"])
  kinds = Set(["movie", "episode"])
  @query begin
    keyword in keywords
    keyword.keyword(k, keyword)
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.kind(t, kt)
    kind_type.kind(kt, kind)
    @when kind in kinds
    title.production_year(t, production_year)
    @when production_year > 2000
    title.title(t, title)
    movie_info.movie(mi, t)
    movie_info.info_type(mi, it1)
    info_type.info(it1, "countries")
    movie_info.info(mi, country)
    @when country in countries
    movie_info_idx.movie(mii, t)
    movie_info_idx.info_type(mii, it2)
    info_type.info(it2, "rating")
    movie_info_idx.info(mii, rating)
    @when rating < "8.5"
    movie_companies.movie(mc, t)
    movie_companies.company(mc, cn)
    company_name.country_code(cn, code)
    @when code != "[us]"
    company_name.name(cn, name)
    movie_companies.note(mc, mc_note)
    @when !contains(mc_note, "(USA)") && ismatch(r"\(200.*\)", mc_note)
    complete_cast.movie(cc, t)
    complete_cast.subject(cc, cct1)
    comp_cast_type.kind(cct1, "crew")
    complete_cast.status(cc, cct2)
    comp_cast_type.kind(cct2, cct2_kind)
    @when cct2_kind != "complete+verified"
    return (name::String, rating::String, title::String)
  end
end

function q28b()
  keywords = ["murder", "murder-in-title", "blood", "violence"]
  countries = Set(["Sweden","Germany","Swedish","German"])
  kinds = Set(["movie", "episode"])
  @query begin
    keyword in keywords
    keyword.keyword(k, keyword)
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.kind(t, kt)
    kind_type.kind(kt, kind)
    @when kind in kinds
    title.production_year(t, production_year)
    @when production_year > 2005
    title.title(t, title)
    movie_info.movie(mi, t)
    movie_info.info_type(mi, it1)
    info_type.info(it1, "countries")
    movie_info.info(mi, country)
    @when country in countries
    movie_info_idx.movie(mii, t)
    movie_info_idx.info_type(mii, it2)
    info_type.info(it2, "rating")
    movie_info_idx.info(mii, rating)
    @when rating > "6.5"
    movie_companies.movie(mc, t)
    movie_companies.company(mc, cn)
    company_name.country_code(cn, code)
    @when code != "[us]"
    company_name.name(cn, name)
    movie_companies.note(mc, mc_note)
    @when !contains(mc_note, "(USA)") && ismatch(r"\(200.*\)", mc_note)
    complete_cast.movie(cc, t)
    complete_cast.subject(cc, cct1)
    comp_cast_type.kind(cct1, "crew")
    complete_cast.status(cc, cct2)
    comp_cast_type.kind(cct2, cct2_kind)
    @when cct2_kind != "complete+verified"
    return (name::String, rating::String, title::String)
  end
end

function q28c()
  keywords = ["murder", "murder-in-title", "blood", "violence"]
  countries = Set(["Sweden","Norway","Germany","Denmark","Swedish","Denish","Norwegian","German","USA","American"])
  kinds = Set(["movie", "episode"])
  @query begin
    keyword in keywords
    keyword.keyword(k, keyword)
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.kind(t, kt)
    kind_type.kind(kt, kind)
    @when kind in kinds
    title.production_year(t, production_year)
    @when production_year > 2005
    title.title(t, title)
    movie_info.movie(mi, t)
    movie_info.info_type(mi, it1)
    info_type.info(it1, "countries")
    movie_info.info(mi, country)
    @when country in countries
    movie_info_idx.movie(mii, t)
    movie_info_idx.info_type(mii, it2)
    info_type.info(it2, "rating")
    movie_info_idx.info(mii, rating)
    @when rating < "8.5"
    movie_companies.movie(mc, t)
    movie_companies.company(mc, cn)
    company_name.country_code(cn, code)
    @when code != "[us]"
    company_name.name(cn, name)
    movie_companies.note(mc, mc_note)
    @when !contains(mc_note, "(USA)") && ismatch(r"\(200.*\)", mc_note)
    complete_cast.movie(cc, t)
    complete_cast.subject(cc, cct1)
    comp_cast_type.kind(cct1, "cast")
    complete_cast.status(cc, cct2)
    comp_cast_type.kind(cct2, "complete")
    return (name::String, rating::String, title::String)
  end
end

function q29a()
  ci_notes = Set(["(voice)", "(voice) (uncredited)", "(voice: English version)"])
  @query begin
    char_name = "Queen"
    title = "Shrek 2"
    title.title(t, title)
    title.production_year(t, production_year)
    @when 2000 <= production_year <= 2010
    cast_info.movie(ci, t)
    cast_info.person_role(ci, chn)
    char_name.name(chn, char_name)
    cast_info.person(ci, n)    
    name.gender(n, "f")
    name.name(n, name)
    @when contains(name, "An")
    cast_info.role(ci, rt)
    role_type.role(rt, "actress")
    cast_info.note(ci, ci_note)
    @when ci_note in ci_notes
    movie_companies.movie(mc, t)
    movie_companies.company(mc, cn)
    company_name.country_code(cn, "[us]")
    movie_keyword.movie(mk, t)
    movie_keyword.keyword(mk, k)
    keyword.keyword(k, "computer-animation")
    movie_info.movie(mi, t)
    movie_info.info_type(mi, it)
    info_type.info(it, "release dates")
    movie_info.info(mi, mi_info)
    @when ismatch(r"Japan:.*200.", mi_info) || ismatch(r"USA:.*200.", mi_info)
    complete_cast.movie(cc, t)
    complete_cast.subject(cc, cct1)
    comp_cast_type.kind(cct1, "cast")
    complete_cast.status(cc, cct2)
    comp_cast_type.kind(cct2, "complete+verified")
    aka_name.person(an, n)
    person_info.person(pi, n)
    person_info.info_type(pi, it2)
    info_type.info(it2, "trivia")
    return (char_name::String, name::String, title::String)
  end
end

function q29b()
  ci_notes = Set(["(voice)", "(voice) (uncredited)", "(voice: English version)"])
  @query begin
    char_name = "Queen"
    title = "Shrek 2"
    title.title(t, title)
    title.production_year(t, production_year)
    @when 2000 <= production_year <= 2005
    cast_info.movie(ci, t)
    cast_info.person_role(ci, chn)
    char_name.name(chn, char_name)
    cast_info.person(ci, n)    
    name.gender(n, "f")
    name.name(n, name)
    @when contains(name, "An")
    cast_info.role(ci, rt)
    role_type.role(rt, "actress")
    cast_info.note(ci, ci_note)
    @when ci_note in ci_notes
    movie_companies.movie(mc, t)
    movie_companies.company(mc, cn)
    company_name.country_code(cn, "[us]")
    movie_keyword.movie(mk, t)
    movie_keyword.keyword(mk, k)
    keyword.keyword(k, "computer-animation")
    movie_info.movie(mi, t)
    movie_info.info_type(mi, it)
    info_type.info(it, "release dates")
    movie_info.info(mi, mi_info)
    @when ismatch(r"USA:.*200.", mi_info)
    complete_cast.movie(cc, t)
    complete_cast.subject(cc, cct1)
    comp_cast_type.kind(cct1, "cast")
    complete_cast.status(cc, cct2)
    comp_cast_type.kind(cct2, "complete+verified")
    aka_name.person(an, n)
    person_info.person(pi, n)
    person_info.info_type(pi, it2)
    info_type.info(it2, "height")
    return (char_name::String, name::String, title::String)
  end
end

function q29c()
  ci_notes = Set(["(voice)", "(voice: Japanese version)", "(voice) (uncredited)", "(voice: English version)"])
  @query begin
    keyword.keyword(k, "computer-animation")
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t)
    title.title(t, title)
    title.production_year(t, production_year)
    @when 2000 <= production_year <= 2010
    cast_info.movie(ci, t)
    cast_info.person(ci, n)    
    cast_info.role(ci, rt)
    role_type.role(rt, "actress")
    name.gender(n, "f")
    name.name(n, name)
    @when contains(name, "An")
    cast_info.person_role(ci, chn)
    char_name.name(chn, char_name)
    cast_info.note(ci, ci_note)
    @when ci_note in ci_notes
    movie_companies.movie(mc, t)
    movie_companies.company(mc, cn)
    company_name.country_code(cn, "[us]")
    movie_info.movie(mi, t)
    movie_info.info_type(mi, it)
    info_type.info(it, "release dates")
    movie_info.info(mi, mi_info)
    @when ismatch(r"Japan:.*200.", mi_info) || ismatch(r"USA:.*200.", mi_info)
    complete_cast.movie(cc, t)
    complete_cast.subject(cc, cct1)
    comp_cast_type.kind(cct1, "cast")
    complete_cast.status(cc, cct2)
    comp_cast_type.kind(cct2, "complete+verified")
    aka_name.person(an, n)
    person_info.person(pi, n)
    person_info.info_type(pi, it2)
    info_type.info(it2, "trivia")
    return (char_name::String, name::String, title::String)
  end
end

function q30a()
  genres = ["Horror","Thriller"]
  ci_notes = Set(["(writer)","(head writer)","(written by)","(story)","(story editor)"])
  keywords = Set(["murder", "violence", "blood", "gore", "death", "female-nudity", "hospital"])
  kinds = Set(["cast", "crew"])
  @query begin
    genre in genres
    movie_info.info(mi, genre)
    movie_info.info_type(mi, it1)
    info_type.info(it1, "genres")
    movie_info.movie(mi, t)
    title.production_year(t, production_year)
    @when production_year > 2000
    movie_info_idx.movie(mii, t)
    movie_info_idx.info_type(mii, it2)
    info_type.info(it2, "votes")
    movie_info_idx.info(mii, votes)
    title.title(t, title)
    movie_keyword.movie(mk, t)
    movie_keyword.keyword(mk, k)
    keyword.keyword(k, keyword)
    @when keyword in keywords
    cast_info.movie(ci, t)
    cast_info.person(ci, n)
    name.gender(n, "m")
    cast_info.note(ci, ci_note)
    @when ci_note in ci_notes
    name.name(n, name)  
    complete_cast.movie(cc, t)
    complete_cast.subject(cc, cct1)
    comp_cast_type.kind(cct1, kind)
    @when kind in kinds
    complete_cast.status(cc, cct2)
    comp_cast_type.kind(cct2, "complete+verified")
    return (genre::String, votes::String, name::String, title::String)
  end
end

function q30b()
  genres = ["Horror","Thriller"]
  ci_notes = Set(["(writer)","(head writer)","(written by)","(story)","(story editor)"])
  keywords = Set(["murder", "violence", "blood", "gore", "death", "female-nudity", "hospital"])
  kinds = Set(["cast", "crew"])
  @query begin
    genre in genres
    movie_info.info(mi, genre)
    movie_info.info_type(mi, it1)
    info_type.info(it1, "genres")
    movie_info.movie(mi, t)
    title.production_year(t, production_year)
    @when production_year > 2000
    title.title(t, title)
    @when contains(title, "Freddy") || contains(title, "Jason") || startswith(title, "Saw")
    movie_info_idx.movie(mii, t)
    movie_info_idx.info_type(mii, it2)
    info_type.info(it2, "votes")
    movie_info_idx.info(mii, votes)
    movie_keyword.movie(mk, t)
    movie_keyword.keyword(mk, k)
    keyword.keyword(k, keyword)
    @when keyword in keywords
    cast_info.movie(ci, t)
    cast_info.person(ci, n)
    name.gender(n, "m")
    cast_info.note(ci, ci_note)
    @when ci_note in ci_notes
    name.name(n, name)  
    complete_cast.movie(cc, t)
    complete_cast.subject(cc, cct1)
    comp_cast_type.kind(cct1, kind)
    @when kind in kinds
    complete_cast.status(cc, cct2)
    comp_cast_type.kind(cct2, "complete+verified")
    return (genre::String, votes::String, name::String, title::String)
  end
end

function q30c()
  genres = ["Horror","Action","Sci-Fi","Thriller","Crime","War"]
  ci_notes = Set(["(writer)","(head writer)","(written by)","(story)","(story editor)"])
  keywords = Set(["murder", "violence", "blood", "gore", "death", "female-nudity", "hospital"])
  @query begin
    genre in genres
    movie_info.info(mi, genre)
    movie_info.info_type(mi, it1)
    info_type.info(it1, "genres")
    movie_info.movie(mi, t)
    movie_info_idx.movie(mii, t)
    movie_info_idx.info_type(mii, it2)
    info_type.info(it2, "votes")
    movie_info_idx.info(mii, votes)
    title.title(t, title)
    movie_keyword.movie(mk, t)
    movie_keyword.keyword(mk, k)
    keyword.keyword(k, keyword)
    @when keyword in keywords
    cast_info.movie(ci, t)
    cast_info.person(ci, n)
    name.gender(n, "m")
    cast_info.note(ci, ci_note)
    @when ci_note in ci_notes
    name.name(n, name)  
    complete_cast.movie(cc, t)
    complete_cast.subject(cc, cct1)
    comp_cast_type.kind(cct1, "cast")
    complete_cast.status(cc, cct2)
    comp_cast_type.kind(cct2, "complete+verified")
    return (genre::String, votes::String, name::String, title::String)
  end
end

function q31a()
  genres = ["Horror","Thriller"]
  ci_notes = Set(["(writer)","(head writer)","(written by)","(story)","(story editor)"])
  keywords = Set(["murder", "violence", "blood", "gore", "death", "female-nudity", "hospital"])
  @query begin
    genre in genres
    movie_info.info(mi, genre)
    movie_info.info_type(mi, it1)
    info_type.info(it1, "genres")
    movie_info.movie(mi, t)
    movie_companies.movie(mc, t)
    movie_companies.company(mc, cn)
    company_name.name(cn, company_name)
    @when startswith(company_name, "Lionsgate")
    movie_info_idx.movie(mii, t)
    movie_info_idx.info_type(mii, it2)
    info_type.info(it2, "votes")
    movie_info_idx.info(mii, votes)
    title.title(t, title)
    movie_keyword.movie(mk, t)
    movie_keyword.keyword(mk, k)
    keyword.keyword(k, keyword)
    @when keyword in keywords
    cast_info.movie(ci, t)
    cast_info.person(ci, n)
    name.gender(n, "m")
    cast_info.note(ci, ci_note)
    @when ci_note in ci_notes
    name.name(n, name)  
    return (genre::String, votes::String, name::String, title::String)
  end
end

function q31b()
  genres = ["Horror","Thriller"]
  ci_notes = Set(["(writer)","(head writer)","(written by)","(story)","(story editor)"])
  keywords = Set(["murder", "violence", "blood", "gore", "death", "female-nudity", "hospital"])
  @query begin
    genre in genres
    movie_info.info(mi, genre)
    movie_info.info_type(mi, it1)
    info_type.info(it1, "genres")
    movie_info.movie(mi, t)
    title.production_year(t, production_year)
    @when production_year > 2000
    title.title(t, title)
    @when contains(title, "Freddy") || contains(title, "Jason") || startswith(title, "Saw")
    movie_companies.movie(mc, t)
    movie_companies.company(mc, cn)
    company_name.name(cn, company_name)
    @when startswith(company_name, "Lionsgate")
    movie_companies.note(mc, mc_note)
    @when contains(mc_note, "(Blu-ray)")
    movie_info_idx.movie(mii, t)
    movie_info_idx.info_type(mii, it2)
    info_type.info(it2, "votes")
    movie_info_idx.info(mii, votes)
    title.title(t, title)
    movie_keyword.movie(mk, t)
    movie_keyword.keyword(mk, k)
    keyword.keyword(k, keyword)
    @when keyword in keywords
    cast_info.movie(ci, t)
    cast_info.person(ci, n)
    name.gender(n, "m")
    cast_info.note(ci, ci_note)
    @when ci_note in ci_notes
    name.name(n, name)  
    return (genre::String, votes::String, name::String, title::String)
  end
end

function q31c()
  genres = Set(["Horror","Action","Sci-Fi","Thriller","Crime","War"])
  ci_notes = Set(["(writer)","(head writer)","(written by)","(story)","(story editor)"])
  keywords = Set(["murder", "violence", "blood", "gore", "death", "female-nudity", "hospital"])
  @query begin
    @when startswith(company_name, "Lionsgate")
    company_name.name(cn, company_name)
    movie_companies.company(mc, cn)
    movie_companies.movie(mc, t)
    movie_info.movie(mi, t)
    info_type.info(it1, "genres")
    movie_info.info_type(mi, it1)
    movie_info.info(mi, genre)
    @when genre in genres
    movie_info_idx.movie(mii, t)
    movie_info_idx.info_type(mii, it2)
    info_type.info(it2, "votes")
    movie_info_idx.info(mii, votes)
    title.title(t, title)
    movie_keyword.movie(mk, t)
    movie_keyword.keyword(mk, k)
    keyword.keyword(k, keyword)
    @when keyword in keywords
    cast_info.movie(ci, t)
    cast_info.person(ci, n)
    cast_info.note(ci, ci_note)
    @when ci_note in ci_notes
    name.name(n, name)  
    return (genre::String, votes::String, name::String, title::String)
  end
end

function q32a()
  @query begin
    keyword.keyword(k, "10,000-mile-club")
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t1)
    title.title(t1, title1)
    movie_link.movie(ml, t1)
    movie_link.link_type(ml, lt)
    link_type.link(lt, link)
    movie_link.linked_movie(ml, t2)
    title.title(t2, title2)
    return (link::String, title1::String, title2::String)
  end
end

function q32b()
  @query begin
    keyword.keyword(k, "character-name-in-title")
    movie_keyword.keyword(mk, k)
    movie_keyword.movie(mk, t1)
    title.title(t1, title1)
    movie_link.movie(ml, t1)
    movie_link.link_type(ml, lt)
    link_type.link(lt, link)
    movie_link.linked_movie(ml, t2)
    title.title(t2, title2)
    return (link::String, title1::String, title2::String)
  end
end

function q33a()
  links = ["sequel", "follows", "followed by"]
  @query begin
    info_type.info(it, "rating")
    kind_type.kind(kt, "tv series")
    link in links
    link_type.link(lt, link)
    movie_link.link_type(ml, lt)
    movie_link.linked_movie(ml, t2)
    title.kind(t2, kt)
    title.production_year(t2, production_year)
    @when 2005 <= production_year <= 2008
    title.title(t2, title2)
    movie_info_idx.movie(mii2, t2)
    movie_info_idx.info_type(mii2, it)
    movie_info_idx.info(mii2, rating2)
    @when rating2 < "3.0"
    movie_companies.movie(mc2, t2)
    movie_companies.company(mc2, cn2)
    company_name.name(cn2, name2)
    movie_link.movie(ml, t1)
    title.kind(t1, kt)
    title.title(t1, title1)
    movie_companies.movie(mc1, t1)
    movie_companies.company(mc1, cn1)
    company_name.country_code(cn1, "[us]")
    company_name.name(cn1, name1)
    movie_info_idx.info_type(mii1, it)
    movie_info_idx.movie(mii1, t1)
    movie_info_idx.info(mii1, rating1)
    return (name1::String, name2::String, rating1::String, rating2::String, title1::String, title2::String)
  end
end

function q33b()
  @query begin
    company_name.country_code(cn1, "[nl]")
    movie_companies.company(mc1, cn1)
    movie_companies.movie(mc1, t1)
    title.kind(t1, kt)
    title.title(t1, title1)
    movie_link.movie(ml, t1)
    link_type.link(lt, link)
    @when contains(link, "follow")
    info_type.info(it, "rating")
    kind_type.kind(kt, "tv series")
    movie_link.link_type(ml, lt)
    movie_link.linked_movie(ml, t2)
    title.kind(t2, kt)
    title.production_year(t2, Int16(2007))
    title.title(t2, title2)
    movie_info_idx.movie(mii2, t2)
    movie_info_idx.info_type(mii2, it)
    movie_info_idx.info(mii2, rating2)
    @when rating2 < "3.0"
    movie_companies.movie(mc2, t2)
    movie_companies.company(mc2, cn2)
    company_name.name(cn2, name2)
    company_name.name(cn1, name1)
    movie_info_idx.info_type(mii1, it)
    movie_info_idx.movie(mii1, t1)
    movie_info_idx.info(mii1, rating1)
    return (name1::String, name2::String, rating1::String, rating2::String, title1::String, title2::String)
  end
end
    
function q33c()
  kt_kinds = ["tv series", "episode"]
  links = ["sequel", "follows", "followed by"]
  @query begin
    info_type.info(it, "rating")
    kt_kind in kt_kinds
    kind_type.kind(kt, kt_kind)
    link in links
    link_type.link(lt, link)
    movie_link.link_type(ml, lt)
    movie_link.linked_movie(ml, t2)
    title.kind(t2, kt)
    title.production_year(t2, production_year)
    @when 2000 <= production_year <= 2010
    title.title(t2, title2)
    movie_info_idx.movie(mii2, t2)
    movie_info_idx.info_type(mii2, it)
    movie_info_idx.info(mii2, rating2)
    @when rating2 < "3.5"
    movie_companies.movie(mc2, t2)
    movie_companies.company(mc2, cn2)
    company_name.name(cn2, name2)
    movie_link.movie(ml, t1)
    title.kind(t1, kt)
    title.title(t1, title1)
    movie_companies.movie(mc1, t1)
    movie_companies.company(mc1, cn1)
    company_name.country_code(cn1, code)
    @when code != "[us]"
    company_name.name(cn1, name1)
    movie_info_idx.info_type(mii1, it)
    movie_info_idx.movie(mii1, t1)
    movie_info_idx.info(mii1, rating1)
    return (name1::String, name2::String, rating1::String, rating2::String, title1::String, title2::String)
  end
end

function q33c_factored()
  rating_type = @query begin 
    info_type.info(it, "rating")
    return (it::Int8,)
  end
  kind_types = @query begin
    kt_kind in ["tv series", "episode"]
    kind_type.kind(kt, kt_kind)
    return (kt::Int8,)
  end
  movie_links = @query begin
    link in ["sequel", "follows", "followed by"]
    link_type.link(lt, link)
    movie_link.link_type(ml, lt)
    return (ml::Int16,)
  end
  linked_movies = @query begin
    rating_type(it)
    movie_links(ml)
    movie_link.linked_movie(ml, t2)
    title.kind(t2, kt)
    kind_types(kt)
    title.production_year(t2, production_year)
    @when 2000 <= production_year <= 2010
    movie_info_idx.movie(mii2, t2)
    movie_info_idx.info_type(mii2, it)
    movie_info_idx.info(mii2, rating2)
    @when rating2 < "3.5"
    return (ml::Int16, t2::Int32, rating2::String)
  end
  linking_movies = @query begin
    rating_type(it)
    linked_movies(ml, _, _)
    movie_link.movie(ml, t1)
    title.kind(t1, kt)
    kind_types(kt)
    movie_companies.movie(mc1, t1)
    movie_companies.company(mc1, cn1)
    company_name.country_code(cn1, code)
    @when code != "[us]"
    title.title(t1, title1)
    movie_info_idx.movie(mii1, t1)
    movie_info_idx.info_type(mii1, it)
    movie_info_idx.info(mii1, rating1)
    company_name.name(cn1, name1)
    return (ml::Int16, t1::Int32, name1::String, rating1::String, title1::String)
  end
  @query begin
    linking_movies(ml, t1, name1, rating1, title1)
    linked_movies(ml, t2, rating2)
    title.title(t2, title2)
    movie_companies.movie(mc2, t2)
    movie_companies.company(mc2, cn2)
    company_name.name(cn2, name2)
    return (name1::String, name2::String, rating1::String, rating2::String, title1::String, title2::String)
  end
end

function query_names(nums=1:33)
  query_names = []
  for num in nums
    for char in "abcdef"
      query_name = "$num$char"
      if isdefined(Symbol("q$query_name"))
        push!(query_names, query_name)
      end
    end
  end
  query_names
end

function test(qs = query_names())
  for query_name in qs
    results_imp = eval(Symbol("q$(query_name)"))()
    query = rstrip(readline("../job/$(query_name).sql"))
    query = query[1:(length(query)-1)] # drop ';' at end
    query = replace(query, "MIN", "")
    query = "copy ($query) to '/tmp/results.csv' with CSV DELIMITER ',';"
    run(`sudo -u postgres psql -c $query`)
    frame = DataFrames.readtable(open("/tmp/results.csv"), header=false, eltypes=[eltype(c) for c in results_imp.columns])
    num_columns = length(results_imp)
    @show query_name now()
    if length(frame.columns) == 0
      @test length(results_imp[1]) == 0
    else
      results_pg = Relation(tuple((frame[ix].data for ix in 1:num_columns)...), num_columns)
      (imp_only, pg_only) = Data.diff(results_imp, results_pg)
      imp_only = map((c) -> c[1:min(10, length(c))], imp_only)
      pg_only = map((c) -> c[1:min(10, length(c))], pg_only)
      @test imp_only == pg_only # ie both empty - but @test will print both otherwise
    end
  end
end

function test_sqlite(qs = query_names())
  db = SQLite.DB("../imdb/imdb.sqlite")
  SQLite.execute!(db, "PRAGMA cache_size = -1000000000;")
  SQLite.execute!(db, "PRAGMA temp_store = memory;")
  for query_name in qs
    results_imp = eval(Symbol("q$(query_name)"))()
    query = rstrip(readline("../job/$(query_name).sql"))
    query = query[1:(length(query)-1)] # drop ';' at end
    query = replace(query, "MIN", "")
    frame = SQLite.query(db, query)
    num_columns = length(results_imp)
    if length(frame.columns) == 0
      correct = (length(results_imp[1]) == 0)
    else
      results_sqlite = Relation(tuple((convert(typeof(results_imp.columns[ix]), frame[ix].values) for ix in 1:num_columns)...), num_columns)
      (imp_only, sqlite_only) = Data.diff(results_imp, results_sqlite)
      imp_only = map((c) -> c[1:min(10, length(c))], imp_only)
      sqlite_only = map((c) -> c[1:min(10, length(c))], sqlite_only)
      correct = (imp_only == sqlite_only)
    end
    println("$query_name $correct")
  end
end

function bench(qs = query_names())
  println("query_name imp pg")
  for (query_name, imp, pg) in zip(qs, bench_imp(qs), bench_pg(qs))
    println("$query_name $imp $pg $sqlite")
  end
end

function bench_imp(qs = query_names())
  medians = []
  for query_name in qs
    @show query_name now()
    eval(Symbol("q$(query_name)"))()
    trial = @show @benchmark $(eval(Symbol("q$(query_name)")))() evals=3
    push!(medians, median(trial.times) / 1000000)
  end
  medians
end

function bench_sqlite(qs = query_names())
  db = SQLite.DB("../imdb/imdb.sqlite")
  SQLite.execute!(db, "PRAGMA cache_size = -1000000000;")
  SQLite.execute!(db, "PRAGMA temp_store = memory;")
  @show :overhead
  query = "select * from movie_info limit 1"
  @show @benchmark SQLite.query($db, $query)
  medians = []
  for query_name in qs
    @show query_name now()
    query = rstrip(readline("../job/$(query_name).sql"))
    SQLite.query(db, query)
    trial = @show @benchmark SQLite.query($db, $query) evals=3
    push!(medians, median(trial.times) / 1000000)
  end
  medians
end

function bench_pg(qs = query_names())
  medians = []
  for query_name in qs
    query = rstrip(readline("../job/$(query_name).sql"))
    query = query[1:(length(query)-1)] # drop ';' at end
    bench = "explain analyze $query"
    cmd = `sudo -u postgres psql -c $bench`
    times = Float64[]
    @show query_name now()
    readstring(cmd)
    @show @benchmark push!($times, parse(Float64, match(r"Execution time: (\S*) ms", readstring($cmd))[1])) evals=3
    push!(medians, median(times))
  end
  medians
end

end
