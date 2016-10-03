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
    @show query_name
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

function bench(qs = query_names())
  for (query_name, imp, sqlite, pg) in zip(qs, bench_imp(qs), bench_sqlite(qs), bench_pg(qs))
    # if (imp > sqlite) || (imp > pg)
      println("$query_name imp=$imp pg=$pg sqlite=$sqlite")
    # end
  end
end

function bench_imp(qs = query_names())
  medians = []
  for query_name in qs
    @show query_name
    eval(Symbol("q$(query_name)"))()
    trial = @show @benchmark $(eval(Symbol("q$(query_name)")))()
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
    @show query_name
    query = rstrip(readline("../job/$(query_name).sql"))
    SQLite.query(db, query)
    trial = @show @benchmark SQLite.query($db, $query)
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
    @show query_name
    readstring(cmd)
    @show @benchmark push!($times, parse(Float64, match(r"Execution time: (\S*) ms", readstring($cmd))[1]))
    push!(medians, median(times))
  end
  medians
end

end
